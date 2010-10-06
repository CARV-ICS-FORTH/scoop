(*
 *
 * Copyright (c) 2010, 
 *  Polyvios Pratikakis <polyvios@ics.forth.gr>
 *  Foivos Zakkak        <zakkak@ics.forth.gr>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)

open Pretty
open Cil
open Lockutil
module E = Errormsg
module H = Hashtbl
module S = Str
module L = List
module CG = Callgraph
module Lprof = Lockprofile

let debug = ref false
let stats = ref false
let thread = ref false
let out_name = ref "final"
let queue_size = ref "16"
let currentFunction = ref dummyFunDec

let options =
  [
    "--debug-tpctool",
      Arg.Set(debug),
      " S2S: Print debugging information.";

    "--out-name",
      Arg.String(fun s -> out_name := s),
      " S2S: Specify the output files' prefix. e.g. (default: final) will produce final.c and final_func.c";

    "--queue-size",
      Arg.String(fun s -> queue_size := s),
      " S2S: Specify the queue size of the spes";

    "--with-stats",
      Arg.Set(stats),
      " S2S: Enable code for statistics, for use with -DSTATISTICS";

    "--threaded",
      Arg.Set(thread),
      " S2S: Generate thread safe code, for use with -DTPC_MULTITHREADED";
  ]


type spu_task =
  string * (string * arg_t * string * string * string) list
and arg_t =
    In
  | Out
  | InOut
  | SIn
  | SOut
  | SInOut

(* create 1 global list (the spe output file) *)
let spu_tasks = ref []
(* create a ref to the input file *)
(* let in_file = ref dummyFile *)
(* create a ref to the new spu file *)
let spu_file = ref dummyFile
(* create a ref to the new ppe file *)
let ppc_file = ref dummyFile
(* keeps the current funcid for the new tpc_function *)
let func_id = ref 0

(* define the ppu_vector *)
let ppu_vector = Attr("altivec", [ACons("vector__", [])])

(* find the function definition of variable <name> in file f *)
exception Found_fundec of fundec
let find_function_fundec (f: file) (name: string) : fundec =
  let findit = function
    | GFun(fd, _) when fd.svar.vname = name -> raise (Found_fundec fd)
    | _ -> ()
  in
  try
    iterGlobals f findit;
    raise Not_found
  with Found_fundec v -> v

(* searches a global list for a function definition with name <name> *)
let find_function_fundec_g (g: global list) (name: string) : fundec =
  let findit = function
    | GFun(fd, _) when fd.svar.vname = name -> raise (Found_fundec fd)
    | _ -> ()
  in
  try
    List.iter findit g;
    raise Not_found
  with Found_fundec v -> v

(* find the function signature for <name> function *)
exception Found_sign of varinfo
let find_function_sign (f: file) (name: string) : varinfo =
  let findit = function
    | GVarDecl(vi, _) when vi.vname = name -> raise (Found_sign vi)
    | _ -> ()
  in
  try
    iterGlobals f findit;
    raise Not_found
  with Found_sign v -> v

(* find the (first) typedef for type "name" in file f *)
exception Found_type of typ
let find_type (f: file) (name: string) : typ =
  let findit = function
    | GType(ti, _) when ti.tname = name -> raise (Found_type (TNamed(ti, [])))
    | _ -> ()
  in
  try
    iterGlobals f findit;
    raise Not_found
  with Found_type t -> t

(* find the struct or union named struct/union <name> *)
let find_tcomp (f: file) (name: string) : typ =
  let findit = function
    | GCompTag(ci, _) when ci.cname = name -> raise (Found_type (TComp(ci, [])))
    | _ -> ()
  in
  try
    iterGlobals f findit;
    raise Not_found
  with Found_type t -> t

(* find the variable named <name> in file <f> *)
exception Found_var of varinfo
let find_global_var (f: file) (name: string) : varinfo =
  let findit = function
    | GVarDecl(vi, _) when vi.vname = name -> raise (Found_var vi)
    | GVar(vi, _, _) when vi.vname = name -> raise (Found_var vi)
    | _ -> ()
  in
  try
    iterGlobals f findit;
    ignore(E.warn  "\"%s\" is not globally defined in %s\n" name f.fileName);
    raise Not_found
  with Found_var v -> v

(* find the variable named <name> in the formals of <fd> *)
let find_formal_var (fd: fundec) (name: string) : varinfo =
  let findit = function
    | vi when vi.vname = name -> raise (Found_var vi)
    | _ -> ()
  in
  try
    List.iter findit fd.sformals;
    ignore(E.warn  "\"%s\" is not a formal of %s\n" name fd.svar.vname);
    raise Not_found
  with Found_var v -> v

(* find the variable named <name> in the locals of <fd> *)
let find_local_var (fd: fundec) (name: string) : varinfo =
  let findit = function
    | vi when vi.vname = name -> raise (Found_var vi)
    | _ -> ()
  in
  try
    List.iter findit fd.slocals;
    ignore(E.warn "\"%s\" is not a local of %s" name fd.svar.vname);
    raise Not_found
  with Found_var v -> v

(* find the variable named <name> in fundec <fd>
   else look if it's a global of file <f> *)
let find_scoped_var (fd: fundec) (f: file) (name: string) : varinfo =
  try
    find_local_var fd name
  with Not_found -> 
      ( try
        find_formal_var fd name
      with Not_found -> 
          ( try
            find_global_var f name
          with Not_found -> (
              ignore(E.warn "\"%s\" is not accessible from %s" name fd.svar.vname);
              raise Not_found)
          )
      )

(* find the enum named <name> in file f *)
exception Found_enum of enuminfo
let find_enum (f: file) (name: string) : enuminfo =
  let findit = function
    | GEnumTag(ei, _) when ei.ename = name -> raise (Found_enum ei)
    | _ -> ()
  in 
  try
    iterGlobals f findit;
    raise Not_found
  with Found_enum ei -> ei

(* find the variable named <name> in fd's locals *)
let findLocal (fd: fundec) (name: string) : varinfo =
  let findit = function
    | vi when vi.vname = name -> raise (Found_var vi)
    | _ -> ()
  in
  try
    List.iter findit fd.slocals;
    ignore(E.error "\"%s\" is not a local of %s" name fd.svar.vname);
    raise Not_found
  with Found_var v -> v

(* Converts the strings describing the argument type to arg_t *)
let translate_arg (arg: string) (strided: bool) : arg_t =
  if (strided) then
    match arg with
        "in" -> SIn
      | "out" -> SOut
      | "inout" -> SInOut
      | _ -> ignore(E.error "Only in/out/inout are allowed"); assert false
  else
    match arg with
        "input" -> In
      | "output" -> Out
      | "inout" -> InOut
      | _ -> ignore(E.error "Only in/out/inout are allowed"); assert false

(* check if an arguments type is stride *)
let is_strided (arg: arg_t) : bool =
   match arg with
    | SIn
    | SOut
    | SInOut -> true
    | _ -> false

(* check if an arguments type is out *)
let is_out_arg (arg: arg_t) : bool =
   match arg with
    | Out
    | InOut
    | SOut
    | SInOut -> true
    | _ -> false

(* returns the argument type from an argument description 
  (string * arg_t * string * string *string ) *)
let get_arg_type (arg: (string * arg_t * string * string *string )) : arg_t =
  match arg with
    (_, arg_type ,_ ,_ ,_) -> arg_type

let doArgument (i: int) (local_arg: lval) (avail_task: lval) (tmpvec: lval) (fd: fundec)
 (arg: (string * arg_t * string * string * string)) : instr list = begin
  let arg_size = Lval( var (find_formal_var fd ("arg_size"^(string_of_int i)))) in
  let arg_addr = Lval( var (List.nth fd.sformals i)) in
  let arg_type = get_arg_type arg in
  let il = ref [] in
  (* tmpvec = (volatile vector unsigned char * )&avail_task->arguments[i]; *)
  if (!stats) then begin
    let total_bytes = var (find_local_var fd "total_bytes") in
    let arg_bytes = var (find_local_var fd "arg_bytes") in
    if (is_strided arg_type) then
      let arg_elsz = Lval( var (find_formal_var fd ("arg_elsz"^(string_of_int i)))) in
      let arg_els = Lval( var (find_formal_var fd ("arg_els"^(string_of_int i)))) in
      (* arg_bytes = TPC_EXTRACT_STRIDEARG_ELEMSZ(arg_size)*TPC_EXTRACT_STRIDEARG_ELEMS(arg_size); *)
      il := Set(arg_bytes, BinOp(Mult, arg_els, arg_elsz, intType), locUnknown)::!il
    else begin
      (* arg_bytes = arg_size; *)
      il := Set(arg_bytes, arg_size, locUnknown)::!il
    end;
    (* total_bytes += ( arg_bytes<< TPC_IS_INOUTARG(arg_flag)); *)
    let total_size = 
      if (is_out_arg arg_type) then begin
        BinOp(PlusA, Lval(total_bytes), BinOp(Mult, integer 2, Lval(arg_bytes), intType), intType)
      end else begin
        BinOp(PlusA, Lval(total_bytes), Lval(arg_bytes), intType)
      end
    in
    il := Set(total_bytes, total_size, locUnknown)::!il
  end;
  let vector_uchar_p = TPtr(TInt(IUChar, [Attr("volatile", [])]), [ppu_vector]) in
  let av_task_arg = mkPtrFieldAccess avail_task "arguments" in
  let av_task_arg_idx = addOffsetLval (Index(integer i,NoOffset)) av_task_arg in
  il := Set(tmpvec, CastE(vector_uchar_p, AddrOf(av_task_arg_idx)) , locUnknown)::!il;

  (*TODO: if !stats then
     if( TPC_IS_STRIDEARG(arg_flag) ) {
       arg_bytes = TPC_EXTRACT_STRIDEARG_ELEMSZ(arg_size)*TPC_EXTRACT_STRIDEARG_ELEMS(arg_size);
     } else {
       arg_bytes = arg_size;
     }
     total_bytes += ( arg_bytes<< TPC_IS_INOUTARG(arg_flag));*)

  (* local_arg.eal = (uint32_t)(arg_addr64); *)
  let eal = mkFieldAccess local_arg "eal" in
  il := Set(eal, CastE(find_type !spu_file "uint32_t", arg_addr), locUnknown)::!il;
  let size = mkFieldAccess local_arg "size" in
  if (is_strided arg_type) then begin
    let arg_elsz = Lval( var (find_formal_var fd ("arg_elsz"^(string_of_int i)))) in
    let arg_els = Lval( var (find_formal_var fd ("arg_els"^(string_of_int i)))) in
    (* #define TPC_BUILD_STRIDEARG(elems, elemsz)    (((elems)<<16U) | (elemsz)) *)
    (* local_arg.size = TPC_BUILD_STRIDEARG(els,elsz); *)
    let build_stride = BinOp(BOr, BinOp(Shiftlt, arg_els, (integer 16), intType), arg_elsz, intType) in
    il := Set(size, build_stride, locUnknown)::!il;
    (* TODO: local_arg.stride = arg_size; *)
    let stride = mkFieldAccess local_arg "stride" in
    il := Set(stride, arg_size, locUnknown)::!il;
  end else
    (* local_arg.size = arg_size; *)
    il := Set(size, arg_size, locUnknown)::!il;
  (* local_arg.flag = arg_flag; *)
  let flag = mkFieldAccess local_arg "flag" in
  let arg_type_i = ref 0 in
  (match arg_type with
    In -> arg_type_i := 1;
    | Out -> arg_type_i := 2;
    | InOut -> arg_type_i := 3;
    | SIn -> arg_type_i := 5;
    | SOut -> arg_type_i := 6;
    | SInOut -> arg_type_i := 7;);
  il:= Set(flag, integer !arg_type_i, locUnknown)::!il;
  (* *tmpvec = *((volatile vector unsigned char * )&local_arg); *)
  let casted_la = CastE(vector_uchar_p, AddrOf(local_arg)) in
  il := Set(mkMem (Lval(tmpvec)) NoOffset, Lval(mkMem casted_la NoOffset), locUnknown)::!il;
  !il
end

(* change the return type of a function *)
let setFunctionReturnType (f: fundec) (t: typ) : unit = begin
  match unrollType f.svar.vtype with
    TFun (_, Some args, va, a) -> 
      f.svar.vtype <- TFun(t, Some args, va, a);
    | _ -> assert(false);
end

(* make a tpc_ version of the function (for use on the ppc side)
 * uses the tpc_call_tpcAD65 from tpc_skeleton_tpc.c as a template
 *)
let make_tpc_func (func_vi: varinfo) (args: (string * arg_t * string * string * string ) list) : fundec = begin
  print_endline ("Creating tpc_function_" ^ func_vi.vname);
  let skeleton = find_function_fundec (!ppc_file) "tpc_call_tpcAD65" in
  let f_new = copyFunction skeleton ("tpc_function_" ^ func_vi.vname) in
  f_new.sformals <- [];
  (* set the formals to much the original function's arguments *)
  setFunctionTypeMakeFormals f_new func_vi.vtype;
  setFunctionReturnType f_new intType;
  (* create the arg_size*[, arg_elsz*, arg_els*] formals *)
  let args_num = (List.length f_new.sformals)-1 in
  for i = 0 to args_num do
    let (_, arg_type, _, _, _) = List.nth args i in
    ignore(makeFormalVar f_new ("arg_size"^(string_of_int i)) intType);
    if (is_strided arg_type) then begin
      ignore(makeFormalVar f_new ("arg_els"^(string_of_int i)) intType);
      ignore(makeFormalVar f_new ("arg_elsz"^(string_of_int i)) intType)
    end;
  done;
  let avail_task = var (findLocal f_new "avail_task") in
  let instrs : instr list ref = ref [] in
  (* avail_task->funcid = (uint8_t)funcid; *)
  instrs := Set (mkPtrFieldAccess avail_task "funcid",
  CastE(find_type !spu_file "uint8_t", integer !func_id), locUnknown):: !instrs;
  (* avail_task->total_arguments = (uint8_t)arguments.size() *)
  instrs := Set (mkPtrFieldAccess avail_task "total_arguments",
  CastE(find_type !spu_file "uint8_t", integer (args_num+1)), locUnknown)::!instrs;
  
  (* if we have arguments *)
  if (f_new.sformals <> []) then begin
    (* volatile vector unsigned char *tmpvec   where vector is __attribute__((altivec(vector__))) *)
    let vector_uchar_p = TPtr(TInt(IUChar, [Attr("volatile", [])]), [ppu_vector]) in
    let tmpvec = var (makeLocalVar f_new "tmpvec" vector_uchar_p) in
    (* struct tpc_arg_element local_arg *)
    let local_arg = var (makeLocalVar f_new "local_arg" (find_tcomp !spu_file "tpc_arg_element")) in
    for i = 0 to args_num do
      let arg = List.nth args i in
      (* local_arg <- argument description *)
      instrs := (doArgument i local_arg avail_task tmpvec f_new arg )@(!instrs);
    done;
  end;

  (* insert instrs before avail_task->active = ACTIVE;
     we place a Foo_32412312231() call just above avail_task->active = ACTIVE
     to achieve that *)
  f_new.sbody.bstmts <- List.map (fun s -> replace_fake_call s "Foo_32412312231" (List.rev !instrs)) f_new.sbody.bstmts;

  incr func_id;
  f_new
end

(* parses the #pragma css task arguments and pushes them to ptdepa *)
let rec ptdepa_process_args typ args : unit =
  if ( args <> []) then begin
    match (L.hd args) with
      AIndex(ACons(varname, []), ACons(varsize, [])) -> begin 
        Ptdepa.task_args_l := (varname , typ, !currentFunction)::!Ptdepa.task_args_l;
        Ptdepa.addArg (varname, typ, !currentFunction);
      end
      | _ -> ignore(E.log "Syntax error in #pragma tpc task %s(...)" typ);
    ptdepa_process_args typ (L.tl args)
  end

let rec ptdepa_process io : unit =
  match io with 
    (cur::rest) -> begin
      match cur with
        AStr("highpriority") -> (* simply ignore it *) ();
        | ACons(arg_typ, args) -> ptdepa_process_args arg_typ args
        | _ -> ignore(E.log "Syntax error in #pragma tpc task");
      ptdepa_process rest
    end
    | _ -> ();

(* populates the calls list for Ptdepa module *)
class findTaggedCals = object
  inherit nopCilVisitor
  (* visits all stmts and checks for pragma directives *)
  method vstmt (s: stmt) : stmt visitAction =
    let prags = s.pragmas in
    if (prags <> []) then begin
      match (List.hd prags) with 
        (Attr("css", sub::rest), loc) -> begin
          match sub with
            AStr("task") -> begin
              match s.skind with 
                Instr(Call(_, Lval((Var(vi), _)), _, _)::_) -> begin
                  ptdepa_process rest;
                  Ptdepa.addTask vi.vname !currentFunction;
                  DoChildren
                end
                | Block(b) -> ignore(E.warn "Ignoring block pragma at %a" d_loc loc); DoChildren
                | _ -> ignore(E.warn "Ignoring pragma at %a" d_loc loc); DoChildren
            end
            | AStr("wait") -> begin
              match rest with 
                ACons("on", exps)::_ -> (* wait on *) DoChildren
                | [] -> (* wait all? *) DoChildren
                | _ -> ignore(E.warn "Ignoring pragma at %a" d_loc loc); DoChildren
            end
            | _ -> ignore(E.warn "Ignoring pragma at %a" d_loc loc); DoChildren
        end
        | _ -> ignore(E.warn "Unrecognized pragma"); DoChildren
     end else
       DoChildren
end

(* recursively copies a function definition and all it's callees
   from the ppc_file to the spu_file *)
let rec deep_copy_function (task: string) (callgraph: CG.callgraph) = begin
    (* First copy the Callees *)
    let cnode: CG.callnode = H.find callgraph task in
    let nodeName (n: CG.nodeinfo) : string =
      match n with
        CG.NIVar (v, _) -> v.vname
      | CG.NIIndirect (n, _) -> n
    in
    let deep_copy _ (n: CG.callnode) : unit =
      let name = nodeName n.CG.cnInfo in
      deep_copy_function name callgraph
    in
    Inthash.iter deep_copy cnode.CG.cnCallees;
    
    (* now copy current *)
    let new_fd = GFun(find_function_fundec (!ppc_file) task, locUnknown) in
    (!spu_file).globals <- (!spu_file).globals@[new_fd];
end

(* function that checks if an exp uses an indice *)
let uses_indice (e: exp) : bool =
  match (e) with 
    (BinOp(PlusPI, _, _, _))
    | (BinOp(IndexPI, _, _, _))
    | (Lval(_, Index(_, _))) -> true
    | _ -> false

(* function that checks if a stmt is tagged with a #pragma tpc... *)
let tpc_call_with_arrray (st: stmt) : bool =
  if (st.pragmas <> []) then begin
    match (L.hd st.pragmas) with
      (Attr("css", _), _) -> begin
        match (st.skind)  with
          Instr(Call(_, _, args, _)::_) -> L.exists uses_indice args
          | _ -> false
      end
      | _ -> false
  end else
    false

(* parses the #pragma css task arguments and pushes them to ptdepa *)
let rec s2s_process_args typ args =
  match args with
    (arg::rest) -> begin
      match arg with
        AIndex(ACons(varname, []), ACons(varsize, [])) ->
          (varname, (translate_arg typ false), varsize, "", "")::(s2s_process_args typ rest)
(*         | handle strided... *)
        | _ -> ignore(E.log "Syntax error in #pragma tpc task %s(...)" typ); []
    end
    | _ -> []

let rec s2s_process io =
  match io with 
    (cur::rest) -> begin
      match cur with
        AStr("highpriority") -> s2s_process rest
        | ACons(arg_typ, args) -> s2s_process_args arg_typ args
        | _ -> ignore(E.log "Syntax error in #pragma tpc task"); []
    end
    | _ -> []

(* populates the global list of spu tasks [spu_tasks] *)
class findSPUDeclVisitor cgraph = object
  inherit nopCilVisitor
  val callgraph = cgraph 
  (* visits all stmts and checks for pragma directives *)
  method vstmt (s: stmt) : stmt visitAction =
    let prags = s.pragmas in
    if (prags <> []) then begin
      match s.skind with 
        Instr(Call(_, Lval((Var(vi), _)), _, _)::_) -> begin
          match (List.hd prags) with 
            (Attr("tpc", args), _) -> begin
              let funname = vi.vname in
              let args' =
                List.map (fun arg -> match arg with
                    ACons(varname, ACons(arg_typ, [])::ACons(varsize, [])::[]) ->
                      (* give all the arguments to Dtdepa*)
                      (varname, (translate_arg arg_typ false), varsize, "", "")
                  | ACons(varname, ACons(arg_typ, [])::ACons(varsize, [])::ACons(elsize, [])::ACons(elnum, [])::[]) ->
                      (* give all the arguments to Dtdepa don't care for strided  *)
                      (varname, (translate_arg arg_typ true), varsize, elsize, elnum)
                  | _ -> ignore(E.error "impossible"); assert false
                ) args in
              ignore(E.log "Found task \"%s\"\n" funname);
              let rest new_fd = 
                (* push arguments to the call *)
                let call_args = ref [] in
                let args_num = (List.length args')-1 in
                for i = 0 to args_num do
                  let (vname, _, _, _, _) = List.nth args' i in
                  call_args := Lval(var (find_scoped_var !currentFunction !ppc_file vname))::!call_args;
                done;
                for i = 0 to args_num do
                  let (_, arg_type, vsize, velsz, vels) = List.nth args' i in
                  call_args := Lval(var (find_scoped_var !currentFunction !ppc_file vsize))::!call_args;
                  if (is_strided arg_type) then
                    call_args := Lval(var (find_scoped_var !currentFunction !ppc_file vels))::
                      Lval(var (find_scoped_var !currentFunction !ppc_file velsz))::!call_args;
                done;
                let instr = Call (None, Lval (var new_fd.svar), List.rev !call_args, locUnknown) in
                let call = mkStmtOneInstr instr in
                ChangeTo(call) in
              try
                (* check if we have seen this function before *)
                let (new_fd, _, fargs) = List.assoc funname !spu_tasks in
                rest new_fd
              with Not_found -> begin
                let rest2 var_i = 
                  let new_fd = make_tpc_func var_i args' in
                  add_after_s !ppc_file var_i.vname new_fd;
                  spu_tasks := (funname, (new_fd, var_i, args'))::!spu_tasks;
                  rest new_fd in
                (* try to find the function definition *)
                try
                  (* checking for the function definition *)
                  let task = find_function_fundec (!ppc_file) funname in
                  (* copy itself and the callees *)
                  deep_copy_function funname callgraph;
                  rest2 task.svar
                (* else try to find the function signature/prototype *)
                with Not_found -> begin
                  let task = find_function_sign (!ppc_file) funname in
                  rest2 task
                end
              end
            end
            | (Attr("tpc_wait_all", _), _) -> (* Must insert tpc_wait_all() *) DoChildren
            | (Attr("tpc_wait_for", AStr("val")::_), _) -> (* Must insert tpc_wait(val) *) DoChildren
            (* Support for CellSs syntax *)
            | (Attr("css", sub::rest), loc) -> begin
              match sub with
                (* Support #pragma css task... *)
                AStr("task")-> begin
                  match s.skind with 
                    Instr(Call(_, Lval((Var(vi), _)), _, _)::_) -> begin
                      let funname = vi.vname in
                      let args = s2s_process rest in
                      ignore(E.log "Found task \"%s\"\n" funname);
                      let rest new_fd = 
                        (* add arguments to the call *)
                        let call_args = ref [] in
                        let args_num = (List.length args)-1 in
                        for i = 0 to args_num do
                          let (vname, _, _, _, _) = List.nth args i in
                          call_args := Lval(var (find_scoped_var !currentFunction !ppc_file vname))::!call_args;
                        done;
                        for i = 0 to args_num do
                          let (_, arg_type, vsize, velsz, vels) = List.nth args i in
                          call_args := Lval(var (find_scoped_var !currentFunction !ppc_file vsize))::!call_args;
                          if (is_strided arg_type) then
                            call_args := Lval(var (find_scoped_var !currentFunction !ppc_file vels))::
                              Lval(var (find_scoped_var !currentFunction !ppc_file velsz))::!call_args;
                        done;
                        let instr = Call (None, Lval (var new_fd.svar), List.rev !call_args, locUnknown) in
                        let call = mkStmtOneInstr instr in
                        ChangeTo(call) in
                      try
                        (* check if we have seen this function before *)
                        let (new_fd, _, fargs) = List.assoc funname !spu_tasks in
                        rest new_fd
                      with Not_found -> begin
                        let rest2 var_i = 
                          let new_fd = make_tpc_func var_i args in
                          add_after_s !ppc_file var_i.vname new_fd;
                          spu_tasks := (funname, (new_fd, var_i, args))::!spu_tasks;
                          rest new_fd in
                        (* try to find the function definition *)
                        try
                          (* checking for the function definition *)
                          let task = find_function_fundec (!ppc_file) funname in
                          (* copy itself and the callees *)
                          deep_copy_function funname callgraph;
                          rest2 task.svar
                        (* else try to find the function signature/prototype *)
                        with Not_found -> begin
                          let task = find_function_sign (!ppc_file) funname in
                          rest2 task
                        end
                      end
                    end
                    | Block(b) -> ignore(E.warn "Ignoring block pragma at %a" d_loc loc); DoChildren
                    | _ -> ignore(E.warn "Ignoring pragma at %a" d_loc loc); DoChildren
                end
                (* Support #pragma css wait on(...) *)
                | AStr("wait")-> begin
                  match rest with 
                    ACons("on", exps)::_ -> (* wait on *) DoChildren
                    | [] -> (* wait all? *) DoChildren
                    | _ -> ignore(E.warn "Ignoring pragma at %a" d_loc loc); DoChildren
                end
                | _ -> ignore(E.warn "Unrecognized pragma"); DoChildren
            end
            | _ -> ignore(E.warn "Unrecognized pragma"); DoChildren
          end
        (* Get info about array indices from loops *)
        | Loop(b_code, _, _, _) -> begin
          (* Check if there is any task inside the loop *)
          if (L.exists tpc_call_with_arrray b_code.bstmts) then
            (* visit the b_code.bstmts to find the Upper and the successor function *)
            DoChildren else
          DoChildren
        end
        | Block(b) -> ignore(E.unimp "Ignoring block pragma"); DoChildren
        | _ -> ignore(E.warn "Ignoring pragma"); DoChildren
    end else
      DoChildren
end

(* returns the compiler added variables of the function *)
let get_tpc_added_formals (new_f: fundec) (old_f: fundec) : varinfo list = begin
  List.filter 
    (fun formal -> 
        List.exists 
          (fun formal2 -> formal <> formal2 )
          old_f.sformals
    )
    new_f.sformals
end

let make_case execfun (task: varinfo) (task_info: varinfo)
              (ex_task: varinfo) (args: (string * arg_t * string * string * string) list): stmt = begin
  let res = ref [] in
  assert(isFunctionType task.vtype);
  let ret, arglopt, hasvararg, _ = splitFunctionType task.vtype in
  assert(not hasvararg);
  let argl = match arglopt with None -> [] | Some l -> l in
  let argaddr = makeTempVar execfun voidPtrType in
  res := Set(var argaddr, Lval (mkPtrFieldAccess (var task_info) "ls_addr"), locUnknown) :: !res;
  let nextaddr n stride =
    let lv = mkPtrFieldAccess (var ex_task) "arguments" in
    let t = typeOfLval lv in
    assert(isArrayType t);
    let idxlv = addOffsetLval (Index(integer n, NoOffset)) lv in
    let szlv = mkFieldAccess idxlv "size" in
    let plus = 
      if (stride) then
        (* next = previous + ((ex_task->arguments[pre].size >>16U)
                        *(ex_task->arguments[pre].size & 0x0FFFFU)) *)
        let els = BinOp(Shiftrt, Lval(szlv), integer 16, intType) in
        let elsz = BinOp(BAnd, Lval(szlv), integer 0x0FFFF, intType) in
        (BinOp(PlusPI, (Lval(var argaddr)), BinOp(Mult, els, elsz,intType), voidPtrType))
      else
        (* next = previous + ex_task->arguments[pre].size *)
        (BinOp(PlusPI, (Lval(var argaddr)), Lval(szlv), voidPtrType))
    in
    Set(var argaddr, plus, locUnknown);
  in
  let i = ref 0 in
  let carry = ref dummyInstr in
  let arglist = List.map
    (fun (_, argt, _) ->
      let argvar = makeTempVar execfun argt in
      let castexp = CastE(argt, Lval(var argaddr)) in
      let castinstr = Set(var argvar, castexp, locUnknown) in
      let arg_type = get_arg_type (List.nth args !i) in
      let advptrinstr = nextaddr !i (is_strided arg_type) in
      incr i;
      if !carry <> dummyInstr then res := !carry::!res;
      carry := advptrinstr;
      res := castinstr :: !res;
      Lval(var argvar)
    )
    argl
  in
  res := Call (None, Lval (var task), arglist, locUnknown)::!res;
  mkStmt (Instr (List.rev !res))
end
(*
    case 0:
      //printf("SPU: Dispatch (%p) (%d,%d,%p)\n", task_info->ls_addr,
//          task_info->state, task_info->dmatag, task_info->dmalist);
      arg1 = (float * )task_info->ls_addr;
      arg2 = (float * )((void * )arg1 + ex_task->arguments[0].size);
      arg3 = (int * )((void * )arg2 + ex_task->arguments[1].size);
      matrix_add_row(arg1, arg2, arg3);
      task_info->state = EXECUTED; no need for it in every case
                                      moved it out of the swith
      break;
*)

(* Make the execute_func function that branches on the task id and
 * calls the actual task function on the spe *)
let make_exec_func (f: file) (tasks: (fundec * varinfo * (string * arg_t * string * string * string) list) list) : global = begin
  (* make the function *)
  let exec_func = emptyFunction "execute_task" in
  exec_func.svar.vtype <- TFun(intType, Some [], false,[]);
(*  (* make "queue_entry_t * volatile  ex_task" *)
  let ex_task = makeFormalVar exec_func "ex_task" (TPtr((find_type f "queue_entry_t"), [Attr("volatile", [])])) in*)
  (* make "queue_entry_t * ex_task" *)
  let ex_task = makeFormalVar exec_func "ex_task" (TPtr((find_type f "queue_entry_t"), [])) in
  (* make "tpc_spe_task_state_t task_info" *)
  let task_info = makeFormalVar exec_func "task_info" (TPtr(find_type f "tpc_spe_task_state_t", [])) in
  (* make an int variable for the return value *)
  let lexit = makeLocalVar exec_func "exit" intType in
  (* make a switch statement with one case per task starting from zero *)
  let id = ref 0 in
  let switchcases = List.map
    (fun (tpc_call, task, fargs) ->
      let c = Case (integer !id, locUnknown) in
      incr id;
      let body = make_case exec_func task task_info ex_task fargs in
      (* add the arguments' declarations *)
      body.labels <- [c];
      let stmt_list = [body; mkStmt (Break locUnknown)] in
      stmt_list
    )
    tasks
  in
  let cases = List.map List.hd switchcases in
  (* default: exit=1; break; *)
  let assignment = mkStmtOneInstr (Set (var lexit, one, locUnknown)) in
  assignment.labels <- [Default(locUnknown)];
  let switchcases2 = (List.append (List.flatten switchcases) [assignment; mkStmt (Break locUnknown)]) in
  (* exit=0; *)
  let exit0 = mkStmtOneInstr (Set (var lexit, zero, locUnknown)) in
  (* return exit; *)
  let retstmt = mkStmt (Return (Some (Lval (var lexit)), locUnknown)) in

  (* the case expression of the switch statement (switch(expr)) *)
  let expr = Lval(mkPtrFieldAccess (var ex_task) "funcid") in
  let switchstmt = mkStmt(Switch(expr, mkBlock switchcases2, cases, locUnknown)) in
  (* get the task_state enuminfo *)
  let task_state_enum = find_enum !spu_file "task_state" in
  (* task_info->state = EXECUTED no need for it in every case *)
  let rec find_executed = function [] -> raise Not_found | ("EXECUTED", e, _)::_ -> e | _::tl -> find_executed tl in
  let executed = find_executed task_state_enum.eitems in
  let exec_s = mkStmtOneInstr(Set (mkPtrFieldAccess (var task_info) "state", executed, locUnknown)) in
  (* the function body: exit = 0; switch (taskid); return exit; *)
  exec_func.sbody <- mkBlock [exit0; switchstmt; exec_s; retstmt];
  GFun (exec_func, locUnknown)
end

(* write an AST (list of globals) into a file *)
let writeNewFile f fname globals = begin
  let file = { f with
    fileName = fname;
    globals = globals;
  } in
  let oc = open_out fname in
  Rmtmps.removeUnusedTemps file;
  dumpFile defaultCilPrinter oc fname file;
  close_out oc
end

(* write out file <f> *)
let writeFile f = begin
  let oc = open_out f.fileName in
  Rmtmps.removeUnusedTemps f;
  dumpFile defaultCilPrinter oc f.fileName f;
  close_out oc
end

(* Preprocess the header file <header> and merges it with f.  The
 * given header should be in the gcc include path.  Modifies f
 *) (* the original can be found in lockpick.ml *)
let preprocessAndMergeWithHeader (f: file) (header: string) (def: string): unit = begin
  (* FIXME: what if we move arround the executable? *)
  let statistics = ref "" in
  if (!stats) then
    statistics := "-DSTATISTICS=1";
  ignore (Sys.command ("echo | gcc -E -D"^def^"=1 -DMAX_QUEUE_ENTRIES="^(!queue_size)^" "^(!statistics)^" -I./include/ppu -I./include/spu "^(header)^" - >/tmp/_cil_rewritten_tmp.h"));
print_endline ("gcc -E -D"^def^"=1 -DMAX_QUEUE_ENTRIES="^(!queue_size)^" "^(!statistics)^" -I./include/ppu -I./include/spu "^(header));
  let add_h = Frontc.parse "/tmp/_cil_rewritten_tmp.h" () in
  let f' = Mergecil.merge [add_h; f] "stdout" in
  f.globals <- f'.globals;
end

(* Checks if <g> is *not* the function declaration of "main"  *)
let isNotMain (g: global) : bool = match g with
    GFun({svar = vi}, _) when (vi.vname = "main") -> false
  | _ -> true

(* Checks if <g> is *not* the function declaration of "tpc_call_tpcAD65"  *)
let isNotSkeleton (g: global) : bool = match g with
    GFun({svar = vi}, _) when (vi.vname = "tpc_call_tpcAD65") -> false
  | _ -> true

(* Checks if <g> is a typedef, enum, struct or union *)
let is_typedef (g: global) : bool = match g with
    GType(_, _)
  | GCompTag(_, _)
  | GCompTagDecl(_, _)
  | GEnumTag(_, _)
  | GEnumTagDecl(_, _) -> true
  | _ -> false

let feature : featureDescr = 
  { fd_name = "findspucode";
    fd_enabled = ref true;
    fd_description = "find all pragmas declaring spu tasks";
    fd_extraopt = options
    @ Ptatype.options
    @ Uniqueness.options
    @ Locksettings.options
    (*@ Livevars.options*)
    @ Shared.options
    @ Correlation.options
    @ Controlflow.options
    @ Bansheemlifc.options
    @ Labelflow.options
    @ Lprof.options
    @ Ptdepa.options
    ;
    fd_doit = 
    (function (f: file) -> 
      ignore(E.log "Welcome to S2S!!!\n");
      (* create two copies of the initial file *)
(*       in_file := f; *)
(*       spu_file := { f with fileName = (!out_name^"_func.c");}; *)
      spu_file := { dummyFile with fileName = (!out_name^"_func.c");};
      ppc_file := { f with fileName = (!out_name^".c");};

      (* create a call graph and print it *)
      let callgraph = CG.computeGraph f in

      (* find tpc_decl pragmas *)
      let fspuVisitor = new findSPUDeclVisitor callgraph in
      let ftagVisitor = new findTaggedCals in
	
      (* create a global list (the spu output file) *)
(*       let spu_glist = ref [] in *)

      (* copy all code from file f to file_ppc *)
      preprocessAndMergeWithHeader !ppc_file "tpc_s2s.h" "PPU";

      (* copy all typedefs and enums/structs/unions from ppc_file to spu_file
         plus the needed headers *)
      let new_types_l = List.filter is_typedef (!ppc_file).globals in
      (!spu_file).globals <- new_types_l;
      preprocessAndMergeWithHeader !spu_file "tpc_s2s.h" "SPU";

      Cil.iterGlobals !ppc_file 
        (function
          GFun(fd,_) ->
            currentFunction := fd;
            ignore(visitCilFunction ftagVisitor fd);
          | _ -> ()
        )
      ;

      (* kasas was here :P *)
      Ptdepa.find_dependencies f;

      Cil.iterGlobals !ppc_file 
        (function
          GFun(fd,_) ->
            currentFunction := fd;
            ignore(visitCilFunction fspuVisitor fd);
        | _ -> ()
        )
      ;

      (* copy all globals except the function declaration of "tpc_call_tpcAD65" *)
      (!ppc_file).globals <- List.filter isNotSkeleton (!ppc_file).globals;
      (* copy all globals except the function declaration of "main" *)
(*       spu_glist := List.filter isNotMain (!spu_file).globals; *)


      (* tasks  (new_tpc * old_original * args) *)
      let tasks : (fundec * varinfo * (string * arg_t * string * string * string) list) list = List.map
        (fun (name, (new_fd, old_fd, args)) -> (new_fd, old_fd, args))
        (List.rev !spu_tasks)
      in
      (!spu_file).globals <- (!spu_file).globals@[(make_exec_func !spu_file tasks)];
      (*(* remove the "tpc_call_tpcAD65" function from the ppc_file *)
      (!ppc_file).globals <- List.filter isNotSkeleton (!ppc_file).globals;*)
      writeFile !ppc_file;
(*       !spu_file.globals <- !spu_glist; *)
      writeFile !spu_file;
(*       writeNewFile f (!out_name^"_func.c") !spu_glist; *)
      );
    fd_post_check = true;
  }



(******************************************************************************)
(******************************************************************************)
(******************************************************************************)
(******************************************************************************)

(*
(* alternative make_tpc_func without the use of a skeleton
 *)
let make_tpc_func (f: fundec) (args: (string * arg_t * string * string * string ) list) : fundec = begin
  let f_new = emptyFunction ("tpc_function_" ^ f.svar.vname) in
  setFunctionTypeMakeFormals f_new f.svar.vtype;
  setFunctionReturnType f_new intType;
  (* TODO: push the size variables as args *)
  (*** Declare the local variables ***)
  (* unsigned int total_bytes *)
  let total_bytes = makeLocalVar f_new "total_bytes" uintType in
  (* volatile queue_entry_t *remote_entry *)
(*   let remote_entry = makeLocalVar f_new "remote_entry" (TPtr((find_type !spu_file "queue_entry_t"), [Attr("volatile", [])])) in *)
  (* volatile queue_entry_t *avail_task *)
  let avail_task = makeLocalVar f_new "avail_task" (TPtr((find_type !spu_file "queue_entry_t"), [Attr("volatile", [])])) in
  (* TODO: add the #ifdef
    #ifdef STATISTICS
      uint64_t tmptime1, tmptime2, tmptime3;
      int arg_bytes;
    #endif
  *)
  (* unsigned int *task_id_qs *)
  let task_id_qs = makeLocalVar f_new "task_id_qs" (TPtr(uintType, [])) in
  (* unsigned int task_id *)
  let task_id = makeLocalVar f_new "task_id" uintType in
  (* volatile completions_status_t *st *)
  let st = makeLocalVar f_new "st" (TPtr((find_tcomp !spu_file "completions_status_t"), [Attr("volatile", [])])) in

  (* create the arg_size*[, arg_elsz*, arg_els*] formals *)
  let args_num = (List.length f_new.sformals)-1 in
  for i = 0 to args_num do
    let (_, arg_type, _, _, _) = List.nth args i in
    ignore(makeFormalVar f_new ("arg_size"^(string_of_int i)) intType);
    if (is_strided arg_type) then begin
      ignore(makeFormalVar f_new ("arg_els"^(string_of_int i)) intType);
      ignore(makeFormalVar f_new ("arg_elsz"^(string_of_int i)) intType)
    end;
  done;
  let avail_task = var (findLocal f_new "avail_task") in
  let instrs : instr list ref = ref [] in
  (* avail_task->funcid = (uint8_t)funcid; *)
  instrs := Set (mkPtrFieldAccess avail_task "funcid",
  CastE(find_type !spu_file "uint8_t", integer !func_id), locUnknown):: !instrs;
  (* avail_task->total_arguments = (uint8_t)arguments.size() *)
  instrs := Set (mkPtrFieldAccess avail_task "total_arguments",
  CastE(find_type !spu_file "uint8_t", integer (args_num+1)), locUnknown)::!instrs;
  
  (* if we have arguments *)
  if (f_new.sformals <> []) then begin
    (* volatile vector unsigned char *tmpvec   where vector is __attribute__((altivec(vector__))) *)
    let vector_uchar_p = TPtr(TInt(IUChar, [Attr("volatile", [])]), [ppu_vector]) in
    let tmpvec = var (makeLocalVar f_new "tmpvec" vector_uchar_p) in
    (* struct tpc_arg_element local_arg *)
    let local_arg = var (makeLocalVar f_new "local_arg" (find_tcomp !spu_file "tpc_arg_element")) in
    for i = 0 to args_num do
      let arg = List.nth args i in
      (* local_arg <- argument description *)
      instrs := (doArgument i local_arg avail_task tmpvec f_new arg )@(!instrs);
    done;
  end;

  (* if (f_new.sformals <> []) then begin *)
    (* void *arg_addr64 *)
    let arg_addr64 = makeLocalVar f_new "arg_addr64" voidPtrType in
    (* unsigned int arg_size *)
    let arg_size = makeLocalVar f_new "arg_size" uintType in
    (* unsigned int arg_flag *)
    let arg_flag = makeLocalVar f_new "arg_flag" uintType in
    (* unsigned int arg_stride *)
    let arg_stride = makeLocalVar f_new "arg_stride" uintType in
    (* vector unsigned char *tmpvec   where vector is __attribute__((vector_size(8))) *)
    let tmpvec = makeLocalVar f_new "tmpvec" (TPtr(TInt(IUChar, []), [Attr("__attribute__", [ACons("vector_size", [AInt(8)])])])) in
    (* struct tpc_arg_element local_arg *)
    let local_arg = makeLocalVar f_new "local_arg" (find_tcomp !spu_file "tpc_arg_element") in
  (* end *)

  (*** Initialize local variables ***)
  let stmts = ref [] in
  (* remote_entry=NULL *)
(*   stmts := mkStmtOneInstr (Set (var remote_entry, CastE(voidPtrType ,zero), locUnknown))::!stmts; *)
  (* avail_task=NULL *)
  stmts := mkStmtOneInstr (Set (var avail_task, CastE(voidPtrType ,zero), locUnknown))::!stmts;
  (* if (f_new.sformals <> []) then begin *)
    (* arg_addr64=NULL *)
    stmts := mkStmtOneInstr (Set (var arg_addr64, CastE(voidPtrType ,zero), locUnknown))::!stmts;
    (* arg_size=0 *)
    stmts := mkStmtOneInstr (Set (var arg_size, zero, locUnknown))::!stmts;
    (* arg_flag=0 *)
    stmts := mkStmtOneInstr (Set (var arg_flag, zero, locUnknown))::!stmts;
    (* arg_stride=0 *)
    stmts := mkStmtOneInstr (Set (var arg_stride, zero, locUnknown))::!stmts;
  (* end *)
  (* TODO: Add the #ifdef
    #ifdef TPC_MULTITHREADED
      pthread_mutex_lock( &tpc_callwait_mutex );
    #endif
    READ_TIME_REG(tmptime1);
  *)
  (* get the compl_status enuminfo *)
  let compl_status_enum = find_enum !spu_file "compl_status" in
  (* get the entry_status enuminfo *)
  let entry_status_enum = find_enum !spu_file "entry_status" in
  (* get the s_available_spe varinfo *)
  let g_max_spes = find_global_var !spu_file "G_max_spes" in
  (* get the s_available_spe varinfo *)
  let s_available_spe = find_global_var !spu_file "s_available_spe" in
  (* get the task_queue_tail varinfo *)
  let task_queue_tail = find_global_var !spu_file "task_queue_tail" in
  (* get the task_queue_tail varinfo *)
  let compl_queue = find_global_var !spu_file "compl_queue" in
  (* get the g_task_current_id varinfo *)
  let g_task_current_id = find_global_var !spu_file "g_task_current_id" in
  (* get the g_task_current_id varinfo *)
  let g_task_id_queue = find_global_var !spu_file "g_task_id_queue" in
  (* get the task_queue varinfo *)
  let task_queue = find_global_var !spu_file "task_queue" in
  (* create the while body *)
  let w_body = ref [] in
  (* create the [s_available_spe][task_queue_tail[s_available_spe]] offset *)
  let big_offset = Index(Lval(var s_available_spe), Index(Lval((Var(task_queue_tail) , Index(Lval(var s_available_spe), NoOffset))), NoOffset)) in
  (* st = &compl_queue[s_available_spe][task_queue_tail[s_available_spe]] *)
  w_body := mkStmtOneInstr (Set (var st, AddrOf((Var compl_queue, big_offset)) , locUnknown))::!w_body;
  let st_status = mkPtrFieldAccess (var st) "status" in
  (* st->status = WAITING; break; *)
  let bthen = mkBlock [ mkStmtOneInstr (Set (st_status, Const(CEnum(zero, "WAITING", compl_status_enum)), locUnknown) ); mkStmt (Break locUnknown) ] in
  (* s_available_spe = (s_available_spe+1) % G_max_spes; *)
  let next_spe_s = mkStmtOneInstr(Set (var s_available_spe, BinOp(Mod, BinOp(PlusA, Lval(var s_available_spe), one, intType), Lval(var g_max_spes), intType), locUnknown) ) in
  let belse = mkBlock [ next_spe_s ] in
  (*
    if(st->status == COMPLETED) {
      st->status = WAITING;
      break;
    } else {
      s_available_spe = (s_available_spe+1) % G_max_spes;
    }
  *)
  w_body := mkStmt(If(BinOp(Eq, Lval(st_status), Const(CEnum(one, "COMPLETED", compl_status_enum)), TInt(IBool, [])), bthen, belse, locUnknown))::!w_body;
  (* push while in stmts *)
  stmts := mkStmt(Loop( mkBlock (List.rev !w_body), locUnknown, None, None))::!stmts;
  (* g_task_current_id[s_available_spe] *)
  let gtc_indexed = (Var g_task_current_id, Index(Lval(var s_available_spe), NoOffset)) in
  (** task_id = g_task_current_id[s_available_spe]++; **)
  (* task_id = g_task_current_id[s_available_spe]; *)
  stmts := mkStmtOneInstr(Set (var task_id, Lval(gtc_indexed), locUnknown))::!stmts;
  (* g_task_current_id[s_available_spe]++; *)
  stmts := mkStmtOneInstr(Set (gtc_indexed, BinOp(PlusA, Lval(gtc_indexed), one, intType), locUnknown))::!stmts;
  (* task_id_qs = &g_task_id_queue[s_available_spe][task_queue_tail[s_available_spe]]; *)
  stmts := mkStmtOneInstr(Set (var task_id_qs, AddrOf((Var g_task_id_queue, big_offset)) , locUnknown))::!stmts;
  (** task_id = (task_id & 0x0FFFFFFF) | (s_available_spe << 28); **)
  (* (task_id & 0x0FFFFFFF) *)
  let lbs = BinOp(BAnd, Lval(var task_id), Const(CInt64(Int64.of_string "0x0FFFFFFF", IInt, None)), intType) in
  (* (s_available_spe << 28) *)
  let rbs = BinOp(Shiftlt, Lval(var s_available_spe), Const(CInt64(Int64.of_int 28, IInt, None)), intType) in
  (* task_id = lbs | rbs; *)
  stmts := mkStmtOneInstr(Set (var task_id, BinOp(BOr, lbs, rbs, intType), locUnknown))::!stmts;
  (* *task_id_qs = task_id; *)
  stmts := mkStmtOneInstr(Set ((mkMem (Lval(var task_id_qs)) NoOffset), Lval(var task_id), locUnknown))::!stmts;
  (* TODO: READ_TIME_REG(tmptime2); *)
  (* avail_task = &task_queue[s_available_spe][task_queue_tail[s_available_spe]]; *)
  stmts := mkStmtOneInstr(Set (var avail_task, AddrOf((Var task_queue, big_offset)) , locUnknown))::!stmts;
  (* avail_task->funcid = (uint8_t)funcid; *)
  stmts := mkStmtOneInstr(Set (mkPtrFieldAccess (var avail_task) "funcid", CastE(find_type !spu_file "uint8_t", Const(CInt64(Int64.of_int !func_id, IInt, None))), locUnknown))::!stmts;
  (* avail_task->total_arguments = (uint8_t)arguments.size() *)
  stmts := mkStmtOneInstr(Set (mkPtrFieldAccess (var avail_task) "total_arguments", CastE(find_type !spu_file "uint8_t", Const(CInt64(Int64.of_int (List.length f_new.sformals), IInt, None))), locUnknown))::!stmts;
  (* total_bytes=0; *)
  stmts := mkStmtOneInstr(Set (var total_bytes, zero, locUnknown))::!stmts;

  (* TODO: complete code depending on arguments *)

  (* avail_task->active = ACTIVE *)
  stmts := mkStmtOneInstr(Set (mkPtrFieldAccess (var avail_task) "active", Const(CEnum(one, "ACTIVE", entry_status_enum)), locUnknown))::!stmts;
  (* TODO:
    READ_TIME_REG(tmptime3);
    #ifdef STATISTICS
      G_ppe_stats.stat_tpc_per_spe[s_available_spe] += 1;
      G_ppe_stats.bytes_per_spe[s_available_spe] += total_bytes;
      G_ppe_stats.stalled_ticks += (tmptime2 - tmptime1);
      G_ppe_stats.issue_ticks += (tmptime3 - tmptime2);
    #endif
    #ifdef TPC_MULTITHREADED
      pthread_mutex_unlock( &tpc_callwait_mutex );
    #endif
  *)
  (** task_queue_tail[s_available_spe] = c % MAX_QUEUE_ENTRIES; **)
  (* task_queue_tail[s_available_spe] *)
  let tqt_indexed = (Var task_queue_tail, Index(Lval(var s_available_spe), NoOffset)) in
  (* (task_queue_tail[s_available_spe]+1) *)
  let tqt_plus1 = BinOp(PlusA, Lval(tqt_indexed), one, intType) in
  (* tqt_indexed = tqt_plus1 % MAX_QUEUE_ENTRIES; *)
  stmts := mkStmtOneInstr(Set (tqt_indexed, BinOp(Mod, tqt_plus1, Const(CInt64(Int64.of_int 1(* FIXME: MAX_QUEUE_ENTRIES *), IInt, None)), intType), locUnknown))::!stmts;
  (* s_available_spe = (s_available_spe+1) % G_max_spes; *)
  stmts := next_spe_s::!stmts;
  (* return task_id; *)
  stmts := mkStmt (Return (Some (Lval (var task_id)), locUnknown))::!stmts;
  (* reverse the stmt list and put it in the body *)
  f_new.sbody <- mkBlock (List.rev !stmts);
  incr func_id;
  f_new
end
*)
