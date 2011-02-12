(*
 *
 * Copyright (c) 2010, 
 *  Foivos Zakkak        <zakkak@ics.forth.gr>
 *  Polyvios Pratikakis <polyvios@ics.forth.gr>
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
open S2s_util
open S2s_x86
open S2s_cell
module E = Errormsg
module H = Hashtbl
module S = Str
module L = List
module CG = Callgraph
module Lprof = Lockprofile

let stats = ref false
let queue_size = ref "0"
let debug = ref false
let thread = ref false
let unaligned_args = ref false
let out_name = ref "final"
let block_size = ref 0
let arch = ref "unknown"
let tpcIncludePath = ref ""
let cflags = ref ""
let currentFunction = ref dummyFunDec
let prevstmt = ref dummyStmt

(* create a ref to the input file *)
(* let in_file = ref dummyFile *)
(* create a ref to the new spu file *)
let spu_file = ref dummyFile
(* create a ref to the new ppe file *)
let ppc_file = ref dummyFile

let options =
  [
    "--arch",
      Arg.String(fun s -> arch := s),
      " S2S: Define the target architecture (x86/cell).";

    "--cflags",
      Arg.String(fun s -> cflags := s),
      " S2S: Define the flags you want to pass to gcc.";

    "--tpcIncludePath",
      Arg.String(fun s -> tpcIncludePath := s),
      " S2S: Define the include path for the tpc runtime.";

    "--debugS2S",
      Arg.Set(debug),
      " S2S: Print debugging information.";

    "--out-name",
      Arg.String(fun s -> out_name := s),
      " S2S: Specify the output files' prefix. e.g. (default: final) will produce final.c and final_func.c";

    "--queue-size",
      Arg.String(fun s -> queue_size := s),
      " S2S: Specify the queue size for Cell. Defined in the Makefile as MAX_QUEUE_ENTRIES";

    "--block-size",
      Arg.Int(fun s -> block_size := s),
      " S2S: Specify the block size for x86. Defined in the Makefile as BLOCK_SZ";

    "--with-stats",
      Arg.Set(stats),
      " S2S: Enable code for statistics, for use with -DSTATISTICS";

    "--with-unaligned-arguments",
      Arg.Set(unaligned_args),
      " S2S: Allow unalligned arguments in x86, for use with -DUNALIGNED_ARGUMENTS_ALLOWED";

    "--threaded",
      Arg.Set(thread),
      " S2S: Generate thread safe code, for use with -DTPC_MULTITHREADED";
  ]

(* create 1 global list (the spe output file) *)
let spu_tasks = ref []
(* keeps the current funcid for the new tpc_function *)
let func_id = ref 0

class changeStmtVisitor (name: string) (stl: stmt list) : cilVisitor =
  object (self)
    inherit nopCilVisitor
    method vstmt (s: stmt) = match s.skind with
      Instr(Call(_, Lval((Var(vi), _)), _, _)::res) when vi.vname = name ->
        ChangeTo (mkStmt (Block (mkBlock (stl@[mkStmt (Instr(res))]))))
    | _ -> SkipChildren
  end

let replace_fake_call_with_stmt (s: stmt) (fake: string) (stl: stmt list) =
  let v = new changeStmtVisitor fake stl in
  visitCilStmt v s


(* make a tpc_ version of the function (for use on the ppc side)
 * uses the tpc_call_tpcAD65 from tpc_skeleton_tpc.c as a template
 *)
let make_tpc_func (func_vi: varinfo) (args: (string * (arg_t * exp * exp * exp )) list) : fundec = begin
  print_endline ("Creating tpc_function_" ^ func_vi.vname);
  let skeleton = find_function_fundec (!ppc_file) "tpc_call_tpcAD65" in
  let f_new = copyFunction skeleton ("tpc_function_" ^ func_vi.vname) in
  f_new.sformals <- [];
  (* set the formals to much the original function's arguments *)
  setFunctionTypeMakeFormals f_new func_vi.vtype;
  setFunctionReturnType f_new intType;
  (* create the arg_size*[, arg_elsz*, arg_els*] formals *)
  let args_num = (List.length f_new.sformals)-1 in
  if ( args_num > (List.length args) ) then
  begin
    ignore(E.error "Number of arguments described in #pragma doesn't much the\
          number of arguments in the function declaration"); assert false
  end;
  for i = 0 to args_num do
    let (_, (arg_type, _, _, _)) = List.nth args i in
    ignore(makeFormalVar f_new ("arg_size"^(string_of_int i)) intType);
    if (is_strided arg_type) then begin
      ignore(makeFormalVar f_new ("arg_els"^(string_of_int i)) intType);
      ignore(makeFormalVar f_new ("arg_elsz"^(string_of_int i)) intType)
    end;
  done;

  if (!arch="cell") then begin
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
        instrs := (doArgument_cell i local_arg avail_task tmpvec f_new arg 
                    !stats !spu_file )@(!instrs);
      done;
    end;

    (* insert instrs before avail_task->active = ACTIVE;
      we place a Foo_32412312231() call just above avail_task->active = ACTIVE
      to achieve that for cell *)
    f_new.sbody.bstmts <- List.map (fun s -> replace_fake_call s "Foo_32412312231" (L.rev !instrs)) f_new.sbody.bstmts;
  end else begin
    let this = var (findLocal f_new "this") in
    let stmts : stmt list ref = ref [] in
    (* this->closure.funcid = (uint8_t)funcid; *)
    let this_closure = mkPtrFieldAccess this "closure" in
    let funcid_set = Set (mkFieldAccess this_closure "funcid",
    CastE(find_type !spu_file "uint8_t", integer !func_id), locUnknown) in
    (*(* this->closure.total_arguments = (uint8_t)arguments.size() *)
    instrs := Set (mkFieldAccess this_closure "total_arguments",
    CastE(find_type !spu_file "uint8_t", integer (args_num+1)), locUnknown)::!instrs;*)
    
    (* uint32_t limit *)
    let limit = makeLocalVar f_new "limit" (find_type !spu_file "uint32_t") in
    (* uint32_t e_addr; *)
    let e_addr = var (makeLocalVar f_new "e_addr" (find_type !spu_file "uint32_t")) in

    (* for each argument*)
    for i = 0 to args_num do
      let arg = List.nth args i in

      (* local_arg <- argument description *)
      stmts := (doArgument_x86 i this e_addr (var limit) f_new arg !spu_file
              !unaligned_args !block_size !ppc_file)@[mkStmtOneInstr funcid_set];
    done;

    (* Foo_32412312231 is located before assert(this->closure.total_arguments<MAX_ARGS); 
      for x86*)
    f_new.sbody.bstmts <- List.map (fun s -> replace_fake_call_with_stmt s "Foo_32412312231" (L.rev !stmts)) f_new.sbody.bstmts;
  end;

  incr func_id;
  f_new
end

(* parses the #pragma css task arguments and pushes them to ptdepa *)
let rec ptdepa_process_args typ args : unit =
  if ( args <> []) then begin
    match (L.hd args) with
      AIndex(ACons(varname, []), varsize) -> begin 
        Ptdepa.addArg (varname, typ, !currentFunction);
      end
      | _ -> ignore(E.log "Syntax error in #pragma tpc task %s(...)" typ);
    ptdepa_process_args typ (L.tl args)
  end

let rec ptdepa_process io : unit = begin
  match io with 
    (cur::rest) -> begin
      match cur with
        AStr("highpriority") -> (* simply ignore it *) ();
        | ACons(arg_typ, args) -> ptdepa_process_args arg_typ args
        | _ -> ignore(E.log "Syntax error in #pragma tpc task");
      ptdepa_process rest
    end
    | _ -> ();
end

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
                Instr(Call(_, Lval((Var(vi), _)), _, loc)::_) -> begin
                  ptdepa_process rest;
                  Ptdepa.addTask vi.vname !currentFunction loc;
                  prevstmt := s; DoChildren
                end
                | Block(b) -> ignore(E.warn "Ignoring block pragma at %a" d_loc loc); prevstmt := s; DoChildren
                | _ -> ignore(E.warn "Ignoring pragma at %a" d_loc loc); prevstmt := s; DoChildren
            end
            | _ -> ignore(E.warn "Ptdepa: Ignoring pragma at %a" d_loc loc); prevstmt := s; DoChildren
        end
        | (Attr("tpc", args), _) -> begin
          match s.skind with 
            Instr(Call(_, Lval((Var(vi), _)), _, loc)::_) -> begin
(*               let funname = vi.vname in *)
                ignore(List.map (fun arg -> match arg with
                    ACons(varname, ACons(arg_typ, [])::ACons(varsize, [])::[]) -> 
                      (* give all the arguments to Dtdepa*)
                      Ptdepa.addArg (varname, arg_typ, !currentFunction);
                  | ACons(varname, ACons(arg_typ, [])::ACons(varsize, [])::ACons(elsize, [])::ACons(elnum, [])::[]) ->
                      (* give all the arguments to Dtdepa don't care for strided  *)
                      Ptdepa.addArg (varname, arg_typ, !currentFunction);
                  | _ -> ignore(E.error "impossible"); assert false
                ) args);
                Ptdepa.addTask vi.vname !currentFunction loc;
                prevstmt := s; DoChildren
              end
            | Block(b) -> ignore(E.unimp "Ignoring block pragma"); prevstmt := s; DoChildren
            | _ -> ignore(E.warn "Ignoring pragma"); prevstmt := s; DoChildren
          end
        | _ -> ignore(E.warn "Unrecognized pragma"); prevstmt := s; DoChildren
     end else begin
       (* Get info about array indices from loops *)
       match s.skind with
          Loop(b_code, _, _, _) -> begin
            (* Check if there is any task inside the loop *)
            let tagged_stmts = L.filter tpc_call_with_arrray b_code.bstmts in
            if (tagged_stmts<>[]) then
              let successor = get_loop_successor s in
              let upper = get_loop_condition s in
              let lower = get_loop_lower s !prevstmt in
              ignore(E.log "\tlower=%a\n" d_exp lower);
              ignore(E.log "\tupper=%a\n" d_exp upper);
              ignore(E.log "\tsucc=%a\n"  d_exp successor);
              (*for each call
                let indice = get_indice tagged_stmt in*)
              (* visit the b_code.bstmts to find the Upper and the successor function *)
              prevstmt := s; DoChildren else begin
            prevstmt := s; DoChildren end
          end
        | _ -> prevstmt := s; DoChildren
    end
end

(* parses the #pragma css task arguments and pushes them to ptdepa *)
let rec s2s_process_args typ args =
  match args with
    (arg::rest) -> begin
      match arg with
(*         AIndex(ACons(varname, []), ACons(varsize, [])) -> *)
        AIndex(ACons(varname, []), varsize) ->
          let tmp_size = attrParamToExp varsize !currentFunction !ppc_file in
          (varname, ((translate_arg typ false),
              tmp_size, tmp_size, tmp_size))::(s2s_process_args typ rest)
(* TODO add support for optional sizes example inta would have size of sizeof(inta) *)
(*         | handle strided... *)
        | _ -> ignore(E.log "Syntax error in #pragma css task %s(...)" typ); []
    end
    | _ -> []

let rec s2s_process io =
  match io with 
    (cur::rest) -> begin
      match cur with
        AStr("highpriority") -> s2s_process rest
        | ACons(arg_typ, args) -> (s2s_process_args arg_typ args)@(s2s_process rest)
        | _ -> ignore(E.log "Syntax error in #pragma css task"); []
    end
    | _ -> []

(* populates the global list of spu tasks [spu_tasks] *)
class findSPUDeclVisitor cgraph = object
  inherit nopCilVisitor
  val callgraph = cgraph 
  (* visits all stmts and checks for pragma directives *)
  method vstmt (s: stmt) : stmt visitAction =
    (*ignore(match s.skind with 
      Instr(Call(_, Lval((Var(vi), _)), args, _)::_) ->
        L.iter (fun a -> ignore(E.log "arg= %a\n" d_exp a)) args;
      | _ -> (););*)
(*     print_endline ("Now in "^(!currentFunction).svar.vname); *)
(* if ((!currentFunction).svar.vname="ComputeLikelihood") then *)
(*     (dumpStmt defaultCilPrinter stdout 2 s); *)
(*     print_endline (""); *)
    let prags = s.pragmas in
    if (prags <> []) then (
      match (List.hd prags) with
        (Attr("css", AStr("wait")::rest), loc) -> (
          (* Support #pragma css wait on(...) *)
          match rest with 
              ACons("on", exps)::_ -> (* wait on *) DoChildren
            | AStr("all")::_ -> (
                let twa = find_function_sign (!ppc_file) "tpc_wait_all" in
                let instr = Call (None, Lval (var twa), [], locUnknown) in
                let s' = {s with pragmas = List.tl s.pragmas} in
                ChangeDoChildrenPost ((mkStmt (Block (mkBlock [ mkStmtOneInstr instr; s' ]))), fun x -> x)
(*                 ChangeTo (mkStmt (Block (mkBlock [ mkStmtOneInstr instr; s ]))) *)
                (* wait all *)
            )
            | _ -> ignore(E.warn "Ignoring wait pragma at %a" d_loc loc); DoChildren
        )
        | _ -> ();
      match s.skind with 
        Instr(Call(_, Lval((Var(vi), _)), _, _)::_) -> begin
          match (List.hd prags) with 
            (Attr("tpc", args), _) -> begin
              let funname = vi.vname in
              let args' =
                List.map (fun arg -> match arg with
(*                     ACons(varname, ACons(arg_typ, [])::ACons(varsize, [])::[]) -> *)
                    ACons(varname, ACons(arg_typ, [])::varsize::[]) ->
                      (* give all the arguments to Dtdepa*)
                      (varname, ((translate_arg arg_typ false),
                          attrParamToExp varsize !currentFunction !ppc_file,
                          attrParamToExp varsize !currentFunction !ppc_file,
                          attrParamToExp varsize !currentFunction !ppc_file))
(*                   | ACons(varname, ACons(arg_typ, [])::ACons(varsize, [])::ACons(elsize, [])::ACons(elnum, [])::[]) -> *)
                  | ACons(varname, ACons(arg_typ, [])::varsize::elsize::elnum::[]) ->
                      (* give all the arguments to Dtdepa don't care for strided  *)
                      (varname, ((translate_arg arg_typ true),
                        attrParamToExp varsize !currentFunction !ppc_file,
                        attrParamToExp elsize !currentFunction !ppc_file,
                        attrParamToExp elnum !currentFunction !ppc_file))
                  | _ -> ignore(E.error "impossible"); assert false
                ) args in
              ignore(E.log "Found task \"%s\"\n" funname);
              let rest new_fd = 
                (* push arguments to the call *)
                let call_args = ref [] in
                let args_num = (List.length args')-1 in
                for i = 0 to args_num do
                  let (vname, (_, _, _, _)) = List.nth args' i in
                  call_args := Lval(var (find_scoped_var !currentFunction !ppc_file vname))::!call_args;
                done;
                for i = 0 to args_num do
                  let (_, (arg_type, vsize, velsz, vels)) = List.nth args' i in
(*                   call_args := Lval(var (find_scoped_var !currentFunction !ppc_file vsize))::!call_args; *)
                  call_args := vsize::!call_args;
                  if (is_strided arg_type) then
(*                    call_args := Lval(var (find_scoped_var !currentFunction !ppc_file vels))::
                      Lval(var (find_scoped_var !currentFunction !ppc_file velsz))::!call_args;*)
                    call_args := vels::velsz::!call_args;
                done;
                let instr = Call (None, Lval (var new_fd.svar), L.rev !call_args, locUnknown) in
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
                  deep_copy_function funname callgraph !spu_file !ppc_file;
                  rest2 task.svar
                (* else try to find the function signature/prototype *)
                with Not_found -> begin
                  let task = find_function_sign (!ppc_file) funname in
                  rest2 task
                end
              end
            end
            (* Support for CellSs syntax *)
            | (Attr("css", sub::rest), loc) -> begin
              match sub with
                (* Support #pragma css task... *)
                AStr("task")-> begin
                  match s.skind with 
                    Instr(Call(_, Lval((Var(vi), _)), oargs, _)::_) -> begin
                      let funname = vi.vname in
                      let args = s2s_process rest in
                      ignore(E.log "Found task \"%s\"\n" funname);
                      let rest new_fd = 
                        (* add arguments to the call *)
                        let call_args = ref (L.rev oargs) in
(*                         let args_num = (List.length args)-1 in *)
                        
                        (* push call args from the start...
                        for i = 0 to args_num do
                          let (vname, _, _, _, _) = List.nth args i in
                          call_args := Lval(var (find_scoped_var !currentFunction !ppc_file vname))::!call_args;
                        done;*)

                        (* for each actual argument of the call find it's (pragma)
                           declared size and push it to the argument list of them
                           new call *)
                        let rec getSize ex = match ex with
                          Lval((Var(vi),_)) -> (
                            let (arg_type, vsize, velsz, vels) = L.assoc vi.vname args in
                            call_args := vsize::!call_args;
                          )
                          | CastE (_, ex') -> getSize ex';
                          | Const _ -> raise (Invalid_argument "Const");
                          | SizeOf _ -> raise (Invalid_argument "Sizeof");
                          | SizeOfE _ -> raise (Invalid_argument "SizeofE");
                          | SizeOfStr _ -> raise (Invalid_argument "SizeofStr");
                          | AlignOf _ -> raise (Invalid_argument "Alignof");
                          | AlignOfE _ -> raise (Invalid_argument "AlignOfE");
                          | UnOp _ -> raise (Invalid_argument "UnOp");
                          | BinOp _ -> raise (Invalid_argument "BinOp");
                          | AddrOf _ -> raise (Invalid_argument "AddrOf");
                          | StartOf _ -> raise (Invalid_argument "StartOf");
                        in
                        L.iter getSize oargs;

(*                        for i = 0 to args_num do
                          let (_, arg_type, vsize, velsz, vels) = List.nth args i in
(*                           call_args := Lval(var (find_scoped_var !currentFunction !ppc_file vsize))::!call_args; *)
                          call_args := vsize::!call_args;
                          if (is_strided arg_type) then
                            (*call_args := Lval(var (find_scoped_var !currentFunction !ppc_file vels))::
                              Lval(var (find_scoped_var !currentFunction !ppc_file velsz))::!call_args;*)
                            call_args := vels::velsz::!call_args;
                        done;*)
                        let instr = Call (None, Lval (var new_fd.svar), L.rev !call_args, locUnknown) in
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
                          deep_copy_function funname callgraph !spu_file !ppc_file;
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
                | _ -> ignore(E.warn "Unrecognized pragma"); DoChildren
            end
            | _ -> ignore(E.warn "Unrecognized pragma"); DoChildren
          end
        | Block(b) -> ignore(E.unimp "Ignoring block pragma"); DoChildren
        | _ -> ignore(E.warn "Ignoring pragma"); DoChildren
    ) else 
      DoChildren
end

let make_case execfun (task: varinfo) (task_info: varinfo)
              (ex_task: varinfo) (args: arg_descr list): stmt = begin
  let res = ref [] in
  assert(isFunctionType task.vtype);
  let ret, arglopt, hasvararg, _ = splitFunctionType task.vtype in
  assert(not hasvararg);
  let argl = match arglopt with None -> [] | Some l -> l in
  let argaddr = makeTempVar execfun voidPtrType in
  if (!arch = "cell") then
    res := Set(var argaddr, Lval (mkPtrFieldAccess (var task_info) "ls_addr"), locUnknown) :: !res;
(*  else begin
    res := Set(var argaddr, Lval (mkPtrFieldAccess (var task_info) "local"), locUnknown) :: !res;
  end*)
  let nextaddr n stride =
    if (!arch = "cell") then begin (* Cell *)
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
    end else begin (* X86 *)
      let lv = mkPtrFieldAccess (var task_info) "local" in
      let t = typeOfLval lv in
      assert(isArrayType t);
      let idxlv = addOffsetLval (Index(integer n, NoOffset)) lv in
      Set(var argaddr, Lval(idxlv), locUnknown);
    end
  in
  let i = ref 0 in
  let carry = ref dummyInstr in
  let arglist = List.map
    (fun (_, argt, _) ->
      let argvar = makeTempVar execfun argt in
      let rec castexp atyp = match atyp with
        TInt(_, _)
        | TFloat(_, _)
        | TEnum(_, _)
        | TComp(_, _) -> 
          CastE(argt, Lval(mkMem (CastE( TPtr(argt, []), Lval(var argaddr))) NoOffset))
        | TNamed(_, _) -> castexp (unrollType atyp)
        | _ -> CastE(argt, Lval(var argaddr))
      in
      let castinstr = Set(var argvar, castexp argt, locUnknown) in
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
  mkStmt (Instr (L.rev !res))
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
let make_exec_func (f: file) (tasks: (fundec * varinfo * arg_descr list) list) : global = begin
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
      if (!arch = "unknown") then
        ignore(E.error "No architecture specified. Exiting!\n")
      else if (!queue_size = "0") then
        ignore(E.error "No queue_size specified. Exiting!\n")
      else begin
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

        let def = ref (" -DMAX_QUEUE_ENTRIES="^(!queue_size)) in
        def := " "^(!cflags)^" "^(!def);
        if (!stats) then
          def := " -DSTATISTICS=1"^(!def);
        if(!arch = "cell") then begin
          (* copy all code from file f to file_ppc *)
(*           ignore(E.warn "Path = %s\n" !tpcIncludePath); *)
          
          preprocessAndMergeWithHeader !ppc_file ((!tpcIncludePath)^"/s2s/tpc_s2s.h") (" -DPPU=1"^(!def))
                                      !arch !tpcIncludePath;

          (* copy all typedefs and enums/structs/unions from ppc_file to spu_file
            plus the needed headers *)
          let new_types_l = List.filter is_typedef (!ppc_file).globals in
          (!spu_file).globals <- new_types_l;
          preprocessAndMergeWithHeader !spu_file ((!tpcIncludePath)^"/s2s/tpc_s2s.h") (" -DSPU=1"^(!def))
                                      !arch !tpcIncludePath;
        end else
          preprocessAndMergeWithHeader !ppc_file ((!tpcIncludePath)^"/s2s/tpc_s2s.h") (" -DX86tpc=1"^(!def))
                                      !arch !tpcIncludePath;

        Cil.iterGlobals !ppc_file 
          (function
            GFun(fd,_) ->
              currentFunction := fd;
              ignore(visitCilFunction ftagVisitor fd);
            | _ -> ()
          )
        ;

        (* kasas was here :P *)
(*         Ptdepa.find_dependencies f; *)

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
        let tasks : (fundec * varinfo * arg_descr list) list = List.map
          (fun (name, (new_fd, old_fd, args)) -> (new_fd, old_fd, args))
          (L.rev !spu_tasks)
        in
        (!spu_file).globals <- (!spu_file).globals@[(make_exec_func !spu_file tasks)];
        (*(* remove the "tpc_call_tpcAD65" function from the ppc_file *)
        (!ppc_file).globals <- List.filter isNotSkeleton (!ppc_file).globals;*)
        writeFile !ppc_file;
  (*       !spu_file.globals <- !spu_glist; *)
        writeFile !spu_file;
  (*       writeNewFile f (!out_name^"_func.c") !spu_glist; *)
      end
    );
    fd_post_check = true;
  }
