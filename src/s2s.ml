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
let unaligned_args = ref false
let out_name = ref "final"
let queue_size = ref "0"
let block_size = ref 0
let arch = ref "unknown"
let currentFunction = ref dummyFunDec
let prevstmt = ref dummyStmt

let options =
  [
    "--arch",
      Arg.String(fun s -> arch := s),
      " S2S: Define the target architecture (x86/cell).";

    "--debug-tpctool",
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

let voidType = TVoid([])
let intType = TInt(IInt,[])
let uintType = TInt(IUInt,[])
let longType = TInt(ILong,[])
let ulongType = TInt(IULong,[])
let charType = TInt(IChar, [])
let boolType = TInt(IBool, [])

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
        "in" (* legacy *)
      | "input" -> In
      | "out" (* legacy *)
      | "output" -> Out
      | "inout" -> InOut
      | _ -> ignore(E.error "Only input/output/inout are allowed"); assert false

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
let get_arg_type (arg: (string * arg_t * exp * exp * exp )) : arg_t =
  match arg with
    (_, arg_type ,_ ,_ ,_) -> arg_type


let arg_t2integer = function
    | In -> integer 1
    | Out -> integer 2
    | InOut -> integer 3
    | SIn -> integer 5
    | SOut -> integer 6
    | SInOut -> integer 7

let arg_t2int = function
    | In -> 1
    | Out -> 2
    | InOut -> 3
    | SIn -> 5
    | SOut -> 6
    | SInOut -> 7

let doArgument_cell (i: int) (local_arg: lval) (avail_task: lval) (tmpvec: lval) (fd: fundec)
 (arg: (string * arg_t * exp * exp * exp)) : instr list = begin
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
    (* local_arg.stride = arg_size; *)
    let stride = mkFieldAccess local_arg "stride" in
    il := Set(stride, arg_size, locUnknown)::!il;
  end else
    (* local_arg.size = arg_size; *)
    il := Set(size, arg_size, locUnknown)::!il;
  (* local_arg.flag = arg_flag; *)
  let flag = mkFieldAccess local_arg "flag" in
  il:= Set(flag, arg_t2integer arg_type, locUnknown)::!il;
  (* *tmpvec = *((volatile vector unsigned char * )&local_arg); *)
  let casted_la = CastE(vector_uchar_p, AddrOf(local_arg)) in
  il := Set(mkMem (Lval(tmpvec)) NoOffset, Lval(mkMem casted_la NoOffset), locUnknown)::!il;
  !il
end


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

let doArgument_x86 (i: int) (this: lval) (e_addr: lval) (limit: lval) (fd: fundec)
 (arg: (string * arg_t * exp * exp * exp)) : stmt list = begin
  let closure = mkPtrFieldAccess this "closure" in
  let arg_size = Lval( var (find_formal_var fd ("arg_size"^(string_of_int i)))) in
  let arg_addr = var (List.nth fd.sformals i) in
  let arg_type = get_arg_type arg in
  let stl = ref [] in
  let il = ref [] in
  let total_arguments = mkFieldAccess closure "total_arguments" in
  let arguments = mkFieldAccess closure "arguments" in
  let t = typeOfLval arguments in
  assert(isArrayType t);
  (* this->closure.arguments[  this->closure.total_arguments ].stride=0;
     due to not supporting stride args*)
  let idxlv = addOffsetLval (Index(Lval total_arguments, NoOffset)) arguments in
  let stride = mkFieldAccess idxlv "stride" in
  il := Set(stride, (integer 0), locUnknown)::!il;

  (* uint32_t block_index_start=this->closure.total_arguments; *)
  let uint32_t = (find_type !spu_file "uint32_t") in
  let bis = var (makeLocalVar fd "block_index_start" uint32_t) in
  il := Set(bis, Lval total_arguments, locUnknown)::!il;

  (* limit=(((uint32_t)arg_addr64)+arg_size); *)
  let plus = (BinOp(PlusA, CastE(uint32_t, Lval arg_addr), arg_size, uint32_t)) in
  il := Set(limit, plus, locUnknown)::!il;

  let size = mkFieldAccess idxlv "size" in
  let flag = mkFieldAccess idxlv "flag" in
  let pplus = (BinOp(PlusA, Lval total_arguments, integer 1, intType)) in

  (* TODO take list from ptdepa *)
  if (false) then begin
    (* if(TPC_IS_SAFEARG(arg_flag)){

        this->closure.arguments[  this->closure.total_arguments ].size    = arg_size;
        this->closure.arguments[  this->closure.total_arguments ].flag    = arg_flag|TPC_START_ARG;

        this->closure.arguments[  this->closure.total_arguments ].eal_in  = (uint32_t) arg_addr64;
        this->closure.arguments[  this->closure.total_arguments ].eal_out = (uint32_t) arg_addr64;
        this->closure.total_arguments++;
        continue; //We don't need continue here, we are not in a loop :)
      }
      #define TPC_START_ARG   0x10
    *)
    il := Set(size, arg_size, locUnknown)::!il;
    il := Set(flag, integer ( (arg_t2int arg_type) lor 0x10), locUnknown)::!il;
    let eal_in = mkFieldAccess idxlv "eal_in" in
    il := Set(eal_in, CastE(uint32_t, Lval arg_addr), locUnknown)::!il;
    let eal_out = mkFieldAccess idxlv "eal_out" in
    il := Set(eal_out, CastE(uint32_t, Lval arg_addr), locUnknown)::!il;
    stl := (*mkStmt(Continue locUnknown)::*)[mkStmt(Instr (L.rev !il))];
  end else begin

    (*#ifdef UNALIGNED_ARGUMENTS_ALLOWED
        uint32_t tmp_addr=(uint32_t)arg_addr64;
        arg_addr64 = (void* )(((uint32_t)(tmp_addr/BLOCK_SZ))*BLOCK_SZ);
        this->closure.arguments[ this->closure.total_arguments].stride = tmp_addr-(uint32_t)arg_addr64;
        arg_size +=this->closure.arguments[ this->closure.total_arguments ].stride;
        //      limit +=this->closure.arguments[ this->closure.total_arguments ].stride;
        e_addr=(uint32_t)arg_addr64;
      #endif*)
    if (!unaligned_args) then begin
      let tmp_addr = var (makeLocalVar fd "tmp_addr" uint32_t) in
      il := Set(tmp_addr, Lval arg_addr, locUnknown)::!il; 
      let div = BinOp(Div, Lval tmp_addr, integer !block_size, uint32_t) in
      let mul = BinOp(Mult, CastE(uint32_t, div), integer !block_size, voidPtrType) in
      il := Set(arg_addr, CastE(voidPtrType, mul), locUnknown)::!il;
      let new_stride = BinOp(MinusA, Lval tmp_addr, CastE(uint32_t, Lval arg_addr), intType) in
      il := Set(stride, new_stride, locUnknown)::!il;
      il := Set(e_addr, CastE(uint32_t, Lval arg_addr), locUnknown)::!il;
    end;

    (*for(e_addr=(uint32_t)arg_addr64;e_addr + BLOCK_SZ <= limit ;e_addr+=BLOCK_SZ){
      this->closure.arguments[  this->closure.total_arguments ].flag = arg_flag;
      this->closure.arguments[  this->closure.total_arguments ].size = BLOCK_SZ;
      AddAttribute_Task( this, (void* )(e_addr), arg_flag,BLOCK_SZ);
      this -> closure.total_arguments++;
      this->closure.arguments[ this->closure.total_arguments ].stride=0;
    }*)
    let closure_flag = Set(flag, arg_t2integer arg_type, locUnknown) in
    let ilt = ref [closure_flag] in
    ilt := Set(size, integer !block_size, locUnknown)::!ilt;
    let addAttribute_Task = find_function_sign (!ppc_file) "AddAttribute_Task" in
    let args = [Lval this; Lval e_addr; arg_t2integer arg_type; integer !block_size ] in
    ilt := Call (None, Lval (var addAttribute_Task), args, locUnknown)::!ilt;
    ilt := Set(total_arguments, pplus, locUnknown)::!ilt;
    let start = [mkStmtOneInstr (Set(e_addr, Lval arg_addr, locUnknown))] in
    let e_addr_plus = BinOp(PlusA, Lval e_addr, integer !block_size, intType) in
    let guard = BinOp(Le, e_addr_plus, Lval limit, boolType) in
    let next = [mkStmtOneInstr (Set(e_addr, e_addr_plus, locUnknown))] in
    let body = [mkStmt (Instr (L.rev !ilt))] in
    stl := L.rev (mkStmt(Instr (L.rev !il))::(mkFor start guard next body));

    (*if(limit-e_addr){
      this->closure.arguments[  this->closure.total_arguments ].flag = arg_flag;
      this->closure.arguments[  this->closure.total_arguments ].size = limit-e_addr;
      AddAttribute_Task( this, (void* )(e_addr), arg_flag,this->closure.arguments[  this->closure.total_arguments ].size);
      this -> closure.total_arguments++;
    }*)
    let sub = (BinOp(MinusA, Lval limit, Lval e_addr, boolType)) in
    ilt := [closure_flag];
    ilt := Set(size, sub, locUnknown)::!ilt;
    let args = [Lval this; Lval e_addr; arg_t2integer arg_type; Lval size] in
    ilt := Call (None, Lval (var addAttribute_Task), args, locUnknown)::!ilt;
    ilt := Set(total_arguments, pplus, locUnknown)::!ilt;
    let bl = mkBlock [mkStmt(Instr (L.rev !ilt))] in
    stl := (mkStmt (If(sub, bl, mkBlock [], locUnknown)))::!stl;

    (* this->closure.arguments[ block_index_start ].flag|=TPC_START_ARG;
      tpc_common.h:20:#define TPC_START_ARG   0x10 *)
    let idxlv = addOffsetLval (Index(Lval bis, NoOffset)) arguments in
    let flag = mkFieldAccess idxlv "flag" in
    stl := mkStmtOneInstr(Set(flag, integer 0x10, locUnknown))::!stl;
  end;

  (* skipping assert( (((unsigned)arg_addr64&0xF) == 0) && ((arg_size&0xF) == 0)); *)
  !stl
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
let make_tpc_func (func_vi: varinfo) (args: (string * arg_t * exp * exp * exp ) list) : fundec = begin
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
        instrs := (doArgument_cell i local_arg avail_task tmpvec f_new arg )@(!instrs);
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
      stmts := (doArgument_x86 i this e_addr (var limit) f_new arg )@[mkStmtOneInstr funcid_set];
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

exception CouldntGetLoopLower of stmt 
let get_lower (s: stmt) : exp =
  match (!prevstmt).skind with
      Instr(il) -> begin
        match (L.hd (L.rev il)) with
          Set(_, e, _) -> e
        | _ -> raise (CouldntGetLoopLower s)
      end
    | _ -> raise (CouldntGetLoopLower s)

exception CouldntGetLoopUpper of stmt 
let get_upper (s: stmt) : exp =
  match s.skind with
      Loop(b, _, _, _) -> begin
        match (L.hd b.bstmts).skind with
            If(ec, _, _, _) -> ec
(*           | If(UnOp(LNot, ec, _), _, _, _) -> ec *)
          | _ -> raise (CouldntGetLoopUpper s)
      end
    | _ -> raise (CouldntGetLoopUpper s)

exception CouldntGetLoopSuccesor of stmt 
let get_successor (s: stmt) : exp =
  match s.skind with
      Loop(b, _, _, _) -> begin
        let lstmt = L.hd (L.rev b.bstmts) in
          match lstmt.skind with
            Instr(il) -> begin
              match (L.hd (L.rev il)) with
                Set(_, e, _) -> e
              | _ -> raise (CouldntGetLoopSuccesor s)
            end
          | _ -> raise (CouldntGetLoopSuccesor s)
      end
    | _ -> raise (CouldntGetLoopSuccesor s)

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
                  prevstmt := s; DoChildren
                end
                | Block(b) -> ignore(E.warn "Ignoring block pragma at %a" d_loc loc); prevstmt := s; DoChildren
                | _ -> ignore(E.warn "Ignoring pragma at %a" d_loc loc); prevstmt := s; DoChildren
            end
            | _ -> ignore(E.warn "Ptdepa: Ignoring pragma at %a" d_loc loc); prevstmt := s; DoChildren
        end
        | (Attr("tpc", args), _) -> begin
          match s.skind with 
            Instr(Call(_, Lval((Var(vi), _)), _, _)::_) -> begin
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
                Ptdepa.addTask vi.vname !currentFunction;
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
              let successor = get_successor s in
              let upper = get_upper s in
              let lower = get_lower s in
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

(* Convert an attribute into an expression, if possible. Otherwise raise 
 * NotAnExpression *)
exception NotAnExpression of attrparam
let rec attrParamToExp (a: attrparam) : exp= 
  match a with
      AInt(i) -> integer i                    (** An integer constant *)
    | AStr(s) -> Const(CStr s)                (** A string constant *)
    | ACons(name, []) ->                      (** An id *)
      Lval (Var (find_scoped_var !currentFunction !ppc_file name) , NoOffset)
    (* We don't support function calls as argument size *)
    (*| ACons(name, args) ->                    (** A function call *)
      let args' = L.map (fun a -> attrParamToExp a) args in
      let instr = Call (None, Lval (var find_function_sign !ppc_file name), args', locUnknown) in
      let call = mkStmtOneInstr instr in
      Lval (var (find_scoped_var !currentFunction !ppc_file name) , NoOffset)*)
    | ASizeOf(t) -> SizeOf t                  (** A way to talk about types *)
    | ASizeOfE(a) -> SizeOfE (attrParamToExp a)
    | AAlignOf(t) -> AlignOf t
    | AAlignOfE(a) -> AlignOfE (attrParamToExp a)
    | AUnOp(op, a) -> UnOp(op, attrParamToExp a, intType) (* how would i know what type to put? *)
    | ABinOp(op, a, b) -> BinOp(op, attrParamToExp a, attrParamToExp b, intType) (* same as above *)
    | ADot(a, s) -> begin                    (** a.foo **)
      let predot = attrParamToExp a in
      match predot with Lval(v) ->
          Lval (mkFieldAccess v s)
        | _ -> raise (NotAnExpression a)
    end
    | AStar(a) -> Lval(mkMem (attrParamToExp a) NoOffset) (** * a *)
    | AAddrOf(a) -> begin                                 (** & a **)
      let ar = attrParamToExp a in
      match ar with Lval(v) ->
          mkAddrOf v
        | _ -> raise (NotAnExpression a)
    end
    | AIndex(a, i) -> begin                               (** a1[a2] *)
      let arr = attrParamToExp a in
      match arr with Lval(v) ->
          Lval(addOffsetLval (Index(attrParamToExp i, NoOffset)) v)
        | _ -> raise (NotAnExpression a)
    end
    (* not supported *)
(*    | AQuestion of attrparam * attrparam * attrparam (** a1 ? a2 : a3 **)*)
    | _ -> raise (NotAnExpression a)

(* parses the #pragma css task arguments and pushes them to ptdepa *)
let rec s2s_process_args typ args =
  match args with
    (arg::rest) -> begin
      match arg with
(*         AIndex(ACons(varname, []), ACons(varsize, [])) -> *)
        AIndex(ACons(varname, []), varsize) ->
          (varname, (translate_arg typ false), attrParamToExp varsize, attrParamToExp varsize, attrParamToExp varsize)::(s2s_process_args typ rest)
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
    (*ignore(match s.skind with 
      Instr(Call(_, Lval((Var(vi), _)), args, _)::_) ->
        L.iter (fun a -> ignore(E.log "arg= %a\n" d_exp a)) args;
      | _ -> (););*)
    let prags = s.pragmas in
    if (prags <> []) then begin
      match (List.hd prags) with
        (Attr("css", AStr("wait")::rest), loc) -> begin
          (* Support #pragma css wait on(...) *)
          match rest with 
              ACons("on", exps)::_ -> (* wait on *) DoChildren
            | AStr("all")::_ -> begin
                let twa = find_function_sign (!ppc_file) "tpc_wait_all" in
                let instr = Call (None, Lval (var twa), [], locUnknown) in
                ChangeTo (mkStmt (Block (mkBlock [ mkStmtOneInstr instr; s ])))
                (* wait all *)
            end
            | _ -> ignore(E.warn "Ignoring wait pragma at %a" d_loc loc); DoChildren
        end
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
                      (varname, (translate_arg arg_typ false), attrParamToExp varsize, attrParamToExp varsize, attrParamToExp varsize)
(*                   | ACons(varname, ACons(arg_typ, [])::ACons(varsize, [])::ACons(elsize, [])::ACons(elnum, [])::[]) -> *)
                  | ACons(varname, ACons(arg_typ, [])::varsize::elsize::elnum::[]) ->
                      (* give all the arguments to Dtdepa don't care for strided  *)
                      (varname, (translate_arg arg_typ true), attrParamToExp varsize, attrParamToExp elsize, attrParamToExp elnum)
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
                  deep_copy_function funname callgraph;
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
                        let args_num = (List.length args)-1 in(*
                        for i = 0 to args_num do
                          let (vname, _, _, _, _) = List.nth args i in
                          call_args := Lval(var (find_scoped_var !currentFunction !ppc_file vname))::!call_args;
                        done;*)
                        for i = 0 to args_num do
                          let (_, arg_type, vsize, velsz, vels) = List.nth args i in
(*                           call_args := Lval(var (find_scoped_var !currentFunction !ppc_file vsize))::!call_args; *)
                          call_args := vsize::!call_args;
                          if (is_strided arg_type) then
                            (*call_args := Lval(var (find_scoped_var !currentFunction !ppc_file vels))::
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
                | _ -> ignore(E.warn "Unrecognized pragma"); DoChildren
            end
            | _ -> ignore(E.warn "Unrecognized pragma"); DoChildren
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
              (ex_task: varinfo) (args: (string * arg_t * exp * exp * exp) list): stmt = begin
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
let make_exec_func (f: file) (tasks: (fundec * varinfo * (string * arg_t * exp * exp * exp) list) list) : global = begin
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
  ignore (Sys.command ("echo | gcc -E -D"^def^"=1 -DMAX_QUEUE_ENTRIES="^(!queue_size)^"U "^(!statistics)^" -I./include/ppu -I./include/spu "^(header)^" - >/tmp/_cil_rewritten_tmp.h"));
(* print_endline ("gcc -E -D"^def^"=1 -DMAX_QUEUE_ENTRIES="^(!queue_size)^"U "^(!statistics)^" -I./include/ppu -I./include/spu "^(header)); *)
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

        if(!arch = "cell") then begin
          (* copy all code from file f to file_ppc *)
          preprocessAndMergeWithHeader !ppc_file "tpc_s2s.h" "PPU";

          (* copy all typedefs and enums/structs/unions from ppc_file to spu_file
            plus the needed headers *)
          let new_types_l = List.filter is_typedef (!ppc_file).globals in
          (!spu_file).globals <- new_types_l;
          preprocessAndMergeWithHeader !spu_file "tpc_s2s.h" "SPU";
        end else
          preprocessAndMergeWithHeader !ppc_file "tpc_s2s.h" "X86tpc";

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
        let tasks : (fundec * varinfo * (string * arg_t * exp * exp * exp) list) list = List.map
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
