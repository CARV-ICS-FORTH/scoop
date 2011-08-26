(*
 *
 * Copyright (c) 2010, 
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

open Cil
open Scoop_util
module L = List
module E = Errormsg

(* keeps the current funcid for the new tpc_function *)
let func_id = ref 0

(* a unique id for the tpc_function_* *)
let un_id = ref 0

let doArgument (i: int) (this: lval) (e_addr: lval) (bis: lval)
 (fd: fundec) (arg: (int * arg_descr) ) (ppc_file: file) : stmt list = (
  let closure = mkPtrFieldAccess this "closure" in
  let uint32_t = (find_type ppc_file "uint32_t") in
  let uint64_t = (find_type ppc_file "uint64_t") in
  let arg_size = var (find_formal_var fd ("arg_size"^(string_of_int i))) in
  let block_size = var (find_global_var ppc_file "__block_sz") in
  let arg_addr = var (List.nth fd.sformals i) in
  let (i_m, (arg_name, (arg_type ,_ ,_ ,_))) = arg in
(*   print_endline ("Doing "^arg_name); *)
  let stl = ref [] in
  let il = ref [] in
  let total_arguments = mkFieldAccess closure "total_arguments" in
  let arguments = mkFieldAccess closure "arguments" in
  let t = typeOfLval arguments in
  assert(isArrayType t);
  (* this->closure.arguments[  this->closure.total_arguments ].stride=TPC_IS_STRIDEARG(arg_flag)? va_arg(arg_list, int):0; *)
  let idxlv = addOffsetLval (Index(Lval total_arguments, NoOffset)) arguments in
  let stride = mkFieldAccess idxlv "stride" in
  il := (* FIX here *)
    ( if (is_strided arg_type) then (
        Set(stride, Lval arg_size, locUnknown)
      ) else
        Set(stride, (integer 0), locUnknown)
    )::!il;


  let size = mkFieldAccess idxlv "size" in
  let flag = mkFieldAccess idxlv "flag" in
  let pplus = (BinOp(PlusA, Lval total_arguments, one, intType)) in

(*  this->closure.arguments[this->closure.total_arguments].eal_in = arg_addr64;
  this->closure.arguments[this->closure.total_arguments].eal_out = arg_addr64;
  this->closure.arguments[this->closure.total_arguments].size = arg_size;
  this->closure.arguments[this->closure.total_arguments].flag = arg_flag;*)
  let eal_in = mkFieldAccess idxlv "eal_in" in
  il := Set(eal_in, CastE(voidPtrType, Lval arg_addr), locUnknown)::!il;
  let eal_out = mkFieldAccess idxlv "eal_out" in
  il := Set(eal_out, CastE(voidPtrType, Lval arg_addr), locUnknown)::!il;
  il := Set(size, Lval arg_size, locUnknown)::!il;
(*   il := Set(flag, integer (arg_t2int arg_type), locUnknown)::!il; *)

  (* invoke isSafeArg from PtDepa to check whether this argument is a no dep *)
  if (Ptdepa.isSafeArg arg_name) then (
(*       let (Var vi, _) = arg_addr in *)
(*       print_endline ("And it's safe "^vi.vname); *)
(*       print_endline ("And it's safe "^arg_name); *)
    (* if(TPC_IS_SAFEARG(arg_flag)){
        //uint64_t e_addr=(uint64_t) arg_addr64;
        //this->closure.arguments[  this->closure.total_arguments ].size = arg_size;
        this->closure.arguments[  this->closure.total_arguments ].flag = arg_flag|TPC_START_ARG;

        //this->closure.arguments[  this->closure.total_arguments ].eal_in  = (void* )e_addr;
        //this->closure.arguments[  this->closure.total_arguments ].eal_out = (void* )e_addr;
        this->closure.total_arguments++;
        continue;
      }
      #define TPC_START_ARG   0x10
      #define TPC_SAFE_ARG    0x8
    *)
(*     il := Set(size, Lval arg_size, locUnknown)::!il; *)
    (* this->closure.arguments[  this->closure.total_arguments ].flag = arg_flag|TPC_START_ARG|TPC_SAFE_ARG; *)
    il := Set(flag, integer ( (arg_t2int arg_type) lor 0x18), locUnknown)::!il;
(*    let eal_in = mkFieldAccess idxlv "eal_in" in
    il := Set(eal_in, CastE(uint64_t, Lval arg_addr), locUnknown)::!il;
    let eal_out = mkFieldAccess idxlv "eal_out" in
    il := Set(eal_out, CastE(uint64_t, Lval arg_addr), locUnknown)::!il;*)
    (* this -> closure.total_arguments++; *)
    il := Set(total_arguments, pplus, locUnknown)::!il;
    stl := (*mkStmt(Continue locUnknown)::*)[mkStmt(Instr (L.rev !il))];
  ) else (

    (* this->closure.arguments[this->closure.total_arguments].flag = arg_flag; *)
    il := Set(flag, integer (arg_t2int arg_type), locUnknown)::!il;
    (* uint32_t block_index_start=this->closure.total_arguments; *)
    il := Set(bis, Lval total_arguments, locUnknown)::!il;

    if (is_strided arg_type) then (
      let arg_elsz = Lval (var (find_formal_var fd ("arg_elsz"^(string_of_int i_m)))) in
      let arg_els = Lval (var (find_formal_var fd ("arg_els"^(string_of_int i_m)))) in

      (*  if(TPC_IS_STRIDEARG(arg_flag)){
          uint32_t j;
          uint32_t stride=this->closure.arguments[  this->closure.total_arguments ].stride ;
          uint64_t e_addr=(uint64_t)arg_addr64;
          uint32_t numElems=TPC_EXTRACT_STRIDEARG_ELEMS(arg_size);
        #ifdef UNALIGNED_ARGUMENTS_ALLOWED
          this->closure.arguments[ this->closure.total_arguments ].stride = 0;
        #endif
          for(j=0;j<numElems/*(unsigned)TPC_EXTRACT_STRIDEARG_ELEMS(arg_size)*/;j++,e_addr+=stride){
          //this->closure.arguments[  this->closure.total_arguments ].flag = arg_flag&~TPC_STRIDE_ARG;
          //this->closure.arguments[  this->closure.total_arguments ].size = TPC_EXTRACT_STRIDEARG_ELEMSZ(arg_size);
            AddAttribute_Task(this, (void * )(e_addr/*(uint32_t)arg_addr64 + stride *j*/), arg_flag&~TPC_STRIDE_ARG,TPC_EXTRACT_STRIDEARG_ELEMSZ(arg_size));
      //this->closure.total_arguments++;
          }
        }*)
      il :=  Set(e_addr, CastE(uint64_t, Lval arg_addr), locUnknown)::!il;
      let j_var = 
        try var (__find_local_var fd "j")
        with Not_found -> var (makeLocalVar fd "j" uint32_t)
      in
      let stride_var =
        try var (__find_local_var fd "stride")
        with Not_found -> var (makeLocalVar fd "stride" uint32_t)
      in
      il := Set(stride_var, Lval stride, locUnknown)::!il;
      if (!unaligned_args) then (
        il := Set(stride, (integer 0), locUnknown)::!il;
      );

      let ilt = ref [] in
  (*     let closure_flag = Set(flag, arg_t2integer arg_type, locUnknown) in *)
  (*     ilt := (closure_flag::!ilt; *)
  (*     ilt := Set(size, Lval block_size, locUnknown)::!ilt; *)
      let addAttribute_Task = find_function_sign ppc_file "AddAttribute_Task" in
      let args = [Lval this; CastE(voidPtrType, Lval e_addr); integer (arg_t2int arg_type); arg_elsz] in
      ilt := Call (None, Lval (var addAttribute_Task), args, locUnknown)::!ilt;
  (*     ilt := Set(total_arguments, pplus, locUnknown)::!ilt; *)
      let start = [mkStmtOneInstr (Set(j_var, zero, locUnknown))] in
      let e_addr_plus = BinOp(PlusA, Lval e_addr, Lval stride_var, intType) in
      let j_plus = BinOp(PlusA, Lval j_var, one, intType) in
      let guard = BinOp(Le, j_plus, arg_els, boolType) in
      let next = [mkStmt( Instr ([Set(j_var, j_plus, locUnknown); Set(e_addr, e_addr_plus, locUnknown)]))] in
      let body = [mkStmt (Instr (L.rev !ilt))] in
      stl := L.rev (mkStmt(Instr (L.rev !il))::(mkFor start guard next body));

    ) else (

      (* const uint64_t limit=(((uint64_t)arg_addr64)+arg_size); *)
      let limit =
        try var (__find_local_var fd "limit")
        with Not_found -> var (makeLocalVar fd "limit" uint64_t)
      in
      let plus = (BinOp(PlusA, CastE(uint64_t, Lval arg_addr), Lval arg_size, uint64_t)) in
      il := Set(limit, plus, locUnknown)::!il;

      (*#ifdef UNALIGNED_ARGUMENTS_ALLOWED
          printf("ADAM Warning: Unaligned argument\n");
          uint64_t tmp_addr=(uint64_t)arg_addr64;
          arg_addr64=((uint64_t)(tmp_addr/BLOCK_SZ))*BLOCK_SZ;
          this->closure.arguments[arg_index].stride = tmp_addr-(uint64_t)arg_addr64;
        #endif*)
      if (!unaligned_args) then (
        let printf = find_function_sign ppc_file "printf" in
        let args = [Const(CStr("ADAM Warning: Unaligned argument\n"))] in
        il := Call (None, Lval (var printf), args, locUnknown)::!il;
        let tmp_addr = var (makeLocalVar fd "tmp_addr" uint64_t) in
        il := Set(tmp_addr, Lval arg_addr, locUnknown)::!il; 
        let div = BinOp(Div, Lval tmp_addr, Lval block_size, uint64_t) in
        let mul = BinOp(Mult, CastE(uint64_t, div), Lval block_size, voidPtrType) in
        il := Set(arg_addr, CastE(voidPtrType, mul), locUnknown)::!il;
        let new_stride = BinOp(MinusA, Lval tmp_addr, CastE(uint64_t, Lval arg_addr), intType) in
        il := Set(stride, new_stride, locUnknown)::!il;
      );

      (*for(e_addr=(uint64_t)arg_addr64;e_addr + BLOCK_SZ <= limit ;e_addr+=BLOCK_SZ){
        //this->closure.arguments[  this->closure.total_arguments ].flag = arg_flag;
        //this->closure.arguments[  this->closure.total_arguments ].size = BLOCK_SZ;
        AddAttribute_Task( this, (void * )(e_addr), arg_flag,BLOCK_SZ);
        //this -> closure.total_arguments++;
      }*)
      let ilt = ref [] in
  (*     let closure_flag = Set(flag, arg_t2integer arg_type, locUnknown) in *)
  (*     ilt := (closure_flag::!ilt; *)
  (*     ilt := Set(size, Lval block_size, locUnknown)::!ilt; *)
      let addAttribute_Task = find_function_sign ppc_file "AddAttribute_Task" in
      let args = [Lval this; CastE(voidPtrType, Lval e_addr); arg_t2integer arg_type; Lval block_size ] in
      ilt := Call (None, Lval (var addAttribute_Task), args, locUnknown)::!ilt;
  (*     ilt := Set(total_arguments, pplus, locUnknown)::!ilt; *)
      let start = [mkStmtOneInstr (Set(e_addr, CastE(uint64_t, Lval arg_addr), locUnknown))] in
      let e_addr_plus = BinOp(PlusA, Lval e_addr, Lval block_size, intType) in
      let guard = BinOp(Le, e_addr_plus, Lval limit, boolType) in
      let next = [mkStmtOneInstr (Set(e_addr, e_addr_plus, locUnknown))] in
      let body = [mkStmt (Instr (L.rev !ilt))] in
      stl := L.rev (mkStmt(Instr (L.rev !il))::(mkFor start guard next body));

      (*if(limit-e_addr){
        //this->closure.arguments[  this->closure.total_arguments ].flag = arg_flag;
        //this->closure.arguments[  this->closure.total_arguments ].size = limit-e_addr;
        AddAttribute_Task( this, (void * )(e_addr), arg_flag,this->closure.arguments[  this->closure.total_arguments ].size);
        //this -> closure.total_arguments++;
      }*)
      let sub = (BinOp(MinusA, Lval limit, Lval e_addr, boolType)) in
      ilt := [];
  (*     ilt := [closure_flag]; *)
  (*     ilt := Set(size, sub, locUnknown)::!ilt; *)
      let args = [Lval this; CastE(voidPtrType, Lval e_addr); arg_t2integer arg_type; Lval size] in
      ilt := Call (None, Lval (var addAttribute_Task), args, locUnknown)::!ilt;
  (*     ilt := Set(total_arguments, pplus, locUnknown)::!ilt; *)
      let bl = mkBlock [mkStmt(Instr (L.rev !ilt))] in
      stl := (mkStmt (If(sub, bl, mkBlock [], locUnknown)))::!stl;
    );

    (* this -> closure.total_arguments++; *)
    stl := mkStmtOneInstr(Set(total_arguments, pplus, locUnknown))::!stl;
    
    (* this->closure.arguments[ block_index_start ].flag|=TPC_START_ARG;
      tpc_common.h:20:#define TPC_START_ARG   0x10 *)
    let idxlv = addOffsetLval (Index(Lval bis, NoOffset)) arguments in
    let flag = mkFieldAccess idxlv "flag" in
    stl := mkStmtOneInstr(Set(flag, BinOp( BOr, Lval flag, integer 0x10, intType), locUnknown))::!stl;

    (* skipping assert( (((unsigned)arg_addr64&0xF) == 0) && ((arg_size&0xF) == 0)); *)
  );
  !stl
)

(* Preprocess the header file <header> and merges it with f.  The
 * given header should be in the gcc include path.  Modifies f
 *) (* the original can be found in lockpick.ml *)
let preprocessAndMergeWithHeader_x86 (f: file) (header: string) (def: string)
    : unit = (
  (* //Defining _GNU_SOURCE to fix "undefined reference to `__isoc99_sscanf'" *)
  ignore (Sys.command ("echo | gcc -E -D_GNU_SOURCE "^def^" "^header^" - >/tmp/_cil_rewritten_tmp.h"));
  let add_h = Frontc.parse "/tmp/_cil_rewritten_tmp.h" () in
  let f' = Mergecil.merge [add_h; f] "stdout" in
  f.globals <- f'.globals;
)

(* make a tpc_ version of the function (for use on the ppc side)
 * uses the tpc_call_tpcAD65 from tpc_skeleton_tpc.c as a template
 *)
let make_tpc_func (is_hp: bool) (loc: location) (func_vi: varinfo) (oargs: exp list)
    (args: arg_descr list) (f: file ref) (spu_file: file ref)
    : (fundec * (int * arg_descr) list) = (
  incr un_id;
  print_endline ("Creating tpc_function_" ^ func_vi.vname ^ (string_of_int !un_id));
(*   let args = List.sort sort_args args in *)
  let skeleton = find_function_fundec (!f) "tpc_call_tpcAD65" in
  let f_new = copyFunction skeleton ("tpc_function_" ^ func_vi.vname ^ (string_of_int !un_id)) in
  f_new.sformals <- [];
  (* set the formals to much the original function's arguments *)
  setFunctionTypeMakeFormals f_new func_vi.vtype;
  setFunctionReturnType f_new intType;
(*   formalScalarsToPointers loc f_new; *)
  (* create the arg_size*[, arg_elsz*, arg_els*] formals *)
  let args_num = (List.length f_new.sformals)-1 in
  if ( args_num <> (List.length args)-1 ) then
    E.s (errorLoc loc "Number of arguments described in #pragma doesn't much the \
          number of arguments in the function declaration");
  for i = 0 to args_num do
    let ex_arg = (List.nth oargs i) in
    let name = getNameOfExp ex_arg in
    try
      let (_, (arg_type, _, _, _)) = List.find 
        ( fun (vname, _) -> (vname = name) )
      args in
      ignore(makeFormalVar f_new ("arg_size"^(string_of_int i)) intType);
      if (is_strided arg_type) then (
        ignore(makeFormalVar f_new ("arg_els"^(string_of_int i)) intType);
        ignore(makeFormalVar f_new ("arg_elsz"^(string_of_int i)) intType)
      );
    with Not_found ->
      E.s (errorLoc loc "\"%s\" not found in the #pragma css task\n" name)
  done;

  let this = var (find_local_var f_new "this") in
  (* this->closure.funcid = (uint8_t)funcid; *)
  let this_closure = mkPtrFieldAccess this "closure" in
  let funcid_set = Set (mkFieldAccess this_closure "funcid",
    CastE(find_type !f "uint8_t", integer !func_id), locUnknown) in
  let stmts = ref [mkStmtOneInstr funcid_set] in
  (*(* this->closure.total_arguments = (uint8_t)arguments.size() *)
  instrs := Set (mkFieldAccess this_closure "total_arguments",
  CastE(find_type !f "uint8_t", integer (args_num+1)), locUnknown)::!instrs;*)

  let uint32_t = (find_type !f "uint32_t") in
  let uint64_t = (find_type !f "uint64_t") in
  (* uint32_t block_index_start *)
  let bis = var (makeLocalVar f_new "block_index_start" uint32_t) in
  (* uint64_t e_addr; *)
  let e_addr = var (makeLocalVar f_new "e_addr" uint64_t) in
  
  let args_n =
    (* if we have arguments *)
    if (f_new.sformals <> []) then (
      (* volatile vector unsigned char *tmpvec   where vector is __attribute__((altivec(vector__))) *)
      let args_n = number_args args oargs in
      let args_n = List.sort sort_args_n args_n in
      let i_n = ref (args_num+1) in
      let mapped = L.flatten (List.map
        (fun arg -> decr i_n; doArgument !i_n this e_addr bis f_new arg !f)
        args_n) in
      stmts := mapped@(!stmts);
      args_n
    ) else [] in


  (*if(TPC_IS_HIGHPRIORITYARG(highpriority_arg))
    {
    this->highpriority = 1;
    }
  *)
  if (is_hp) then (
    let this_highpriority = mkPtrFieldAccess this "highpriority" in
    let hp_set = Set(this_highpriority, one, locUnknown) in
    stmts := (mkStmtOneInstr hp_set)::(!stmts);
  );


  (* Foo_32412312231 is located before assert(this->closure.total_arguments<MAX_ARGS); 
    for x86*)
  let map_fun = (fun s -> Scoop_util.replace_fake_call_with_stmt s "Foo_32412312231" (List.rev !stmts)) in
  f_new.sbody.bstmts <- List.map map_fun f_new.sbody.bstmts;

  incr func_id;
  (f_new, args_n)
)