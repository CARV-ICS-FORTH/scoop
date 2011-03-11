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
open S2s_util
module E = Errormsg
module L = List

(* keeps the current funcid for the new tpc_function *)
let func_id = ref 0

let unaligned_args = ref false
let block_size = ref 0

let doArgument (i: int) (this: lval) (e_addr: lval) (limit: lval) (fd: fundec)
 (arg: arg_descr) (spu_file: file) (unaligned_args: bool)
 (block_size: int) (ppc_file: file) : stmt list = begin
  let closure = mkPtrFieldAccess this "closure" in
  let uint32_t = (find_type spu_file "uint32_t") in
  let bis = var (makeLocalVar fd "block_index_start" uint32_t) in
  let arg_size = var (find_formal_var fd ("arg_size"^(string_of_int i))) in
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

  let size = mkFieldAccess idxlv "size" in
  let flag = mkFieldAccess idxlv "flag" in
  let pplus = (BinOp(PlusA, Lval total_arguments, integer 1, intType)) in

  (* invoke isSafeArg from PtDepa to check whether this argument is a no dep *)
  let (arg_name,(_,_,_,_)) = arg in
  if (Ptdepa.isSafeArg fd arg_name) then (
    print_endline "SAFE";
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
    il := Set(size, Lval arg_size, locUnknown)::!il;
    il := Set(flag, integer ( (arg_t2int arg_type) lor 0x10), locUnknown)::!il;
    let eal_in = mkFieldAccess idxlv "eal_in" in
    il := Set(eal_in, CastE(uint32_t, Lval arg_addr), locUnknown)::!il;
    let eal_out = mkFieldAccess idxlv "eal_out" in
    il := Set(eal_out, CastE(uint32_t, Lval arg_addr), locUnknown)::!il;
    stl := (*mkStmt(Continue locUnknown)::*)[mkStmt(Instr (L.rev !il))];
  ) else (
    (* uint32_t block_index_start=this->closure.total_arguments; *)
    il := Set(bis, Lval total_arguments, locUnknown)::!il;

    (* limit=(((uint32_t)arg_addr64)+arg_size); *)
    let plus = (BinOp(PlusA, CastE(uint32_t, Lval arg_addr), Lval arg_size, uint32_t)) in
    il := Set(limit, plus, locUnknown)::!il;

    (* e_addr=(uint32_t)arg_addr64; *)
    il := Set(e_addr, CastE(uint32_t, Lval arg_addr), locUnknown)::!il;

    (*#ifdef UNALIGNED_ARGUMENTS_ALLOWED
        uint32_t tmp_addr=(uint32_t)arg_addr64;
        arg_addr64 = (void* )(((uint32_t)(tmp_addr/BLOCK_SZ))*BLOCK_SZ);
        this->closure.arguments[ this->closure.total_arguments].stride = tmp_addr-(uint32_t)arg_addr64;
        arg_size +=this->closure.arguments[ this->closure.total_arguments ].stride;
        //      limit +=this->closure.arguments[ this->closure.total_arguments ].stride;
      #endif*)
    if (unaligned_args) then (
      let tmp_addr = var (makeLocalVar fd "tmp_addr" uint32_t) in
      il := Set(tmp_addr, Lval arg_addr, locUnknown)::!il; 
      let div = BinOp(Div, Lval tmp_addr, integer block_size, uint32_t) in
      let mul = BinOp(Mult, CastE(uint32_t, div), integer block_size, voidPtrType) in
      il := Set(arg_addr, CastE(voidPtrType, mul), locUnknown)::!il;
      let new_stride = BinOp(MinusA, Lval tmp_addr, CastE(uint32_t, Lval arg_addr), intType) in
      il := Set(stride, new_stride, locUnknown)::!il;
      let add = (BinOp(PlusA, Lval arg_size, Lval stride, uint32_t)) in
      il := Set(arg_size, add, locUnknown)::!il;
    );

    (*for(e_addr=(uint32_t)arg_addr64;e_addr + BLOCK_SZ <= limit ;e_addr+=BLOCK_SZ){
      this->closure.arguments[  this->closure.total_arguments ].flag = arg_flag;
      this->closure.arguments[  this->closure.total_arguments ].size = BLOCK_SZ;
      AddAttribute_Task( this, (void* )(e_addr), arg_flag,BLOCK_SZ);
      this -> closure.total_arguments++;
      this->closure.arguments[ this->closure.total_arguments ].stride=0;
    }*)
    let closure_flag = Set(flag, arg_t2integer arg_type, locUnknown) in
    let ilt = ref [closure_flag] in
    ilt := Set(size, integer block_size, locUnknown)::!ilt;
    let addAttribute_Task = find_function_sign ppc_file "AddAttribute_Task" in
    let args = [Lval this; Lval e_addr; arg_t2integer arg_type; integer block_size ] in
    ilt := Call (None, Lval (var addAttribute_Task), args, locUnknown)::!ilt;
    ilt := Set(total_arguments, pplus, locUnknown)::!ilt;
    let start = [mkStmtOneInstr (Set(e_addr, Lval arg_addr, locUnknown))] in
    let e_addr_plus = BinOp(PlusA, Lval e_addr, integer block_size, intType) in
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
  );

  (* skipping assert( (((unsigned)arg_addr64&0xF) == 0) && ((arg_size&0xF) == 0)); *)
  !stl
end

(* make a tpc_ version of the function (for use on the ppc side)
 * uses the tpc_call_tpcAD65 from tpc_skeleton_tpc.c as a template
 *)
let make_tpc_func (func_vi: varinfo) (args: (string * (arg_t * exp * exp * exp )) list)
    (f:file ref) (spu_file:file ref) : fundec = begin
  print_endline ("Creating tpc_function_" ^ func_vi.vname);
  let skeleton = find_function_fundec (!f) "tpc_call_tpcAD65" in
  let f_new = copyFunction skeleton ("tpc_function_" ^ func_vi.vname) in
  f_new.sformals <- [];
  (* set the formals to much the original function's arguments *)
  setFunctionTypeMakeFormals f_new func_vi.vtype;
  setFunctionReturnType f_new intType;
  (* create the arg_size*[, arg_elsz*, arg_els*] formals *)
  let args_num = (List.length f_new.sformals)-1 in
  if ( args_num > (List.length args) ) then (
    ignore(E.error "Number of arguments described in #pragma doesn't much the\
          number of arguments in the function declaration");
    assert false
  );
  for i = 0 to args_num do
    let (_, (arg_type, _, _, _)) = List.nth args i in
    ignore(makeFormalVar f_new ("arg_size"^(string_of_int i)) intType);
    if (is_strided arg_type) then (
      ignore(makeFormalVar f_new ("arg_els"^(string_of_int i)) intType);
      ignore(makeFormalVar f_new ("arg_elsz"^(string_of_int i)) intType)
    );
  done;

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
    stmts := (doArgument i this e_addr (var limit) f_new arg !spu_file
            !unaligned_args !block_size !f)@[mkStmtOneInstr funcid_set];
  done;

  (* Foo_32412312231 is located before assert(this->closure.total_arguments<MAX_ARGS); 
    for x86*)
  f_new.sbody.bstmts <- List.map (fun s -> S2s_util.replace_fake_call_with_stmt s "Foo_32412312231" (L.rev !stmts)) f_new.sbody.bstmts;

  incr func_id;
  f_new
end
