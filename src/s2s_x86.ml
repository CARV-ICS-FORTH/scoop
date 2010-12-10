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
module L = List

let doArgument_x86 (i: int) (this: lval) (e_addr: lval) (limit: lval) (fd: fundec)
 (arg: (string * arg_t * exp * exp * exp)) (spu_file: file) (unaligned_args: bool)
 (block_size: int) (ppc_file: file) : stmt list = begin
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
  let uint32_t = (find_type spu_file "uint32_t") in
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
    if (unaligned_args) then begin
      let tmp_addr = var (makeLocalVar fd "tmp_addr" uint32_t) in
      il := Set(tmp_addr, Lval arg_addr, locUnknown)::!il; 
      let div = BinOp(Div, Lval tmp_addr, integer block_size, uint32_t) in
      let mul = BinOp(Mult, CastE(uint32_t, div), integer block_size, voidPtrType) in
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
  end;

  (* skipping assert( (((unsigned)arg_addr64&0xF) == 0) && ((arg_size&0xF) == 0)); *)
  !stl
end