(*
 *
 * Copyright (c) 2011,
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

(* XXX: this is for sdam *)
let querie_no = ref 0

(* a unique id for the tpc_function_* *)
let un_id = ref 0

let makeGlobalVar n t f=
  let v = makeGlobalVar n t in
  let glob = GVar(v, {init = None;}, locUnknown) in
  add_at_top f [glob];
  v

let doArgument (taskd_args: lval) (f : file) (orig_tname: string) (tid: int)
 (arg: (int * arg_descr) ) : stmt list = (
  let (i_m, (arg_name, (arg_addr, arg_type, arg_size, arg_elsz, arg_els))) = arg in

  let il = ref [] in

  (* taskd->args[i] *)
  let tpc_task_argument_pt = TPtr(find_type f  "tpc_task_argument", []) in
  il := Set(taskd_args, BinOp( PlusPI, Lval taskd_args, integer 32, tpc_task_argument_pt) , locUnknown)::!il;
(*   let idxlv = addOffsetLval (Index(integer i, NoOffset)) arguments in *)
  let idxlv = taskd_args in
  (*  void * addr_in;
      void * addr_out;
      uint32_t type;
      uint32_t size;
      uint32_t stride;
      uint32_t element_num; *)
  let addr_in = mkFieldAccess idxlv "addr_in" in
  let addr_out = mkFieldAccess idxlv "addr_out" in
  let flag = mkFieldAccess idxlv "type" in
  let size = mkFieldAccess idxlv "size" in
  let stride = mkFieldAccess idxlv "stride" in
  let element_num = mkFieldAccess idxlv "element_num" in

  il := Set(addr_in, CastE(voidPtrType, arg_addr), locUnknown)::!il;
  il := Set(addr_out, CastE(voidPtrType, arg_addr), locUnknown)::!il;

  (* invoke isSafeArg from PtDepa to check whether this argument is a no dep *)
  if (Sdam.isSafeArg orig_tname tid arg_name) then (
    (* arg_flag|TPC_SAFE_ARG; *)
    il := Set(flag, integer ( (arg_t2int arg_type) lor 0x8), locUnknown)::!il;
  ) else (
    (* arg_flag; *)
    il := Set(flag, integer (arg_t2int arg_type), locUnknown)::!il;
  );

  if (is_strided arg_type) then (
    il := Set(size, arg_elsz, locUnknown)::!il;
    il := Set(stride, arg_size, locUnknown)::!il;
    il := Set(element_num, arg_els, locUnknown)::!il;
  ) else (
    il := Set(size, arg_size, locUnknown)::!il;
    il := Set(stride, zero, locUnknown)::!il;
    il := Set(element_num, zero, locUnknown)::!il;
  );

  [mkStmt (Instr (L.rev !il))]
)

(* TODO hold the number of arguments somewhere for
    const int tpc_task_arguments_list[] = {2, 3, 5, 9};
*)
(* generates the code to issue a task *)
let make_tpc_issue (is_hp: bool) (loc: location) (func_vi: varinfo) (oargs: exp list)
    (args: arg_descr list) (f: file) (cur_fd: fundec) : (stmt list * (int * arg_descr) list) = (
  incr un_id;

  let instrs = ref [] in
  let args_num = List.length oargs in
  let args_num_i = integer args_num in
  let tpc_task_descriptor_pt = TPtr(find_type f "tpc_task_descriptor", []) in
  let tpc_task_argument_pt = TPtr(find_type f "tpc_task_argument", []) in
  let taskd = var (makeTempVar cur_fd tpc_task_descriptor_pt) in
  (* task_desc = tpc_task_descriptor_alloc(args_num); *)
  let tpc_task_descriptor_alloc = find_function_sign f "tpc_task_descriptor_alloc" in
  instrs := Call (Some taskd, Lval (var tpc_task_descriptor_alloc), [args_num_i], locUnknown)::!instrs;
  (* TODO make the wrapper *)
  (* task_desc->task = wrapper_func; *)
  let taskd_task = mkFieldAccess taskd "task" in
(*   instrs := Set(taskd_task, wrapper, voidPtrType)::!instrs; *)
  (* task_desc->args = task_desc; *)
  let taskd_args = mkFieldAccess taskd "args" in
(*   instrs := Set(taskd_args, BinOp( PlusPI, Lval taskd, integer 32, tpc_task_argument_pt) , locUnknown)::!instrs; *)
  instrs := Set(taskd_args, Lval taskd, locUnknown)::!instrs;
  (* task_desc->args_no = args_num; *)
  let taskd_args_no = mkFieldAccess taskd "args_no" in
  instrs := Set(taskd_args_no, args_num_i, locUnknown)::!instrs;
  (* Leave uninitialized
     task_desc->rfu and task_desc->extras *)
  let stmts = ref [] in

  let args_n =
    (* if we have arguments *)
    if (oargs <> []) then (
      let args_n = number_args args oargs in
      let args_n = List.sort sort_args_n args_n in
      incr querie_no;
      let doArgument = doArgument taskd_args f func_vi.vname !querie_no in
      let mapped = L.flatten (List.map doArgument args_n) in
      stmts := mkStmt (Instr (L.rev !instrs))::mapped;
      instrs := [];
      args_n
    ) else []
  in

  (* task_desc->args = task_desc+32; *)
  instrs := Set(taskd_args, BinOp( PlusPI, Lval taskd, integer 32, tpc_task_argument_pt) , locUnknown)::!instrs;
  (* tpc_call(taskd); *)
  let tpc_call_f = find_function_sign f "tpc_call" in
  instrs := Call (None, Lval (var tpc_call_f), [Lval taskd], locUnknown)::!instrs;

  stmts := !stmts@[mkStmt (Instr(L.rev !instrs))];

  incr func_id;
  (!stmts, args_n)
)
