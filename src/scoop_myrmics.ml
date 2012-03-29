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

(** Responsible for generating code for the myrmics runtime on the
    formic architecture
    @author Foivos Zakkak, zakkak\@ics.forth.gr *)

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

(** generates a Set that does table[i]=arg
 * @param table the array we want to change
 * @param i the array's index
 * @param arg the argument we want to place in table[i]
 * @return the Set instr generated
 *)
let mkSet table i arg =
  let curr = addOffsetLval (Index(integer i, NoOffset)) table in
  Set( curr, arg, locUnknown)

(** passes the arguments to the arrays in the correct order
 * @param targs the arguments' table
 * @param ttyps the types' table
 * @param orig_tname the original function's name
 * @param tid an id marking the query (needed by SDAM)
 * @param arg the argument we want to place
 * @return the generated Set instrs
 *)
let doArgument targs ttyps (orig_tname: string) (tid: int) 
 (arg: (int * arg_descr) ) : instr list = (
  let (i_m, arg_desc) = arg in
  let arg_addr = arg_desc.address in
  let arg_type = arg_desc.atype in
  let arg_name = arg_desc.aname in

  let arg_type_tmp = arg_type2int arg_type in
  let arg_type_tmp =
    (* TPC_BYVALUE_ARG; *)
    if (isScalar arg_desc) then (
      0x9
    (* invoke isSafeArg from PtDepa to check whether this argument is a no dep *)
    (* arg_flag|TPC_SAFE_ARG; *)
    ) else if (Sdam.isSafeArg orig_tname tid arg_name) then (
      arg_type_tmp lor 0x8
    ) else (
      arg_type_tmp
    )
  in
  let arg_type_tmp =
    (* arg_flag|TPC_REGION_ARG; *)
    if (isRegion arg_desc) then
      arg_type_tmp lor 0x16
    else
      arg_type_tmp
  in

  [mkSet targs i_m arg_addr; mkSet ttyps i_m (integer arg_type_tmp)]
)

(** Creates two arrays, one containing the arguments' addresses and another
 * containing the arguments' types the invokes _sys_spawn with those as args
 * @param is_hp whether this call is high priority
 * @param loc the current file location
 * @param func_vi the varinfo of the original function
 * @param oargs the original arguments
 * @param args the arguments from the pragma directive
 * @param f the file we are modifying
 * @param cur_fd the current function's fundec
 * @return the stmts that will replace the call paired with a list of numbered
 * argument descriptors
 *)
let make_tpc_issue (is_hp: bool) (loc: location) (func_vi: varinfo) (oargs: exp list)
    (args: arg_descr list) (f: file) (cur_fd: fundec) : (stmt list * (int * arg_descr) list) = (
  incr un_id;

  let args_num = List.length oargs in
  let args_num_i = integer args_num in

  let scoop2179_args =
    try __find_local_var cur_fd "scoop2179_s_args"
    with Not_found ->
      makeLocalVar cur_fd "scoop2179_s_args" (TArray(voidPtrType, Some(args_num_i), []))
  in
  (* Check if the array is large enough for this spawn *)
  ( match scoop2179_args.vtype with
    TArray(_, Some(Const(CInt64(size, _, _))), _) ->
      if args_num > (i64_to_int size) then
        scoop2179_args.vtype <- TArray(voidPtrType, Some(args_num_i), []);
    | _ -> assert false;
  );
  let scoop2179_typs =
    try __find_local_var cur_fd "scoop2179_t_args"
    with Not_found ->
      makeLocalVar cur_fd "scoop2179_t_args" (TArray(voidPtrType, Some(args_num_i), []))
  in
  (* Check if the array is large enough for this spawn *)
  ( match scoop2179_typs.vtype with
    TArray(_, Some(Const(CInt64(size, _, _))), _) ->
      if args_num > (i64_to_int size) then
        scoop2179_typs.vtype <- TArray(voidPtrType, Some(args_num_i), []);
    | _ -> assert false;
  );
  let scoop2179_args = var scoop2179_args in
  let scoop2179_typs = var scoop2179_typs in

  let args_n, instrs =
    (* if we have arguments *)
    if (oargs <> []) then (
      let args_n = number_args args oargs in

(*       ignore(L.map (fun (i, (name, _)) ->  print_endline ("A= "^(string_of_int i)^" "^name) ) args_n); *)

      let args_n = List.sort sort_args_n args_n in
(*       ignore(L.map (fun (i, (name, _)) ->  print_endline ("B= "^(string_of_int i)^" "^name) ) args_n); *)
      incr querie_no;
      let doArgument = doArgument scoop2179_args scoop2179_typs func_vi.vname !querie_no in
      let mapped = L.flatten (List.map doArgument args_n) in
      (args_n, mapped)
    ) else ([], [])
  in

  (* tpc_call(taskd); *)
  let tpc_call_f = find_function_sign f "_sys_spawn" in

  let call_args = [integer !func_id; Lval scoop2179_args; Lval scoop2179_typs; args_num_i] in
  let instrs = Call (None, Lval (var tpc_call_f), call_args, locUnknown)::instrs in

  incr func_id;
  ([mkStmt (Instr(L.rev instrs))], args_n)
)

let make_wait_on (cur_fd: fundec) (f : file) (loc : location) (exps: attrparam list) (s: stmt): stmt visitAction =
  let two = find_function_sign f "_sys_wait_on" in
  let num_args = L.length exps in
  let scoop2179_args =
    try __find_local_var cur_fd "scoop2179_wo_args"
    with Not_found ->
      makeLocalVar cur_fd "scoop2179_wo_args" (TArray(voidPtrType, Some(integer num_args), []))
  in
  (* Check if the array is large enough for this wait on *)
  ( match scoop2179_args.vtype with
    TArray(_, Some(Const(CInt64(size, _, _))), _) ->
      if (L.length exps) > (i64_to_int size) then
        scoop2179_args.vtype <- TArray(voidPtrType, Some(integer num_args), []);
    | _ -> assert false;
  );
  let args = L.map (attrParamToExp f loc) exps in
  let i = ref (-1) in
  let mkSet = mkSet (var scoop2179_args) in
  let init_args = L.map (fun a -> incr i; mkSet !i a) args in
  let instr = Call (None, Lval (var two), [Lval (var scoop2179_args); integer num_args], locUnknown) in
  let s' = {s with pragmas = List.tl s.pragmas} in
  ChangeDoChildrenPost ((mkStmt (Block (mkBlock [ mkStmt (Instr(init_args)); mkStmtOneInstr instr; s' ]))), fun x -> x)
