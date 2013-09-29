(****************************************************************************)
(* Copyright (c) 2010-13,                                                   *)
(*                        Foivos    Zakkak          <zakkak@ics.forth.gr>   *)
(*                        Polyvios  Pratikakis      <polyvios@ics.forth.gr> *)
(*                                                                          *)
(*                        FORTH-ICS / CARV                                  *)
(*                        (Foundation for Research & Technology -- Hellas,  *)
(*                         Institute of Computer Science,                   *)
(*                         Computer Architecture & VLSI Systems Laboratory) *)
(*                                                                          *)
(*                                                                          *)
(*                                                                          *)
(* Licensed under the Apache License, Version 2.0 (the "License");          *)
(* you may not use this file except in compliance with the License.         *)
(* You may obtain a copy of the License at                                  *)
(*                                                                          *)
(*     http://www.apache.org/licenses/LICENSE-2.0                           *)
(*                                                                          *)
(* Unless required by applicable law or agreed to in writing, software      *)
(* distributed under the License is distributed on an "AS IS" BASIS,        *)
(* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. *)
(* See the License for the specific language governing permissions and      *)
(* limitations under the License.                                           *)
(****************************************************************************)

(** Responsible for generating the [execute_task]
    Cell Processor
    @author Foivos Zakkak, zakkak\@ics.forth.gr
    @author Polyvios Pratikakis, polyvios\@ics.forth.gr *)

open Cil
open Scoop_util

(** Make the execute_func function that branches on the task id and
    calls the actual task function on the spe
    @param arch the runtime/architecture to target
    @param f the file where the execute_func is going to be placed
    @param tasks the tasks to be handled by the execute_task
           {e tasks} include:
           - the [fundec] of the created tpc_function
           - the [varinfo] of the original function to be executed as task
           - a list of int*Scoop_util.arg_descr pairs describing each argument and holding
              its position/mapping in the argument_list of the actual call

    @returns the execute_task as a [Cil.global]
 *)
let make_exec_func (arch: string) (f: file)
  (tasks: (fundec * varinfo * (int * Scoop_util.arg_descr) list) list) : global = (
  (* make the function *)
  let exec_func = emptyFunction "execute_task" in
  exec_func.svar.vtype <- TFun(intType, Some [], false,[]);
  (* make "queue_entry_t * volatile  ex_task" *)
  let ex_task = makeFormalVar exec_func "ex_task" (TPtr((find_type f "queue_entry_t"), [Attr("volatile", [])])) in
  (* make "tpc_spe_task_state_t task_info" *)
  let task_info = makeFormalVar exec_func "task_info" (TPtr(find_type f "tpc_spe_task_state_t", [])) in
  (* make an int variable for the return value *)
  let lexit = makeLocalVar exec_func "exit" intType in
  (* make a switch statement with one case per task starting from zero *)
  let id = ref 0 in
  let switchcases = List.map
    (fun (_, task, fargs) ->
      let func_id = integer !id in
      incr id;
      let c = Case (func_id, locUnknown) in
      let make_casef = (
        if (arch = "cell") then
          Scoop_cell.make_case
        else
          Scoop_cellgod.make_case
      ) in
      let body = make_casef exec_func task task_info ex_task fargs in
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
  let expr = Lval(mkFieldAccess (var ex_task) "funcid") in
  let switchstmt = mkStmt(Switch(expr, mkBlock switchcases2, cases, locUnknown)) in
  (* get the task_state enuminfo *)
  let task_state_enum = find_enum f "task_state" in
  (* task_info->state = EXECUTED no need for it in every case *)
  let rec find_executed = function [] -> raise Not_found | ("EXECUTED", e, _)::_ -> e | _::tl -> find_executed tl in
  let executed = find_executed task_state_enum.eitems in
  let exec_s = mkStmtOneInstr(Set (mkFieldAccess (var task_info) "state", executed, locUnknown)) in
  (* the function body: exit = 0; switch (taskid); return exit; *)
  exec_func.sbody <- mkBlock [exit0; switchstmt; exec_s; retstmt];
  GFun (exec_func, locUnknown)
)
