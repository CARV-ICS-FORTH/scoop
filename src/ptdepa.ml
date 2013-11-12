(****************************************************************************)
(* Copyright (c) 2010-13,                                                   *)
(*                        Dimitris  Chassapis       <hassapis@ics.forth.gr> *)
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

open Cil
open Sdam
open Ptatypes

module E = Errormsg
module LT = Locktype
module LF = Labelflow
module CF = Controlflow
module BS = Barrierstate
module PT = Ptatype
module LP = Loopa

let do_graph_out = ref false

let do_task_graph_out = ref false

let debug = ref false

let debug_footprints = ref false

let options = [
  "--save-graph",
  Arg.Set(do_graph_out),
  " SDAM-Pointer analysis: Write constraints in \"graph.dot\".";

  "--save-dependencies-graph",
  Arg.Set(do_task_graph_out),
  " SDAM-Pointer analysis: Write task dependecies in \"task-dep.dot\".";

  "--debug-ptdepa",
  Arg.Set(debug),
  " SDAM-Pointer analysis: debugging output.";
]

let taskScope1 = ref dummyFunDec
let taskScope2 = ref dummyFunDec

(** return rhoSet for arg. If arg is a scalar it will return an empty set
    @param argname the argument whose aliasing set we seek
    @return the rhoSet with variables that alias with arg
    @exception if arg type is region
*)
exception RegionType;;
let get_rhoSet (argname: string) (scope: fundec) : LF.rhoSet =
  let env = List.assoc scope !PT.global_fun_envs in
  let (argtype, argaddress) = PT.env_lookup argname env in
  ignore(E.log "lookup of %s gives location %a with type %a\n" argname LF.d_rho argaddress d_tau argtype);
  match argtype.Ptatypes.t with
  | Ptatypes.ITPtr(_, r) -> LF.close_rhoset_pn (LF.RhoSet.singleton r)
  | Ptatypes.ITRegion th -> raise RegionType
  | _ ->  if !debug then ignore(E.log "Warning: %s is not a pointer\n" argname);
    LF.RhoSet.empty (* if arg is not a pointer, return an empty set
                     * so that is_aliased returns false *)
(*
  let get_thetaSet (argname: string) (scope: fundec) : LF.thetaSet =
  let env = List.assoc scope !PT.global_fun_envs in
  let (argtype, argaddress) = PT.env_lookup argname env in
  ignore(E.log "lookup of %s gives location %a with type %a\n" argname LF.d_rho argaddress d_tau argtype);
  match argtype.Ptatypes.t with
  | Ptatypes.ITRegion th -> LF.close_thetaset_pn (LF.ThetaSet.singleton th)
  | _ ->  if !debug then ignore(E.log "Warning: %s is not a region\n" argname);
  LF.RhoSet.empty (* if arg is not a pointer, return an empty set
  * so that is_aliased returns false *)
*)

let get_thetaSet (argname: string) (scope: fundec) : LF.thetaSet =
  let env = List.assoc scope !PT.global_fun_envs in
  let (argtype, _) = PT.env_lookup argname env in
  ignore(E.log "lookup of %s with type %a\n" argname d_tau argtype);
  match argtype.Ptatypes.t with
    Ptatypes.ITRegion th -> LF.close_thetaset_pn (LF.ThetaSet.singleton th)
  | _ ->  if !debug then ignore(E.log "Warning: %s is not a region\n" argname); LF.ThetaSet.empty

(** return true if argument arg is scalar
    @param arg the argument we want to check
    @return true if arg is scalar
*)
let is_scalar2 (arg: arg_descr) (scope: fundec) : bool =
  let env = List.assoc scope !PT.global_fun_envs in
  let (argtype, argaddress) = PT.env_lookup arg.argname env in
  match argtype.Ptatypes.t with
  | Ptatypes.ITPtr(_, r) -> ignore (E.log "argument %s is of pointer type\n" arg.argname); false
  | Ptatypes.ITAbs _ -> ignore (E.log "argument %s is of abstract type\n" arg.argname); false
  | _ ->  ignore (E.log "argument %s is of unknown type\n" arg.argname); true

let is_scalar (arg: arg_descr) (scope: fundec) : bool =
  let env = List.assoc scope !PT.global_fun_envs in
  let (argtype, argaddress) = PT.env_lookup arg.argname env in
  match argtype.Ptatypes.t with
  | Ptatypes.ITPtr(_, _)
  |    Ptatypes.ITRegion _ -> false
  | _ ->  true

(** checks if arg1 aliases to arg2
    @param arg1 the arg description of the first task argument
    @param arg2 the arg description of the second task argument
    @return true, if rhoSets intersection is not empty
*)
let alias (arg1: arg_descr) (arg2: arg_descr) : bool =
  (* if both arguments are only inputs, return false (they could
     be aliased, but we treat them as if the were not) *)
  if((is_in_arg arg1.iotype) && (is_in_arg arg2.iotype)) then (
                                                  (* ignore(E.log "comparing %s(%d) - %s(%d): both in ingore deps\n" arg1.argname arg1.aid arg2.argname arg2.aid); *)
                                                                                              false
  )
  else (
    let env1 = List.assoc !taskScope1 !PT.global_fun_envs in
    let (argtype1, _) = PT.env_lookup arg1.argname env1 in
    let env2 = List.assoc !taskScope2 !PT.global_fun_envs in
    let (argtype2, _) = PT.env_lookup arg2.argname env2 in
    match argtype1.Ptatypes.t, argtype2.Ptatypes.t with
      Ptatypes.ITPtr(_, r1), Ptatypes.ITPtr(_, r2) -> (
        let set1 = LF.close_rhoset_pn (LF.RhoSet.singleton r1) in
        let set2 = LF.close_rhoset_pn (LF.RhoSet.singleton r2) in
        let final_set = LF.RhoSet.inter set1 set2 in
        if !debug then (
          ignore(E.log "comparing %s(%d) - %s(%d)\n" arg1.argname arg1.aid arg2.argname arg2.aid);
          ignore(E.log "%s set           : %a\n" arg1.argname LF.d_rhoset set1);
          ignore(E.log "%s set           : %a\n" arg2.argname LF.d_rhoset set2);
          ignore(E.log "rhoset intersection: %a\n" LF.d_rhoset final_set);
        );
        not (LF.RhoSet.is_empty final_set)
      )
    | Ptatypes.ITRegion(th1), Ptatypes.ITRegion(th2) -> (
      let set1 = LF.close_thetaset_pn (LF.ThetaSet.singleton th1) in
      let set2 = LF.close_thetaset_pn (LF.ThetaSet.singleton th2) in
      let final_set = LF.ThetaSet.inter set1 set2 in
      if !debug then (
        ignore(E.log "comparing %s(%d) - %s(%d)\n" arg1.argname arg1.aid arg2.argname arg2.aid);
        ignore(E.log "%s set           : %a\n" arg1.argname LF.d_thetaset set1);
        ignore(E.log "%s set           : %a\n" arg2.argname LF.d_thetaset set2);
        ignore(E.log "rhoset intersection: %a\n" LF.d_thetaset final_set);
      );
      not (LF.ThetaSet.is_empty final_set)
    )
    | Ptatypes.ITPtr(_, r), Ptatypes.ITRegion(th) -> (
      let set1 = LF.close_rhoset_pn (LF.RhoSet.singleton r) in
      let set2 = LF.close_rhoset_from_theta_pn (LF.ThetaSet.singleton th) in
      let final_set = LF.RhoSet.inter set1 set2 in
      if !debug then (
        ignore(E.log "comparing %s(%d) - %s(%d)\n" arg1.argname arg1.aid arg2.argname arg2.aid);
        ignore(E.log "%s set           : %a\n" arg1.argname LF.d_rhoset set1);
        ignore(E.log "%s set           : %a\n" arg2.argname LF.d_rhoset set2);
        ignore(E.log "rhoset intersection: %a\n" LF.d_rhoset final_set);
      );
      not (LF.RhoSet.is_empty final_set)
    )
    | Ptatypes.ITRegion(th), Ptatypes.ITPtr(_, r) -> (
      let set1 = LF.close_rhoset_from_theta_pn (LF.ThetaSet.singleton th) in
      let set2 = LF.close_rhoset_pn (LF.RhoSet.singleton r) in
      let final_set = LF.RhoSet.inter set1 set2 in
      if !debug then (
        ignore(E.log "comparing %s(%d) - %s(%d)\n" arg1.argname arg1.aid arg2.argname arg2.aid);
        ignore(E.log "%s set           : %a\n" arg1.argname LF.d_rhoset set1);
        ignore(E.log "%s set           : %a\n" arg2.argname LF.d_rhoset set2);
        ignore(E.log "rhoset intersection: %a\n" LF.d_rhoset final_set);
      );
      not (LF.RhoSet.is_empty final_set)
    )
    | _ -> if !debug then ignore(E.log "Warning: Not a region or pointer\n"); true (*conservative but safe approach*)
  (*
    try (
    let set1 = get_rhoSet arg1.argname !taskScope1 in
    let set2 = get_rhoSet arg2.argname !taskScope2 in
      (* get concrete rhoSet, meaning get only array and mallocs *)
      (* FIXME: concrete set always empty???? *)
      (* let final_set = LF.concrete_rhoset (LF.RhoSet.inter set1 set2) in *)
    let final_set = LF.RhoSet.inter set1 set2 in
    if !debug then (
    ignore(E.log "comparing %s(%d) - %s(%d)\n" arg1.argname arg1.aid arg2.argname arg2.aid);
    ignore(E.log "%s set           : %a\n" arg1.argname LF.d_rhoset set1);
    ignore(E.log "%s set           : %a\n" arg2.argname LF.d_rhoset set2);
    ignore(E.log "rhoset intersection: %a\n" LF.d_rhoset final_set);
    );
      (*
    if((compare arg1.argname "e") == 0) then (
    ignore(E.log "arg:%s - arg:%s\n" arg1.argname arg2.argname);
    LF.RhoSet.iter (fun fromRho ->
    LF.RhoSet.iter (fun toRho ->
    ignore(E.log "%a\n" LF.d_rhopath (fromRho, toRho));
    ) set2
    ) set1;
    );
  *)
    not (LF.RhoSet.is_empty final_set)
    ) with RegionType -> (
    ignore(E.log "Argument %s is a region\n" arg1.argname);
    let set1 = get_thetaSet arg1.argname !taskScope1 in
    ignore(E.log "%s set           : %a\n" arg1.argname LF.d_thetaset set1);
    true
    )
  *)
  )


(** find dependencies between arguments
    @param task1 the task descriptor whose taks argument we check for dependencies
    @param task2 the task descriptor of the task whose argument we want to check with arg
    @param arg the current argumetn under question
    @return unit
*)
exception Done
let solve_arg_dependencies ((task1: task_descr), (tasks: task_descr list)) (arg: arg_descr)  : unit =
  try (
    if(is_scalar arg task1.scope) then (
      if !debug then ignore(E.log "Argument:%s(%d) is scalar and safe\n" arg.argname arg.aid);
      arg.safe <- true;
      raise Done
    );
    List.iter (fun task2 ->
      List.iter (fun arg' ->
        taskScope1 := task1.scope;
        taskScope2 := task2.scope;
        (* do not check with self  if task is not in a loop *)
        if (not (BS.is_inLoop task1) && task1.taskid == task2.taskid) then (
          if !debug then ignore(E.log "not in loop\n");
          arg.safe <- true;
        )
        else (
          (if((BS.is_inLoop task1) && arg.aid == arg'.aid) then (
            let res = not (alias arg arg') || LP.array_bounds_safe arg in
            if(arg.force_safe && not res) then (
              ignore(E.log "Warning:Argument \"%s\" has manually been marked as safe but the analysis found dependencies!\n" arg.argname);
              raise Done
            );
            arg.safe <- res;
           )
           else (
             let res = not (alias arg arg') in
             if(arg.force_safe && not res) then (
               ignore(E.log "Warning:Argument \"%s\" has manually been marked as safe but the analysis found dependencies!\n" arg.argname);
               raise Done
             );
             arg.safe <- res;
           ));
          if (not arg.safe) then (
            arg.dependencies := arg'::!(arg.dependencies);
            raise Done
          )
        )
      ) task2.arguments
    ) tasks
  ) with Done -> ()


(** returns a list of task descriptor for the corresponding task id's found
    in the taskset
    @param taskset the set of tasks from BS module
    @param tasks_l a list of tasks descriptors to match with the taskid's
    from the taskset
    @return the list of tasks whose taskid's are present in the taskset
*)
exception Found_tasks of (task_descr list)
let getTasks (taskset: BS.taskSet) (tasks_l: task_descr list) : task_descr list =
  if !debug then ignore(E.log "BS.taskset=%a\n" BS.d_taskset taskset);
  BS.TaskSet.fold (fun (_, taskid) tasks ->
    List.append (List.find_all (fun t -> taskid == t.taskid) tasks_l) tasks
  ) taskset []

(**    find dependencies between task arguments
       @param tasks the list of tasks in the prorgram
       @return unit
*)
let solve_task_dependencies (tasks_l: task_descr list) : unit =
  (* traverse the list of tasks, find the taskSet that can happen in parallel with
     current task, and check between their arguments for dependencies. *)
  let solve_task_deps task = (
    if !debug then ignore(E.log "checking Task:%a\n" d_task task);
    let tasks = getTasks (BS.getTaskSet task) tasks_l in
    (* 0. if sdam is disabled then mark only scalars as safe, do not run the analysis *)
    if !debug then
      List.iter (fun task -> ignore(E.log "TaskSet:%a\n" d_task task); ) tasks;
    (* 1. check if tasks exists in the set, then maintain self loops, else remove them *)
    if (not (BS.is_inLoop task)) then (
      if !debug then ignore(E.log "Not self dependent\n");
      List.iter (fun a -> a.safe <- true;) task.arguments
    );
    (* 2. check for dependencies with other tasks *)
    List.iter (solve_arg_dependencies (task, tasks)) task.arguments
  ) in List.iter solve_task_deps tasks_l


let type_arguments (tasks_l: task_descr list) : unit =
  if !debug_footprints then ignore(E.log "Typing formal arguments\n");
  List.iter (fun task ->
    let (read_vars, write_vars) = Labelflow.solve_chi_m task.t_inf.fd_chi in
    if !debug_footprints then (
      ignore(E.log "=== %s ===\n" task.taskname);
      ignore(E.log "readRho\n%a\n" LF.d_rhoset read_vars);
      ignore(E.log "writeRho\n%a\n" LF.d_rhoset write_vars);
      ignore(E.log "%a\n" d_env task.t_gamma);
      List.iter (fun act ->
        ignore(E.log "Task:%s:actual:%s\n" task.taskname act);
      ) task.actuals;
      List.iter (fun arg ->
        let (argname, arg_t) = arg in
        (match arg_t.t with
          ITPtr(_, r) -> (
            if((LF.RhoSet.mem r read_vars) && (LF.RhoSet.mem r write_vars)) then (
              ignore(E.log "Task:%s:formal:%s - inout\n" task.taskname argname);
            )
            else if((LF.RhoSet.mem r write_vars)) then (
              ignore(E.log "Task:%s:formal:%s - out\n" task.taskname argname);
            )
            else if((LF.RhoSet.mem r read_vars)) then (
              ignore(E.log "Task:%s:formal:%s - in\n" task.taskname argname);
            )
            else (
              ignore(E.log "Task:%s:formal:%s - inconclusive\n" task.taskname argname);
            )
          )
        | _ -> ignore(E.log "Task:%s:formal:%s - in(scalar)\n" task.taskname argname);
        )
      ) (task.t_inf).fd_arg_tau_list
    );
  ) tasks_l

(** print dependencies in graphiz format
    @param task_l is the list of tasks in the program
    @param outf is the output channel/file
    @return unit
*)
let plot_task_dep_graph (task_l: task_descr list) (outf: out_channel) : unit = begin
  (* print nodes *)
  List.iter (fun task ->
    let tasknode = task.taskname^"_"^(string_of_int task.taskid) in
    Printf.fprintf outf "\tsubgraph cluster_%s{\n" tasknode;
    Printf.fprintf outf "\t\tlabel=\"%s\";\n" task.taskname;
    Printf.fprintf outf "\t\tcolor=blue;\n";
    List.iter (fun arg ->
      Printf.fprintf outf "\t\t%s [label=\"%s\"]\n" (arg.argname^"_"^(string_of_int arg.aid)) arg.argname ;
    ) task.arguments;
    Printf.fprintf outf "\t}\n";
  ) task_l;
  (* print edges *)
  List.iter (fun task ->
    List.iter (fun arg ->
      List.iter (fun arg' ->
        Printf.fprintf outf "\t%s -> %s\n" (arg.argname^"_"^(string_of_int arg.aid)) (arg'.argname^"_"^(string_of_int arg'.aid));
      ) !(arg.dependencies);
    ) task.arguments;
  ) task_l;
end

(** Entrance function to call the static analysis for task dependencies
    @param the file we apply the analysis
    @return unit
*)
let find_dependencies (f: file) (disable_sdam: bool) : unit = begin
  program_file := f;

  (*  Rmtmps.removeUnusedTemps f;*)
  (*  Rmalias.removeAliasAttr f; *)
  Cfg.computeFileCFG f;
  if !debug then ignore(E.log "SDAM:Finding data dependencies...\n");

  PT.generate_constraints f;
  LF.done_adding ();
  if(disable_sdam) then (
    ignore(E.log "SDAM is disabled\n");
    List.iter(fun task ->
      List.iter (fun a -> if ( (is_scalar a task.scope) || a.force_safe) then (
        if(is_scalar a task.scope) then incr total_scalar_args;
        a.safe <- true;
      ) else (
        a.safe <- false;
      )
      ) task.arguments
    ) !tasks_l;
    count_tasks_and_args (List.rev !tasks_l);
    ignore(E.log "SCOOP: Total tasks=%d, total arguments=%d, total scalar arguments=%d, total safe arguments(incl. scalars)=%d\n" !total_tasks !total_args !total_scalar_args !total_safe_args);
  )
  else (
    ignore(E.log "SDAM is enabled\n");
    (* count scalar args *)
    List.iter(fun task ->
      List.iter (fun a ->
        if (is_scalar a task.scope) then (incr total_scalar_args);
      ) task.arguments;
    ) !tasks_l;
    (* Run the analysis *)
    BS.solve();
    solve_task_dependencies (List.rev !tasks_l);
    type_arguments (List.rev !tasks_l);
    (* BS.solve(); *)
    if !do_graph_out then begin
      Dotpretty.init_file "graph-begin.dot" "initial constraints";
      Labelflow.print_graph !Dotpretty.outf;
      Dotpretty.close_file ();

      Dotpretty.init_file "cf-graph.dot" "control flow graph";
      Lockstate.print_graph !Dotpretty.outf;
      Dotpretty.close_file ();
    end;
    (*   ignore(E.log "SDAM: Checking for argument dependencies.\n");
         List.iter find_task_dependencies !tasks_l;
         if !do_verbose_output then begin (* this is bogus... *)
         ignore(E.log "SDAM: Dependencies resolved.\n");
         List.iter print_task_dependencies !task_dep_l;
         end;
         if !do_task_graph_out then begin
         ignore(E.log "Creating dot file\n");
         Dotpretty.init_file "task-dep.dot" "task dependencies";
         plot_task_dep_graph !Dotpretty.outf;
         Dotpretty.close_file ();

         Dotpretty.init_file "cf-graph.dot" "control flow graph";
         Lockstate.print_graph !Dotpretty.outf;
         Dotpretty.close_file ();
         end; *)
    if !debug then ignore(E.log "SDAM: static dependence analysis has now completed.\n");
    (*   count_safe_args !task_dep_l; *)
    if !do_task_graph_out then (
      Dotpretty.init_file "task-dep.dot" "task dependencies";
      plot_task_dep_graph !tasks_l !Dotpretty.outf;
      Dotpretty.close_file ();
    );
    count_tasks_and_args (List.rev !tasks_l);
    ignore(E.log "SDAM: Total tasks=%d, total arguments=%d, total scalar arguments=%d, total safe arguments(incl. scalars)=%d\n" !total_tasks !total_args !total_scalar_args !total_safe_args);
  )
end
