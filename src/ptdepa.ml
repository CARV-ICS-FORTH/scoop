open Cil
open Sdam

module E = Errormsg
module LT = Locktype
module LF = Labelflow
module BS = Barrierstate
module PT = Ptatype
module LP = Loopa
 
let do_graph_out = ref false

let do_task_graph_out = ref false

let debug = ref false

let options = [
  "--save-graph",
  Arg.Set(do_graph_out),
  "SDAM-Pointer analysis: Write constraints in \"graph.dot\".";

  "--save-dependencies-graph",
  Arg.Set(do_task_graph_out),
  "SDAM-Pointer analysis: Write task dependecies in \"task-dep.dot\".";

  "--debug-ptdepa",
  Arg.Set(debug),
  "SDAM-Pointer analysis: debugging output.";
]

let taskScope1 = ref dummyFunDec
let taskScope2 = ref dummyFunDec

(** return rhoSet for arg
			@param arg the argument whose aliasing set we seek
			@return the rhoSet with variables that alias with arg
*)
let get_rhoSet (arg: arg_descr) (scope: fundec) : LF.rhoSet =
  let env = List.assoc scope !PT.global_fun_envs in
  let (argtype, argaddress) = PT.env_lookup arg.argname env in
  (* ignore(E.log "lookup of %s gives location %a with type %a\n" argname
            LF.d_rho argaddress PT.d_tau argtype); *)
  match argtype.PT.t with
  | PT.ITPtr(_, r) -> LF.close_rhoset_pn (LF.RhoSet.singleton r)
  | _ ->  if !debug then ignore(E.log "Warning: %s is not a pointer\n" arg.argname);
	  LF.RhoSet.empty (* if arg is not a pointer, return an empty set
			   * so that is_aliased returns false *)

(** return true if argument arg is scalar
			@param arg the argument we want to check
			@return true if arg is scalar
*)
let is_scalar (arg: arg_descr) (scope: fundec) : bool =
  let env = List.assoc scope !PT.global_fun_envs in
  let (argtype, argaddress) = PT.env_lookup arg.argname env in
  match argtype.PT.t with
  | PT.ITPtr(_, r) -> false
  | _ ->  true
				 
(** checks if arg1 aliases to arg2 
			@param arg1 the arg description of the first task argument
			@param arg2 the arg description of the second task argument
			@return true, if rhoSets intersection is not empty
*)
let alias (arg1: arg_descr) (arg2: arg_descr) : bool =
  (* if both arguments are only inputs, return false (they could 
     be aliased, but we treat them as if the were not) *)
  if((is_in_arg arg1.iotype) && (is_in_arg arg2.iotype)) then false
  else (
    let set1 = get_rhoSet arg1 !taskScope1 in
    let set2 = get_rhoSet arg2 !taskScope2 in
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
    not (LF.RhoSet.is_empty final_set)
	)
            

(** find dependencies between arguments 	
			@param task1 the task descriptor whose taks argument we check for dependencies
			@param task2 the task descriptor of the task whose argument we want to check with arg
			@param arg the current argumetn under question
			@return unit
*)
exception Done
let solve_arg_dependencies ((task1: task_descr), (tasks: BS.taskSet)) (arg: arg_descr)  : unit =
	try (
		BS.TaskSet.iter (fun task2 -> 
			List.iter (fun arg' -> 
				taskScope1 := task1.scope;
				taskScope2 := task2.scope;
				(* do not check with self  if task is not in a loop *)
				if (not (BS.isInLoop task1) && arg.aid == arg'.aid) then (
					arg.safe <- true;
				)
				else (
					(if((BS.isInLoop task1) && arg.aid == arg'.aid) then (
						let res = not (alias arg arg') || LP.array_bounds_safe arg in
						if(arg.force_safe && not res) then (
							ignore(E.log "Warning:Argument has manually been marked as safe but the analysis found dependencies!\n");
							raise Done
						);
						arg.safe <- res;
					)
					else (
						let res = not (alias arg arg') in
						if(arg.force_safe && not res) then (
							ignore(E.log "Warning:Argument has manually been marked as safe but the analysis found dependencies!\n");
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

	
(**	find dependencies between task arguments
			@param tasks the list of tasks in the prorgram
			@return unit
*)
let solve_task_dependencies (tasks_l: task_descr list) : unit =
	(* traverse the list of tasks, find the taskSet that can happen in parallel with
			current task, and check between their arguments for dependencies. *)
	let solve_task_deps task = (
		if !debug then ignore(E.log "checking Task:%a\n" d_task task);
		let tasks = BS.getTaskSet task in
		(* 0. if sdam is disabled then mark only scalars as safe, do not run the analysis *)
		if !debug then ignore(E.log "TaskSet:%a\n" BS.d_taskset tasks);
		(* 1. check if tasks exists in the set, then maintain self loops, else remove them *)
		if (not (BS.isInLoop task)) then ( 
			if !debug then ignore(E.log "Not self dependent\n");
			List.iter (fun a -> a.safe <- true;) task.arguments
		);
		(* 2. check for dependencies with other tasks *)
		List.iter (solve_arg_dependencies (task, tasks)) task.arguments
	) in List.iter solve_task_deps tasks_l


(** print dependencies in graphiz format 
		@param task_l is the list of tasks in the program
		@param outf is the output channel/file
		@return unit
*)
let plot_task_dep_graph (task_l: task_descr list) (outf: out_channel) : unit = begin
	(* print nodes *)
	List.iter (fun task -> 
		let tasknode = task.taskname^"_"^(string_of_int task.tid) in
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
	ignore(E.log "SDAM:Initializing Cil...\n");
  Rmtmps.removeUnusedTemps f;
  Rmalias.removeAliasAttr f;
	Cfg.computeFileCFG f;
  ignore(E.log "SDAM:Finding data dependencies...\n");
  PT.generate_constraints f;
  LF.done_adding ();
  if(disable_sdam) then (
  	List.iter(fun task -> 
			List.iter (fun a -> if ( (is_scalar a task.scope) or a.force_safe) then (
											a.safe <- true; 
                    ) else ( 
											a.safe <- false;
										)
			) task.arguments
  	) !tasks_l;
  )
  else (
		BS.solve();
		solve_task_dependencies (List.rev !tasks_l);
  (* BS.solve(); *)
(*   if !do_graph_out then begin
    Dotpretty.init_file "graph-begin.dot" "initial constraints";
    Labelflow.print_graph !Dotpretty.outf;
    Dotpretty.close_file ();
  end; *)
(*   ignore(E.log "SDAM: Checking for argument dependencies.\n");
  List.iter find_task_dependencies !tasks_l;
  if !do_verbose_output then begin (* this is bocus... *)
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
  ignore(E.log "SDAM: static dependence analysis has now completed.\n");
(*   count_safe_args !task_dep_l; *)
		if !do_task_graph_out then (
			Dotpretty.init_file "task-dep.dot" "task dependencies";
			plot_task_dep_graph !tasks_l !Dotpretty.outf;
			Dotpretty.close_file ();
		);
		print_tasks (List.rev !tasks_l);
		ignore(E.log "SDAM: Total tasks=%d, total arguments=%d, total safe arguments=%d\n" !total_tasks !total_args !total_safe_args);
	)
end

