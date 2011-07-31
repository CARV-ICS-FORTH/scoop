open Cil
open Printf
module E = Errormsg
          (* (id * taskname * (callsiteloc * taskscope)) *) 
type task_descr = (int * string * (location * fundec))
					(* in/out type * arg varinfo * arg size*)
and arg_info = (string * varinfo * exp) (* FIXME: add size info *)
            (* argname * in/out type * task it belongs to *)
and arg_type = (string * arg_info * task_descr) (* FIXME:fundec is the global one, not the function it belogs to *)
          (* (task scope * taskname) * argument list *)
and task_type = (task_descr * arg_type list) 
                      (* parent task_descr  *)
and dep_node = arg_type * task_descr

and arg_dep_node = (arg_type * dep_node list)

and task_dep_node = (task_descr * arg_dep_node list)
			 (* loop index info * loop index expr *)
and loop_index = (varinfo * exp) option
					 (* loop id * loop index  *)
and loop_descr = (int * loop_index) option
			(* same as arg type but without in/out flag, it is used when we work on actuals *)
and arg_descr = (string * task_descr)
						(* array name * index *)
and array_descr = (varinfo * exp)
														(* array * loop descriptor*)
and array_loop_descr = (array_descr * loop_descr)


(* each node is a task, with a list of tasks that are not depended with each other *)
let tasks_l : task_type list ref = ref []
(* temp list, used to collect task arguments *)
let args_l : arg_type list ref = ref []
(* each node is a task with its arguments, for each argument there is a list
  of depended arguments *)
let task_dep_l : task_dep_node list ref = ref []

let task_cnt = ref 0

let program_file = ref dummyFile

(** Utility functions **)

(* return a fresh task id *)
let next_id = 
	let counter = ref 0 in
	fun id ->
		incr counter;
		!counter

(* return a string representation for a Cil.location *)
let location_to_string (loc: location) : string = 
	loc.file^":"^(string_of_int loc.line)

let new_task_d (taskname: string) (callsite: location) (scope: fundec) : task_descr =
	((next_id 0), taskname, (callsite, scope))

let new_loop_d (lpi: loop_index) : loop_descr = 
	Some((next_id 0), lpi)

(** SDAM API **)

(*
 * first collect all task arguments, then 
 * call addTask to add a new task with 
 * its arguments
 *)
let addTask task_d : task_descr =
  tasks_l := (task_d, !args_l)::!tasks_l;
  args_l := [];
	task_d

let addArg (arg: arg_type) : unit =
  args_l := arg::!args_l

