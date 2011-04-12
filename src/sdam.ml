open Cil
open Printf
module E = Errormsg

             (* argname * in/out type * fundec *)
type arg_type = (string * string * fundec) (* FIXME:fundec is the global one, note the function it belogs to *)
          (* (id * taskname * (callsiteloc * taskscope)) *) 
and task_descr = (int * string * (location * fundec))
          (* (task scope * taskname) * argument list *)
and task_type = (task_descr * arg_type list) 
                      (* parent task_descr  *)
and dep_node = arg_type * task_descr

and arg_dep_node = (arg_type * dep_node list)

and task_dep_node = (task_descr * arg_dep_node list)

(* each node is a task, with a list of tasks that are not depended with each other *)
let tasks_l : task_type list ref = ref []
(* temp list, used to collect task arguments *)
let args_l : arg_type list ref = ref []
(* each node is a task with its arguments, for each argument there is a list
  of depended arguments *)
let task_dep_l : task_dep_node list ref = ref []

let task_cnt = ref 0

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

(** SDAM API **)

(*
 * first collect all task arguments, then 
 * call addTask to add a new task with 
 * its arguments
 *)
let addTask (taskname: string) (scope: fundec) (callsite: location): task_descr =
	let task_d = ((next_id 0), taskname, (callsite, scope)) in
  tasks_l := (task_d, !args_l)::!tasks_l;
  args_l := [];
	task_d

let addArg (arg: arg_type) : unit =
  args_l := arg::!args_l

