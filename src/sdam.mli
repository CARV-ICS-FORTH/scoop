open Cil

           (* argname * in/out type * fundec *)
type arg_type = (string * string * fundec) (* FIXME:fundec is the global one, note the function it belogs to *)
          (* (taskname * (callsiteloc * taskscope)) *) 
and task_descr = (string * (location * fundec))
          (* (task scope * taskname) * argument list *)
and task_type = (task_descr * arg_type list) 
                      (* parent task_descr  *)
and dep_node = arg_type * task_descr

and arg_dep_node = (arg_type * dep_node list)

and task_dep_node = (task_descr * arg_dep_node list)

(* each node is a task, with a list of tasks that are not depended with each other *)
val tasks_l : task_type list ref
(* temp list, used to collect task arguments *)
val args_l : arg_type list ref
(* each node is a task with its arguments, for each argument there is a list
  of depended arguments *)
val task_dep_l : task_dep_node list ref

(** SDAM API **)
(*
 * first collect all task arguments, then 
 * call addTask to add a new task with 
 * the collected arguments (args will then
 * be then reset)
 *)
val addTask : string -> fundec -> location -> unit

 
(* use to collect arguments *)
val addArg : arg_type -> unit



(** Utility functions **)

(* return a string representation for a Cil.location *)
val location_to_string : location -> string
