open Cil

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
val tasks_l : task_type list ref
(* temp list, used to collect task arguments *)
val args_l : arg_type list ref
(* each node is a task with its arguments, for each argument there is a list
  of depended arguments *)
val task_dep_l : task_dep_node list ref

val program_file : file ref

val total_safe_args : int ref
val total_tasks : int ref
val total_args : int ref

(** Utility functions **)

(* return a string representation for a Cil.location *)
val location_to_string : location -> string

val new_task_d : string -> location -> fundec -> task_descr

val new_loop_d : loop_index -> loop_descr

(** SDAM API **)
(*
 * first collect all task arguments, then 
 * call addTask to add a new task with 
 * the collected arguments (args will then
 * be then reset)
 *)
val addTask : task_descr -> task_descr

 
(* use to collect arguments *)
val addArg : arg_type -> unit


