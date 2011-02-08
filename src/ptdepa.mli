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

val options : (string * Arg.spec * string) list

(*
 * first collect all task arguments, then 
 * call addTask to add a new task with 
 * the collected arguments (args will then
 * be then reset)
 *)
val addTask : string -> fundec -> location -> unit

 
(* use to collect arguments *)
val addArg : arg_type -> unit

(* Static analysis for task dependencies *)
val find_dependencies : file -> unit

(* returns true if the argument has no dependencies  *)
val isSafeArg : fundec -> string -> bool
