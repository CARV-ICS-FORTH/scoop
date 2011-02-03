open Cil

             (* argname * in/out type * fundec *)
type arg_type = (string * string * fundec)
      (* (task scope * taskname) * argument list *)
and task_type = ((fundec * string) * arg_type list) 
                      (* taskname *)
and dep_node = arg_type * string

and arg_dep_node = (arg_type * dep_node list)

and task_dep_node = ((fundec * string) * arg_dep_node list)



val options : (string * Arg.spec * string) list

val task_dep_l : task_dep_node list ref

(*
 * first collect all task arguments, then 
 * call addTask to add a new task with 
 * the collected arguments (args will then
 * be then reset)
 *)
val addTask : string -> fundec -> unit

 
(* use to collect arguments *)
val addArg : arg_type -> unit

(* Static analysis for task dependencies *)
val find_dependencies : file -> unit

(* returns true if the argument has no dependencies  *)
val isSafeArg : fundec -> string -> bool
