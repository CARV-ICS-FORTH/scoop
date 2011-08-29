open Cil
open Sdam

val options : (string * Arg.spec * string) list

(*	find dependencies between task arguments *)
val solve_task_dependencies : task_descr list -> unit

(* Entrance function to call the static analysis for task dependencies *)
val find_dependencies : file -> bool -> unit	
