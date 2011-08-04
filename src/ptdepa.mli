open Cil

val options : (string * Arg.spec * string) list

(* Static analysis for task dependencies *)
val find_dependencies : file -> unit

(* returns true if the argument has no dependencies  *)
val isSafeArg : (*fundec ->*) string -> bool
