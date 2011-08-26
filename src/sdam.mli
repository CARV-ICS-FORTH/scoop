open Cil
open Printf

(* the type that describes a task argument *)
type arg_descr = {
	aid: int;
	argname: string;
	iotype: string; 
	arginfo: varinfo;
	argsize: exp;
	mutable safe: bool;
}
(* the type that describes a task *) 
type task_descr= {
	tid: int;
	taskname: string;
	callsite: location;
	scope: fundec;
	read_vars: Labelflow.rhoSet;
	write_vars: Labelflow.rhoSet;
	arguments: arg_descr list;
}
(* the type that describes a loop *)
type loop_descr
(* the type that describes an array or pointer referenced in a loop *)
type array_descr

(* global lists of tasks *)
val tasks_l : task_descr list ref

(* the current program file *)
val program_file : file ref

(* total number of tasks *)
val total_tasks : int ref
(* total number of task arguments *)
val total_args : int ref
(* total number of safe task arguments *)
val total_safe_args : int ref

(** Utility functions **)

(* returns a string representation for a Cil.location *)
val location_to_string : location -> string

(** Constructors **)
	
(* constructor for task_descr struct *)	
val make_task_descr : string -> location -> fundec -> Labelflow.rhoSet -> Labelflow.rhoSet -> arg_descr list -> task_descr

(*	constructor of the arg_descr struct *)
val make_arg_descr : string ->  string -> varinfo ->  exp -> arg_descr

(* constructor of the loop_descr struct *)	
val make_loop_descr : varinfo -> exp -> loop_descr

(* constructor of the array_descr struct *)
val make_array_descr : varinfo -> exp -> array_descr
	
(** SDAM API **)

(* appends a task to the global list of tasks *)
val addTask : task_descr -> unit

(* Checks if the argument with argname is safe *)
val isSafeArg : string -> bool

(** Printing functions **)

(* Prints list of tasks	*)
val print_tasks : task_descr list -> unit	

(* Pretty print task *)
val d_task : unit -> task_descr -> Pretty.doc
