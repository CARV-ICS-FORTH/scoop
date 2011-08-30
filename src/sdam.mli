open Cil
open Printf

(* the type that describes a loop *)
type loop_descr = {
	lid: int;
	l_index_info: varinfo;
	l_index_exp: exp;
}
(* the type that describes an array or pointer referenced in a loop *)
and array_descr = {
	array_info: varinfo;
	a_index_exp: exp;
}
(* the type that describes a task argument *)
and arg_descr = {
	aid: int;
	argname: string;
	argloc: location
	iotype: string; 
	arginfo: varinfo;
	argsize: exp;
	loop_d: loop_descr option;
	array_d: array_descr option;
	mutable safe: bool;
}
(* the type that describes a task *) 
and task_descr= {
	tid: int;
	taskname: string;
	callsite: location;
	scope: fundec;
	read_vars: Labelflow.rhoSet;
	write_vars: Labelflow.rhoSet;
	arguments: arg_descr list;
}

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

(* return true if iotype is strided *)
val is_strided_arg : string -> bool

(* return true if iotype is input *)
val is_in_arg : string -> bool

(** Constructors **)
	
(* constructor for task_descr struct *)	
val make_task_descr : string -> location -> fundec -> Labelflow.rhoSet -> Labelflow.rhoSet -> arg_descr list -> task_descr

(*	constructor of the arg_descr struct *)
val make_arg_descr : string ->  string -> location -> varinfo ->  exp -> loop_descr option -> array_descr option -> arg_descr

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

(* returns a string representation for a Cil.location *)
val location_to_string : location -> string

(* Prints list of tasks	*)
val print_tasks : task_descr list -> unit	

(* Pretty print task *)
val d_task : unit -> task_descr -> Pretty.doc
