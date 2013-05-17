open Cil
open Ptatypes
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
	argloc: location;
	iotype: string;
	strided: bool;
	arginfo: varinfo;
	argsize: exp;
	loop_d: loop_descr option;
	array_d: array_descr option;
	dependencies: arg_descr list ref;
	mutable safe: bool;
	mutable force_safe: bool;
	mutable iotype_deduction: string;
}
(* the type that describes a task *)
and task_descr = {
	taskid: int;
	taskname: string;
	callsite: location;
	scope: fundec;
	t_inf: fdinfo;
	t_gamma: env;
	arguments: arg_descr list;
	actuals: string list;
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
(* total number of scalar variables passed as task arguments *)
val total_scalar_args: int ref

(** Utility functions **)

(* return true if iotype is input *)
val is_in_arg : string -> bool

(** Constructors **)

(* constructor for task_descr struct *)
val make_task_descr : string  -> location -> fundec -> fdinfo -> env -> arg_descr list -> string list -> task_descr

(*	constructor of the arg_descr struct *)
val make_arg_descr : string -> location -> string  -> bool -> varinfo -> exp -> loop_descr option -> array_descr option -> arg_descr

(* constructor of the loop_descr struct *)
val make_loop_descr : varinfo -> exp -> loop_descr

(* constructor of the array_descr struct *)
val make_array_descr : varinfo -> exp -> array_descr

(** SDAM API **)

(* appends a task to the global list of tasks *)
val addTask : task_descr -> unit

(* Checks if the argument with argname is safe *)
val isSafeArg : string -> int -> string -> bool

(** Printing functions **)

(* returns a string representation for a Cil.location *)
val location_to_string : location -> string

(* Counts the number of tasks their arguments and which of them are
   scalar, safe. Additionally if !debug it prints them out *)
val count_tasks_and_args : task_descr list -> unit

(* Pretty print task *)
val d_task : unit -> task_descr -> Pretty.doc
