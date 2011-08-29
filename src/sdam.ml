open Cil
open Printf
open Pretty

module LF = Labelflow
module E = Errormsg

(** the type that describes a loop *)
type loop_descr = {
	lid: int;
	l_index_info: varinfo;
	l_index_exp: exp;
}
(** the type that describes an array or pointer referenced in a loop *)
and array_descr = {
	array_info: varinfo;
	a_index_exp: exp;
}
(** the type that describes a task argument *)
and arg_descr = {
	aid: int;
	argname: string;
	iotype: string; 
	arginfo: varinfo;
	argsize: exp;
	loop_d: loop_descr option;
	array_d: array_descr option;
	mutable safe: bool;
}
(** the type that describes a task *) 
and task_descr = {
	tid: int;
	taskname: string;
	callsite: location;
	scope: fundec;
	read_vars: LF.rhoSet;
	write_vars: LF.rhoSet;
	arguments: arg_descr list;
}

(** global lists of tasks *)
let tasks_l : task_descr list ref = ref []

(** the current program file *)
let program_file = ref dummyFile

(** total number of tasks *)
let total_tasks = ref 0
(** total number of task arguments *)
let total_args = ref 0
(** total number of safe task arguments *)
let total_safe_args = ref 0

let next_task_id = ref 0
let next_arg_id = ref 0
let next_loop_id = ref 0

(** Utility functions **)

(** returns a string representation for a Cil.location 
		@param loc the Cil.location
		@return the string representation
*)
let location_to_string (loc: location) : string = 
	loc.file^":"^(string_of_int loc.line)

	
(** Constructors **)
	
(**	constructor for task_descr struct
		@param tname the name of the task
		@param csite the callsite of the task (line in code)
		@param scp the function in which the task is called (e.g. main)
		@param args the list of the task arguments (acutals)
		@return a new task_descr
*)	
let make_task_descr (tname: string) (csite: location) (scp: fundec) 
										(read_v: LF.rhoSet) (write_v: LF.rhoSet) 
										(args: arg_descr list) : task_descr =
	incr next_task_id;
	{	tid = !next_task_id;
		taskname = tname;
		callsite = csite;
		scope = scp;
		read_vars = read_v;
		write_vars = write_v;
		arguments = args;
	}

(**	constructor of the arg_descr struct
		@param a_name the name of the argument
		@param iot the input/output type of the argument
		@param a_inf the varinfo of the argument variable
		@param a_size the size of the argument used in the task
		@return a new arg_descr
*)
let make_arg_descr (a_name: string) (iot: string) (a_inf: varinfo) (a_size: exp) 
									 (ld: loop_descr option) (ad: array_descr option) : arg_descr =
	incr next_arg_id;
	{ aid = !next_arg_id;
		argname = a_name;
		iotype = iot;
		arginfo = a_inf;
		argsize = a_size;
		loop_d = ld;
		array_d = ad;
		safe = false;
	}

(** constructor of the loop_descr struct
		@param i_inf the varinfo of the index variable
		@param i_exp the expression of the index step in the loop
		@return a new loop descriptor
*)
let make_loop_descr (i_inf: varinfo) (i_exp: exp) : loop_descr =
	incr next_loop_id;
	{ lid = !next_loop_id;
		l_index_info = i_inf;
		l_index_exp = i_exp;
	}

(** constructor of the array_descr struct
		@param a_inf the varinfo of the array variable
		@param i_exp the expression of the array index
		@return a new array_descr
*)
let make_array_descr (a_inf: varinfo) (i_exp: exp) : array_descr = 
	{ array_info = a_inf;
		a_index_exp = i_exp;
	}
	
	
(** SDAM API **)

(** appends a task to the global list of tasks
		@param task descriptor of the task that is to be 
		added to the list
		@return unit
*)
let addTask (task_d: task_descr) : unit = begin
	incr total_tasks;
  tasks_l := task_d::!tasks_l;
end

(** Checks if the argument with argname is safe
		@param argname the name of the argument under question
		@return true if argument is safe, else false
*)
let isSafeArg (argname: string) : bool =
	let rec check_task = (function
			[] -> raise Not_found
		| (task::rest) -> (
					try (List.find (fun arg -> if(argname == arg.argname) then true else false) task.arguments) 
					with Not_found -> check_task rest
				)
	) in 
	try (let arg = check_task !tasks_l in arg.safe) 
	with Not_found -> false


(** Utility Functions **)

(** return true if iotype is strided 
			@param t the iotype of the argument
			@return true if argument type is strided
*)
let is_strided_arg (t: string): bool = 
  match t with 
      "sinput"
    | "sin"
    | "soutput"
    | "sout"
    | "sinout" -> true
    | _ -> false

(** return true if iotype is input
			@param t the iotype of the argument
			@return true if argument is input
*)
let is_in_arg (t: string): bool = 
  match t with 
      "input" (* legacy, remove *)
    | "in"
    | "sinput"
    | "sin" -> true
    | _ -> false


(** Printing functions **)

(* prints a task argument *)
let print_arg arg = begin 
	incr total_args;
	ignore(E.log "\tArgument:%s(id:%d), iotype:%s, size:%a, " arg.argname arg.aid arg.iotype d_exp arg.argsize);
	if(arg.safe) then (ignore(E.log "safe\n"); incr total_safe_args;)
	else (ignore(E.log "not safe\n");) 
end

(** Prints list of tasks
		@param a unit
		@return unit
*)
let print_tasks (tasks: task_descr list) : unit = 
	let rec print_tasks' = (function
			[] -> ();
		| (task::rest) -> (
				ignore(E.log "Task:%s\n" task.taskname);
				ignore(E.log "\tArgs:\n");
				List.iter print_arg task.arguments; 
				ignore(E.log "\tRead/Write vars:\n");
				ignore(E.log "\tread-vars:%a\n" LF.d_rhoset task.read_vars);
				ignore(E.log "\twrite-vars:%a\n" LF.d_rhoset task.write_vars);
				print_tasks' rest
			);
		) in print_tasks' tasks
		
(** Pretty print task 
		@param task the descriptor of the task to print
		@return the task in doc format
*)
let d_task () (task: task_descr) : doc =
	text(task.taskname^":"^(string_of_int task.tid))
