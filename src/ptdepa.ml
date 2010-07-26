open Pretty
open Cil
open Printf

module PT = Ptatype
module LF = Labelflow

type arg_type = (string * fundec * string)
 
let task_args_l : arg_type list ref = ref []
(* let task_l : (fundec * (string * string) list) list ref = ref [] *)
(* each node is an argument with all dependent arguments *)
let arg_dep_l : (arg_type * arg_type list) list ref = ref []
(* temporary usage, hold a tmp list of aliased args to be stored in arg_dep_l *)
let aliased_args : arg_type list ref = ref []
(* each node is a task, with its args(only name),
 * and for each arg a list of depended tasks *)
let task_dep_l : ((fundec * string) * (string * arg_type list) list) list ref = ref []

(* visit all functions and prepare CFG *)
class prepareCFGVisitor = object
  inherit nopCilVisitor
  method vfunc (f: fundec) : fundec visitAction = 
	prepareCFG f;
	DoChildren	
end

(*
addfun fd : 3578

*)
(* appends the new argument to the corresponding task, in task dependence list *)
let append2task_dep_l (task: (fundec * string)) (arg: (string * arg_type list)) : unit =
	try
		let dep_l = ref (List.assoc task !task_dep_l) in 
		dep_l :=  arg::!dep_l
	with Not_found -> 
		let new_task_dep = (task, [arg]) in
		task_dep_l := new_task_dep::!task_dep_l 

(* return rhoSet for the specific arg (argument is argname * function descriptor) *)
let get_rhoSet (arg: (string * fundec)) : LF.rhoSet =
	let (argname, func) = arg in 
	let env = List.assoc func !PT.global_fun_envs in 
	let (argtype, argaddress) = PT.env_lookup argname env in 
	match argtype.PT.t with 
	| PT.ITPtr(_, _) -> LF.get_rho_p2set_m argaddress
	| _ ->  print_endline ("Warning: "^argname^" is not a pointer.");
		LF.RhoSet.empty (* if arg is not a pointer, return an empty set 
				 * so that is_aliased returns false *)

(* check if arg1 is aliased to arg2 *)
let is_aliased (arg1: (string * fundec)) (arg2: (string * fundec)) : bool = 
	let set1 = get_rhoSet arg1 in 
	let set2 = get_rhoSet arg2 in
	let final_set = LF.RhoSet.inter set1 set2 in
	not (LF.RhoSet.is_empty final_set)

(* finds all ptr dependencies for arg1 *)
let find_arg_dependencies (arg1: (string * fundec* string)) : unit = begin 
	let args_number = List.length !task_args_l in
		for i = 0 to (args_number-1) do 
			let arg2 = List.nth !task_args_l i in
			let (argname1, func1, taskname1) = arg1 in
			let (argname2, func2, taskname2) = arg2 in
			(* check only in different tasks *)
			if(func1 != func2 && taskname1 <> taskname2 ) then 
				if(is_aliased (argname1, func1) (argname2, func2)) then
					aliased_args := arg2::!aliased_args;
		done;
	arg_dep_l := (arg1, !aliased_args)::!arg_dep_l;
	let (argname, func, taskname) = arg1 in
	append2task_dep_l (func, taskname) (argname, !aliased_args);				
	aliased_args := [];
end

let arg2string (arg : (string * fundec * string)) : string = 
	let (varname, func, taskname) = arg in
	"arg: "^varname^" in task: "^taskname^"in func: "^func.svar.vname

let print_dependencies (dep_l : (string * fundec * string) * (string * fundec * string ) list) : unit = begin
	print_endline (arg2string (fst dep_l));
	let dep_num = List.length (snd dep_l) in
	for i = 0 to (dep_num-1) do
		let arg = List.nth (snd dep_l) i in
		print_endline (arg2string arg);		
	done;
end

let print_arg arg = print_endline (arg2string arg)  

let find_dependencies (f: file) : unit = begin	
	print_endline "no dependence shall escape!";
	let list_size = List.length !task_args_l in
	printf "size %d\n" list_size;
	List.iter print_arg !task_args_l;
	(* let prepareCFGs = new prepareCFGVisitor in 
	   visitCilFile prepareCFGs f; *) 
	(* Rmtmps.removeUnusedTemps f;
        Rmalias.removeAliasAttr f; *)
	PT.generate_constraints f;
	LF.done_adding ();
	List.iter find_arg_dependencies !task_args_l;
	List.iter print_dependencies !arg_dep_l;
	print_endline "dependencies found.";
end

(*
let feature : featureDescr = 
  { fd_name = "findptrdep";
    fd_enabled = ref false;
    fd_extraopt = [];
    fd_description = "find ptr dependencies";
    fd_doit =
	
	(function (f: file) ->
		print_endline "ptda running";
		find_dependencies f;
	);
	fd_post_check = true; 
  }
*)
