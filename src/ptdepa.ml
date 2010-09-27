open Pretty
open Cil
open Printf

module PT = Ptatype
module LF = Labelflow
module E = Errormsg

let do_graph_out = ref false

let do_task_graph_out = ref false

let options = [
  "--save-graph",
  Arg.Set(do_graph_out),
  " Write constraints in \"graph.dot\".";

  "--save-dependencies-graph",
  Arg.Set(do_task_graph_out),
  " Write task dependecies in \"task-dep.dot\"";
]

             (* argname * in/out type * fundec *)
type arg_type = (string * string * fundec)
      (* (task scope * taskname) * argument list *)
and task_t = ((fundec * string) * arg_type list) 

and s_task_t = (fundec * string)

let task_args_l : arg_type list ref = ref []
(* each node is an argument with all dependent arguments *)
let arg_dep_l : (arg_type * arg_type list) list ref = ref []
(* temporary usage, hold a tmp list of aliased args to be stored in arg_dep_l *)
let aliased_args : arg_type list ref = ref []
(* list that contains task dependencies *)
let task_dep_l : ((fundec * string) * (fundec * string) list) list ref = ref []
(* each node is a task, with a list of tasks that are not depended with each other*)
let tasks_l : task_t list ref = ref []
(* temp list, used to collect task arguments *)
let args_l : arg_type list ref = ref []
(* temporary usage, collect depended tasks *)
let depended_tasks : (fundec * string) list ref = ref []

(* visit all functions and prepare CFG *)
class prepareCFGVisitor = object
  inherit nopCilVisitor

  method vfunc (f: fundec) : fundec visitAction =
	prepareCFG f;
	DoChildren
end

(*
 * first collect all arguments, then 
 * call addTask to add a new task with 
 * its arguments
 *)
let addTask (taskname: string) (scope: fundec) : unit =
  tasks_l := ((scope,  taskname), !args_l)::!tasks_l;
  args_l := []

let addArg (arg: arg_type) : unit = 
  args_l := arg::!args_l

(*
(* appends the new argument to the corresponding task, in task dependence list *)
let append2task_dep_l (task: (fundec * string)) (arg: (string * arg_type list)) : unit =
  try
    let dep_l = ref (List.assoc task !task_dep_l) in
      dep_l :=  arg::!dep_l
      with Not_found ->
        let new_task_dep = (task, [arg]) in
	task_dep_l := new_task_dep::!task_dep_l
*)

(* return rhoSet for the specific arg (argument is argname * function descriptor) *)
let get_rhoSet (arg: arg_type) : LF.rhoSet =
  let (argname, _, func) = arg in
  let env = List.assoc func !PT.global_fun_envs in
  let (argtype, argaddress) = PT.env_lookup argname env in
  (* ignore(E.log "lookup of %s gives location %a with type %a\n" argname
            LF.d_rho argaddress PT.d_tau argtype); *)
  match argtype.PT.t with
  | PT.ITPtr(_, r) -> LF.close_rhoset_m (LF.RhoSet.singleton r)
  | _ ->  ignore(E.log "Warning: %s is not a pointer" argname);
	  LF.RhoSet.empty (* if arg is not a pointer, return an empty set
			   * so that is_aliased returns false *)
(* return true if arg_t is In or SIn *)
let is_in_arg (arg: string): bool = 
  match arg with 
      "In"
    | "SIn" -> true 
    | _ -> false

(* check if arg1 aliases to arg2 *)
let is_aliased (arg1: arg_type) (arg2: arg_type) : bool =
  (* if both arguments are only inputs, return false (they could 
     be alised, but we treat them as if the were not) *)
  let (_, t, _) = arg1 in
  let (_, t', _) = arg2 in
  if((is_in_arg t) && (is_in_arg t')) then false 
  else
    let set1 = get_rhoSet arg1 in
    let set2 = get_rhoSet arg2 in
    let final_set = LF.RhoSet.inter set1 set2 in
    (*
    let (argname1, _) = arg1 in
    let (argname2, _) = arg2 in 
    ignore(E.log "%s set           : %a\n" argname1 LF.d_rhoset set1);
    ignore(E.log "%s set           : %a\n" argname2 LF.d_rhoset set2);
    ignore(E.log "rhoset intersection: %a\n" LF.d_rhoset final_set);
    *)
    not (LF.RhoSet.is_empty final_set)

(* 
 * check if there are any dependencies between any of the arguments 
 * of the two tasks.  This version stops further checking if any 
 * dependency is found.
 *)
exception Task_dependence_Found
let are_depended (args: arg_type list) (arg: arg_type) : unit = begin
  let args_number = List.length args in
  for i = 0 to (args_number-1) do 
    let arg' = List.nth args i in
    if(is_aliased arg arg') then raise(Task_dependence_Found)
  done;
end


(* finds all ptr dependencies for arg1 *)
let find_arg_dependencies (arg1: arg_type) : unit = begin
  let args_number = List.length !task_args_l in
    for i = 0 to (args_number-1) do
      let arg2 = List.nth !task_args_l i in
      let (argname1, _, func1) = arg1 in
      let (argname2, _, func2) = arg2 in
      if(is_aliased arg1 arg2) then
        aliased_args := arg2::!aliased_args;
    done;
    arg_dep_l := (arg1, !aliased_args)::!arg_dep_l;
    (* let (argname, func, taskname) = arg1 in
    append2task_dep_l (func, taskname) (argname, !aliased_args); *)				
    aliased_args := [];
end
  

let find_task_dependencies (elem: task_t) : unit = begin
  let (task, args) = elem in ( 
  let tasks_number = List.length !tasks_l in 
  for i = 0 to (tasks_number-1) do  
    let (task', args') = List.nth !tasks_l i in
    try (List.iter (are_depended args) args') (* will raise exception if arg dependencies are found *) 
    with Task_dependence_Found -> depended_tasks := task'::!depended_tasks;
  done;
  task_dep_l := (task, !depended_tasks)::!task_dep_l;
  depended_tasks := [];)
end

(* utilities *)
let arg2string (arg: arg_type) : string =
  let (argname, _, _) = arg in
  "arg: "^argname

let print_arg_dependencies (dep_l: arg_type * arg_type list) : unit = begin
  ignore(E.log "%s\n" (arg2string (fst dep_l)));
  let dep_num = List.length (snd dep_l) in
  ignore(E.log "arg aliases to %d other args\n" dep_num);
  for i = 0 to (dep_num-1) do
    let arg = List.nth (snd dep_l) i in
    ignore(E.log "\t%s\n" (arg2string arg));		
  done;
end

let task2string (task: s_task_t) : string =
  let (_, taskname) = task in
  "task: "^taskname

let print_task_dependencies (task_l: (s_task_t * s_task_t list)) : unit = begin
  ignore(E.log "%s\n" (task2string (fst task_l)));
  let task_num = List.length (snd task_l) in
  ignore(E.log "task is depended with %d other tasks\n" task_num);
  for i = 0 to (task_num-1) do
    let task = List.nth (snd task_l) i in
    ignore(E.log "\t%s\n" (task2string task));
  done;
end

(* routines used to produce a graph (graphviz language) *)
let print_task_graph (outf: out_channel) : unit = 
  List.iter 
    (fun (n: (s_task_t * s_task_t list)) -> begin 
      let (_, taskname) = (fst n) in
      let dep_l = (snd n) in
      List.iter 
        (fun dn -> begin
          Printf.fprintf outf "%s->%s\n" taskname (snd dn);
        end) dep_l;
      Printf.fprintf outf "%s\n" taskname;
      end)
    !task_dep_l
      
let print_args_graph (outf: out_channel) : unit = 
  List.iter 
    (fun (n: (arg_type * arg_type list)) -> begin 
      let (argname, _, _) = (fst n) in
      let dep_l = (snd n) in
      List.iter 
        (fun dn -> begin
          let (argname', _, _) = dn in
          Printf.fprintf outf "%s->%s\n" argname argname';
        end) dep_l;
      Printf.fprintf outf "%s\n" argname;
      end)
    !arg_dep_l
 

let find_dependencies (f: file) : unit = begin	
  (* let prepareCFGs = new prepareCFGVisitor in
     visitCilFile prepareCFGs f; *)
  Rmtmps.removeUnusedTemps f;
  Rmalias.removeAliasAttr f;
  PT.generate_constraints f;
  (* LF.done_adding (); *)
  if !do_graph_out then begin
    Dotpretty.init_file "graph-begin.dot" "initial constraints";
    Labelflow.print_graph !Dotpretty.outf;
    (*
    Semiunification.print_graph !Dotpretty.outf;
    Lockstate.print_graph !Dotpretty.outf;u
    *)
    Dotpretty.close_file ();
  end;
  List.iter find_arg_dependencies !task_args_l;
  List.iter print_arg_dependencies !arg_dep_l;
  List.iter find_task_dependencies !tasks_l;
  List.iter print_task_dependencies !task_dep_l;
  if !do_task_graph_out then begin
    Dotpretty.init_file "arg-dep.dot" "argument dependencies";
    print_args_graph !Dotpretty.outf;
    Dotpretty.close_file ();
    Dotpretty.init_file "task-dep.dot" "task dependencies";
    print_task_graph !Dotpretty.outf;
    Dotpretty.close_file ();
  end;
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
