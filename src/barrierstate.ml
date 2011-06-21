open Pretty
open Controlflow
open Sdam

module E = Errormsg
module LF = Labelflow

let debug = ref false
let disable = ref false

module Task =
  struct
    type t = task_descr
    
		let compare (x: t) (y: t) : int =
			let (x_id, _, _) = x in
			let (y_id, _, _) = y in
			if(x_id > y_id) then 1
			else if(x_id == y_id) then 0
			else -1
 
    let equal (x: t) (y: t) : bool = 
      let (x_id, _, _) = x in
			let (y_id, _, _) = y in
			if(x_id == y_id) then true
			else false

    let hash (x: t) : int = 
			let (id, _, _) = x in
			id

		let task_to_string (x: t) : string =
			let (id, taskname, _) = x in
			"task:"^(string_of_int id)^":"^taskname;

  end 

module TaskHT = Hashtbl.Make(Task)
module TaskSet = Set.Make(Task)
type taskSet = TaskSet.t

let empty_state = TaskSet.empty

(* let currState : taskSet ref = ref TaskSet.empty *)
(* Here we store all avaible task states. FIXME: Make that a set? how? *)
let taskStates : taskSet list ref = ref []

(* Check if two tasks may happen in parallel *)
let happen_parallel (n1: dep_node) (n2: dep_node) : bool = 
	(* search for a set that contains both tasks *)
	if(!disable) then true
	else
	let ((argname1, _, _), task1) = n1 in
	let ((argname2, _, _), task2) = n2 in
	if((Task.equal task1 task2) && (argname1 == argname2)) then true
	else ( 
		let rec contains_tasks = (function
			[] -> false
		| (state::rest) -> (
			let state_l = (TaskSet.elements state) in 
				if((List.mem task1 state_l) && (List.mem task2 state_l)) then true
				else (contains_tasks rest)
		)) in contains_tasks !taskStates 
	)

(* TaskSet formatting *)
let d_taskset () (t: taskSet) : doc = 
  if TaskSet.is_empty t then text "<empty>" else
  align ++ text ( let tasks = TaskSet.elements t in
								 	let rec d_taskset' taskset_str = (function
								 		[] -> taskset_str
									| (task::rest) -> d_taskset' (taskset_str^" "^(Task.task_to_string task)) rest
									) in d_taskset' "" tasks
								) ++ unalign

let print_state (t: taskSet) : unit =
	if(TaskSet.is_empty t) then ignore(E.log "SDAM: taskSet is empty.\n")
	else 
		let tasks = TaskSet.elements t in
		List.iter (fun task -> 
			ignore(E.log "%s\n" (Task.task_to_string task));
		) tasks

let print_taskStates a = begin
	a;
	ignore(E.log "task_states_size=%d\n" (List.length !taskStates));
	List.iter (fun task_s -> 
		let tasks = TaskSet.elements task_s in
		ignore(E.log "print task set (elmts num=%d):\n" (List.length tasks));
		List.iter (fun task -> 
			let (id, taskname, _) = task in
			ignore(E.log "\ttask:%d:%s\n" id taskname);
		) tasks;
	) !taskStates;
end

(* 
 * This analysis seeks barriers, while collecting tasks.  The set of tasks is the
 * state of the analysis.  TODO:loop detection for a task
 *)
module BarrierStateTransfer = 
	struct
		type state = taskSet
		
		let state_before_phi = PhiHT.create 1000

		let transfer_fwd (p: phi) worklist (acq: state) : state option =
    begin
      let k = get_phi_kind p in
      match k with
				PhiTask task -> (
					Some (TaskSet.add task acq)
				)
			| PhiBarrier -> (
					Some TaskSet.empty
				)
			| _ -> Some acq
    end

    let starting_state (p: phi) : state option = None

    let merge_state (acq1: state) (acq2: state) : state =
      TaskSet.inter acq1 acq2

		    let merge_state (acq1: state) (acq2: state) : state =
				  TaskSet.union acq1 acq2

		(* TODO: This is probably usefull to detect if a task is in a loop *)
    let equal_state (acq1: state) (acq2: state) : bool =
      (TaskSet.equal acq1 acq2)

		(* FIXME: I do not think we need context sensitivity, return id *)
    let translate_state_in (acq: state) (i: LF.instantiation) : state =
      acq

    let translate_state_out (acq: state) (i: LF.instantiation) : state =
      acq

    let check_state (p: phi) (acq: state) : unit = ()

    let pretty () acq =
      align ++ text "acq: " ++ d_taskset () acq ++ unalign
  end

module BS = MakeForwardsAnalysis(BarrierStateTransfer)

let solve () =
	if !debug then begin
		ignore(E.log "SDAM:solve barrier analysis\n");
	end;	
	match !starting_phis with
		[] -> if !debug then ignore(E.log "phi set empty\n");
  | _ -> 	if !debug then ignore(E.log "phi set not empty\n"); (
	  List.iter
    (fun p -> PhiHT.replace BarrierStateTransfer.state_before_phi p empty_state)
    !starting_phis;	
		BS.solve !starting_phis;
		(* we only need states before each barrier *)
		let barrier_phis = List.filter 
			(fun p -> let k = get_phi_kind p in 
								match k with
									PhiBarrier -> true
								| _ -> false)	!starting_phis in
		taskStates := List.map (fun p -> (PhiHT.find BarrierStateTransfer.state_before_phi p)) barrier_phis;
	)

