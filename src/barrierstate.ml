open Pretty
open Controlflow
open Sdam

module E = Errormsg
module LF = Labelflow

let debug = ref false
let disable = ref false

let options = [
  "--debug-barrierstate",
  Arg.Set(debug),
  "SDAM-Barrier Analysis: debugging output.";
]

module Task =
  struct
    type t = task_descr
    
		let compare (x: task_descr) (y: task_descr) : int =
			if(x.tid > y.tid) then 1
			else if(x.tid == y.tid) then 0
			else -1
 
    let equal (x: task_descr) (y: task_descr) : bool = 
			if(x.tid == y.tid) then true
			else false

    let hash (x: task_descr) : int = x.tid

		let task_to_string (x: task_descr) : string =
			"task:"^(string_of_int x.tid)^":"^x.taskname;

  end 

module TaskHT = Hashtbl.Make(Task)
module TaskSet = Set.Make(Task)
type taskSet = TaskSet.t

let empty_state = TaskSet.empty

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
			| PhiLoopStart _ -> ignore(E.log "Loop!!!\n"); Some acq
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

(** if task is included in the taskset of it's phi then task is in a loop
			@param task the task which we want to know if it's in a loop
			@return true if task is in loop
*)
let isInLoop (task: task_descr) : bool =
	let match_task task p = (
		let k = get_phi_kind p in
		match k with 
			PhiTask t when (task.tid == t.tid) -> true
		| _ -> false
	) in 
	let task_phi = List.find (match_task task) !starting_phis in
	let tasks = PhiHT.find BarrierStateTransfer.state_before_phi task_phi in
	TaskSet.mem task tasks

(** return a set of tasks that can happen parallely with the task 
			in question
			@param task the task whose taskSet we seek
			@return of a set of tasks that can happen in parallel
			@exception Not_found if task is not included in any set 
				(this is an error, it should not happen)
*)	
let getTaskSet (task: task_descr) : taskSet =
	(* traverse states before barriers until we find the one that our task is in *)
	let rec find_set = (function
			[] -> raise Not_found
		| (curphi::rest) -> (
			let k = get_phi_kind curphi in
			(match k with 
				PhiBarrier -> (
					let tasks = PhiHT.find BarrierStateTransfer.state_before_phi curphi in
					if (TaskSet.mem task tasks) then tasks 
					else find_set rest  
				)
			| _ -> find_set rest
			)
		)
	) 
	in 
	try (find_set !starting_phis)
	with Not_found -> TaskSet.empty (* if task is not found, we can presume that is never run *)
	
(** make the forward analysis of barriers 
			@return unit
*)
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
	)

