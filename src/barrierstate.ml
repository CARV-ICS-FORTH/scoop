open Pretty
open Controlflow
open Sdam

module E = Errormsg
module LF = Labelflow

module Task =
  struct
    type t = task_descr
    let compare (x: t) (y: t) : int =
      -1
    let equal (x: t) (y: t) : bool = 
      false (* FIXME:dummy functions, need to  *)
    let hash (x: t) : int = 1
  end

module TaskHT = Hashtbl.Make(Task)
module TaskSet = Set.Make(Task)
type taskSet = TaskSet.t

(* TaskSet formatting *)
let d_taskset () (t: taskSet) : doc = 
  if TaskSet.is_empty t then text "<empty>" else
  align ++ text "<not empty>" ++ unalign

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
			ignore(E.log "transfer_fwd\n");
      let k = get_phi_kind p in
      match k with
      | PhiVar -> Some acq
			| PhiBarrier -> ignore(E.log "barrier node found!\n"); Some acq (* here create a new state, but save the previous set of tasks *)
			| PhiTask -> ignore(E.log "task node found!\n"); Some acq	(* add a task to the state *)
			| _ -> Some acq
    end

    let starting_state (p: phi) : state option = None

    let merge_state (acq1: state) (acq2: state) : state =
      TaskSet.inter acq1 acq2

		(* TODO: This is probably usefull to detect if a task is in a loop *)
    let equal_state (acq1: state) (acq2: state) : bool =
      (TaskSet.equal acq1 acq2)

		(* FIXME: I do not think we need context sensitivity,,, *)
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
	ignore(E.log "solve barrier analysis\n");
  BS.solve !starting_phis

