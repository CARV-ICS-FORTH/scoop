(****************************************************************************)
(* Copyright (c) 2010-13,                                                   *)
(*                        Dimitris  Chassapis       <hassapis@ics.forth.gr> *)
(*                        Polyvios  Pratikakis      <polyvios@ics.forth.gr> *)
(*                                                                          *)
(*                        FORTH-ICS / CARV                                  *)
(*                        (Foundation for Research & Technology -- Hellas,  *)
(*                         Institute of Computer Science,                   *)
(*                         Computer Architecture & VLSI Systems Laboratory) *)
(*                                                                          *)
(*                                                                          *)
(*                                                                          *)
(* Licensed under the Apache License, Version 2.0 (the "License");          *)
(* you may not use this file except in compliance with the License.         *)
(* You may obtain a copy of the License at                                  *)
(*                                                                          *)
(*     http://www.apache.org/licenses/LICENSE-2.0                           *)
(*                                                                          *)
(* Unless required by applicable law or agreed to in writing, software      *)
(* distributed under the License is distributed on an "AS IS" BASIS,        *)
(* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. *)
(* See the License for the specific language governing permissions and      *)
(* limitations under the License.                                           *)
(****************************************************************************)

(*
 *
 * Copyright (c) 2004-2010,
 *  Polyvios Pratikakis <polyvios@cs.umd.edu>
 *  Michael Hicks       <mwh@cs.umd.edu>
 *  Jeff Foster         <jfoster@cs.umd.edu>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)
open Cil
open Sdam
open Printf
open Pretty
open Lockutil
open Labelflow
open Scoop_util
open Shared
open Ptatypes

module E      = Errormsg
module U      = Uref
module Lprof  = Lockprofile
module Uniq   = Uniqueness
module LV     = Liveness
module LN     = Labelname
module CG     = Callgraph
module LF     = Labelflow
module VS     = Usedef.VS
module RD     = Reachingdefs
module GB     = Correlation
module Conf   = Locksettings
module Q      = Worklist.QueueWorklist
module CF     = Controlflow
module PhiSet = CF.PhiSet
module LP     = Loopa

let string_of_doc d = Pretty.sprint 800 d
let string_of_exp e = string_of_doc (d_exp () e)

let current_function = ref dummyFunDec
let currSid = ref (-1)

(* wrappers *)
let join_phi (p1: phi) (p2: phi) (k: CF.phi_kind) : phi =
  if p1 == p2 then p1 else begin
    let p = CF.join_phi p1 p2 in
    CF.set_phi_kind p k;
    p
  end

let make_phi (s: string) (k: CF.phi_kind) : phi =
  let p = CF.make_phi s in
  CF.set_phi_kind p k;
  p

(* user interface *)
(* the str following the pragma *)
let pragma_str = ref ""
(* debug moved to ptatypes.ml *)
let debug_SDAM = ref false
let debug_region = ref false
(* debug_void moved to ptatypes.ml *)
let debug_one_effect = ref false
let do_typing_stats = ref false
let do_starting_forks = ref false

let do_void_conflate = ref false
let do_void_single = ref false
let do_uniq = ref true
let do_existentials = ref true
let do_ignore_casts = ref true
let do_compact_structs = ref false
let do_one_effect = ref true
let do_context_sensitive = ref true
let do_field_sensitive = ref true
let do_asm = ref true
let do_contextual = ref false

let options = [
  "--debug-ptatype",
  Arg.Set(debug),
  "SDAM: Print debugging information during sdam-typechecking phace (constraint//phi generation).";

  "--debug-typing",
  Arg.Set(debug),
  "SDAM: Print progress information during the typechecking phase (constraint generation)";

  "--do-typing-stats",
  Arg.Set(do_typing_stats),
  "SDAM: Print statistics on void*/lazy field results and fork/non-fork functions";

  "--do-starting-forks",
  Arg.Set(do_starting_forks),
  "SDAM: Consider thread creation points to be independent";

  "--no-void",
  Arg.Set(do_void_conflate),
  "SDAM: Conflate at void* casts";

  "--single-void",
  Arg.Set(do_void_single),
  "SDAM: Conflate at void* casts for more than 1 types";

  "--debug-void",
  Arg.Set(debug_void),
  "SDAM: Print debugging status information about void* and struct-field computation";

  "--no-uniqueness",
  Arg.Clear(do_uniq),
  "SDAM: Don't use uniqueness analysis";

  "--no-existentials",
  Arg.Unit(fun () -> do_existentials := false; do_compact_structs := true),
  "SDAM: Don't use existential types";

  "--no-ignore-casts",
  Arg.Clear(do_ignore_casts),
  "SDAM: Create subtyping constraints for each cast, rather than ignoring them";

  "--no-use-one-effect",
  Arg.Clear(do_one_effect),
  "SDAM: Don't use only one effect variable for the whole body of non-forking functions";

  "--debug-one-effect",
  Arg.Set(debug_one_effect),
  "SDAM: Print details about forking/non-forking functions";

  "--context-insensitive",
  Arg.Clear(do_context_sensitive),
  "SDAM: Don't treat function calls context sensitively.";

  "--field-insensitive",
  Arg.Clear(do_field_sensitive),
  "SDAM: Field-insensitive analysis: all fields of a struct are aliased.";

  "--no-asm",
  Arg.Clear(do_asm),
  "SDAM: Ignore asm blocks.";

  "--contextual",
  Arg.Set(do_contextual),
  "SDAM: Use the function call rule from the contextual-effects paper.";
(*
  "--compact-structs",
  Arg.Set(do_compact_structs),
  "SDAM: Allow struct pointers in structs to be unfolded once";
*)
]

exception TypingBug
exception Ignore_pragma

let current_function : fundec ref = ref Cil.dummyFunDec
let current_chi: chi ref = ref (make_chi "X")
let pragmaKeyword : string = "existential"

let forced_effect: effect option ref = ref None
let force_effect (e: effect) : unit = begin
  assert (!forced_effect = None);
  forced_effect := Some e;
end
let unforce_effect () : unit = begin
  assert (!forced_effect <> None);
  forced_effect := None;
end
let make_effect x t =
  if isSome !forced_effect
  then getSome !forced_effect
  else LF.make_effect x t

let d_instantiation () i =
  LF.d_instantiation () i ++
    (if !debug then dprintf "(%s)" (dotstring_of_inst i) else nil)
(*****************************************************************************)

let functions_that_call_fork : (string, unit) Hashtbl.t = Hashtbl.create 117
let typenames : (string, Cil.typ) Hashtbl.t = Hashtbl.create 117
let atomic_functions : (Cil.fundec, chi) Hashtbl.t = Hashtbl.create 42
let regiontypes : Cil.typ list ref = ref []
let regiontypesigs : Cil.typsig list ref = ref []

class unrollTypeVisitor = object
  inherit nopCilVisitor
  method vglob = function
    | GType(ti, _) ->
        let t = (Cil.unrollTypeDeep ti.ttype) in
        Hashtbl.add typenames ti.tname t;
        if List.mem ti.tname !Conf.region_type_names then (
          regiontypes := TNamed(ti,[])::!regiontypes;
          regiontypesigs := (typeSig ti.ttype)::(typeSig t)::!regiontypesigs;
        );
        DoChildren
    | GCompTag(ci,_) ->
        Hashtbl.add typenames ("struct "^ci.cname) (TComp(ci,[]));
        DoChildren
    | _ -> DoChildren
end


(*****************************************************************************)

(* XXX: types moved to ptatype_type.ml *)

type special_function_t = exp list -> lval option -> (tau*uniq) list -> env ->
                          phi -> effect -> gamma

type global_kind =
    KGlobal       (* A global variable, always live *)
  | KMalloc_Addr  (* A malloc or address of a local, which have
                     self-loops but are not live, unless they
                     flow to a true global *)
(*****************************************************************************)
(*type index = Cil.exp*)

(*****************************************************************************)

(* type constructors moved to ptatypes.ml *)

(*****************************************************************************)

let thread_local_rhos : rhoSet ref = ref RhoSet.empty (* OBSOLETE *)

(* globals *)
let all_vinfo: vinfo list ref = ref []
let all_cinfo: cinfo list ref = ref []
let worklist_vinfo: vinfo Q.t = Q.create ()
let worklist_cinfo: cinfo Q.t = Q.create ()
let next_info: int ref = ref 1
let next_tau_id = ref 0
let undef_functions : Strset.t ref = ref Strset.empty
let def_functions : Strset.t ref = ref Strset.empty
let used_functions : Strset.t ref = ref Strset.empty
let quantified_map : (string, exp) Hashtbl.t = Hashtbl.create 1
let global_var_tau : tau list ref = ref []  (* taus of global variables *)
let global_malloc_addr_tau : tau list ref= ref [] (* taus of mallocs and
                                                     (local) &vars *)
let global_var_rhos : RhoSet.t ref = ref RhoSet.empty (* rhos of global_var_tau *)
let global_var_thetas : ThetaSet.t ref = ref ThetaSet.empty (* thetas of global_var_tau *)
let inst_cinfo_hash : cinfo CinfoInstHash.t = CinfoInstHash.create 100
let inst_vinfo_hash : vinfo VinfoInstHash.t = VinfoInstHash.create 100
let inst_edges : unit InstEdgeTbl.t = InstEdgeTbl.create 100
let sub_edges : TauPairSet.t ref = ref TauPairSet.empty
let current_uniqueness : Uniq.state ref = ref Uniq.empty_state
let eval_after_typing : (Cil.location * (unit -> unit)) Q.t = Q.create ()

let global_vars_computed = ref false
let get_global_var_rhos () =
  assert !global_vars_computed;
  !global_var_rhos
let get_global_var_thetas () =
  assert !global_vars_computed;
  !global_var_thetas
(** obsolete, used only in locksmith to reclaim some memory *)
let clear_globals() : unit = begin
  all_vinfo := [];
  all_cinfo := [];
  undef_functions := Strset.empty;
  def_functions := Strset.empty;
  used_functions := Strset.empty;
  Hashtbl.clear quantified_map;
  CinfoInstHash.clear inst_cinfo_hash;
  VinfoInstHash.clear inst_vinfo_hash;
  InstEdgeTbl.clear inst_edges;
  global_var_tau := [];
  global_malloc_addr_tau := [];
  (* Do not clear global_var_rhos *)
  sub_edges := TauPairSet.empty;
  thread_local_rhos := RhoSet.empty;
  current_uniqueness := Uniq.empty_state;
end

type label =
  | Rho of rho
  | Theta of theta
  | Effect of effect
  | Phi of phi


let add_label_to_labelsets (l: label) (rs, ts, es, ps: labelsets)
                           : labelsets =
  match l with
    Rho(r) -> (RhoSet.add r rs),ts, es, ps
  | Theta(th) -> rs, (ThetaSet.add th ts), es, ps
  | Effect(e) -> rs, ts, (EffectSet.add e es), ps
  | Phi(p) -> rs, ts, es, (PhiSet.add p ps)

let get_top_label (t: tau) (e: exp) : label =
  match t.t with
      ITVoid _
    | ITInt _
    | ITComp _
    | ITFloat
    | ITExists _ ->
        ignore(error
          "Invalid expression in the list of existentially quantified variables. \
          Expression is not a pointer: %a\n" d_exp e);
        raise TypingBug
    | ITBuiltin_va_list _
    | ITFun _
    | ITAbs _ -> assert false
    | ITPtr (_,r) ->
        Rho r
    | ITRegion th -> Theta th

(* OBSOLETE *)
let fill_thread_local (env: env) : unit = begin
  thread_local_rhos := RhoSet.empty;
  Uniq.forall_unique
    (fun v ->
      if !debug then ignore (E.log "%s is thread-local\n" v);
      let (t,r) = Strmap.find v env.var_map in
      thread_local_rhos := RhoSet.add r !thread_local_rhos;
      let rs =
        match t.t with
        | ITPtr (t,r) ->
            RhoSet.add r (
            match !t.t with
            | ITComp ci ->
                let c = U.deref ci in
                StrHT.fold
                  (fun fn (r,t) rs -> RhoSet.add r rs)
                  c.cinfo_fields
                  RhoSet.empty
            | _ -> RhoSet.empty
          )
        | ITAbs _ -> assert false
        | ITBuiltin_va_list _
        | ITFun _
        | ITVoid _
        | ITInt _
        | ITFloat
        | ITRegion _
        | ITExists _ -> RhoSet.empty
        | ITComp ci ->
            let c = U.deref ci in
            StrHT.fold
              (fun fn (r,t) rs -> RhoSet.add r rs)
              c.cinfo_fields
              RhoSet.empty
      in
      thread_local_rhos := RhoSet.union rs !thread_local_rhos;
    ) !current_uniqueness;
end

let is_existential (s: string) (al: attribute list) : bool =
  let rec f = function
    [] -> true
  | (Attr("packed",[]))::_ -> false
  | _::tl -> f tl
  in
  Hashtbl.mem quantified_map s && f al

let is_atomic (v: varinfo) : bool =
  Cil.hasAttribute "atomic" v.vattr

let uniq_deref u =
  match u with
    UnqVar -> UnqStorage
  | UnqStorage -> NotUnq
  | NotUnq -> NotUnq
let safe_ignore u =
  match u with
    UnqVar | UnqStorage -> true
  | NotUnq -> false
let uniq2str u =
  match u with
    UnqVar -> "Unique Variable"
  | UnqStorage -> "Unique Storage"
  | NotUnq -> "Shared"


let read_rho (r: rho) (p: phi) (e:effect) (u:uniq): unit =
  if !debug then ignore(E.log "%a: read access to %a\n" d_loc !currentLoc d_rho r);
  if LF.Rho.equal r unknown_rho then () else
  if !do_uniq && (safe_ignore u) then (
    (* SANITY; thread_local_rhos is obsolete *)
    if !debug && not (RhoSet.mem r !thread_local_rhos) then
      ignore(warn "NEW unique read of %a\n" d_rho r);
    if !debug then
      ignore(warn "removing thread-local read %a\n" d_rho r);
  ) else (
    (* SANITY; thread_local_rhos is obsolete *)
    if !debug && (RhoSet.mem r !thread_local_rhos) then
      ignore(warn "NO LONGER unique read of %a\n" d_rho r);
    add_to_read_effect r e;
    add_to_read_chi r !current_chi;
    Correlation.deref r p e
  )

let write_rho (r: rho) (p: phi) (e:effect) (u:uniq): unit =
  if !debug then ignore(E.log "%a: write access to %a\n" d_loc !currentLoc d_rho r);
  if LF.Rho.equal r unknown_rho then () else
  if !do_uniq && (safe_ignore u) then (
    (* SANITY; thread_local_rhos is obsolete *)
    if !debug && not (RhoSet.mem r !thread_local_rhos) then
      ignore(warn "MISSED unique write of %a\n" d_rho r);
    if !debug then
      ignore(warn "removing thread-local write %a\n" d_rho r);
  ) else (
    (* SANITY; thread_local_rhos is obsolete *)
    if !debug && (RhoSet.mem r !thread_local_rhos) then
      ignore(warn "NO LONGER unique write of %a\n" d_rho r);
    add_to_write_effect r e;
    add_to_write_chi r !current_chi;
    Correlation.deref r p e
  )

let write_theta (th: theta) (p: phi) (e:effect) (u:uniq): unit =
  if !debug then ignore(E.log "%a: write access to %a\n" d_loc !currentLoc d_theta th);
  if LF.Theta.equal th unknown_theta then () else
  if !do_uniq && (safe_ignore u) then (
    if !debug then
      ignore(warn "MISSED unique write of %a\n" d_theta th);
    if !debug then
      ignore(warn "removing thread-local write %a\n" d_theta th);
  ) else (
    if !debug then
      ignore(warn "NO LONGER unique write of %a\n" d_theta th);
    add_theta_to_write_effect th e;
    add_theta_to_write_chi th !current_chi;
    (* Correlation.deref th p e *)
  )

let defer (f: unit -> unit) : unit =
  let l = !Cil.currentLoc in
  Q.push (l,f) eval_after_typing

let done_typing () =
  let rec loop () =
    let l, f = Q.pop eval_after_typing in
    let tmp = !Cil.currentLoc in
    Cil.currentLoc := l;
    f();
    Cil.currentLoc := tmp;
    loop()
  in
  try loop() with Q.Empty -> ()



(*****************************************************************************)
(* pretty-printing *)

(* MOVED TO ptatypes.ml *)

(*****************************************************************************)

let make_tau (t: tau_t) (ts: tau_sig) : tau =
  incr next_tau_id;
  { t = t;
    ts = ts;
    tid = !next_tau_id;
    tau_free_rho = None;
  }

let integer_tau = make_tau (ITInt false) STInt
let void_tau = make_tau (ITVoid None) STVoid

let gen_if_int t =
  if t.t = ITInt true then integer_tau
  else t

(*
let is_existential (al: attributes) : bool =
  let rec f = function
    [] -> false
  | (Attr("existential",[]))::_ -> true
  | _::tl -> f tl
  in
  f al
*)

let rec typsig_to_tau_sig (t: Cil.typsig) : tau_sig =
  match t with
  | TSBase(TInt _) -> STInt
  | TSBase(TFloat _) -> STFloat
  | TSBase(TVoid _) -> STVoid
  | TSBase(TBuiltin_va_list _) -> STBuiltin_va_list
  | TSEnum _ -> STInt
  | TSPtr (ts, _) -> STPtr (typsig_to_tau_sig ts)
  | TSArray (t,l,_) -> STPtr(typsig_to_tau_sig t)
  | TSComp (s, n, al) -> STComp(s,n)
  | TSFun(rts,argts,_,_) ->
      STFun(typsig_to_tau_sig rts,
      List.map (fun ts -> (typsig_to_tau_sig ts)) argts)
  | _ -> assert false

let typ_tau_sig (t: Cil.typ) : tau_sig =
  typsig_to_tau_sig (typeSigWithAttrs (fun x -> x) t)

(*
let make_optional_rho (name: LN.label_name) (concrete: bool): rho option =
  if !do_void_conflate || !do_void_single
  then Some (make_rho name concrete)
  else None
*)

let make_optional_phi_var (name: string): phi option =
  if !do_void_conflate || !do_void_single
  then Some (make_phi name CF.PhiVar) else None

let make_vinfo (r: rho): vinfo =
  if !debug_void then ignore(E.log "make_vinfo: #%d at %a\n" !next_info d_loc !Cil.currentLoc);
  let r_opt =
    if (!do_void_conflate || !do_void_single)
    then Some r
    else None
  in
  let vd = U.uref {
    vinfo_id = !next_info;
    vinfo_rho = r_opt;
    vinfo_phi_in = make_optional_phi_var "conflated_in";
    vinfo_phi_out = make_optional_phi_var "conflated_out";
    vinfo_loc = !Cil.currentLoc;
    vinfo_known = [];
    vinfo_alloc = [];
    vinfo_inst_in_edges = InstHT.create 1;
    vinfo_inst_out_edges = InstHT.create 1;
    vinfo_types = if !do_void_conflate then ConflatedRho else ListTypes [];
  } in
  incr next_info;
  all_vinfo := vd::!all_vinfo;
  Q.push vd worklist_vinfo;
  vd

let make_cinfo (c: compinfo) (name: LN.label_name) =
  if !debug_void then ignore(E.log "make_cinfo: #%d at %a\n" !next_info d_loc !Cil.currentLoc);
  let ci = U.uref {
    compinfo = c;
    cinfo_label_name = name; (*LN.List(U.uref [name]);*)
    cinfo_fields = StrHT.create 0; (* assume many are empty *)
    cinfo_id = !next_info;
    cinfo_loc = !Cil.currentLoc;
    cinfo_known = [];
    cinfo_from_rho = None;
    cinfo_to_rho = None;
    cinfo_field_rho = if !do_field_sensitive then None else Some (make_rho name false);
    cinfo_alloc = [];
    cinfo_inst_in_edges = InstHT.create 1;
    cinfo_inst_out_edges = InstHT.create 1;
  } in
  incr next_info;
  all_cinfo := ci::!all_cinfo;
  Q.push ci worklist_cinfo;
  ci

let mkEmptyExist ts : tau =
  let e = {
    exist_tau = integer_tau;
    exist_effect = LF.make_effect "exists" true;
    exist_phi = make_phi "exists" CF.PhiPacked;
    exist_abs = [];
    exist_rhoset = RhoSet.empty;
    exist_effectset = EffectSet.empty;
    exist_initialized = false;
  } in
  make_tau (ITExists e) (STExists ts)


(*****************************************************************************)
(* unify/join *)
let join_types (s1: tau) (s2: tau) : tau =
  if s1.tid = s2.tid then s1
  else (
    ignore(E.log "%a <-> %a\n" d_tau s1 d_tau s2);
    raise TypingBug (* not implemented *)
  )

(*****************************************************************************)

(* If r is not in rs, then mark as global and additionally, if it is
   from a global variable, store in global_var_rhos *)
let mark_global_rho (r: rho) (k: global_kind) (rs: RhoSet.t)  : unit =
  if RhoSet.mem r rs then () else begin
    set_global_rho r;
    if k = KGlobal then global_var_rhos := RhoSet.add r !global_var_rhos
  end

(* If th is not in ths, then mark as global and additionally, if it is
   from a global variable, store in global_var_thetass *)
let mark_global_theta (th: theta) (k: global_kind) (ths: ThetaSet.t)  : unit =
  if ThetaSet.mem th ths then () else begin
    set_global_theta th;
    if k = KGlobal then global_var_thetas := ThetaSet.add th !global_var_thetas
  end

let known_globals = TauHT.create 0

let rec set_global_tau_r (t: tau)
                         (k: global_kind)
                         (quantified_labels: labelsets)
                         : unit =
  let qr, qt, qe, qp = quantified_labels in
  if TauHT.mem known_globals t then () else
  match t.t with
    ITVoid None -> ()
  | ITInt b -> assert (not b)
  | ITFloat -> ()
  | ITRegion(th) -> mark_global_theta th k qt
  | ITBuiltin_va_list(ur)
  | ITVoid (Some ur) -> begin
      let xd = U.deref ur in
      match xd.vinfo_types with
        ConflatedRho ->
          mark_global_rho (getSome xd.vinfo_rho) k qr;
          CF.set_global_phi (getSome xd.vinfo_phi_in) qp;
          CF.set_global_phi (getSome xd.vinfo_phi_out) qp;
      | ListTypes tl ->
          List.iter (fun (_, t) -> set_global_tau_r t k quantified_labels) tl
    end
  | ITPtr(tref, r) ->
      TauHT.add known_globals t ();
      mark_global_rho r k qr;
      set_global_tau_r !tref k quantified_labels
  | ITFun fi ->
      TauHT.add known_globals t ();
      List.iter
        (fun (_,argt) -> set_global_tau_r argt k quantified_labels)
        fi.fd_arg_tau_list;
      set_global_tau_r fi.fd_output_tau k quantified_labels;
      CF.set_global_phi fi.fd_input_phi qp;
      CF.set_global_phi fi.fd_output_phi qp;
      set_global_effect fi.fd_input_effect qe;
      set_global_effect fi.fd_output_effect qe;
      set_global_chi fi.fd_chi;
  | ITComp(ci) ->
      TauHT.add known_globals t ();
      let xd = U.deref ci in
      StrHT.iter
        (fun f (r,u) ->
          mark_global_rho r k qr;
          set_global_tau_r u k quantified_labels)
        xd.cinfo_fields;
  | ITAbs _ -> assert false
  | ITExists ei ->
      TauHT.add known_globals t ();
      let qset = (
        RhoSet.union qr ei.exist_rhoset,
        qt, (* FIXME: handle existencial types and theta labels *)
        EffectSet.union qe ei.exist_effectset,
        qp) in
      set_global_tau_r ei.exist_tau k qset
(*end*)

let empty_labelsets : labelsets =
  (RhoSet.empty, ThetaSet.empty, EffectSet.empty, PhiSet.empty)

(* Stores t in either global_var_tau or global_malloc_addr_tau,
   depending on value of k *)
let mark_global_tau (t: tau) (k: global_kind) : unit = begin
  match (t.t, k) with
    (ITAbs tref, _) -> () (* set_global_tau_r !tref [] *)
  | (_, KGlobal) -> global_var_tau := t::(!global_var_tau)
  | (_, KMalloc_Addr) -> global_malloc_addr_tau := t::(!global_malloc_addr_tau)
end

let set_globals () : unit = begin
  TauHT.clear known_globals;
  (** Mark self-loops on global variables, and add to global_var_rho
      as appropriate **)
  let max = List.length !global_var_tau in
  let count = ref 0 in
  if !debug then
    ignore(E.log "Setting global variables, length=%d\n" max);
   List.iter
    (fun t ->
      count := !count + 1;
      if !debug then
        ignore(E.log "setting global var %d/%d %s\n" !count max (Lprof.timestamp ()));
      set_global_tau_r t KGlobal empty_labelsets)
    !global_var_tau;
  global_var_tau := [];
  (** Mark self-loops on mallocs and &local variables **)
  let max = List.length !global_malloc_addr_tau in
  let count = ref 0 in
  if !debug then
    ignore(E.log "Setting malloc/addr variables, length=%d\n" max);
   List.iter
    (fun t ->
      count := !count + 1;
      if !debug then
        ignore(E.log "setting malloc/addr var %d/%d %s: %a\n"
                     !count max (Lprof.timestamp ()) d_short_tau t);
      set_global_tau_r t KMalloc_Addr empty_labelsets;
      if !debug then
        ignore(E.log "done setting malloc/addr var %d/%d %s: %a\n"
                     !count max (Lprof.timestamp ()) d_short_tau t)
    )
    !global_malloc_addr_tau;
  global_malloc_addr_tau := [];
  (** Cleanup **)
  global_vars_computed := true;
  TauHT.clear known_globals;
  if !debug then ignore (E.log "Done %s\n" (Lprof.timestamp ()));
  if !debug then
    begin
      RhoSet.iter
        (fun r -> ignore(E.log "global var rho: %a\n" d_rho r))
        (get_global_var_rhos ());
    end
end

(* get a type and "allocate" it.
 * creates a constant R that flows to every \rho in t, excluding \rhos under
 * pointers.
 *)
let rec allocate (t: tau) : unit = begin
  match t.t with
  | ITVoid None
  | ITRegion _ -> (* ignore(E.log "region allocation\n"); *) ()
  | ITAbs _ ->
      assert false (* these should never happen *)
  | ITExists _ ->
      ignore(error "%a: trying to allocate existential type" Cil.d_loc !Cil.currentLoc);
      raise TypingBug
  | ITBuiltin_va_list ur
  | ITVoid (Some ur) ->
      let u = (U.deref ur) in
      u.vinfo_alloc <- !Cil.currentLoc ::u.vinfo_alloc;
  | ITInt b -> assert (not b)
  | ITFloat -> ()
  | ITPtr(_, _) -> () (* pointer allocation doesn't allocate a value for it *)
  | ITFun _ -> assert false (* allocating a function?! *)
  | ITComp ci  -> begin
      let c = (U.deref ci) in
      c.cinfo_alloc <- !Cil.currentLoc::c.cinfo_alloc;
    end
end

let rec visit_concrete_tau (f: rho -> unit) (t: tau) : unit = begin
  match t.t with
  | ITBuiltin_va_list _ -> assert false (* never happens? *)
  | ITVoid None
  | ITAbs _ -> assert false (* these should never happen *)
  | ITExists _ ->
      ignore(error "dereferencing existential labels outside unpack");
      raise TypingBug
  | ITVoid (Some ur) ->
      defer
        (fun () ->
          let u = (U.deref ur) in
          match u.vinfo_types with
            ConflatedRho -> f (getSome u.vinfo_rho)
          | ListTypes tl ->
              List.iter (fun (_,t) -> visit_concrete_tau f t) tl
        )
  | ITInt b -> assert (not b)
  | ITFloat -> ()
  | ITRegion _ -> ()
  | ITPtr(_, _) -> ()
  | ITFun _ -> assert false (* allocating a function?! *)
  | ITComp ci  ->
      defer
        (fun () ->
          let c = (U.deref ci) in
          StrHT.iter
            (fun s (r,t) ->
              f r;
              visit_concrete_tau f t)
            c.cinfo_fields
        )
end

(*****************************************************************************)
(* env utils *)
let fresh_env () = {
  goto_tbl = Hashtbl.create 1;
  var_map = Strmap.empty;
  unpacked_map = Strmap.empty;
}

let env_add_var (e: env) (varname: string) (a: tau * rho) : env =
  { e with var_map = Strmap.add varname a e.var_map; }

let env_add_unpack_ei (e: env) (name: string) (ei: existinfo) (a: tau * rho) : env =
  assert (not (Strmap.mem name e.unpacked_map));
  { e with
    var_map = Strmap.add name a e.var_map;
    unpacked_map = Strmap.add name ei e.unpacked_map; }

let env_del_unpack_ei (e: env) (name: string) : env =
  assert (Strmap.mem name e.unpacked_map);
  { e with unpacked_map = Strmap.remove name e.unpacked_map; }

let global_env : env ref = ref (fresh_env())
let global_fun_envs : (fundec * env) list ref = ref []

let env_lookup (varname: string) (e: env) : (tau * rho) =
  try
    Strmap.find varname e.var_map
  with Not_found ->
    Strmap.find varname !global_env.var_map

let rec get_free_vars t free_vars known =
  if TauSet.mem t known then free_vars else
  let free_rhos = free_vars in
  match t.t with
  | ITPtr (tref,r) ->
      if RhoSet.mem r free_rhos then free_vars else
      let rs = RhoSet.add r free_rhos in
      get_free_vars !tref rs (TauSet.add t known)
  | ITInt _
  | ITVoid None
  | ITFloat
  | ITRegion _
  | ITAbs _ -> free_vars
  | ITBuiltin_va_list vi
  | ITVoid (Some vi) -> begin
      let v = U.deref vi in
      match v.vinfo_types with
        ConflatedRho ->
          RhoSet.add (getSome v.vinfo_rho) free_rhos
      | ListTypes tl ->
          List.fold_left
            (fun vars (_, t') -> get_free_vars t' vars (TauSet.add t known))
            free_vars
            tl
    end
  | ITComp ci ->
      let c = U.deref ci in
      StrHT.fold
        (fun _ (r,t') fr ->
          get_free_vars t' (RhoSet.add r fr) (TauSet.add t known))
        c.cinfo_fields
        free_vars
  | ITFun fdi ->
      List.fold_left
        (fun fv (_,t') -> get_free_vars t' fv (TauSet.add t known))
        (get_free_vars fdi.fd_output_tau free_vars (TauSet.add t known))
        fdi.fd_arg_tau_list
  | ITExists ei ->
        let r1 =
          get_free_vars ei.exist_tau (RhoSet.empty)
          (TauSet.add t known)
        in
        let r2 = RhoSet.diff r1 ei.exist_rhoset in
        (RhoSet.union r2 free_rhos)

let find_free_vars env =
  Strmap.fold
    (fun varname (t,r) fr ->
       let fr' = RhoSet.add r fr in
       get_free_vars t fr' TauSet.empty)
    env.var_map
    (RhoSet.empty)

let find_live_vars env live_vars =
  VS.fold
    (fun vi fr ->
      let t,r = env_lookup vi.vname env in
      let fr' = RhoSet.add r fr in
      if !debug then ignore(E.log "live at down: %s\n" vi.vname);
      get_free_vars t fr' TauSet.empty)
    live_vars
    (RhoSet.empty)

let isPtr t =
  match t.t with
    ITPtr(_, _) -> true
  | _ -> false

let isVoidptr t =
  match t.t with
    ITPtr(tref, _) ->
      (match (!tref).t with
        ITVoid(Some _) -> true
      | _ -> false)
  | _ -> false


(*****************************************************************************)
let is_goto_target (s: stmt) : bool =
  match s.labels with
    [] -> true
  | _ -> false

let rec unify_env (e1: env) (e2: env) : unit =
  if e1 == e2 then () else begin
    (* only join envs from the same context: *)
    assert (e1.goto_tbl == e2.goto_tbl);
    (*assert (e1.unpacked_map = e2.unpacked_map);*)
    assert (Strmap.equal
      (fun (t1,r1) (t2,r2) -> unify_types t1 t2; unify_rho r1 r2; true)
      e1.var_map e2.var_map);
  end

and env_flows (e1: env) (e2: env) : unit =
  if e1 == e2 then () else begin
    assert (e1.goto_tbl == e2.goto_tbl);
    assert (e1.unpacked_map = e2.unpacked_map);
    assert (Strmap.equal
      (fun (t1,r1) (t2,r2) -> sub_tau t1 t2; rho_flows r1 r2; true)
      e1.var_map e2.var_map);
  end

and join_gamma (env1,phi1,eff1: gamma) (env2,phi2,eff2: gamma) : gamma =
  unify_env env1 env2;
  (env1, join_phi phi1 phi2 CF.PhiVar, join_effects eff1 eff2)

(* goto utils *)
and set_goto_target (e: env) (p: phi) (eff: effect) (s: stmt) : unit =
  try
    let (e',p',eff') = Hashtbl.find e.goto_tbl s in
    unify_env e e';
    effect_flows eff' eff;
    CF.phi_flows p p';
  with Not_found -> begin
    let p' = make_phi "label" CF.PhiVar in
    let eff' = make_effect "e" false in
    Hashtbl.add e.goto_tbl s (e, p', eff');
    effect_flows eff' eff;
    CF.phi_flows p p';
  end

and reannotate (t: tau)
               (known: (tau_sig * tau) list)
               (name: LN.label_name) (* used to create readable names for labels *)
               : tau =
  let result =
  if List.mem_assoc t.ts known
  then List.assoc t.ts known
  else begin
    match t.t with
      ITVoid None -> t
      | ITRegion _ -> make_tau (ITRegion(make_theta name false)) t.ts
    | ITVoid (Some ur) -> assert false
    | ITPtr(tref, _) -> begin
        (*if List.mem_assq !tref known
        then List.assq !tref known else*)
        match (!tref).t with
          ITVoid (Some _) ->
            (* Make a new empty void* *)
            let r = make_rho name false in
            let vi = make_vinfo r in
            let t1 = make_tau (ITVoid (Some vi)) (!tref).ts in
            let rt = make_tau (ITPtr(ref t1, r)) t.ts in
            assert ((U.deref vi).vinfo_known = []);
            (U.deref vi).vinfo_known <- (t.ts, rt)::known;
            rt
        | _ ->
            let newtref = ref (make_tau (ITInt false) (!tref).ts) in
            let newptrt = make_tau (ITPtr(newtref, make_rho name false)) t.ts in
            newtref := reannotate !tref ((*(t.ts, newptrt)::*)known) (LN.Deref name);
            newptrt
      end
    | ITInt b -> assert (not b); t
    | ITFloat -> t
    | ITFun fi ->
        let inphi = make_phi "in" CF.PhiVar in
        let ineff = LF.make_effect "reannot-in" false in
        let newfi = {
          fd_arg_tau_list = [];
          fd_input_phi = inphi;
          fd_input_effect = ineff;
          fd_output_tau = make_tau (ITInt false) fi.fd_output_tau.ts;
          fd_output_phi = make_phi "out" CF.PhiVar;
          fd_output_effect = LF.make_effect "reannot-out" false;
          fd_chi = make_chi "X";
        } in
        let newt = make_tau (ITFun newfi) t.ts in
        let argtypes =
          List.map
            (fun (argname, argt) ->
              let t = reannotate argt ((*(t.ts,newt)::*)known) (LN.Field (name, argname)) in
              let r = (argname, t) in r)
            fi.fd_arg_tau_list
        in
        let newret =
          reannotate fi.fd_output_tau ((*(t.ts,newt)::*)known) (LN.Field (name,"return")) in
        newfi.fd_output_tau <- newret;
        newfi.fd_arg_tau_list <- argtypes;
        newt
    | ITComp(c1) ->
        let c2 = make_cinfo (U.deref c1).compinfo name in
        let rt = make_tau (ITComp(c2)) t.ts in
        (U.deref c2).cinfo_known <- (t.ts,rt)::known;
        rt
    | ITBuiltin_va_list _ ->
        let r = (make_rho (LN.Const "va_list") false) in
        let vi = make_vinfo r in
        make_tau (ITBuiltin_va_list vi) t.ts
    | ITAbs _ -> assert false (* never happens *)
    | ITExists ei -> begin
        let newt = mkEmptyExist ei.exist_tau.ts in
        let newtpacked =
          reannotate ei.exist_tau ((t.ts,newt)::known) name in
        fillExist newt newtpacked;
        newt
      end
  end
  in
  (*
  ignore(E.log "reannotate returns: %a : %a\n" d_sig result.ts d_tau result);
  *)
  result


(*****************************************************************************)
and annotate (t: typ)
             (known: (tau_sig * tau) list)
             (name: LN.label_name)
             : tau =
  let ts = typ_tau_sig t in
  if List.mem_assoc ts known
  then List.assoc ts known
  else
  if
    let t = Cil.unrollTypeDeep t in
    try (List.mem (typeSig t) !regiontypesigs)
    with Not_found -> false
  then (
      (* ignore(E.log "Found Region Type (%a )\n"  LN.d_label_name name); *)
      make_tau (ITRegion(make_theta name false)) STRegion
      (* void_tau *)
  )
  else
  let result = (
  match t with
    TVoid _  -> void_tau
  | TPtr(tptd,_)
  | TArray(tptd, _, _) -> begin
      let tsptr = typ_tau_sig tptd in
      if !do_compact_structs && (List.mem_assoc tsptr known) then
        let rtref = ref (List.assoc tsptr known) in
        make_tau (ITPtr(rtref, make_rho name false)) (STPtr tsptr)
      else begin
        match tptd with
          TVoid(_) ->
            let r = make_rho name false in
            let vi = make_vinfo r in
            let tt = make_tau (ITVoid (Some vi)) tsptr in
            let rt = make_tau (ITPtr(ref tt, r)) ts in
            assert ((U.deref vi).vinfo_known = []);
            (U.deref vi).vinfo_known <- (ts,rt)::known;
            rt
        | TComp(c, al) ->
            let rt1 = annotate tptd known (LN.Deref name) in
            let r =
              match rt1.t with
                ITComp(ci) when not !do_field_sensitive -> getSome (U.deref ci).cinfo_field_rho
              | _ -> make_rho name false
            in
            let rt, ts =
              if (!do_existentials && is_existential c.cname al) then (
                let t = mkEmptyExist rt1.ts in
                fillExist t rt1;
                t, (STExists tsptr)
               ) else rt1, tsptr
            in
            let pt = make_tau (ITPtr(ref rt, r)) (STPtr ts) in
            pt
        | TNamed(ti, attr) ->
            annotate (TPtr (Cil.typeAddAttributes attr ti.ttype, [])) known name
        | _ ->
            let rtptr = annotate tptd known (LN.Deref name) in
            make_tau (ITPtr(ref rtptr, make_rho name false)) ts
      end
  end
  | TInt(_,_) -> integer_tau
  | TFloat(_,_) -> make_tau ITFloat ts
  | TFun(tret, args, b, _) ->
      let fi = {
        fd_arg_tau_list = [];
        fd_input_phi = make_phi "in" CF.PhiVar;
        fd_input_effect = LF.make_effect "inputeffect" false;
        fd_output_tau = make_tau (ITInt false) (typ_tau_sig tret);
        fd_output_phi = make_phi "out" CF.PhiVar;
        fd_output_effect = LF.make_effect "outputeffect" false;
        fd_chi = make_chi "X";
      } in
      let fun_tau = make_tau (ITFun fi) ts in
      let arglist =
        match args with
          None -> [("",TVoid([]),[])]
        | Some(l) -> l
      in
      let arglist =
        if b then arglist@[("",TBuiltin_va_list([]),[])] else arglist
      in
      (*let known = (ts, fun_tau)::known in*)
      let i = ref 1 in
      let argtypes =
        List.map
          (fun (s, ta, _) ->
            let s = if s = "" then sprintf "arg%d" !i else s in
            incr i;
            let r = (s, annotate ta known (LN.Field(name, s))) in
            r
          )
          arglist
      in
      let ot = annotate tret known (LN.Field (name, "return")) in
      fi.fd_output_tau <- ot;
      fi.fd_arg_tau_list <- argtypes;
      fun_tau
  | TNamed(ti, attr) ->
      annotate ti.ttype known name
  | TComp(c, al) ->
      let cinfo = make_cinfo c name in
      let rt = make_tau (ITComp cinfo) ts in
      (U.deref cinfo).cinfo_known <- (ts,rt)::known;
      rt
  | TEnum(_,_) -> integer_tau
  | TBuiltin_va_list(_) ->
      let r = make_rho name false in
      let vi = make_vinfo r in
      let rt = make_tau (ITBuiltin_va_list vi) ts in
      assert ((U.deref vi).vinfo_known = []);
      (U.deref vi).vinfo_known <- (ts,rt)::known;
      rt
  ) in
  (*
  ignore(E.log "annotate returns: %a : %a\n" d_sig result.ts d_tau result);
  *)
  result


and inst_vinfo (vi1: vinfo) (vi2: vinfo) (i: instantiation) : unit =
  let v =
    try VinfoInstHash.find inst_vinfo_hash (vi1,i) with
    Not_found -> vi2
  in
  U.unify unify_vinfo (v,vi2);
  begin
    let v1 = U.deref vi1 in
    let v2 = U.deref vi2 in
    if !debug_void then
      ignore(E.log "inst_vinfo: #%d to #%d on %a\n"
             v1.vinfo_id v2.vinfo_id d_instantiation i);
    if !do_void_conflate || !do_void_single then begin
      inst_rho (getSome v1.vinfo_rho) (getSome v2.vinfo_rho) true i;
      inst_rho (getSome v1.vinfo_rho) (getSome v2.vinfo_rho) false i;
      CF.inst_phi (getSome v1.vinfo_phi_in) (getSome v2.vinfo_phi_in) true i;
      CF.inst_phi (getSome v1.vinfo_phi_in) (getSome v2.vinfo_phi_in) false i;
      CF.inst_phi (getSome v1.vinfo_phi_out) (getSome v2.vinfo_phi_out) true i;
      CF.inst_phi (getSome v1.vinfo_phi_out) (getSome v2.vinfo_phi_out) false i;
    end;
    try
      let v = InstHT.find v1.vinfo_inst_out_edges i in
      U.unify unify_vinfo (vi2, v)
    with Not_found -> InstHT.add v1.vinfo_inst_out_edges i vi2;
    try
      let v = InstHT.find v2.vinfo_inst_in_edges i in
      U.unify unify_vinfo (vi1, v)
    with Not_found -> InstHT.add v2.vinfo_inst_in_edges i vi1;
  end

(*****************************************************************************)
and unify_vinfo (src, tgt: voiddata*voiddata) : voiddata =
  let src, tgt =
    if (src.vinfo_id < tgt.vinfo_id) then src, tgt else tgt, src
  in
  if (src.vinfo_id = tgt.vinfo_id) then src
  else begin
  if !debug_void then
    ignore(E.log "unify_vinfo: #%d with #%d\n" src.vinfo_id tgt.vinfo_id);
    InstHT.iter
      (fun i vi ->
        try
          let v = InstHT.find tgt.vinfo_inst_in_edges i in
          U.unify unify_vinfo (vi, v)
        with Not_found -> InstHT.add tgt.vinfo_inst_in_edges i vi)
      src.vinfo_inst_in_edges;
    InstHT.iter
      (fun i vi ->
        try
          let v = InstHT.find tgt.vinfo_inst_out_edges i in
          U.unify unify_vinfo (vi, v)
        with Not_found -> InstHT.add tgt.vinfo_inst_out_edges i vi)
      src.vinfo_inst_out_edges;
    InstHT.clear src.vinfo_inst_in_edges;
    InstHT.clear src.vinfo_inst_out_edges;
    tgt.vinfo_alloc <- src.vinfo_alloc @ tgt.vinfo_alloc;

    if !do_void_conflate || !do_void_single then begin
      unify_rho (getSome src.vinfo_rho) (getSome tgt.vinfo_rho);
      CF.unify_phi (getSome src.vinfo_phi_in) (getSome tgt.vinfo_phi_in);
      CF.unify_phi (getSome src.vinfo_phi_out) (getSome tgt.vinfo_phi_out);
    end;
    begin
      tgt.vinfo_known <- src.vinfo_known @ tgt.vinfo_known;
      src.vinfo_known <- [];
      List.iter
        (fun (ts, t) ->
          let tgtlist = (get_vinfo_types tgt.vinfo_types) in
          (if !do_void_single then assert (List.length tgtlist <= 1));
          try
            let t2 = List.assoc ts tgtlist in
            unify_types t t2
          with Not_found -> begin
            add_type_to_vinfo t tgt
          end
        )
        (get_vinfo_types src.vinfo_types);
    end;
    tgt
  end

and inst_cinfo (ci1: cinfo) (ci2: cinfo) (i: instantiation) : unit =
  begin
    let c1 = U.deref ci1 in
    let c2 = U.deref ci2 in
    if !debug_void then
      ignore(E.log "inst_cinfo: #%d to #%d on %a\n"
             c1.cinfo_id c2.cinfo_id d_instantiation i);
    try
      let c = InstHT.find c1.cinfo_inst_out_edges i in
      U.unify unify_cinfo (ci2, c)
    with Not_found -> InstHT.add c1.cinfo_inst_out_edges i ci2;
    try
      let c = InstHT.find c2.cinfo_inst_in_edges i in
      U.unify unify_cinfo (ci1, c)
    with Not_found -> InstHT.add c2.cinfo_inst_in_edges i ci1;
  end

and unify_cinfo (x, y: compdata*compdata) : compdata =
  if !debug_void then
    ignore(E.log "unify_cinfo: #%d with #%d\n" x.cinfo_id y.cinfo_id);
  assert (x.compinfo == y.compinfo);
  if (x.cinfo_id = y.cinfo_id) then x
  else begin
    let src,tgt = if (x.cinfo_id < y.cinfo_id) then y,x else x,y in
    (*let x,y = (0,0) in (* don't use x, y below here! *)*)
    InstHT.iter
      (fun i ci ->
        try
          let c = InstHT.find tgt.cinfo_inst_in_edges i in
          U.unify unify_cinfo (ci, c)
        with Not_found -> InstHT.add tgt.cinfo_inst_in_edges i ci)
      src.cinfo_inst_in_edges;
    InstHT.iter
      (fun i ci ->
        try
          let c = InstHT.find tgt.cinfo_inst_out_edges i in
          U.unify unify_cinfo (ci, c)
        with Not_found -> InstHT.add tgt.cinfo_inst_out_edges i ci)
      src.cinfo_inst_out_edges;
    InstHT.clear src.cinfo_inst_in_edges;
    InstHT.clear src.cinfo_inst_out_edges;
    tgt.cinfo_known <- src.cinfo_known @ tgt.cinfo_known;
    tgt.cinfo_alloc <- src.cinfo_alloc @ tgt.cinfo_alloc;
    if !do_field_sensitive then () else
      unify_rho (getSome tgt.cinfo_field_rho) (getSome src.cinfo_field_rho);
    src.cinfo_known <- [];
    let max =
      if Labelname.compare tgt.cinfo_label_name src.cinfo_label_name < 0
      then tgt.cinfo_label_name
      else src.cinfo_label_name
    in
    tgt.cinfo_label_name <- max;
    (*
    (match tgt.cinfo_label_name, src.cinfo_label_name with
      LN.List lu1, LN.List lu2 -> U.unify (fun (l1,l2) -> l1 @ l2) (lu1, lu2)
    | _ -> assert false);
    *)
    begin
      match src.cinfo_to_rho, tgt.cinfo_to_rho with
        None, None -> ()
      | Some r, None
      | None, Some r -> tgt.cinfo_to_rho <- Some r
      | Some r1, Some r2 -> unify_rho r1 r2
    end;
    begin
      match src.cinfo_from_rho, tgt.cinfo_from_rho with
        None, None -> ()
      | Some r, None
      | None, Some r -> tgt.cinfo_from_rho <- Some r
      | Some r1, Some r2 -> unify_rho r1 r2
    end;
    StrHT.iter
      (fun f (r,t) ->
        try
          let (r2,t2) = StrHT.find tgt.cinfo_fields f in
          unify_types t t2;
          unify_rho r r2;
        with Not_found -> begin
          StrHT.add tgt.cinfo_fields f (r,t);
        end
      ) src.cinfo_fields;
      StrHT.clear src.cinfo_fields;
    tgt
  end

and sub_tau (src: tau) (tgt: tau) : unit =
  let rec sub_tau1 (src: tau)
                   (tgt: tau)
                   (locopt: (rho * rho) option)
                   : unit =
    if src == tgt then ()
    else if TauPairSet.mem (src,tgt) !sub_edges then ()
    else begin
      sub_edges := TauPairSet.add (src,tgt) !sub_edges;
      match (src.t,tgt.t) with
        ITVoid None, ITVoid None -> ()

      | ITBuiltin_va_list(fromvi), ITBuiltin_va_list(tovi)
      | ITVoid(Some fromvi), ITVoid(Some tovi) ->
          U.unify unify_vinfo (fromvi, tovi);
      | ITFloat, ITFloat -> ()
      | ITFloat, ITInt _ -> ()
      | ITInt _, ITFloat -> ()
      | ITInt _, ITInt b -> assert (not b)
      | ITInt b, ITPtr _ ->
          if (not b) then ignore(warn "assigning number to pointer")
      | ITPtr(tref1, r1), ITPtr(tref2, r2) -> begin
          rho_flows r1 r2;
          match (!tref1).t, (!tref2).t with
            ITVoid(Some fromvi), ITVoid(Some tovi) ->
              U.unify unify_vinfo (fromvi, tovi);
          | ITVoid(Some vi), nonvoid ->
              add_type_to_vinfo tgt (U.deref vi);
          | nonvoid, ITVoid(Some vi) ->
              add_type_to_vinfo src (U.deref vi);
          | ITInt _, ITInt _ -> ()
          | ITInt _, nonint
          | nonint, ITInt _ ->
              ignore(warn "%a: Probably using char* where void* is best \
                            used:\n %a flows to %a.  Losing precision.\n"
                Cil.d_loc !Cil.currentLoc d_tau src d_tau tgt);
              sub_tau1 !tref1 !tref2 (Some(r1, r2));
              sub_tau1 !tref2 !tref1 (Some(r2, r1));
          | _ ->
              sub_tau1 !tref1 !tref2 (Some(r1, r2));
              sub_tau1 !tref2 !tref1 (Some(r2, r1));
        end

      | ITBuiltin_va_list _, _
      | _, ITBuiltin_va_list _ ->
          ignore(E.log "%a -> %a\n" d_tau src d_tau tgt);
          assert false

      | ITPtr(tref1, r1), _ -> begin
          match locopt with
            None ->
              ignore(warn "assigning pointer to non-pointer: %a\n ->\n %a"
                     d_tau src d_tau tgt)
          | Some(srcr,tgtr) ->
              rho_flows r1 tgtr;
              ignore(warn "assigning incompatible pointers, conflating flow");
              let phi_in = make_phi "conflated_in" CF.PhiVar in
              let phi_out = make_phi "conflated_out" CF.PhiVar in
              conflate_to !tref1 tgtr phi_in phi_out TauSet.empty;
        end
      | _, ITPtr(tref2, r2) -> begin
          match locopt with
            None ->
              ignore(warn "assigning non-pointer to pointer: %a\n ->\n %a"
                     d_tau src d_tau tgt
              )
          | Some(srcr,tgtr) ->
              rho_flows srcr r2;
              ignore(warn "assigning incompatible pointers, conflating flow");
              let phi_in = make_phi "conflated_in" CF.PhiVar in
              let phi_out = make_phi "conflated_out" CF.PhiVar in
              conflate_from srcr !tref2 phi_in phi_out TauSet.empty;
        end
      | ITFun fi1, ITFun fi2 ->
          CF.phi_flows fi2.fd_input_phi fi1.fd_input_phi;
          effect_flows fi1.fd_input_effect fi2.fd_input_effect;
          CF.phi_flows fi1.fd_output_phi fi2.fd_output_phi;
          effect_flows fi2.fd_output_effect fi1.fd_output_effect;
          sub_tau1 fi1.fd_output_tau fi2.fd_output_tau None;
          (*chi_flows fi2.fd_chi fi1.fd_chi;*)
          chi_flows fi1.fd_chi fi2.fd_chi;
          let f = (fun (_,s) -> s) in
          let flist1 = List.map f fi1.fd_arg_tau_list in
          let flist2 = List.map f fi2.fd_arg_tau_list in
          sub_tau_list flist2 flist1 None;
      | ITComp(ci1), ITComp(ci2) ->
          if (U.deref ci1).compinfo == (U.deref ci2).compinfo
          then U.unify unify_cinfo (ci1, ci2)
          else (
            match locopt with
              None ->
                ignore(error "assigning concrete structs of different type:\n\
                              %a\n %a\n" d_tau src d_tau tgt)
            | Some(r1,r2) -> begin
                let phi_in = make_phi "conflated_in" CF.PhiVar in
                let phi_out = make_phi "conflated_out" CF.PhiVar in
                conflate src r2 phi_in phi_out TauSet.empty;
              end
          )

      (* from all fields of a union to a type *)
      | ITAbs _, _
      | _, ITAbs _ -> assert false
      | ITExists ei1, ITExists ei2 -> begin
          sub_tau1 ei1.exist_tau ei2.exist_tau locopt;
          effect_flows ei2.exist_effect ei1.exist_effect;
          CF.phi_flows ei1.exist_phi ei2.exist_phi;
        end
      | ITRegion th1, ITRegion th2 -> unify_theta th1 th2;
      | _, ITRegion _
      | ITRegion _, _ -> ignore(warn "subtyping a region to a non region type");
      | _ ->
        ignore(warn "subtyping incompatible types:\n%a\n%a"
               d_tau src d_tau tgt)
    end

  and sub_tau_list (srclist: tau list)
                   (tgtlist: tau list)
                   (locopt: (rho * rho) option)
                   : unit =
    match (srclist, tgtlist) with
      [], [] -> ()
    | [],{t = ITVoid None}::_
    | ({t = ITVoid None}::_,[]) -> ()
    | {t = ITBuiltin_va_list vi1}::[], {t = ITBuiltin_va_list vi2}::[] ->
        sub_tau1 (List.hd srclist) (List.hd tgtlist) locopt;
    | {t = ITBuiltin_va_list vi1}::tl1, {t = ITBuiltin_va_list vi2}::tl2 ->
        sub_tau1 (List.hd srclist) (List.hd tgtlist) locopt;
        sub_tau_list tl1 tl2 locopt;
    | sl, {t = ITBuiltin_va_list _}::s::[]
    | {t = ITBuiltin_va_list _}::s::[], sl ->
        ignore(error "another argument after \"...\": %a\n" d_tau s);
        (*List.iter (fun s -> ignore(E.log "%a\n" d_tau s)) sl;*)
        assert false (* nothing after "..." *)
    | {t = ITBuiltin_va_list vi}::[], sl ->
        List.iter
          (fun s ->
            (* we have to call "gen_if_int" to convert int(0) to int, in
               case we pass in a 0 in va_list.  Otherwise we'd get the 0 type
               propagate through "...".  This never happens in void, because
               0 can only be assigned via a pointer to void*, and we ignore
               0-to-pointer assignments. *)
            let s' = (gen_if_int s) in
            (* we now reannotate the type and add the fresh one in the vinfo,
               to avoid losing all of the subtyping precision and have
               everything unified.  Unifying is ok in the void* case, but
               not in the va_list case, because va_list doesn't have to
               be under a pointer *)
            let s'' = reannotate s' [] (LN.Const "va_list") in
            add_type_to_vinfo s'' (U.deref vi);
            sub_tau s'' s')
          sl;
    | sl, {t = ITBuiltin_va_list vi}::[] ->
        (* exactly the same, except the direction of the subtyping *)
        List.iter
          (fun s ->
            let s' = (gen_if_int s) in
            let s'' = reannotate s' [] (LN.Const "va_list") in
            add_type_to_vinfo s'' (U.deref vi);
            sub_tau s' s'')
          sl;
    | [],t::_ | (t::_,[]) ->
        ignore(warn "number of args non-equal. first extra arg: %a" d_tau t)
    | s1::tl1, s2::tl2 -> begin
        sub_tau1 s1 s2 locopt;
        sub_tau_list tl1 tl2 locopt;
      end
  in begin
    sub_tau1 src tgt None;
  end

and conflate_from (src: rho)
                  (t: tau)
                  (phi_in: phi)
                  (phi_out: phi)
                  (known: TauSet.t) : unit =
  if TauSet.mem t known then () else
  match t.t with
    (* before was doing nothing; may have been unsafe, since
       conflate_from is sometimes called directly *)
  | ITBuiltin_va_list vi
  | ITVoid (Some vi) ->
      (* Contents of void*'s does itself get conflated *)
      let v = U.deref vi in
      if !do_void_conflate || !do_void_single then begin
        unify_rho src (getSome v.vinfo_rho);
        CF.unify_phi phi_in (getSome v.vinfo_phi_in);
        CF.unify_phi phi_out (getSome v.vinfo_phi_out);
      end;
  | ITVoid None -> ()
  | ITInt _ -> () (*assert (not b)*)
  | ITFloat -> ()
  | ITPtr(tref,rho) ->
      conflate !tref src phi_in phi_out (TauSet.add t known);
      rho_flows src rho
  | ITFun fi ->
      ignore(warn "function pointer in downcasting.\n");
      List.iter
        (fun (_,argt) ->
          conflate_to argt src phi_in phi_out (TauSet.add t known))
        fi.fd_arg_tau_list;
      if !do_void_conflate || !do_void_single then begin
        CF.phi_flows fi.fd_input_phi phi_in;
        CF.phi_flows phi_out fi.fd_output_phi;
      end;
      conflate_from src fi.fd_output_tau phi_in phi_out (TauSet.add t known)
  | ITComp(ci) ->
      let c = U.deref ci in
      let r =
        match c.cinfo_from_rho with
          None ->
            defer (fun () ->
              StrHT.iter
                (fun _ (r',t') ->
                  rho_flows src r';
                  conflate_from src t' phi_in phi_out (TauSet.add t known))
                c.cinfo_fields
            );
            src
        | Some r -> r
      in
      (U.deref ci).cinfo_from_rho <- Some r;
      rho_flows src r
  | ITRegion(th) -> ignore(warn "region emerges at downcasting: %a\n" d_theta th)
  | ITAbs _ -> assert false
  | ITExists ei ->
      ignore(warn "existential emerges at downcasting");
      conflate_from src ei.exist_tau phi_in phi_out (TauSet.add t known)

and conflate t r phi_in phi_out known =
  conflate_to t r phi_in phi_out known;
  conflate_from r t phi_in phi_out known

and conflate_to (t: tau)
                (tgt: rho)
                (phi_in: phi)
                (phi_out: phi)
                (known: TauSet.t) : unit =
  if TauSet.mem t known then () else
  let known = TauSet.add t known in
  match t.t with
  | ITBuiltin_va_list vi
  | ITVoid (Some vi) ->
      (* Contents of void*'s does itself get conflated *)
      let v = U.deref vi in
      if !do_void_conflate || !do_void_single then begin
        unify_rho tgt (getSome v.vinfo_rho);
        CF.unify_phi phi_in (getSome v.vinfo_phi_in);
        CF.unify_phi phi_out (getSome v.vinfo_phi_out);
      end
  | ITVoid None
  | ITInt _
  | ITFloat -> ()
  | ITPtr(tref,rho) ->
      conflate !tref tgt phi_in phi_out known;
      rho_flows rho tgt
  | ITFun fi ->
      ignore(warn "function pointer conflated");
      List.iter
        (fun (_,argt) -> conflate_from tgt argt phi_in phi_out known)
        fi.fd_arg_tau_list;
      if !do_void_conflate || !do_void_single then begin
        CF.phi_flows phi_in fi.fd_input_phi;
        CF.phi_flows fi.fd_output_phi phi_out;
      end;
      conflate_to fi.fd_output_tau tgt phi_in phi_out known
  | ITComp(ci) ->
      let c = U.deref ci in
      let r =
        match c.cinfo_to_rho with
          None ->
            defer (fun () ->
              StrHT.iter
                (fun _ (r',t') ->
                  rho_flows r' tgt;
                  conflate_to t' tgt phi_in phi_out (TauSet.add t known))
                c.cinfo_fields);
            tgt
        | Some r -> r
      in
      c.cinfo_to_rho <- Some r;
      rho_flows r tgt
  | ITRegion(th) -> ignore(warn "region is conflated! (%a)" d_theta th)
  | ITAbs _ -> assert false
  | ITExists ei ->
      ignore(warn "Existential labels conflated!!!");
      conflate_to ei.exist_tau tgt phi_in phi_out (TauSet.add t known)

and unify_types (s1: tau) (s2: tau) : unit =
  if s1 == s2 then ()
  else
    match s1.t, s2.t with
      ITAbs si1, ITAbs si2 ->
        sub_tau !si1 !si2;
        sub_tau !si2 !si1;
    | _ ->
        sub_tau s1 s2;
        sub_tau s2 s1;

(* this is used only with --single-void
 *)
and conflate_vinfo (v: voiddata) : unit = begin
  match v.vinfo_types with
    ConflatedRho -> ()
  | ListTypes [(_,t)] -> begin
      let r = getSome v.vinfo_rho in
      let phi_in = getSome v.vinfo_phi_in in
      let phi_out = getSome v.vinfo_phi_out in
      conflate t r phi_in phi_out TauSet.empty;
      v.vinfo_types <- ConflatedRho
    end
  | ListTypes [] ->
      v.vinfo_types <- ConflatedRho
  | _ -> assert false
end

and add_type_to_vinfo (s: tau) (v: voiddata) : unit = begin
  if !debug_void then
    ignore(E.log "add_type_to_vinfo: add %a to #%d\n" d_tau s v.vinfo_id);
  match v.vinfo_types with
    ConflatedRho ->
      if !debug_void then ignore(E.log "conflated\n");
      let r = getSome v.vinfo_rho in
      let phi_in = getSome v.vinfo_phi_in in
      let phi_out = getSome v.vinfo_phi_out in
      conflate s r phi_in phi_out TauSet.empty;
  | ListTypes tl -> begin
      try
        let t = List.assoc s.ts tl in
        unify_types s t
      with Not_found -> begin
        if !debug_void then ignore(E.log "conflating list\n");
        if !do_void_single && (tl <> []) then begin
          conflate_vinfo v;
          add_type_to_vinfo s v;
        end else begin
          v.vinfo_types <- ListTypes ((s.ts,s)::tl)
        end
      end
    end
end

and get_cinfo_field (fi: fieldinfo) (ci: cinfo)
                    (uniq: bool) (* if this is true, any allocated fields will
                                  * add their & rhos in thread_local_rhos *)
                    (known: (tau_sig * tau) list)
                    (name: LN.label_name)
                    : (rho * tau) =
  let compdata = U.deref ci in
    assert (compdata.compinfo == fi.fcomp);
  try StrHT.find compdata.cinfo_fields fi.fname
  with Not_found -> begin
    let storeloc = !Cil.currentLoc in
    Cil.currentLoc := compdata.cinfo_loc;
    let r =
      if !do_field_sensitive then
        make_rho (LN.Field (name, fi.fname)) false
      else
        getSome compdata.cinfo_field_rho
    in
    (* BEGIN OBSOLETE *)
    if uniq then thread_local_rhos := RhoSet.add r !thread_local_rhos;
    (* END OBSOLETE *)
    let s =
      annotate fi.ftype (if !do_compact_structs then known else [])
        (LN.Field (name, fi.fname)) in
    (*XXX these phis are not completely sound *)
    if !debug_void then
      ignore(E.log "get_cinfo_field: add %s:%a to #%d\n"
             fi.fname d_tau s compdata.cinfo_id);
    let phi_in = make_phi "conflated_in" CF.PhiVar in
    let phi_out = make_phi "conflated_out" CF.PhiVar in
    begin
      match compdata.cinfo_to_rho with
        None -> ()
      | Some r' ->
          conflate_to s r' phi_in phi_out TauSet.empty;
          rho_flows r r'
    end;
    begin
      match compdata.cinfo_from_rho with
        None -> ()
      | Some r' ->
          conflate_from r' s phi_in phi_out TauSet.empty;
          rho_flows r' r
    end;
    StrHT.add compdata.cinfo_fields fi.fname (r,s);
    Cil.currentLoc := storeloc;
    (r,s)
  end


and instantiate (tabs: tau)
                (tinst: tau)
                (polarity: bool)
                (i: instantiation)
                : unit =
  let rec instantiate1 (tabs: tau)
                       (tinst: tau)
                       (polarity: bool)
                       (i: instantiation)
                       : unit =
    if InstEdgeTbl.mem inst_edges (tabs,tinst,i,polarity) then ()
    else begin
      InstEdgeTbl.add inst_edges (tabs,tinst,i,polarity) ();
      match (tabs.t,tinst.t) with
      | ITVoid None, ITVoid None -> ()
      | ITFloat, ITFloat -> ()
      | ITPtr(tref1, r1), ITPtr(tref2, r2) -> begin
          match (!tref1).t, (!tref2).t with
            ITVoid(Some vi1), ITVoid(Some vi2) ->
              inst_rho r1 r2 polarity i;
              inst_vinfo vi1 vi2 i
          | _ ->
            inst_rho r1 r2 polarity i;
            (* both plus and minus for pointed type: *)
            instantiate1 !tref1 !tref2 true i;
            instantiate1 !tref1 !tref2 false i;
        end
      | ITRegion th1, ITRegion th2 -> inst_theta th1 th2 i;
      | ITInt b1, ITInt b2 -> assert (not b1); assert (not b2); ()
      | ITInt _, ITFloat -> ()
      | ITFloat, ITInt false -> ()

      | ITVoid (Some _), _
      | _, ITVoid (Some _) -> assert false

      | ITFun fi1, ITFun fi2 ->
          begin
            try
              List.iter2
                (fun (n1,t1) (n2, t2) ->
                  (*assert (n1 = n2);*)
                  instantiate1 t1 t2 (not polarity) i)
                fi1.fd_arg_tau_list
                fi2.fd_arg_tau_list;
            with Invalid_argument _ ->
              ignore(warn "instantiating functions with different \
                           numbers of arguments:\n %a\n %a"
                           d_tau tabs d_tau tinst);
          end;
          let negative = not polarity in
          CF.inst_phi fi1.fd_input_phi fi2.fd_input_phi negative i;
          CF.inst_phi fi1.fd_output_phi fi2.fd_output_phi polarity i;
          inst_effect fi1.fd_input_effect fi2.fd_input_effect polarity i;
          inst_effect fi1.fd_output_effect fi2.fd_output_effect negative i;
          instantiate1 fi1.fd_output_tau fi2.fd_output_tau polarity i;
          (*inst_chi fi1.fd_chi fi2.fd_chi (not polarity) i;*)
          inst_chi fi1.fd_chi fi2.fd_chi polarity i;
      | ITComp(ci1), ITComp(ci2) ->
          let c =
            try CinfoInstHash.find inst_cinfo_hash (ci1,i) with
            Not_found -> ci2
          in
          U.unify unify_cinfo (c, ci2);
          inst_cinfo ci1 c i;
      | ITBuiltin_va_list vi1, ITBuiltin_va_list vi2 ->
          inst_vinfo vi1 vi2 i
      | ITAbs _, _
      | _, ITAbs _ -> assert false
      | ITExists ei1, ITExists ei2 ->
          instantiate1 ei1.exist_tau ei2.exist_tau polarity i;
          inst_effect ei1.exist_effect ei2.exist_effect (not polarity) i;
          inst_effect ei1.exist_effect ei1.exist_effect true i;
          inst_effect ei1.exist_effect ei1.exist_effect false i;
          CF.inst_phi ei1.exist_phi ei2.exist_phi polarity i;
          CF.inst_phi ei1.exist_phi ei1.exist_phi true i;
          CF.inst_phi ei1.exist_phi ei1.exist_phi false i;
          RhoSet.iter
            (fun x -> inst_rho x x true i; inst_rho x x false i)
            ei1.exist_rhoset;
          EffectSet.iter
            (fun x -> inst_effect x x true i; inst_effect x x false i)
            ei1.exist_effectset;
      | (_,_) ->
          ignore(error "instantiating types with different structures:\n\
                        %a \n %a\n" d_tau tabs d_tau tinst);
          raise TypingBug
    end
  in begin
    instantiate1 tabs tinst polarity i;
  end
(****************************************************************************)
(* helper function that decides the return type of binary operators *)
and type_binop (b : binop)
               (s1: tau)
               (s2: tau)
               : tau =
  match b with
    PlusPI
  | PlusA
  | MinusPI
  | IndexPI ->
      if isPtr s1 then s1 else s2
  | MinusPP
  | MinusA | Mult | Div | Mod
  | Shiftlt | Shiftrt | BAnd | BXor | BOr -> integer_tau
  | Lt | Gt | Le | Ge | LAnd | LOr -> integer_tau
  | Eq -> integer_tau
  | Ne -> integer_tau

(*****************************************************************************)
(* JUDGEMENTS *)
(*****************************************************************************)

(* judgements have
 * inputs:
 *  - the structure that is being typed.
 *  - input environment
 *  - input phi
 *  - input effect
 * outputs:
 *  - tau (returned type)
 *  - output rho (for stuff whose address might be taken)
 *  - output environment
 *  - output phi
 *  - output effect
 * when a type-rule doesn't need/return all of them, the corresponding function
 * might not have them in its signature.
 *)

and type_const (c: constant)
               (input_env: env)
               (input_phi: phi)
               (input_effect: effect)
               : ((tau * uniq) * env * phi * effect) =
  match c with
  | CChr(i) ->
      let rt = make_tau (ITInt(i = '\000')) STInt in
      ((rt,NotUnq), input_env, input_phi, input_effect)
  | CInt64(i,_,_) ->
      let rt = make_tau (ITInt(i = Int64.zero)) STInt in
      ((rt,NotUnq), input_env, input_phi, input_effect)
  | CStr(_) | CWStr(_) ->
      let r = make_rho (LN.Const "const-string") true in
      let rt = make_tau (ITPtr(ref integer_tau, r)) (STPtr STInt) in
      ((rt,NotUnq), input_env, input_phi, input_effect)
  | CReal(_,_,_) ->
      let t = make_tau ITFloat STFloat in
      ((t,NotUnq), input_env, input_phi, input_effect)
  | _ -> assert false

and type_var (var: varinfo)     (* name of variable typed *)
             (input_env: env)   (* input environment *)
             (input_phi: phi)   (* pipe the flow-sensitive environment through
                                 * --not used, is there just in case *)
             (input_effect: effect)
             : ((tau * rho * uniq) * env * phi * effect) =
  let (s, r) =
    try
      env_lookup var.vname input_env
    with Not_found ->
      ignore(error "undefined variable %s" var.vname);
      raise TypingBug
  in
  let (out_rho, out_env) = (r, input_env) in
  let unq =
    if !do_uniq && Uniq.is_unique var.vname !current_uniqueness then
      UnqVar
    else NotUnq
  in
  if !debug then ignore(E.log "var |%s| is %s\n" var.vname (uniq2str unq));
  let out_tau = (
    match s.t with
    | ITAbs tref -> (
        match (!tref).t with
          ITFun fi ->
            used_functions := Strset.add var.vname !used_functions;
            if !do_context_sensitive then (
              let i = make_instantiation false var.vname in
              let tinst = reannotate !tref [] (LN.Const var.vname) in
              if !debug then
                ignore(E.log "instantiating %s on %a\n   %a\n   %a\n"
                             var.vname
                             d_instantiation i
                             d_tau !tref
                             d_tau tinst);
              instantiate !tref tinst true i;
              tinst
            ) else (
              !tref
            )
        | _ -> assert false;
      )
    | _ -> s
  ) in
  ((out_tau, out_rho, unq), out_env, input_phi, input_effect)

and type_lval (h,o : lhost * offset) (* lvals are host * offset pairs *)
             (input_env: env)
             (input_phi: phi)
             (input_eff: effect)
             : ((tau * rho * uniq) * env * phi * effect) =
  match (h,o) with
    (Mem (CastE(t,_)), Field(_, _)) ->
      let temp = !do_ignore_casts in
      do_ignore_casts := false;
      let (host_tru, host_env, host_phi, host_effect) =
        type_host h input_env input_phi input_eff in
      let name = LN.Const(string_of_doc (d_lval () (h, NoOffset))) in
      let result = type_offset o host_tru name
                   host_env host_phi host_effect in
      do_ignore_casts := temp;
      result
   | _ ->
      let (host_tru, host_env, host_phi, host_effect) =
      type_host h input_env input_phi input_eff in
      let name = LN.Const (string_of_doc (d_lval () (h, NoOffset))) in
      type_offset o host_tru name host_env host_phi host_effect

and type_offset
               (o: offset)
               (host_tru : tau * rho * uniq)
               (name: LN.label_name)
               (input_env: env)
               (input_phi: phi)
               (input_effect: effect)
               : ((tau * rho * uniq) * env * phi * effect) =
  let (host_type, host_rho, host_uniq) = host_tru in
  match o with
    NoOffset -> (host_tru, input_env, input_phi, input_effect)
  | Field(fi, o1) -> begin
      read_rho host_rho input_phi input_effect host_uniq;
      match host_type.t with
        ITComp(c) -> begin
            (* BEGIN OBSOLETE *)
            let uniq = RhoSet.mem host_rho !thread_local_rhos in
            (* END OBSOLETE *)
            let r,s =
              get_cinfo_field fi c uniq [(host_type.ts, host_type)] name
            in
            (* field offsets inherit the host's uniqueness *)
            let fname = (LN.Field (name, fi.fname)) in
            type_offset o1 (s,r,host_uniq) fname
                        input_env input_phi input_effect
        end
      | _ ->
        ignore(warn "trying to access field %s of non-struct type. \
                    This is usually caused by casting pointers to \
                    or from int or int*:\n %a\n" fi.fname d_tau host_type);
        ((integer_tau, unknown_rho,host_uniq), input_env, input_phi, input_effect)
    end
  | Index(e, o1) ->
      read_rho host_rho input_phi input_effect host_uniq;
      let ((exp_type,exp_uniq), exp_env, exp_phi, exp_effect) =
        type_exp e input_env input_phi input_effect in
      match exp_type.t with
        ITInt _ | ITFloat -> begin
          match host_type.t with
            | ITPtr(s2, r2) -> begin
              read_rho r2 exp_phi exp_effect exp_uniq;
              type_offset o1 (!s2,r2,NotUnq) (LN.Deref name)
                          exp_env exp_phi exp_effect
            end
          | _ ->
              ignore(error "trying to index non-array type.");
              raise TypingBug
        end
      | _ ->
          ignore(error "trying to index array using non-int type: %a\n"
                 d_tau exp_type);
          raise TypingBug

and type_host (h: lhost)
             (input_env: env)
             (input_phi: phi)
             (input_effect: effect)
             : ((tau * rho * uniq) * env * phi * effect) =
  match h with
    Var(v) ->
      type_var v input_env input_phi input_effect
  | Mem(e) ->
      let ((exp_type,exp_uniq), exp_env, exp_phi, exp_effect) =
        type_exp e input_env input_phi input_effect
      in begin
        match exp_type.t with
          ITPtr(s1, r) ->
            (* MWH: this is redundant; either the e under the
               Mem() is an lval and thus we got in in type_exp
               or else it's a constant and we don't care. *)
            read_rho r exp_phi exp_effect exp_uniq;
            ((!s1, r, uniq_deref exp_uniq), exp_env, exp_phi, exp_effect)
        | _ ->
          ignore(E.log "%a: dereferencing non-pointer: %a : %a\n"
                 Cil.d_loc !Cil.currentLoc d_exp e d_tau exp_type);
          ((integer_tau, unknown_rho, exp_uniq), exp_env, exp_phi, exp_effect)
      end

and type_ref (lv: lval)
            (input_env: env)
            (input_phi: phi)
            (input_effect: effect)
            : ((tau * uniq) * env * phi * effect) =
  let ((s, r, _), lval_env, lval_phi, lval_effect) =
    type_lval lv input_env input_phi input_effect
  in
  let rt = make_tau (ITPtr(ref s, r)) (STPtr s.ts) in
  ((rt,NotUnq), lval_env, lval_phi, lval_effect)

and type_exp (e: exp)
             (input_env: env)
             (input_phi: phi)
             (input_effect: effect)
             : ((tau * uniq) * env * phi * effect) =
  match e with
    Const(c) ->
      type_const c input_env input_phi input_effect
  | Lval(lv) -> begin
      let ((s, r, u), lval_env, lval_phi, lval_effect) =
        type_lval lv input_env input_phi input_effect (*false*)
      in
      if !debug then ignore(E.log "lval |%a| is %s\n" d_lval lv (uniq2str u));
      read_rho r lval_phi input_effect u;
      ((s,u), lval_env, lval_phi, lval_effect)
    end
  | SizeOf(_) | SizeOfE(_) | SizeOfStr(_)
  | AlignOf(_) | AlignOfE(_) ->
      ((integer_tau,NotUnq), input_env, input_phi, input_effect)
  | UnOp(_,e1,_) ->
      let (_, exp_env, exp_phi, exp_effect) =
        type_exp e1 input_env input_phi input_effect
      in ((integer_tau,NotUnq), exp_env, exp_phi, exp_effect)
  | BinOp(bop, e1, e2, _) ->
      let ((s1,u1), exp_env1, exp_phi1, exp_effect1) =
        type_exp e1 input_env input_phi input_effect in
      let ((s2,u2), exp_env2, exp_phi2, exp_effect2) =
        type_exp e2 exp_env1 exp_phi1 exp_effect1 in
      let u = match bop with
        PlusA | PlusPI | IndexPI | MinusA | MinusPI | MinusPP -> u1
      | _ -> NotUnq in
      (((type_binop bop s1 s2),u), exp_env2, exp_phi2, exp_effect2)
  | CastE(t,e1) ->
      let ((s1,u1), exp_env, exp_phi, exp_eff) =
        type_exp e1 input_env input_phi input_effect in
      if not (isVoidptr s1) && !do_ignore_casts then
        ((s1,u1), exp_env, exp_phi, exp_eff)
      else
        let s2 = annotate t [] (LN.Const "cast") in
        sub_tau s1 s2;
        ((s2,u1), exp_env, exp_phi, exp_eff)
  | AddrOf(lv) ->
      type_ref lv input_env input_phi input_effect
  | StartOf(lv) ->
      let ((s,_,u), lval_env, lval_phi, lval_effect) =
        type_lval lv input_env input_phi input_effect (*false*) in
      begin
        match s.t with
        ITPtr(_, _) -> ((s,u), lval_env, lval_phi, lval_effect)
        | _ ->
          ignore(error "StartOf operator does not return pointer");
          raise TypingBug
      end
  |    Question(e1, e2, e3, _) ->
          let ((s1,u1), exp_env1, exp_phi1, exp_effect1) =
        type_exp e1 input_env input_phi input_effect in
      let ((s2,u2), exp_env2, exp_phi2, exp_effect2) =
        type_exp e2 exp_env1 exp_phi1 exp_effect1 in
      let ((s3,u3), exp_env3, exp_phi3, exp_effect3) =
        type_exp e2 exp_env2 exp_phi2 exp_effect2 in
      ((s3, u3), exp_env2, exp_phi3, exp_effect3) (* TODO: not sure if everything works fine with this *)

(*****************************************************************************)
and compute_quantified_labels (c: cinfo) (el: exp list) : labelsets =
  let cc = (U.deref c).compinfo in
  let t = make_tau (ITComp(c)) (STComp(cc.cstruct,cc.cname)) in
  let env = env_add_var (fresh_env ()) cc.cname (t, unknown_rho) in
  List.fold_left
    (fun ls e ->
      let (tt,_),_,_,_ = type_exp e env CF.empty_phi empty_effect in
      let l = get_top_label tt e in
      add_label_to_labelsets l ls)
    empty_labelsets
    el

and fillExist (te:tau) (t: tau) : unit = begin
  let e =
    match te.t with
    | ITExists(e) -> e
    | _ -> assert false (*impossible*)
  in
  assert(not e.exist_initialized);
  let c =
    match t.t with
      ITComp c -> c
    | _ ->
      ignore(error "Locksmith BUG: \
                    fillExist should only be called for structs");
      raise TypingBug
  in
  let el = Hashtbl.find_all quantified_map ((U.deref c).compinfo.cname) in
  e.exist_tau <- t;
  e.exist_abs <- el;
  let rs, ths, es, ps = compute_quantified_labels c el in
  e.exist_rhoset <- rs;
  e.exist_effectset <- es;
  e.exist_initialized <- true;
end

and type_exp_list (el: exp list)
                (input_env: env)
                (input_phi: phi)
                (input_effect: effect)
                : ((tau * uniq) list * env * phi * effect) =
  match el with
    [] -> ([], input_env, input_phi, input_effect)
  | h::tl ->
      let (s, exp_env, exp_phi, exp_effect) =
        type_exp h input_env input_phi input_effect in
      let (ls, out_env, out_phi, out_effect) =
        type_exp_list tl exp_env exp_phi exp_effect in
      (s::ls, out_env, out_phi, out_effect)

and type_init (i: init)
              (input_env: env)
              (name: LN.label_name)
              : tau * env =
  match i with
  | SingleInit(e) ->
      let ((s1,_), exp_env, exp_phi, exp_effect) =
        type_exp e input_env CF.empty_phi empty_effect in
      if exp_phi != CF.empty_phi || exp_effect != empty_effect
      then (
        ignore(error "Locksmith BUG: found initializer with effect and/or phi");
        raise TypingBug
      ) else (s1, exp_env)
  | CompoundInit(t, l) ->
      let s = annotate t [] name in
      allocate s;
      let out_env =
        match s.t with
        | ITComp(ci) -> List.fold_left (type_offcinit ci name) input_env l
        | ITPtr(tref, _) -> List.fold_left (type_offainit !tref name) input_env l
        | _ ->
            ignore(error "compound initializer with non-compound type");
            raise TypingBug
      in (s, out_env)

and type_offainit (array_type: tau)
                  (name: LN.label_name)
                  (input_env: env)
                  (o, i: offset * init)
                  : env =
  let (si, out_env) = type_init i input_env (LN.Deref name) in
  match o with
    Index(_, NoOffset) ->
      sub_tau si array_type;
      out_env
  | _ ->
      ignore(error "initializer of the wrong type");
      raise TypingBug

and type_offcinit (c: cinfo)
                  (name: LN.label_name)
                  (input_env: env)
                  (o, i: offset * init)
                  : env =
  match o with
  | Field(f, NoOffset) ->
      let (si, out_env) = type_init i input_env (LN.Field(name,f.fname)) in
      let _,s = get_cinfo_field f c false [] name in
      sub_tau si s;
      out_env
  | _ ->
      ignore(error "initializer of the wrong type");
      raise TypingBug

(*****************************************************************************)

(*let handle_tpc_task il args loc: unit = begin*)
(*  match il with *)
(*    Call(_, Lval((Var(vi), _)), _, loc') ->*)
(*      ignore(List.map (fun arg -> match arg with*)
(*        ACons(varname, ACons(arg_typ, [])::ACons(varsize, [])::[]) -> *)
(*          (* give all the arguments to Dtdepa*)*)
(*          Sdam.addArg (varname, arg_typ, !Sdam.currTask_descr));*)
(*        | ACons(varname, ACons(arg_typ, [])::ACons(varsize, [])::ACons(elsize, [])::ACons(elnum, [])::[]) ->*)
(*          (* give all the arguments to Dtdepa don't care for strided  *)*)
(*           Sdam.addArg (varname, arg_typ, !current_function);*)
(*        | _ -> ignore(E.warn "SDAM:%a:Task annotation error!\n" d_loc loc);*)
(*        ) args);*)
(*      ignore(Sdam.addTask vi.vname !current_function loc');*)
(*      | _ -> ignore(E.warn "SDAM:%a:Cannot use task annotation here!\n" d_loc loc);*)
(*end*)

(*    parses recursively the arguments in an argument data annotation team and returns
        a list with the corresponding argument descriptors
*)
let css_arg_process ((iotyp: string), (loc: location), (args_l: Sdam.arg_descr list), (loop_d: loop_descr option)) arg =
  if not (is_dataflow_tag iotyp) then (
    match arg with
    (* handle safe... *)
    | ACons(varname, []) ->
      (* we need to find the argument in the current arg_l to mark it as safe *)
      List.iter
        (fun a ->
          if(varname == a.argname) then (
            a.safe <- true;
            a.force_safe <- true;
          )
        ) args_l;
      (iotyp, loc, args_l, loop_d)
    | _ ->
      ignore(E.error "SDAM:%a:Syntax error in #pragma css task %s(...)" d_loc loc iotyp);
      raise Ignore_pragma
  ) else (
    match arg with
    (* handle strided... *)
    | AIndex(AIndex(ACons(varname, []), varsize), ABinOp( BOr, _, _)) ->
      let attrparam_to_exp' =
        attrparam_to_exp !program_file loc ~currFunction:!current_function
      in
      let tmp_size = attrparam_to_exp' varsize in
      let var_i = find_scoped_var loc !current_function !program_file varname in
      let array_d = LP.getArrayDescr var_i loc !currSid in
      let arg_d =
        Sdam.make_arg_descr varname loc iotyp true var_i tmp_size loop_d array_d
      in
      (iotyp, loc, arg_d::args_l, loop_d)
    (* Brand new stride syntax... *)
    | AIndex(AIndex(ACons(varname, []), ABinOp( BOr, _, bs_c)), _) ->
      let attrparam_to_exp' =
        attrparam_to_exp !program_file loc ~currFunction:!current_function
      in
      let tmp_size = attrparam_to_exp' bs_c in
      let var_i = find_scoped_var loc !current_function !program_file varname in
      let array_d = LP.getArrayDescr var_i loc !currSid in
      let arg_d =
        Sdam.make_arg_descr varname loc iotyp true var_i tmp_size loop_d array_d
      in
      (iotyp, loc, arg_d::args_l, loop_d)
    (* variable with its size *)
    | AIndex(ACons(varname, []), varsize) ->
      let var_i = find_scoped_var loc !current_function !program_file varname in
      let tmp_size =
        let size = SizeOf( get_basetype var_i.vtype varname ) in
        let n = attrparam_to_exp !program_file loc ~currFunction:!current_function varsize in
          (* argument size = n * sizeof(type) *)
        BinOp(Mult, n, size, intType)
      in
      let array_d = LP.getArrayDescr var_i loc !currSid in
      let arg_d =
        Sdam.make_arg_descr varname loc iotyp false var_i tmp_size loop_d array_d
      in
      (iotyp, loc, arg_d::args_l, loop_d)
    (* support optional sizes example int_a would have size of sizeof(int_a) *)
    | ACons(varname, []) ->
      let var_i = find_scoped_var loc !current_function !program_file varname in
      let tmp_size = SizeOf( get_basetype var_i.vtype varname ) in
      let array_d = LP.getArrayDescr var_i loc !currSid in
      let arg_d =
        Sdam.make_arg_descr varname loc iotyp false var_i tmp_size loop_d array_d
      in
      (iotyp, loc, arg_d::args_l, loop_d)
    | _ -> (
      ignore(E.error "SDAM:%a:Syntax error in #pragma css task %s(...)" d_loc loc iotyp);
      raise Ignore_pragma
    )
  )

(* iterates an expression list and returns a list with the names of the actual
    arguments.  Currently only variables can be arguments, other expressions are
    result in error *)
let process_actuals (el: exp list) loc : string list =
    let rec proc_exp = (fun args_l e ->
        (if(!debug_SDAM) then ignore(E.log "parsing actual %a\n" d_exp e););
        (* ignore(E.log "parsing actual %a\n" d_exp e);
        ignore(E.log "actuals number:%d\n" (List.length args_l)); *)
        match e with
            Lval(Var(vi), _) -> vi.vname::args_l
        |    CastE(_, e2) -> proc_exp args_l e2
        | StartOf(Var(vi), _) -> (
            (* ignore(E.error "SDAM:%a:Cannot evaluate startof expr\n" d_loc loc); *)
            vi.vname::args_l
        )
        | AddrOf(Var(vi), _) -> (
            ignore(E.error "SDAM:%a:Cannot evaluate addrof expr\n" d_loc loc);
            vi.vname::args_l
        )
        | _ -> (
            ignore(E.error "SDAM:%a:Cannot evaluate expression as pragma task argument\n" d_loc loc);
            raise Ignore_pragma
        )
    ) in
    List.rev (List.fold_left proc_exp [] el)

(*    parses recursively the arguments of the pragma task, and returns
        a list with the corresponding argument descriptors
*)
let css_task_process (loc, loop_d) args =
    let rec proc_attrs args_l args =
        match args with
            [] -> args_l
        |    (AStr("highpriority"))::rest -> proc_attrs args_l rest
        | AStr("region")::region_attr
        | AStr("notransfer")::region_attr -> (
            match region_attr with
      | AStr(region_name)::region_type -> (
                match region_type with
        | ACons(iotyp, args')::rest ->
            let var_i = find_scoped_var loc !current_function !program_file region_name in
            let tmp_size = SizeOfE (Lval (var var_i)) in
                let array_d = LP.getArrayDescr var_i loc !currSid in
            let arg_d =
            Sdam.make_arg_descr region_name loc iotyp false var_i tmp_size loop_d array_d
          in
                    (* ignore(E.log "adding argument %s\n" arg_d.argname); *)
                    proc_attrs (arg_d::args_l) rest
                | _ -> (
                    ignore(E.error "SDAM:%a:Region syntax error in #pragma css task\n" d_loc loc);
                    raise Ignore_pragma
                )
            )
      (* support region in/out/inout(a,b,c) *)
      | ACons(iotyp, args)::rest ->
        let process_reg = function
          | ACons(varname, []) ->
              let var_i = find_scoped_var loc !current_function !program_file varname in
            let tmp_size = SizeOfE (Lval (var var_i)) in
            let array_d = LP.getArrayDescr var_i loc !currSid in
            Sdam.make_arg_descr varname loc iotyp false var_i tmp_size loop_d array_d
          | _ ->
            ignore(E.error "SDAM:%a:Region syntax error in #pragma css task\n" d_loc loc);
            raise Ignore_pragma
        in
        proc_attrs ((List.map process_reg args)@args_l) rest
            |    _ -> (
                ignore(E.error "SDAM:%a:Region syntax error in #pragma css task\n" d_loc loc);
                raise Ignore_pragma
            )
        )
        | ACons(iotyp, args')::rest -> (
      let (_, _, args_l', _) =
        List.fold_left css_arg_process (iotyp, loc, args_l, loop_d) args'
      in
            proc_attrs args_l' rest
        )
        | _::rest -> (
            ignore(E.error "SDAM:%a:Syntax error in #pragma css task\n" d_loc loc);
            raise Ignore_pragma
        )
    in proc_attrs [] args

(** parses instruction task to collect task and arguments
        @param il the instruction coupled with current task pragma
        @param args the actual arguments attribute from the pragma annotation
        @param loc the location of the pragma
        @param loop_d the current, innermost loop_d, if any
        @raise Ingore_pragma when pragma fails to be parsed
        @return a new task descriptor
*)
let handle_css_task il env args loc loop_d : task_descr =  begin
    match il with
      Call(_, Lval((Var(vi), _)), acts, loc) -> (
(*        let (_, args_l, _) = List.fold_left css_task_process (loc, [], loop_d) args in *)
            let args_l = css_task_process (loc, loop_d) args in
(*         LP.process_call_actuals acts loc !currSid task_d loop_d; *)
            try (
                let actuals = process_actuals acts loc in
            let (tau, _) = env_lookup vi.vname env in
                let fd = (
                    match tau.t with
                            ITAbs tau_ref -> (
                                match !tau_ref.t with
                                ITFun fd -> fd
                            | _ ->     (
                                    ignore(E.error "SDAM:%a:Pragma task has not been declared as a C function, ignoring pragma\n" d_loc loc);
                                    raise Ignore_pragma
                                )
                            )
                        | _ -> (
                                ignore(E.error "SDAM:%a:Pragma task has not been declared as a C function, ignoring pragma\n" d_loc loc);
                                raise Ignore_pragma
                            )
                ) in
                let task_d = Sdam.make_task_descr vi.vname loc !current_function fd env args_l actuals in
                Sdam.addTask task_d;
                task_d
            ) with Not_found -> (
                    ignore(E.error "SDAM:%a:Pragma task has not been declared as a C function, ignoring pragma\n" d_loc loc);
                    raise Ignore_pragma
                )
        )
  | _ -> (
            ignore(E.error "SDAM:%a:Pragma task should be coupled with a function call, ignoring pragma" d_loc loc);
            raise Ignore_pragma
        )
end

(*****************************************************************************)

let handle_exit (_: exp list)
                (_: lval option)
                (args: (tau*uniq) list)
                (input_env: env)
                (input_phi: phi)
                (input_effect: effect)
                : gamma =
  (input_env, CF.empty_phi, LF.make_effect "exit-effect" false)

let empty_handler _ _ _ e p ef = (e, p, ef)

let handle_fork (_: exp list)
                (_: lval option)
                (args: (tau*uniq) list)
                (input_env: env)
                (input_phi: phi)
                (input_effect: effect)
                : gamma =
  match args with
    ({t =ITPtr(_,r1)},u1)::(t2,u2)::({t= ITPtr(tref, b)},u3)::(arg,_)::[] ->
      let fi =
        match (!tref).t with
          ITFun fi -> fi
        | _ -> assert false;
      in
      let _ =
        match fi.fd_arg_tau_list with
          (_,a)::_ -> sub_tau arg a
        | [] -> ()
      in
      read_rho r1 input_phi input_effect u1;
      read_rho b input_phi input_effect u3;
      begin
        match t2.t with
          ITPtr(_,r2) ->
            read_rho r2 input_phi input_effect u2
        | ITInt true -> ()
        | _ ->
            ignore(error "bad fork");
            raise TypingBug
      end;
      (* both continuation and forked thread effects flow to input effect *)
      let eff_after = LF.make_effect "fork-effect" true in
      let eff_forked = LF.make_effect "forked-effect" true in
      effect_flows fi.fd_input_effect eff_forked;
      effect_flows eff_after input_effect;
      chi_flows fi.fd_chi !current_chi;

      (* starting phi of the new thread and phi immediately after the fork
         both follow after phi_before *)
      let phi_before = input_phi in
      let phi_forked = make_phi "FORK" CF.PhiForked in
      let phi_after = make_phi "afterfork" CF.PhiVar in
      if !do_starting_forks then
        CF.starting_phis := phi_forked::!CF.starting_phis;
      CF.phi_flows phi_forked fi.fd_input_phi;
      CF.phi_flows phi_before phi_after;
      CF.phi_flows phi_before phi_forked;

      add_fork input_effect eff_after eff_forked
        phi_before phi_after phi_forked
        (function () -> find_free_vars input_env);

      (* log *)
      if !debug then
        ignore(E.log "fork: %a\n -> %a\n -> %a\n"
          CF.d_phi phi_before
          CF.d_phi phi_after
          CF.d_phi phi_forked);
      (input_env, phi_after, eff_after)
  | _ ->
      ignore(error "calling fork with bad arguments");
      raise TypingBug

let handle_memset (_: exp list)
                  (_: lval option)
                  (args: (tau*uniq) list)
                  (input_env: env)
                  (input_phi: phi)
                  (input_effect: effect)
                  : gamma =
  match args with
    ({t=(ITPtr(s1,r1))},u1)::_ ->
      write_rho r1 input_phi input_effect u1;
      visit_concrete_tau (fun r -> write_rho r input_phi input_effect u1) !s1;
      (input_env, input_phi, input_effect)
  | _ ->
      ignore(error "calling memset with bad arguments");
      raise TypingBug

let handle_memcpy (_: exp list)
                  (_: lval option)
                  (args: (tau*uniq) list)
                  (input_env: env)
                  (input_phi: phi)
                  (input_effect: effect)
                  : gamma =
  match args with
    ({t=(ITPtr(s1,r1))},u1)::({t=(ITPtr(s2,r2))},u2)::_ ->
      sub_tau !s2 !s1;
      write_rho r1 input_phi input_effect u1;
      read_rho r2 input_phi input_effect u2;
      visit_concrete_tau (fun r -> write_rho r input_phi input_effect u1) !s1;
      visit_concrete_tau (fun r -> read_rho r input_phi input_effect u2) !s2;
      (input_env, input_phi, input_effect)
  | _ ->
      ignore(error "calling memcpy with bad arguments");
      raise TypingBug

let handle_va_start (el: exp list)
                    (_: lval option)
                    (args: (tau*uniq) list)
                    (input_env: env)
                    (input_phi: phi)
                    (input_effect: effect)
                    : gamma =
(* polyvios: we don't really need a va_start handler, i think.
 * it does not dereference its second argument either.  It just
 * gets its address and computes the address of the argumetn that
 * follows it.
 * So, i'm commenting out the body of the handler, it's a no-op from now on
 *)
 (input_env, input_phi, input_effect)
(*match el with
    _::Lval(Var(vi), NoOffset)::_
  | _::CastE(_,Lval(Var(vi), NoOffset))::_ ->
      let ((t,r,u),env,phi,eff) =
        type_var vi input_env input_phi input_effect in
      read_rho r phi eff u;
      (env, phi, eff), S.empty_epsilon
  | _::e::_ ->
      let d = dprintf "%a: calling va_start with bad arguments: %a" Cil.d_loc !Cil.currentLoc Cil.d_exp e in
      raise (TypingBug (string_of_doc d))
  | _ ->
      let d = dprintf "%a: calling va_start with bad arguments" Cil.d_loc !Cil.currentLoc in
      raise (TypingBug (string_of_doc d))
*)

let handle_va_arg (_: exp list)
                  (lvo: lval option)
                  (args: (tau*uniq) list)
                  (input_env: env)
                  (input_phi: phi)
                  (input_effect: effect)
                  : gamma =
  match lvo with
    None ->
        ignore(error "calling va_arg without assigning the result");
        raise TypingBug
  | Some(lv) -> begin
      let ((lv_type, _, _), lv_env, lv_phi, lv_effect) =
        type_lval lv input_env input_phi input_effect in
      match args with
        ({t = ITBuiltin_va_list vi},_)::_ ->
          add_type_to_vinfo lv_type (U.deref vi);
          (lv_env, lv_phi, lv_effect)
      | _ ->
          ignore(error "calling va_arg without a va_list arg");
          raise TypingBug
    end

let handle_strcmp (_: exp list)
                  (_: lval option)
                  (args: (tau*uniq) list)
                  (input_env: env)
                  (input_phi: phi)
                  (input_effect: effect)
                  : gamma =
  match args with
    ({t=(ITPtr(s1,r1))},u1)::({t=(ITPtr(s2,r2))},u2)::_ ->
      read_rho r1 input_phi input_effect u1;
      read_rho r2 input_phi input_effect u2;
      (input_env, input_phi, input_effect)
  | _ ->
      ignore(warn "Calling strcmp with bad arguments.");
      (input_env, input_phi, input_effect)

let handle_start_unpack (el: exp list)
                        (lv: lval option)
                        (args: (tau*uniq) list)
                        (input_env: env)
                        (input_phi: phi)
                        (input_effect: effect)
                        : gamma =
  if !do_existentials then begin
    let err () =
      ignore(error "start_unpack() argument should be a local variable, \
                    which should be a pointer to an existential struct");
      raise TypingBug
    in
    match el with
      [Lval(Var(vi), NoOffset)] -> begin
        let (tabsptr, var_rho) = env_lookup vi.vname input_env in
        if var_rho <> unknown_rho then
          ignore(warn "unpacking variable whose address is taken: unsound!!\n");
        match tabsptr.t with
          ITPtr(tref, ptr_rho) ->
            let ei =
              match !tref.t with
                ITExists ei -> ei
              | _ -> err ()
            in
            begin
              match ei.exist_tau.t with
                ITComp ci -> ()
              | _ -> err ()
            end;
            let newt =
              make_tau (ITPtr(ref ei.exist_tau, ptr_rho))
                       (STPtr ei.exist_tau.ts)
            in
            CF.phi_flows ei.exist_phi input_phi;
            let eff = LF.make_effect "unpack" true in
            effect_flows eff ei.exist_effect;
            effect_flows eff input_effect;
            let e2 = env_add_unpack_ei input_env vi.vname ei (newt, var_rho) in
            (e2, input_phi, eff)
        | _ -> err ()
      end
    | _ -> err ()
  end else (input_env, input_phi, input_effect)

let handle_end_unpack (el: exp list)
                      (lv: lval option)
                      (args: (tau*uniq) list)
                      (input_env: env)
                      (input_phi: phi)
                      (input_effect: effect)
                      : gamma =
  if !do_existentials then begin
    let err () =
      ignore(error "end_unpack() argument should be a local variable, \
                    pointer to an unpacked existential struct");
      raise TypingBug
    in
    match el with
      [Lval(Var(vi), NoOffset)] -> begin
        let (tabsptr, var_rho) = env_lookup vi.vname input_env in
        if var_rho <> unknown_rho then
          ignore(warn "end_unpack() given variable whose address is taken.\n\
                       This can make the analysis unsound.\n");
        match tabsptr.t with
          ITPtr(tref, ptr_rho) ->
            begin
              match !tref.t with
                ITComp ci -> ()
              | _ -> err ()
            end;
            let ei = Strmap.find vi.vname input_env.unpacked_map in
            let ets = (STExists(ei.exist_tau.ts)) in
            let et = make_tau (ITExists ei) ets in
            let newt = make_tau (ITPtr(ref et, var_rho)) (STPtr ets) in
            let e1 = env_add_var input_env vi.vname (newt, var_rho) in
            let e2 = env_del_unpack_ei e1 vi.vname in
            (e2, input_phi, input_effect)
        | _ -> err ()
      end
    | _ -> err ()
  end else (input_env, input_phi, input_effect)

(* pack(x) happens here.  x has to be a variable (not path),
 * pointer to a struct.  An ITExists type is created and returned.
 *)
let handle_pack (el: exp list) (* arguments to pack, should be singleton list *)
                (lvo: lval option)
                (tl: (tau*uniq) list)
                (input_env: env)
                (input_phi: phi)
                (input_effect: effect)
                : gamma =
  if !do_existentials then begin
    let err () =
      ignore(error "pack() should have a single argument, pointer to a struct\n\
                    and return into a pointer to an existential");
      raise TypingBug
    in
    match tl with
      ({t=ITPtr(tref, ptr_rho)},_)::[] -> begin
        begin
        match !tref.t with
          ITComp ci -> ()
          | _ ->
              ignore(E.log "pack argument is: %a\n" d_tau !tref);
              err ()
        end;
        let i = make_instantiation true "pack" in
        let tinst = !tref in
        let name =
          match el with
            [Lval(Var v, NoOffset)] -> LN.Const v.vname
          | _ -> err ()
        in
        let tabs = reannotate tinst [] name in
        instantiate tabs tinst false i;
        let et = mkEmptyExist tabs.ts in
        fillExist et tabs;
        let pack_phi = make_phi "pack" CF.PhiPacked  in
        CF.phi_flows input_phi pack_phi;
        (match et.t with
          ITExists ei ->
            CF.inst_phi ei.exist_phi pack_phi false i;
            inst_effect ei.exist_effect input_effect true i;
        | _ -> assert false);
        let texistsptr = make_tau (ITPtr(ref et, ptr_rho)) (STPtr et.ts) in
        match lvo with
          Some lv ->
            let ((lv_type, _, _), lv_env, lv_phi, lv_effect) =
              type_lval lv input_env input_phi input_effect in
            sub_tau texistsptr lv_type;
            (lv_env, lv_phi, lv_effect)
        | None -> err ()
      end
    | _ -> err ()
  end else begin
    match tl with
      [(t,_)] -> begin
        let (t',_,_), env, phi, eff =
          match lvo with
            None ->
              ignore(warn "The result of pack() is not assigned to anything.");
              (integer_tau, unknown_rho, NotUnq),
                input_env, input_phi, input_effect
          | Some lv ->
              type_lval lv input_env input_phi input_effect
        in
        sub_tau t t';
        (env, phi, eff)
      end
    | _ ->
        ignore(error "pack called with wrong number of arguments");
        raise TypingBug
  end

let handle_alloc (_: exp list)
                 (lvo: lval option)
                 (_: (tau*uniq) list)
                 (input_env: env)
                 (input_phi: phi)
                 (input_effect: effect)
                 : gamma =
  let alloc_rho = make_rho (LN.Const "alloc") true in
  match lvo with
    None ->
      ignore(warn "Calling alloc without assigning the result.");
      (input_env, input_phi, input_effect)
  | Some(lv) ->
      let ((lv_type, _, _), lv_env, lv_phi, lv_effect) =
        type_lval lv input_env input_phi input_effect in
      (match lv_type.t with
        ITPtr(var_tref, var_rho) ->
          allocate !var_tref;
          mark_global_tau !var_tref KMalloc_Addr;
          rho_flows alloc_rho var_rho;
          mark_global_rho alloc_rho KMalloc_Addr RhoSet.empty;
      | _ ->
          ignore (warn "alloc result assigned to a non-pointer: %a"
                 d_tau lv_type);
      );
      (lv_env, lv_phi, lv_effect)

let handle_free (el: exp list)
                (_: lval option)
                (args: (tau*uniq) list)
                (input_env: env)
                (input_phi: phi)
                (input_effect: effect)
                : gamma =
  match args with
    ({t=(ITPtr(s,r))},u)::_ ->
      write_rho r input_phi input_effect u;
      visit_concrete_tau (fun r -> write_rho r input_phi input_effect u) !s;
      (input_env, input_phi, input_effect)
  | _ ->
    ignore(error "calling free with bad arguments: %a" (d_list "\n" d_exp) el);
    raise TypingBug

let handle_new_region (_: exp list)
                             (lvo: lval option)
                            (_: (tau*uniq) list)
                            (input_env: env)
                            (input_phi: phi)
                            (input_effect: effect)
                            : gamma =
  let new_region_theta = make_theta (LN.Const "new_region") true in (*make it concrete*)
  match lvo with
    None ->
      ignore(warn "Creating new region without assigning the result.");
      (input_env, input_phi, input_effect)
  | Some(lv) ->
      let ((lv_type, _, _), lv_env, lv_phi, lv_effect) =
        type_lval lv input_env input_phi input_effect in
      (match lv_type.t with
        ITRegion(var_theta) ->
          unify_theta new_region_theta var_theta;
          (*FIXME: i don't know what this line does... *)
          (* mark_global_rho alloc_rho KMalloc_Addr RhoSet.empty; *)
      | _ ->
          ignore (warn "new region assigned to a non-region var: %a"
                 d_tau lv_type);
      );
      (lv_env, lv_phi, lv_effect)

let handle_deleteregion (el: exp list)
                                (_: lval option)
                                (args: (tau*uniq) list)
                                (input_env: env)
                                (input_phi: phi)
                                (input_effect: effect)
                                : gamma =
  match args with
    ({t=(ITRegion th)},u)::_ ->
      write_theta th input_phi input_effect u;
      (input_env, input_phi, input_effect)
  | _ ->
    ignore(error "calling delete region with bad arguments: %a" (d_list "\n" d_exp) el);
    raise TypingBug

let handle_new_subregion (_: exp list)
                                  (lvo: lval option)
                                 (args: (tau*uniq) list)
                                 (input_env: env)
                                 (input_phi: phi)
                                 (input_effect: effect)
                                 : gamma =
  (* let new_subregion_theta = make_theta (LN.Const "new_subregion") true in (*make it concrete*) *)
  let (parent_region_t, _) = List.hd args in
  match lvo with
    None ->
      ignore(warn "Creating new subregion without assigning the result.");
      (input_env, input_phi, input_effect)
  | Some(lv) ->
      let ((lv_type, _, _), lv_env, lv_phi, lv_effect) =
        type_lval lv input_env input_phi input_effect in
      (match lv_type.t, parent_region_t.t with
        ITRegion(var_theta1), ITRegion(var_theta2) ->
          ignore(E.log "assigning new subregion to variable\n");
          theta_flows var_theta2 var_theta1;
          (*FIXME: i don't know what this line does... *)
          (* mark_global_rho alloc_rho KMalloc_Addr RhoSet.empty; *)
      | _, _ ->
          ignore (warn "new subregion assigned to a non-region var: %a"
                 d_tau lv_type);
      );
      (lv_env, lv_phi, lv_effect)

let handle_ralloc (_: exp list)
                              (lvo: lval option)
                             (args: (tau*uniq) list)
                             (input_env: env)
                             (input_phi: phi)
                             (input_effect: effect)
                             : gamma =
  let (region_t, _) = List.hd args in
  let alloc_rho = make_rho (LN.Const "alloc") true in
  match lvo with
    None ->
      ignore(warn "Allocating region space without assigning the result.");
      (input_env, input_phi, input_effect)
  | Some(lv) ->
      let ((lv_type, _, _), lv_env, lv_phi, lv_effect) =
        type_lval lv input_env input_phi input_effect in
      (match region_t.t,lv_type.t with
        ITRegion(th), ITPtr(var_tref, var_rho) ->
          allocate !var_tref;
          mark_global_tau !var_tref KMalloc_Addr;
          rho_flows alloc_rho var_rho;
          mark_global_rho alloc_rho KMalloc_Addr RhoSet.empty;
          (*FIXME: here add an edge between theta and rho *)
          rho_flows2theta var_rho th;
      | _ ->
          ignore (warn "bddt_ralloc result assigned to a non-pointer: %a"
                 d_tau lv_type);
      );
      (lv_env, lv_phi, lv_effect)

let get_special (k: Conf.handler) : special_function_t =
  match k with
    Conf.Alloc -> handle_alloc
  | Conf.Free -> handle_free
  | Conf.Newlock -> empty_handler
  | Conf.Destroy -> empty_handler
  | Conf.Acquire -> empty_handler
  | Conf.Trylock -> empty_handler
  | Conf.Release -> empty_handler
  | Conf.Fork -> handle_fork
  | Conf.Exit -> handle_exit
  | Conf.Memcpy -> handle_memcpy
  | Conf.Strcmp -> handle_strcmp
  | Conf.Va_start -> handle_va_start
  | Conf.Va_arg -> handle_va_arg
  | Conf.Memset -> handle_memset
  | Conf.Pack -> handle_pack
  | Conf.Start_unpack -> handle_start_unpack
  | Conf.End_unpack -> handle_end_unpack
  | Conf.New_Region -> handle_new_region
  | Conf.Delete_Region -> handle_deleteregion
     | Conf.New_Subregion -> handle_new_subregion
    | Conf.Ralloc -> handle_ralloc

(*****************************************************************************)
let rec type_instr (input_env, input_phi, input_effect) instr : gamma =
  if !debug then ignore(E.log "typing instruction %a\n" d_instr instr);
  if !do_uniq then
    begin
      if !debug then
        ignore(E.log "(before) uniqueness: %a\n"
               Uniq.d_state !current_uniqueness);
      fill_thread_local input_env; (* OBSOLETE *)
      (* FIX: this is conservative; we are using the uniqueness from
         AFTER the instruction while typing the instruction itself.
         The fix would be to rename this function type_instr' and then
         create a separate type_instr that first calls this function
         and then updates the uniqueness. *)
      current_uniqueness := Uniq.through_instr !current_uniqueness instr;
      if !debug then
        ignore(E.log "(after) uniqueness: %a\n"
               Uniq.d_state !current_uniqueness);
    end;
  match instr with
    Set(lv, e, loc) ->
      let e =
        if !do_ignore_casts then e else
        match e with
        | CastE(_,e) -> e
        | _ -> e
      in
      currentLoc := loc;
      let ((s1,_), exp_env, exp_phi, exp_effect) =
        type_exp e input_env input_phi input_effect in
      let ((s2, r, u), lval_env, lval_phi, lval_effect) =
        type_lval lv exp_env exp_phi exp_effect in
      (* ignore(E.log "lval |%a| is %s\n" d_lval lv (uniq2str u)); *)
      sub_tau s1 s2;
      write_rho r lval_phi lval_effect u;
      (lval_env, lval_phi, lval_effect)
  | Call(lvo, e, el, loc) -> begin
          (* ignore(E.log "* with enviroment:\n%a\n" d_env input_env); *)
      currentLoc := loc;
      let (args, arg_env, arg_phi, arg_effect) =
        type_exp_list el input_env input_phi input_effect in
      try (* if it's in special_functions, use the corresponding handler *)
        (match e with
          Lval(Var(f),_) ->
            let special =
              get_special (Strmap.find f.vname !Conf.special_functions) in
            special el lvo args arg_env arg_phi arg_effect
        | _ ->
          raise Not_found
        )
      with Not_found -> begin
      (* not special *)
      let ((e_type,_), e_env, e_phi, e_effect) =
        type_exp e arg_env arg_phi arg_effect
      in
      match e_type.t with
        ITFun fi -> begin
          let callargs = List.map (fun (s,_) -> ("", s)) args in
          let callrett, call_env, call_inphi, call_ineff =
            match lvo with
              None -> fi.fd_output_tau, e_env, e_phi, e_effect
            | Some(lv) ->
                let ((sret, r, u), lv_env, lv_phi, lv_effect) =
                  type_lval lv e_env e_phi e_effect (*false*) in
                write_rho r lv_phi lv_effect u;
                sret, lv_env, lv_phi, lv_effect
          in
          let callargts = List.map (fun (s,_) -> s.ts) args in
          let callts = STFun(callrett.ts,callargts) in
          let ret_phi =
            make_phi "ret" CF.PhiVar
          in
          let call_phi =
            make_phi "call" CF.PhiVar
          (*
            make_phi "ret" (CF.PhiSplitReturn(call_inphi))
            make_phi "call" (CF.PhiSplitCall(ret_phi))
            *)
          in
          let out_phi = make_phi "out" CF.PhiVar in
          CF.phi_flows call_inphi call_phi;
          CF.phi_flows ret_phi out_phi;
          let callt = make_tau (ITFun {
            fd_arg_tau_list = callargs;
            fd_input_phi = call_phi;
            fd_input_effect = if !do_contextual then fi.fd_input_effect else call_ineff;
            fd_output_tau = callrett;
            fd_output_phi = ret_phi;
            fd_output_effect = fi.fd_output_effect;
            fd_chi = !current_chi;
          }) callts in
          sub_tau e_type callt;
          if !do_contextual then begin
            (*ignore(E.log "adding %a to %a\n" d_chi fi.fd_chi d_effect call_ineff);
            defer(
              fun () -> begin
                let rx,wx = solve_chi_m fi.fd_chi in
                let re,we = solve_effect_m fi.fd_output_effect in
                let ree,wee = solve_effect_m call_ineff in
                ignore(E.log "chi solution:\n  read: %a\n  write: %a\n\n" d_rhoset rx d_rhoset wx);
                ignore(E.log "eff solution:\n  read: %a\n  write: %a\n\n" d_rhoset re d_rhoset we);
                ignore(E.log "ineff solution:\n  read: %a\n  write: %a\n\n" d_rhoset ree d_rhoset wee);
              end
            );*)
            chi_in_effect fi.fd_chi call_ineff;
            effect_flows fi.fd_output_effect call_ineff;
          end;
          (call_env, out_phi, fi.fd_output_effect)
        end
      | _ ->
        ignore (error "calling non-function type");
        raise TypingBug
      end
    end
  | Asm(_,_,outlist, inlist,_,loc) -> if !do_asm then (
      currentLoc := loc;
      let rasm = make_rho (LN.Const "asm") false in
      let (env,phi,eff) = List.fold_left
        (fun (env, phi, eff) (_,_,e) ->
          let ((s1,_), oenv, ophi, oeff) = type_exp e env phi eff in
          ignore(warn "conflating flow at asm block");
          let phi_in = make_phi "conflated_in" CF.PhiVar in
          let phi_out = make_phi "conflated_out" CF.PhiVar in
          conflate_from rasm s1 phi_in phi_out TauSet.empty;
          (oenv, ophi, oeff))
        (input_env, input_phi, input_effect)
        inlist
      in
      let (env, phi, eff) = List.fold_left
        (fun (env, phi, eff) (_,_,lv) ->
          let ((s1, r,_), oenv, ophi, oeff) = type_lval lv env phi eff in
          unify_rho r rasm;
          let phi_in = make_phi "conflated_in" CF.PhiVar in
          let phi_out = make_phi "conflated_out" CF.PhiVar in
          conflate_to s1 rasm phi_in phi_out TauSet.empty;
          (oenv, ophi, oeff))
        (env, phi, eff) outlist
      in
      write_rho rasm phi eff NotUnq;
      (*add_to_read_effect rasm eff;
      add_to_read_chi rasm !current_chi;
      add_to_write_effect rasm eff;
      add_to_write_chi rasm !current_chi;*)
      (env, phi, eff)
    ) else (input_env, input_phi, input_effect)

and type_pragma ((env, phi, eff), (kind, (loop_d: loop_descr option))) pragma =
  match pragma with
  | (Attr(pr_str, rest), loc) when pr_str = !pragma_str -> (
    match rest with
    | ACons("start", _)::_ -> (
      if !debug_SDAM then ignore(E.log "SDAM: Start parallel region.\n");
      let barrier_phi = make_phi "Barrier" CF.PhiBarrier in
      CF.phi_flows phi barrier_phi;
      CF.starting_phis := barrier_phi::!CF.starting_phis;
      ((env, barrier_phi, eff), (kind, loop_d))
    )
    | AStr("finish")::_ -> (
      if !debug_SDAM then ignore(E.log "SDAM: Finish parallel region.\n");
      let barrier_phi = make_phi "Barrier" CF.PhiBarrier in
      CF.phi_flows phi barrier_phi;
      CF.starting_phis := barrier_phi::!CF.starting_phis;

      ((env, barrier_phi, eff), (kind, loop_d))
    )
    | AStr("sync")::_
    | AStr("barrier")::_ ->
      if !debug_SDAM then ignore(E.log "SDAM: Barrier found!\n");
      let barrier_phi = make_phi "Barrier" CF.PhiBarrier in
      CF.phi_flows phi barrier_phi;
      CF.starting_phis := barrier_phi::!CF.starting_phis;
      ((env, barrier_phi, eff), (kind, loop_d))
    | AStr("wait")::rest -> ( (* TODO: maybe obsolete syntax *)
      match rest with
      | ACons("on", exps)::_ ->
        ignore(E.warn "Wait on task analysis not supported yet...\n");
        ((env, phi, eff), (kind, loop_d))
      | AStr("all")::_ ->
        if !debug_SDAM then ignore(E.log "SDAM: Barrier found!\n");
        let barrier_phi = make_phi "Barrier" CF.PhiBarrier in
        CF.phi_flows phi barrier_phi;
        CF.starting_phis := barrier_phi::!CF.starting_phis;
        ((env, barrier_phi, eff), (kind, loop_d))
      | _ ->
        if !debug_SDAM then ignore(E.warn "SDAM:%a:Ignoring pragma!\n" d_loc loc);
        ((env, phi, eff), (kind, loop_d))
    )
    |    AStr("task")::args -> (
      match kind with
      | Instr(taskcall::_) -> (* task pragmas must be coupled with function calls *)
        if !debug_SDAM then ignore(E.log "SDAM: Task found.\n");
        let task_d =
          handle_css_task taskcall env args loc loop_d
        in
        let task_phi =
          make_phi "Task" (CF.PhiTask (task_d.taskname, task_d.taskid))
        in
        CF.phi_flows phi task_phi;
        CF.starting_phis := task_phi::!CF.starting_phis;
        ((env, task_phi, eff), (kind, loop_d))
      | _ ->
        ignore(warnLoc loc "SDAM:Invalid syntax!\n");
        ((env, phi, eff), (kind, loop_d))
    )
    | _ ->
      ignore(warnLoc loc "SDAM:Invalid syntax!\n");
      ((env, phi, eff), (kind, loop_d))
  )
    | (_, loc) ->
        if !debug_SDAM then ignore(E.warn "SDAM:%a:Ignoring pragma!\n" d_loc loc);
        ((env, phi, eff), (kind, loop_d))

(*****************************************************************************)

and type_stmt_list ((gm: gamma), (loop_d: loop_descr option)) stmts =
  let g = (gm, loop_d) in
  let f g s =
    let g' = type_stmt g s in
    (g', loop_d)
  in
  List.fold_left f g stmts

and type_pragma_list g pragmas =
  let f g s =
    let g' = type_pragma g s in
    g'
  in
  List.fold_left f g pragmas

and type_stmt (((env, phi, eff): gamma), (loop_d: loop_descr option)) stmt : gamma =
  currSid := stmt.sid;
  if !debug then ignore(E.log "SDAM: typing statement %a\n" d_stmt stmt);
  if !do_uniq then current_uniqueness := Uniq.get_stmt_state stmt;
  set_goto_target env phi eff stmt;
  (* ignore(E.log "stmt=%a\n" d_stmt stmt); *)
  let (env,phi,eff) = Hashtbl.find env.goto_tbl stmt in
(*  let ((env', phi', eff'),_) = List.fold_left (type_pragma) ((env, phi, eff),stmt.skind) stmt.pragmas in*)
  match stmt.skind with
    Instr(il) ->
      let ((env', phi', eff'), _) = type_pragma_list ((env, phi, eff), (stmt.skind, loop_d)) stmt.pragmas in
      List.fold_left (type_instr ) (env', phi', eff') il
  | Return(e, loc) ->
      currentLoc := loc;
      let ((env, phi, eff), _) = type_pragma_list ((env, phi, eff), (stmt.skind, loop_d)) stmt.pragmas in
      let ((sret,_), env, phi, eff) =
        match e with
          None -> ((void_tau,NotUnq), env, phi, eff)
        | Some(e1) -> type_exp e1 env phi eff
      in
      let (t,r) = env_lookup (!current_function).svar.vname env in begin
        match t.t with
          ITAbs(tref) -> begin
            match !tref.t with
              ITFun fi ->
                CF.phi_flows phi fi.fd_output_phi;
                sub_tau sret fi.fd_output_tau;
                effect_flows fi.fd_output_effect eff;
                (env, CF.empty_phi, make_effect "after-return" false)
            | _ ->
                ignore(error "type of current_function must be an \
                              abstracted ITFun");
                raise TypingBug
          end
        | _ ->
            ignore(error "type of current_function must be an abstracted ITFun");
            raise TypingBug
      end
  | Goto(stmt_ref, loc) ->
      currentLoc := loc;
      set_goto_target env phi eff !stmt_ref;
      (env, CF.empty_phi, make_effect "goto-effect" false)
  | If(cond_exp, block1, block2, loc) ->
      currentLoc := loc;
      let ((env, phi, eff), _) = type_pragma_list ((env, phi, eff), (stmt.skind, loop_d)) stmt.pragmas in
      let ((cond_type,_), env, phi, eff) =
        type_exp cond_exp env phi eff in
      let true_phi,false_phi = phi, phi
      in
      let (g1, _) = type_stmt_list ((env, true_phi, eff), loop_d) block1.bstmts in
      let (g2, _) = type_stmt_list ((env, phi, eff), loop_d) block2.bstmts in
      join_gamma g1 g2
  | Loop(b, loc, Some(_), Some(_)) ->
      currentLoc := loc;
            if !debug_SDAM then ignore(E.log "Loop found, analyzing bounds\n");
(*             let loop_i = LP.get_loop_index b.bstmts in
            let loop_d = (
                if(isSone loop_i) then (
                    let (i_inf, i_exp) = getSome loop_i in
                    Sdam.make_loop_descr i_inf i_exp
                )
                else
                    Sdam.make_loop_descr None None
            ) in *)
(*            let ((env', phi', eff'), _) = type_pragma_list ((env, phi, eff),(stmt.skind, loop_d)) stmt.pragmas in *)
            (* FIXME: check whether to use stmt or b.stmts *)
(*            let loopstart_phi = make_phi "LoopStart" (CF.PhiLoopStart (Sdam.new_loop_d (LP.get_loop_index b.bstmts))) in*)
            let begin_phi = make_phi "beginloop" CF.PhiVar in
            CF.phi_flows phi begin_phi;
(*            CF.starting_phis := loopstart_phi::!CF.starting_phis;*)
(*      CF.phi_flows begin_phi loopstart_phi;*)
(*      let (_, smth) = loop_d in*)
(*      if(isSome smth) then ignore(E.log "put something there\n");*)
            let loop_d' = LP.getLoopIndex b.bstmts in
      let ((env2,p2,ef2), _) = type_stmt_list ((env, begin_phi, eff), loop_d') b.bstmts in
      CF.phi_flows p2 begin_phi;
      effect_flows eff ef2;
      effect_flows ef2 eff;
      env_flows env2 env;
      (* match with the last statement, which should be the loop index incrementation *)
(*      let last_stmt = (List.hd (List.rev b.bstmts)) in *)
(*       Loopa.simple_loop_pattern last_stmt.skind;*)

      (*let live_vars = LV.getLiveSet stmt.sid in
      down e env (getSome live_vars);
      *)
      (env2, p2, ef2)
  | Block(b) ->
      let ((env, phi, eff), _) = type_pragma_list ((env, phi, eff), (stmt.skind, loop_d)) stmt.pragmas in
      let (g, _) = type_stmt_list ((env, phi, eff), loop_d) b.bstmts in g
  | Loop(_,_,None,_)
  | Loop(_,_,_,None)
  | Break _
  | Continue _
  | Switch _ ->
    ignore(type_pragma_list ((env, phi, eff), (stmt.skind, loop_d)) stmt.pragmas);
    assert false
  | TryExcept _ (* wrong compiler *)
  | TryFinally _ -> assert false

let addvars (varlist: varinfo list) env : (string * tau) list * env =
  let is_glob = function
      Static -> true
    | _ -> false
  in
  let envref = ref env in
  let typelist =
    List.map
      (fun v ->
        Cil.currentLoc := v.vdecl;
        let name = LN.Const v.vname in
        let t = annotate v.vtype [] name in
        let r =
          if v.vaddrof then (
            allocate t;
            make_rho (LN.AddrOf name) true;
          )
          else unknown_rho
        in
        if is_glob v.vstorage then
          begin
            mark_global_tau t KGlobal;
            mark_global_rho r KGlobal RhoSet.empty;
          end;
        envref := env_add_var !envref v.vname (t, r);
        if !debug then ignore(E.log "%s: %a\n" v.vname d_tau t);
        (v.vname, t))
      varlist
  in
    (typelist,!envref)

let forker = ref 0
let non_forker = ref 0

let addfun (fd: fundec) : unit = begin
  Cil.prepareCFG fd;
  RD.computeRDs fd;
  if !debug then ignore (E.log "typing function %s %s\n" fd.svar.vname (Lprof.timestamp ()));
  let location1 = !Cil.currentLoc in
  LV.computeLiveness fd;
  let location2 = !Cil.currentLoc in
  if !do_uniq then Uniq.compute_uniqueness fd;
  if !debug then ignore (E.log "uniqueness done %s\n" (Lprof.timestamp ()));

  (* if the function doesn't fork, we can reuse one single
   * effect variable for the whole body. *)
  let use_one_effect = !do_one_effect
    && not (Hashtbl.mem functions_that_call_fork fd.svar.vname)
  in
  if use_one_effect then (
    if !debug_one_effect then ignore(E.log "function %s doesn't call fork\n" fd.svar.vname);
    let e = LF.make_effect (fd.svar.vname^"-singleton") false in
    force_effect e;
    incr non_forker;
  ) else (
    if !debug_one_effect then ignore(E.log "function %s calls fork\n" fd.svar.vname);
    incr forker;
  );

  (* actual work *)
  begin
  match fd.svar.vtype with
    TFun(tret,_,b,_) ->
      current_function := fd;
      Cil.currentLoc := location1;
      let name = LN.Const fd.svar.vname in
      let ret_type = annotate tret [] (LN.Field (name, "return")) in
      let in_phi = make_phi (fd.svar.vname) CF.PhiVar in
      if fd.svar.vname = "main" then
        CF.starting_phis := in_phi::!CF.starting_phis;
      let in_eff = make_effect (fd.svar.vname^"-input") false in
      let start_env = fresh_env () in
      let (arg_types, arg_env) = addvars fd.sformals start_env in
      let arg_types =
        if b then
          let r = make_rho (LN.Const "va_list") false in
          let vi = make_vinfo r in
          let t = make_tau (ITBuiltin_va_list vi) STBuiltin_va_list in
          (arg_types@ ([("", t)]))
        else if (List.length arg_types) = 0
          then [("",void_tau)]
          else arg_types
      in
      let const_rho = make_rho (LN.AddrOf name) true in
      let fun_ts = STFun(ret_type.ts, List.map (fun (_,x)-> x.ts) arg_types) in
      current_chi := make_chi "X";
      if is_atomic fd.svar then (
        assert (not (Hashtbl.mem atomic_functions fd));
        Hashtbl.add atomic_functions fd !current_chi
      );
      Cil.currentLoc := location2;
      let out_phi = make_phi (fd.svar.vname^"_out") CF.PhiVar in
      let out_eff = make_effect (fd.svar.vname^"-output") false in
      let fun_type = make_tau (ITFun {
        fd_arg_tau_list = arg_types;
        fd_input_phi = in_phi;
        fd_input_effect = in_eff;
        fd_output_tau = ret_type;
        fd_output_phi = out_phi;
        fd_output_effect = out_eff;
        fd_chi = !current_chi;
      }) fun_ts in
      let fun_abs_type = make_tau (ITAbs (ref fun_type)) (STAbs fun_ts) in

      (* add polymorphic type to global environment *)
      (try
        let (old_type, old_rho) = env_lookup fd.svar.vname !global_env in
        unify_types old_type fun_abs_type;
        unify_rho old_rho const_rho
      with Not_found -> ());
      global_env := env_add_var !global_env fd.svar.vname
                                (fun_abs_type, const_rho);
      (* add locals to the type-environment of the function *)
      let (_, locals_env) = addvars fd.slocals arg_env in
      (* add universal type of function (for polymorphic recursion) *)
      let final_env =
        env_add_var locals_env fd.svar.vname (fun_abs_type, const_rho)
      in
            (* TODO: anchor1 *)

      (* using this Gamma, type the function body *)
      let ((_, phi_stmt, eff_stmt), _) =
        type_stmt_list ((final_env, in_phi, in_eff), None) fd.sbody.bstmts in
      (* flow the output phi, eff, to the formal output phi, eff *)
      effect_flows out_eff eff_stmt;
      CF.phi_flows phi_stmt out_phi;

      (* remember the typing environment for this function *)
      global_fun_envs := (fd, final_env):: !global_fun_envs;

      (* clear state *)
      current_function := Cil.dummyFunDec;
      if !debug then ignore (E.log "function %s: %a\n" fd.svar.vname d_tau fun_abs_type);
      (*TODO:traverse enviroment to find functions *)
  | _ -> assert false
  end;

  (* if we had forced one effect for the whole function,
   * then restore to using flow-sensitive effects again *)
  if use_one_effect then unforce_effect ();
end

let propagate_vinfo_i (vi1: vinfo)
                      (vi2: vinfo)
                      (i: instantiation)
                      : unit =
  let v1 = U.deref vi1 in
  let v2 = U.deref vi2 in
  if !debug_void then ignore(E.log "propagate_vinfo_i: #%d to #%d through %a\n" v1.vinfo_id v2.vinfo_id d_instantiation i);
  if !do_void_conflate || !do_void_single then begin
    inst_rho (getSome v1.vinfo_rho) (getSome v2.vinfo_rho) true i;
    inst_rho (getSome v1.vinfo_rho) (getSome v2.vinfo_rho) false i;
    CF.inst_phi (getSome v1.vinfo_phi_in) (getSome v2.vinfo_phi_in) true i;
    CF.inst_phi (getSome v1.vinfo_phi_in) (getSome v2.vinfo_phi_in) false i;
    CF.inst_phi (getSome v1.vinfo_phi_out) (getSome v2.vinfo_phi_out) true i;
    CF.inst_phi (getSome v1.vinfo_phi_out) (getSome v2.vinfo_phi_out) false i;
  end else ();
  if !do_void_single && (v1.vinfo_types = ConflatedRho || v2.vinfo_types = ConflatedRho) then begin
    conflate_vinfo v1;
    conflate_vinfo v2;
  end;
  let name = LN.Const "void" in (* TODO: make this an input *)
  begin
    List.iter
      (fun (ts, t1) ->
        try
          let tl2 = get_vinfo_types v2.vinfo_types in
          let t2 = List.assoc ts tl2 in
          instantiate t1 t2 true i;
          instantiate t1 t2 false i;
        with Not_found -> begin
          Cil.currentLoc := v2.vinfo_loc;
          let t2 = reannotate t1 v2.vinfo_known name in
          if !debug_void then ignore (E.log "void: adding %a to v%d\n" d_tau t2 v2.vinfo_id);
          Cil.currentLoc := locUnknown;
          instantiate t1 t2 true i;
          instantiate t1 t2 false i;
          add_type_to_vinfo t2 v2;
          Q.push vi2 worklist_vinfo;
        end)
      (get_vinfo_types v1.vinfo_types);
    List.iter
      (fun (ts, t2) ->
        try
          let tl1 = get_vinfo_types v1.vinfo_types in
          (* if it's there, it was instantiated above, so no need to do anything *)
          let _ = List.assoc ts tl1 in ()
        with Not_found -> begin
          Cil.currentLoc := v1.vinfo_loc;
          let t1 = reannotate t2 v1.vinfo_known name in
          Cil.currentLoc := locUnknown;
          instantiate t1 t2 true i;
          instantiate t1 t2 false i;
          add_type_to_vinfo t1 v1;
          Q.push vi1 worklist_vinfo;
        end)
      (get_vinfo_types v2.vinfo_types);
  end

let propagate_cinfo_i (ci1: cinfo) (ci2: cinfo) (i: instantiation): unit =
  let c1 = U.deref ci1 in
  let c2 = U.deref ci2 in
  StrHT.iter
    (fun f (r1,t1) ->
      try
        let (r2,t2) = StrHT.find c2.cinfo_fields f in
        instantiate t1 t2 true i;
        instantiate t1 t2 false i;
        inst_rho r1 r2 true i;
        inst_rho r1 r2 false i;
      with Not_found -> begin
        if !debug_void then ignore(E.log "cinfo: adding %s to %d\n" f c2.cinfo_id);
        Cil.currentLoc := c2.cinfo_loc;
        let t2 = reannotate t1 c2.cinfo_known (LN.Field (c2.cinfo_label_name, f)) in
        let r2 =
          if isSome c2.cinfo_field_rho then (
            assert (not !do_field_sensitive);
            assert (isSome c1.cinfo_field_rho);
            getSome c2.cinfo_field_rho
          ) else
            make_rho (LN.Field (c2.cinfo_label_name, f)) false
        in
        Cil.currentLoc := locUnknown;
        if !debug_void then ignore(E.log "propagate_cinfo_i: instantiate %s on %a:\n   %a\n   %a\n" f d_instantiation i d_tau t1 d_tau t2);
        inst_rho r1 r2 true i;
        inst_rho r1 r2 false i;
        instantiate t1 t2 true i;
        instantiate t1 t2 false i;
        StrHT.add c2.cinfo_fields f (r2,t2);
        Q.push ci2 worklist_cinfo;
      end
    ) c1.cinfo_fields;
  StrHT.iter
    (fun f (r2,t2) ->
      try
        (*if it's there, it was instantiated above, so no need to do anything*)
        let _ = StrHT.find c1.cinfo_fields f in ()
      with Not_found -> begin
        if !debug_void then ignore(E.log "adding %s to %d\n" f c1.cinfo_id);
        Cil.currentLoc := c1.cinfo_loc;
        let name = (LN.Field (c1.cinfo_label_name, f)) in
        let t1 = reannotate t2 c1.cinfo_known name in
        let r1 =
          if isSome c1.cinfo_field_rho then (
            assert (not !do_field_sensitive);
            assert (isSome c2.cinfo_field_rho);
            getSome c1.cinfo_field_rho
          ) else
            make_rho name false
        in
        Cil.currentLoc := locUnknown;
        inst_rho r1 r2 true i;
        inst_rho r1 r2 false i;
        instantiate t1 t2 true i;
        instantiate t1 t2 false i;
        StrHT.add c1.cinfo_fields f (r1,t1);
        Q.push ci1 worklist_cinfo;
      end
    ) c2.cinfo_fields;
  ()

let dump_vinfo_stats () : unit =
  if !debug then ignore(E.log "calculating void* statistics\n");
  let (t,c,e,s) = List.fold_left
    (fun (t,c,e,s) v ->
      let xd = U.deref v in
      match (get_vinfo_types xd.vinfo_types) with
        [] -> (t+1,c,e+1,s)
      | [_] -> (t+1,c,e,s+1)
      | _ -> (t+1,c+1,e,s))
    (0,0,0,0)
    !all_vinfo
  in
  ignore(E.log "Void-types statistics: \n");
  ignore(E.log "total-void-types       : %d\n" t);
  ignore(E.log "empty-void-types       : %d\n" e);
  ignore(E.log "singleton-void-types   : %d\n" s);
  ignore(E.log "polymorphic-void-types : %d\n" c);
  ignore(E.log "\n")

let dump_cinfo_stats () : unit =
  if !debug then ignore(E.log "calculating struct statistics\n");
  let ih = Hashtbl.create 100 in
  let ch = Hashtbl.create 100 in
  let total_struct_types = ref 0 in
  let total_field_types = ref 0 in
  let used_field_types = ref 0 in
  let total_struct_instances = ref 0 in
  let total_field_instances = ref 0 in
  let used_field_instances = ref 0 in
  List.iter
    (fun c ->
      let cd = U.deref c in
      if not (Hashtbl.mem ih cd.cinfo_id) then (
        Hashtbl.add ih cd.cinfo_id ();
        incr total_struct_instances;
        if not (Hashtbl.mem ch cd.compinfo) then (
          Hashtbl.add ch cd.compinfo Strset.empty;
          incr total_struct_types;
          total_field_types := !total_field_types + (List.length cd.compinfo.cfields);
        );
        total_field_instances := !total_field_instances + (List.length cd.compinfo.cfields);
        used_field_instances := !used_field_instances + (StrHT.length cd.cinfo_fields);
        StrHT.iter
          (fun s _ ->
            let set = Hashtbl.find ch cd.compinfo in
            if not (Strset.mem s set) then (
              Hashtbl.replace ch cd.compinfo (Strset.add s set);
              incr used_field_types
            )
          )
          cd.cinfo_fields
      )
    )
    !all_cinfo;
  ignore(E.log "Struct-field optimization statistics\n");
  ignore(E.log "total-struct-types     : %d\n" !total_struct_types);
  ignore(E.log "total-field-types      : %d\n" !total_field_types);
  ignore(E.log "used-field-types       : %d\n" !used_field_types);
  ignore(E.log "total-struct-instances : %d\n" !total_struct_instances);
  ignore(E.log "init-struct-instances  : %d\n" !next_info);
  ignore(E.log "total-field-instances  : %d\n" !total_field_instances);
  ignore(E.log "used-field-instances   : %d\n" !used_field_instances);
  ignore(E.log "\n")

let solve_vinfo_cinfo_constraints () : unit =
  if !debug_void then ignore(E.log "solve vinfo/cinfo\n");
  (* Step 1:  Solve all vinfo and cinfo constraints simultaneously.
     propagate_X_i will add new edges to the worklist, as will any calls
     to make_X done during resolution *)
  while (not (Q.is_empty worklist_vinfo) || not (Q.is_empty worklist_cinfo)) do
    (*if !debug then ignore(E.log "worklist size: %d vinfo, %d cinfo\n"
                            (Q.length worklist_vinfo) (Q.length worklist_cinfo));*)
    if (not (Q.is_empty worklist_vinfo)) then
      let vi1 = Q.pop worklist_vinfo in
      begin
        if !debug_void then ignore(E.log "worklist vinfo: %d\n" (U.deref vi1).vinfo_id);
        InstHT.iter
          (fun i vi2 -> propagate_vinfo_i vi1 vi2 i)
          (U.deref vi1).vinfo_inst_out_edges;
        InstHT.iter
          (fun i vi0 -> propagate_vinfo_i vi0 vi1 i)
          (U.deref vi1).vinfo_inst_in_edges;
      end
    else if (not (Q.is_empty worklist_cinfo)) then
      let ci1 = Q.pop worklist_cinfo in
      begin
        if !debug_void then ignore(E.log "worklist cinfo: %d\n" (U.deref ci1).cinfo_id);
        InstHT.iter
          (fun i ci2 -> propagate_cinfo_i ci1 ci2 i)
          (U.deref ci1).cinfo_inst_out_edges;
        InstHT.iter
          (fun i ci0 -> propagate_cinfo_i ci0 ci1 i)
          (U.deref ci1).cinfo_inst_in_edges;
      end
  done;
  (* Step 2: ``Allocate'' all the vinfos *)
  if !debug then ignore(E.log "allocating vinfos\n");
  List.iter
    (fun x ->
      let xd = U.deref x in
      List.iter
        (fun loc ->
          Cil.currentLoc := loc;
          List.iter
            (fun (_, t) -> allocate t)
            (get_vinfo_types xd.vinfo_types);
          Cil.currentLoc := locUnknown;
        )
        xd.vinfo_alloc
    )
    !all_vinfo;
  (* Step 3:  ``Allocate'' all the cinfos *)
  if !debug then ignore(E.log "allocating cinfos\n");
  (*let scanned = Hashtbl.create 1000 in*)
  let one_more = ref true in
  while !one_more do
    one_more := false;
    (*if !debug_void then ignore(E.log "total: %d cinfos\n" (List.length !all_cinfo));*)
    List.iter
      (fun x ->
        let xd = U.deref x in
        (*if not (Hashtbl.mem scanned xd.cinfo_id) then*)
        begin
          (*Hashtbl.add scanned xd.cinfo_id ();*)
          let l = xd.cinfo_alloc in
          xd.cinfo_alloc <- [];
          List.iter (fun loc ->
            one_more := true;
            (*Hashtbl.remove scanned xd.cinfo_id;*)
            if !do_field_sensitive then
              StrHT.iter
                (fun f (r,t) ->
                  let name = (LN.Field (LN.Const "alloc", f)) in
                  Cil.currentLoc := loc;
                  let rc = make_rho name true in
                  unify_rho rc r;
                  allocate t;
                  Cil.currentLoc := locUnknown;
                )
                xd.cinfo_fields
            else (
              let n = sprintf "alloc(%d)" (StrHT.length xd.cinfo_fields) in
              let name = (LN.Const n) in
              Cil.currentLoc := loc;
              let rc = make_rho name true in
              unify_rho rc (getSome xd.cinfo_field_rho);
              Cil.currentLoc := locUnknown;
            )
          )
          l;
        end
      )
      !all_cinfo
  done


let filter_quantified (l: attrparam list) : unit =
  let err () =
    ignore(error "use #pragma(structname.fieldname,<index list>)");
    raise TypingBug
  in
  match l with
    ACons(structname,[])::tl ->
      if !debug then ignore(E.log "existential %s:\n" structname);
      let func = emptyFunction "dummy_func" in
      let t = (Hashtbl.find typenames ("struct "^structname)) in
      let v = makeLocalVar func structname t in
      begin
        match t with
          TComp(c,_) -> ()
        | _ -> assert false
      end;
      List.iter
        (function
          AStr(e) ->
            let exp = Formatcil.cExp e [structname, Fv v] in
            if !debug then ignore(E.log "quantified: %a\n" d_exp exp);
            Hashtbl.add quantified_map structname exp
        | _ -> err ())
        tl
  | _ -> err ()

class constraintVisitor = object
  inherit nopCilVisitor
  method vglob (g: global) : global list visitAction =
  begin
    match g with
    | GType(_,_) -> DoChildren
    | GCompTag(_, _) -> DoChildren
    | GCompTagDecl(_, _) -> DoChildren
    | GEnumTag(_, _) -> DoChildren
    | GEnumTagDecl(_, _) -> DoChildren
    | GVarDecl(vi, loc) ->
        currentLoc := loc;
        let (var_type, var_rho) =
          try env_lookup vi.vname !global_env
          with Not_found ->
            if Strmap.mem vi.vname !Conf.special_functions then begin
              let f = Strmap.find vi.vname !Conf.special_functions in
              if f = Conf.Alloc then
                match vi.vtype with
                | TFun (t, args, b, al) ->
                    let t' = typeAddAttributes [Attr ("unique",[])] t in
                    vi.vtype <- TFun (t', args, b, al)
                | _ -> assert false
            end;
            let name = (LN.Const vi.vname) in
            let t = annotate vi.vtype [] name in
            let r = (make_rho (LN.AddrOf name) true) in
            match t.t with
              ITFun _ ->
                if (not (Strset.mem vi.vname !def_functions)) &&
                   (not (Strmap.mem vi.vname !Conf.special_functions))
                then undef_functions := Strset.add vi.vname !undef_functions;
                make_tau (ITAbs (ref t)) (STAbs t.ts), r
            | _ -> (t, r)
        in
        mark_global_tau var_type KGlobal;
        mark_global_rho var_rho KGlobal RhoSet.empty;
        global_env := env_add_var !global_env vi.vname (var_type, var_rho);
        if !debug then ignore(E.log "%s : %a\n" vi.vname d_tau var_type);
        DoChildren
    | GVar(vi, ii, loc) -> begin
        currentLoc := loc;
        let name = (LN.Const vi.vname) in
        let var_type =
          let t = annotate vi.vtype [] name in
          match t.t with
            ITFun _ -> make_tau (ITAbs (ref t)) (STAbs t.ts)
          | _ -> begin
            allocate t;
            (* bug fix: if vi is a global array, then we need to
             * explicitly allocate its elements, because "allocate t"
             * will not allocate under the pointer
             *)
            (match t.t,vi.vtype with
              ITPtr(tr,r), TArray(_,_,_) ->
                allocate !tr;
                let r' = make_rho (LN.Deref name) true in
                unify_rho r' r
              | _ -> ()
            );
            t
            end
        in
        let var_rho =
          try
            let (old_type, old_rho) = env_lookup vi.vname !global_env in
            unify_types old_type var_type;
            update_rho_location old_rho loc
          with Not_found -> make_rho (LN.AddrOf name) true
        in
        global_env := env_add_var !global_env vi.vname (var_type, var_rho);
        mark_global_tau var_type KGlobal;
        mark_global_rho var_rho KGlobal RhoSet.empty;
        if !debug then ignore(E.log "%s : %a\n" vi.vname d_tau var_type);
        match ii.init with
        | Some(i) -> begin
            let (init_type, env) = type_init i !global_env name in
            sub_tau init_type var_type;
            if !global_env != env then assert false;
            global_env := env
          end
        | _ -> ()
      end;
        DoChildren
    | GFun(fd, loc) ->
        begin
          currentLoc := loc;
          current_function := fd;
          if (Strset.mem fd.svar.vname !undef_functions) then
            undef_functions := Strset.remove fd.svar.vname !undef_functions;
          if (Strset.mem fd.svar.vname !def_functions) then (
            ignore(error "function %s defined twice!\n" fd.svar.vname);
            raise TypingBug
          );
          def_functions := Strset.add fd.svar.vname !def_functions;
          addfun fd;
          DoChildren
        end
    | GAsm(_, _) -> DoChildren
    | GPragma(a, _) -> begin
        match a with
          Attr(at,l) when at = pragmaKeyword ->
            if !do_existentials then filter_quantified l;
        | _ -> ()
      end;
      DoChildren
    | GText(_) -> DoChildren
  end
end

let handle_undef_function (name: string) : unit =
  assert (Strmap.mem name (!global_env).var_map);
  (*if not (Hashtbl.mem Cil.gccBuiltins name) then*)
  if !debug then ignore(E.log "  %s\n" name);
  let (t,_) = env_lookup name !global_env in
  match t.t with
    ITAbs(tref) -> begin
      match !tref.t with
        ITFun fi ->
          (* make "id", don't touch arguments *)
          CF.phi_flows fi.fd_input_phi fi.fd_output_phi;
          (*CF.phi_flows fi.fd_output_phi fi.fd_input_phi;*)
          effect_flows fi.fd_output_effect fi.fd_input_effect;
          (*effect_flows fi.fd_input_effect fi.fd_output_effect;*)
          (*fd_arg_tau_list: (string * tau) list;
          mutable fd_output_tau: tau;*)
      | _ -> assert false
    end
  | _ -> assert false

let init f : unit = begin
  if !debug then ignore(E.log "unroll types\n");
  let tv = new unrollTypeVisitor in
  visitCilFileSameGlobals tv f;
  global_env := {
    goto_tbl = Hashtbl.create 1;
    var_map =
      Hashtbl.fold
        (fun name (resTyp, argTypes, isva) (h: (tau*rho) Strmap.t) ->
          let t = (TFun(resTyp,
                 Some (List.map (fun at -> ("", at, [])) argTypes),
                 isva, [])) in
          let ts = typ_tau_sig t in
          let tau = make_tau (ITAbs (ref(annotate t [] (LN.Const name)))) ts in
          if not (Strmap.mem name !Conf.special_functions)
          then undef_functions := Strset.add name !undef_functions;
          Strmap.add name (tau,unknown_rho) h)
        Cil.gccBuiltins Strmap.empty;
    unpacked_map = Strmap.empty;
  };

  (* find all functions that call fork *)
  if !debug then ignore(E.log "find what calls fork\n");
  let rec dfs (node: CG.callnode) : unit =
    let name = CG.nodeName node.CG.cnInfo in
    if not (Hashtbl.mem functions_that_call_fork name) then begin
      Hashtbl.add functions_that_call_fork name ();
      Inthash.iter (fun _ n -> dfs n) node.CG.cnCallers;
    end
  in
  let graph: CG.callgraph = (CG.computeGraph f) in
  (*Hashtbl.clear functions_that_call_fork;*)
  Hashtbl.iter
    (fun s n ->
      try
        if (Strmap.find s !Conf.special_functions) = Conf.Fork
        then dfs n
        else ()
      with Not_found -> ())
    graph;
  (*if !debug_one_effect then
    Hashtbl.iter (fun s _ -> ignore(E.log "function %s calls fork\n" s)) functions_that_call_fork;*)
  if !debug then ignore(E.log "found what calls fork\n");
end

let generate_constraints (f: file) : unit = begin
  init f;

  if !debug then ignore(E.log "typing-parsing\n");
  let cv = new constraintVisitor in visitCilFileSameGlobals cv f;
  Lprof.endtime "typing-parsing";
  if !debug then ignore(E.log "typing-parsing done\n");
  Cil.currentLoc := locUnknown;

  if !debug then ignore(E.log "solve void* and lazy-struct-field constraints\n");
  solve_vinfo_cinfo_constraints ();
  if !do_typing_stats then begin
    dump_vinfo_stats ();
    dump_cinfo_stats ();
    ignore(E.log "forker functions: %d\n" !forker);
    ignore(E.log "non-forker functions: %d\n" !non_forker);
  end;
  Lprof.endtime "typing-void*";
  if !debug then ignore(E.log "solve void* and lazy-struct-field constraints done\n");

  if !debug then ignore(E.log "set globals\n");
  set_globals ();
  Lprof.endtime "typing-globals";
  if !debug then ignore(E.log "set globals done\n");

  if !debug then ignore(E.log "applying down\n");
  done_typing();
  (*apply_down ();*)
  Lprof.endtime "typing-down";
  if !debug then ignore(E.log "applying down done\n");

  if !debug then ignore(E.log "constraint generation done\n");

  let useundef = (Strset.inter !undef_functions !used_functions) in
  if not (Strset.is_empty useundef) then
    if !debug then ignore(E.log "functions declared and used but not defined:\n");
  Strset.iter handle_undef_function useundef;
  (*clear_globals();*)
end
