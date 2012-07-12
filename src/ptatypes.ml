open Cil
open Printf
open Pretty
open Lockutil
open Labelflow

module U = Uref
module LN = Labelname
module CF = Controlflow
module PhiSet = CF.PhiSet

let debug_void = ref false
let debug = ref false

type phiSet = PhiSet.t
type phi = CF.phi

type tau_sig =
    STVoid
  | STInt
  | STFloat
  | STPtr of tau_sig
  | STFun of tau_sig * tau_sig list
  | STComp of bool * string
  | STBuiltin_va_list
  | STAbs of tau_sig
  | STExists of tau_sig
  | STRegion

type tau_t =
    ITVoid of vinfo option (* types flowing to and from void *)
  | ITInt of bool (* true if number is 0 (can be used as NULL) *)
  | ITFloat (* true if number is 0 (can be used as NULL) *)
  | ITPtr of tau ref * rho
  (*
    functions:
      1) a list of arguments (string: name of variable, tau: it's type),
      2) input phi
      3) input effect (really output-instantiated, because they flow backwards)
      4) return type
      5) output phi
      6) output effect (is input-instantiated)
   *)
  | ITFun of fdinfo

  (* struct/union *)
  | ITComp of cinfo
  | ITBuiltin_va_list of vinfo

  (* universal type.  can safely assume tau is a ITFun type.
   *)
  | ITAbs of tau ref

  | ITExists of existinfo
  | ITRegion of theta

and tau = {
  t: tau_t;    (* the actual type structure *)
  ts: tau_sig; (* summary used as a hash *)
  tid: int;    (* unique identifier used to compare in O(1) *)
  tau_free_rho: rho option;  (* obsolete, used to lazily compute the free labels of a type *)
}

  (* existential types. tau usually is a ITComp, but so far I think
     the implementation is generic.
       --no it's not, because of the way we mark quantified stuff.
   *)
and existinfo = {
  mutable exist_tau: tau;
  exist_effect: effect;
  exist_phi: phi;
  mutable exist_abs: exp list;
  mutable exist_rhoset: rhoSet;
  mutable exist_effectset: effectSet;
  mutable exist_initialized: bool;
}
and fdinfo = {
  mutable fd_arg_tau_list: (string * tau) list;
  fd_input_phi: phi;
  fd_input_effect: effect;
  mutable fd_output_tau: tau;
  fd_output_phi: phi;
  fd_output_effect: effect;
  fd_chi: chi;
}

and field_set = (rho * tau) StrHT.t

and compdata = {
  cinfo_id: int;
  compinfo: compinfo;

  (* used to print (semi) readable information about labels in the fields *)
  mutable cinfo_label_name : LN.label_name;

  mutable cinfo_loc: Cil.location;

  (* these are used in case this struct is conflated *)
  mutable cinfo_from_rho: rho option;
  mutable cinfo_to_rho: rho option;

  cinfo_field_rho: rho option; (* only used when field-insensitive *)

  cinfo_fields: field_set;

  mutable cinfo_known: (tau_sig * tau) list;
  mutable cinfo_alloc: Cil.location list;
  mutable cinfo_inst_in_edges: cinfo InstHT.t;
  mutable cinfo_inst_out_edges: cinfo InstHT.t;
}

and voiddata_t =
  | ListTypes of (tau_sig * tau) list
  | ConflatedRho

and voiddata = {
    vinfo_id: int;
    vinfo_rho: rho option;      (* only used if void* conflation on *)
    vinfo_phi_in: phi option;   (* only used if void* conflation on *)
    vinfo_phi_out: phi option;  (* only used if void* conflation on *)
    mutable vinfo_loc: Cil.location;
    mutable vinfo_known: (tau_sig * tau) list;
    mutable vinfo_alloc: Cil.location list;
    mutable vinfo_inst_in_edges: vinfo InstHT.t;
    mutable vinfo_inst_out_edges: vinfo InstHT.t;

    mutable vinfo_types: voiddata_t;  (* types this void stands for *)
}

and cinfo = compdata U.uref
and vinfo = voiddata U.uref

type uniq = 
    UnqVar      (* a variable that is unique (both lval and storage) *)
  | UnqStorage  (* storage pointed to be a unique pointer *)
  | NotUnq      (* non-unique storage *)

type env = {
  goto_tbl : (stmt, gamma) Hashtbl.t;
  var_map : (tau * rho) Strmap.t;
  unpacked_map : existinfo Strmap.t;
}
and gamma = (env * phi * effect)

(*****************************************************************************)
let get_vinfo_types : voiddata_t -> (tau_sig * tau) list =
function
  | ListTypes l -> l
  | ConflatedRho -> []
      (* the caller should test for conflation and handle it *)

module TauPair : Set.OrderedType with type t = (tau*tau) =
  struct
    type t = tau*tau
    let compare (x1,x2) (y1,y2) =
      if x1.tid < y1.tid then -1
      else if x1.tid > y1.tid then 1
      else if x2.tid < y2.tid then -1
      else if x2.tid > y2.tid then 1
      else 0
  end
module TauPairSet = Set.Make(TauPair)

module TauHT = Hashtbl.Make(
  struct
    type t = tau
    let equal t1 t2 = t1.tid = t2.tid
    let hash t = Hashtbl.hash t
  end)

module TauSet = Set.Make(
  struct
    type t = tau
    let compare t1 t2 = t1.tid - t2.tid
  end)

module InstEdge : Hashtbl.HashedType
  with type t = (tau*tau*instantiation*bool) =
  struct
    type t = tau*tau*instantiation*bool
    let equal (abs,inst,i,p) (abs',inst',i',p') =
      abs.tid = abs'.tid &&
      inst.tid = inst'.tid &&
      inst_equal i i' &&
      p = p'
    let hash (t1,t2,i,p) = 2 * t1.tid + t2.tid
  end

module InstEdgeTbl = Hashtbl.Make(InstEdge)

module CinfoInst : Hashtbl.HashedType with type t = (cinfo*instantiation) =
  struct
    type t = cinfo*instantiation
    let equal (c1,i1) (c2,i2) =
      (U.deref c1).cinfo_id = (U.deref c2).cinfo_id && (Inst.equal i1 i2)
    let hash (t,i) = (U.deref t).cinfo_id
  end
module CinfoInstHash = Hashtbl.Make(CinfoInst)

module VinfoInst : Hashtbl.HashedType with type t = (vinfo*instantiation) =
  struct
    type t = vinfo*instantiation
    let equal (c1,i1) (c2,i2) =
      (U.deref c1).vinfo_id = (U.deref c2).vinfo_id && (Inst.equal i1 i2)
    let hash (t,i) = (U.deref t).vinfo_id
  end
module VinfoInstHash = Hashtbl.Make(VinfoInst)

type labelsets = (rhoSet * thetaSet * effectSet * phiSet)

(*****************************************************************************)

(*****************************************************************************)
(* pretty-printing *)

let string_of_cinfo (c: cinfo) =
  (U.deref c).compinfo.cname ^
  (if !debug_void then 
    "#" ^ string_of_int (U.deref c).cinfo_id
  else "") 

let rec d_sig () (ts: tau_sig) : doc =
  match ts with
    STVoid -> text "void"
  | STInt -> text "int"
  | STFloat -> text "float"
  | STPtr ts1 -> d_sig () ts1 ++ text "*"
  | STFun (ts1,tsl) ->
      d_sig () ts1 ++
      List.fold_left (fun d x -> d ++ text ", " ++ d_sig () x) (text "(") tsl
      ++ text ")"
  | STComp (s, n) ->
      (if s then text "struct " else text "union ") ++ text n
  | STBuiltin_va_list -> text "..."
  | STAbs ts1 -> text "forall(" ++ d_sig () ts1 ++ text ")"
  | STExists (ts1) -> text "exists(" ++ d_sig () ts1 ++ text ")"
  | STRegion _ -> text "region" 

let rec d_siglist (): tau_sig list -> doc = function
    [] -> nil
  | h::[] -> begin
      d_sig () h
  end
  | h::tl -> begin
      d_sig () h ++ text ",\n" ++ d_siglist () tl
  end

let rec d_short_tau () (t: tau) : doc =
  match t.t with
    ITVoid _ -> text "void"
  | ITInt _ -> text "int"
  | ITFloat -> text "float"
  | ITPtr(tref,_) -> text "*" ++ d_short_tau () !tref
  | ITFun fi -> text "fun"
  | ITComp ci -> text ("struct " ^ (string_of_cinfo ci))
  | ITBuiltin_va_list _ -> text "va-list"
  | ITAbs tref -> text "forall " ++ d_short_tau () !tref
  | ITExists ei -> text "exists " ++ d_short_tau () ei.exist_tau
  | ITRegion _ -> text "region" 

let field_set_to_fieldlist (f: field_set) =
    let l = StrHT.fold (fun fld v a -> (fld,v)::a) f [] in
    List.sort (fun (f1, _) (f2, _) -> (String.compare f1 f2)) l

let rec d_arglist (al: (string * tau) list) (known: TauSet.t) : doc =
  match al with
    [] -> nil
  | (n,h)::[] -> text (n^": ") ++ d_tau_r h known
  | (n,h)::tl -> text (n^": ") ++ d_tau_r h known ++ text ",\n" ++ d_arglist tl known

and d_fieldlist (fl: (string * (rho * tau)) list) (known: TauSet.t) : doc =
  match fl with
    [] -> nil
  | (n, (r,t))::[] ->
      dprintf "  <%a> %s: " d_rho r n ++ d_tau_r t known ++ line
  | (n, (r,t))::tl ->
      dprintf "  <%a> %s: " d_rho r n ++ d_tau_r t known ++ line ++ d_fieldlist tl known
(*
  | (n, (r,t))::[] ->
      line ++ align ++ dprintf "  <%s> %s: " d_rho r n ++ line ++ text "  " ++
        d_tau_r t known ++
      unalign
  | (n, (r,t))::tl ->
      line ++ align ++ dprintf "  <%s> %s: " d_rho r n ++ line ++ text "  " ++
        d_tau_r t known ++ line ++
      unalign ++ d_fieldlist tl known
*)

and d_tauset () ts : doc =
  let first = ref true in
  TauSet.fold
    (fun h d ->
      d ++ (if !first then (first := false; nil) else text ",\n")
      ++ d_tau_r h TauSet.empty)
    ts
    nil

and d_taulist () : tau list -> doc = function
    [] -> nil
  | h::[] -> begin
    d_tau_r h TauSet.empty;
  end
  | h::tl -> begin
    d_tau_r h TauSet.empty ++ text ",\n" ++ d_taulist () tl
  end

and d_tau_r (t:tau) (known: TauSet.t) : doc =
  if TauSet.mem t known then d_sig () t.ts else
  match t.t with
  | ITVoid None  -> text "void"
  | ITVoid (Some vr)  ->
      let v = U.deref vr in
      if !debug then dprintf "(void#%d)" v.vinfo_id
      else text "(void)"
  | ITInt _ -> text "int"
  | ITFloat -> text "float"
  | ITPtr(s,r) ->
      d_tau_r !s (TauSet.add t known) ++ dprintf "*^{%a}" d_rho r;
  | ITFun fi ->
        align ++
          text "(" ++
          align ++
            d_arglist fi.fd_arg_tau_list known ++
            text ",\n" ++ CF.d_phi () fi.fd_input_phi ++
          unalign ++ text ")\n->^{" ++
          align ++
            d_effect () fi.fd_input_effect ++ text ",\n" ++
            d_effect () fi.fd_output_effect ++ text ",\n" ++
            d_chi () fi.fd_chi ++ text "}" ++
          unalign ++ line ++
          text "(" ++
          align ++
            d_tau_r fi.fd_output_tau known ++ text ",\n" ++
            (CF.d_phi () fi.fd_output_phi) ++ text ")" ++
          unalign ++
        unalign
  | ITComp(c) ->
      align ++ dprintf "struct %s {" (string_of_cinfo c) ++ line ++
        align ++
        d_fieldlist
          (field_set_to_fieldlist (U.deref c).cinfo_fields)
          (TauSet.add t known) ++
        unalign ++ text "}" ++ unalign
  | ITBuiltin_va_list(_) -> text "..."
  | ITAbs s  ->
      text "(\\forall" ++ d_tau_r !s known ++ text ")"
  | ITExists ei ->
      dprintf "(\\exists [%a; %a] " (d_list "" d_exp) ei.exist_abs
        d_effect ei.exist_effect
      ++ d_tau_r ei.exist_tau (TauSet.add t known) (*POLYVIOS: why was this TauSet.empty?*)
      ++ text ")"
  | ITRegion theta -> dprintf "region^{%a}" d_theta theta

let d_tau () (s: tau) : doc = d_tau_r s TauSet.empty

let d_env () (e: env) : doc =
  let d_entry (v: string) (s, r: tau * rho) (d: doc) : doc =
    d ++ text v ++ text " : " ++ d_tau_r s TauSet.empty ++ text "\n"
  in
  text "Gamma:" ++ align ++ line ++
       Strmap.fold d_entry e.var_map nil ++
       unalign ++ text "\n\n"

(*****************************************************************************)

