open Cil
open Lockutil
open Labelflow

module U = Uref
module LN = Labelname
module CF = Controlflow
module PhiSet = CF.PhiSet

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

