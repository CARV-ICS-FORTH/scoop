(****************************************************************************)
(* Copyright (c) 2010-13,                                                   *)
(*                        Foivos    Zakkak          <zakkak@ics.forth.gr>   *)
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

(** The main module of SCOOP *)

open Pretty
open Cil
module LU    = Lockutil
module SU    = Scoop_util
module E     = Errormsg
module H     = Hashtbl
module S     = Str
module L     = List
module T     = Trace
module CG    = Callgraph
module Lprof = Lockprofile

(* defining globals *)
(** flag for more prints by SCOOP *)
let debug = ref false
(** the prefix of the files to be produced by SCOOP. Defaults to
    "scoop_trans" *)
let out_name = ref "scoop_trans"
(** the runtime/architecture to target. Currently supporting
    bddt/cell/cellgod/cellBlade/cellgodBlade/myrmics/scc/nesting/XPPFX
    Defaults to unknown *)
let arch = ref "unknown"
(** the str following the pragma, default is css (#pragma css ...) *)
let pragma_str = ref "notset"
(** the path where the runtime headers are located *)
let includePath = ref ""
(** flags to pass to the gcc when merging files *)
let cflags = ref ""
(** flag for disabling SDAM *)
let dis_sdam = ref false
(** The new file to create *)
let gen_file = ref dummyFile

(** the options supported by scoop *)
let options =
  [
    "--runtime",
      Arg.String(fun s -> arch := s),
      " SCOOP: Define the target runtime\nbddt | cell | cellgod | cellBlade | cellgodBlade | myrmics | scc | nesting | XPPFX";

    "--cflags",
      Arg.String(fun s -> cflags := s),
      " SCOOP: Define the flags you want to pass to gcc.";

    (* FIXME: remove it at some later release *)
    "--tpcIncludePath",
      Arg.String(fun s -> includePath := s),
      " SCOOP: Define the include path for the tpc runtime. (DEPRECATED)";

    "--include-path",
      Arg.String(fun s -> includePath := s),
      " SCOOP: Define the path containing the runtime header files.";

    "--debug-SCOOP",
      Arg.Set(debug),
      " SCOOP: Print debugging information.";

    "--out-name",
      Arg.String(fun s -> out_name := s),
      " SCOOP: Specify the output files' prefix. e.g. (default: scoop_trans) will produce scoop_trans.c (and scoop_trans_func.c for cell)";

    "--pragma",
      Arg.String(fun s -> pragma_str := s),
      " SCOOP: Specify the string constant following the pragma e.g. (default: runtime name) myrmics will recognise #pragma myrmics ... ";

    "--without-stats",
      Arg.Clear(SU.stats),
      " SCOOP: Disable code generation for statistics, for use without -DSTATISTICS";

(*    "--with-unaligned-arguments",
      Arg.Set(unaligned_args),
      " SCOOP: Allow unalligned arguments in x86, for use with -DUNALIGNED_ARGUMENTS_ALLOWED";*)

    "--disable-sdam",
      Arg.Set(dis_sdam),
      " SCOOP: Disable the static dependence analysis module (SDAM)";
  ]

let feature : featureDescr =
  { fd_name = "scoop";
    fd_enabled = ref true;
    fd_description = "find all pragmas declaring spu tasks";
    fd_extraopt = options
    @ Uniqueness.options
    @ Locksettings.options
    (*@ Livevars.options*)
    @ Shared.options
    @ Correlation.options
    @ Controlflow.options
    @ Bansheemlifc.options
    @ Labelflow.options
    @ Lprof.options
    @ Ptatype.options
    @ Ptdepa.options
    @ Scoop_bddt.options
    @ Scoop_cell.options
    @ Scoop_cellgod.options
    @ Scoop_myrmics.options
    @ Scoop_nesting.options
    @ Scoop_XPPFX.options
    ;
    fd_doit =
    (function (f: file) ->

      if (!arch = "unknown") then (
        E.s (error "No architecture specified. Exiting!")
      );

      if (!pragma_str="notset") then
        pragma_str := !arch;

      (* Inform SDAM about the different pragma we are using *)
      Ptatype.pragma_str := !pragma_str;

      gen_file := { f with fileName = (!out_name^".c");};

      (* create a call graph *)
      let callgraph = CG.computeGraph f in

      (* find tpc_decl pragmas *)
      let codeGenerator =
        match !arch with
        | "bddt" ->
           new Scoop_bddt.codegen callgraph !gen_file !pragma_str !includePath
        | "cell" ->
           new Scoop_cell.codegen callgraph !gen_file !pragma_str !includePath
               !out_name false
        | "cellBlade" ->
           new Scoop_cell.codegen callgraph !gen_file !pragma_str !includePath
               !out_name true
        | "cellgod" ->
           new Scoop_cellgod.codegen callgraph !gen_file !pragma_str
               !includePath !out_name false
        | "cellgodBlade" ->
           new Scoop_cellgod.codegen callgraph !gen_file !pragma_str
               !includePath !out_name false
        | "myrmics" ->
           new Scoop_myrmics.codegen callgraph !gen_file !pragma_str !includePath
        (* | "nesting" -> *)
        (*    new Scoop_nesting.findTaskDeclVisitor callgraph !gen_file !pragma_str *)
        (* | "XPPFX" -> *)
        (*    new Scoop_XPPFX.findTaskDeclVisitor callgraph !gen_file !pragma_str *)
        | _ -> E.s (unimp "Runtime \"%s\" is not supported" !arch);
      in

      (* Include the runtime's header file  *)
      codeGenerator#preprocessAndMergeWithHeader !cflags;

      (* Declare some globals *)
      codeGenerator#declareGlobals;

      (* let globals = ref [] in *)
      (* let makeGlobalVar ini n t = *)
      (*   globals := GVar(makeGlobalVar n t, {init = ini;}, locUnknown)::!globals; *)
      (* in *)
      (* (match !arch with *)
      (*   (\* const int tpc_task_arguments_list[]; *\) *)
      (*   | "XPPFX" -> ( *)
      (*     makeGlobalVar (Some (SingleInit(zero))) *)
      (*                   "tpc_task_arguments_list" *)
      (*                   (TArray(TInt(IInt, [Attr("const", [])]), None, [])); *)
      (*   ) *)
      (*   | _ -> () *)
      (* ); *)
      (* SU.add_at_top !gen_file !globals; *)

      (* SDAM *)
      codeGenerator#parseFile !dis_sdam;

      (* Create a task table *)
      codeGenerator#makeTaskTable;

      (* eliminate dead code *)
(*        Cfg.computeFileCFG !gen_file;
      Deadcodeelim.dce !gen_file;
      Cfg.computeFileCFG !spu_file;
      Deadcodeelim.dce !spu_file;*)

      (* Scoop_rmtmps.removeUnused !gen_file; *)
      codeGenerator#writeFile;
    );
    fd_post_check = true;
  }
