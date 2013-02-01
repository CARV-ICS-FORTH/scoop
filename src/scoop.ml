(*
 *
 * Copyright (c) 2010,
 *  Foivos Zakkak        <zakkak@ics.forth.gr>
 *  Polyvios Pratikakis <polyvios@ics.forth.gr>
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

(** The main module of SCOOP *)

open Pretty
open Cil
open Lockutil
open Scoop_util
module E = Errormsg
module H = Hashtbl
module S = Str
module L = List
module T = Trace
module CG = Callgraph
module Lprof = Lockprofile

(* defining globals *)
(** holds the TPC's SPEs queue size *)
let queue_size = ref "0"
(** flag for more prints by SCOOP *)
let debug = ref false
(** flag to support multithreading or not *)
let thread = ref false
(** the prefix of the files to be produced by SCOOP. Defaults to "scoop_trans" *)
let out_name = ref "scoop_trans"
(** the runtime/architecture to target. Currently supporting
    adam/bddt/cell/cellgod/cellBlade/cellgodBlade/myrmics/scc/PAPAC/XPPFX
    Defaults to unknown *)
let arch = ref "unknown"
(** the str following the pragma, default is css (#pragma css ...) *)
let pragma_str = ref "css"
(** the main function in the file, default is main *)
let myrmics_main = ref "main"
(** the path where the runtime headers are located *)
let tpcIncludePath = ref ""
(** flags to pass to the gcc when merging files *)
let cflags = ref ""
(** holds the previous visited statement *)
let prevstmt = ref dummyStmt
(** flag for disabling SDAM *)
let dis_sdam = ref false
(** flag for CellBlade *)
let blade = ref false
(** flag for cell runtimes *)
let isCell = ref false

(** The new spu file to create *)
let spu_file = ref dummyFile
(** The new ppu file to create *)
let ppc_file = ref dummyFile

(** the options supported by scoop *)
let options =
  [
    "--runtime",
      Arg.String(fun s -> arch := s),
      " SCOOP: Define the target runtime\nadam | bddt | cell | cellgod | cellBlade | cellgodBlade | myrmics | scc | PAPAC | XPPFX";

    "--cflags",
      Arg.String(fun s -> cflags := s),
      " SCOOP: Define the flags you want to pass to gcc.";

    "--tpcIncludePath",
      Arg.String(fun s -> tpcIncludePath := s),
      " SCOOP: Define the include path for the tpc runtime.";

    "--debugSCOOP",
      Arg.Set(debug),
      " SCOOP: Print debugging information.";

    "--out-name",
      Arg.String(fun s -> out_name := s),
      " SCOOP: Specify the output files' prefix. e.g. (default: scoop_trans) will produce scoop_trans.c (and scoop_trans_func.c for cell)";

    "--pragma",
      Arg.String(fun s -> pragma_str := s),
      " SCOOP: Specify the string constant following the pragma e.g. (default: css) will recognise #pragma css ... (myrmics' default is myrmics)";

    "--queue-size",
      Arg.String(fun s -> queue_size := s),
      " SCOOP: Specify the queue size for Cell. Defined in the Makefile as MAX_QUEUE_ENTRIES";

    "--without-stats",
      Arg.Clear(stats),
      " SCOOP: Disable code generation for statistics, for use without -DSTATISTICS";

    "--myrmics-main-function",
      Arg.String(fun s -> myrmics_main := s),
      " SCOOP: Specify the name of the main function for the myrmics runtime (default: main)";


(*    "--with-unaligned-arguments",
      Arg.Set(unaligned_args),
      " SCOOP: Allow unalligned arguments in x86, for use with -DUNALIGNED_ARGUMENTS_ALLOWED";*)

(*    "--without-blocking",
      Arg.Clear(blocking),
      " SCOOP: Enable blocking arguments in x86. for use with -DBLOCKING";*)

    "--threaded",
      Arg.Set(thread),
      " SCOOP: Generate thread safe code for Cell, for use with -DTPC_MULTITHREADED";

    "--disable-sdam",
      Arg.Set(dis_sdam),
      " SCOOP: Disable the static dependence analysis module";
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
    ;
    fd_doit =
    (function (f: file) ->

      if (!arch = "unknown") then (
        E.s (error "No architecture specified. Exiting!")
      ) else if (!arch = "cell" && !queue_size = "0") then (
        E.s (error "No queue_size specified. Exiting!")
      );

      isCell := Str.string_match (Str.regexp "^cell") !arch 0;

      pragma_str := if (!arch="myrmics") then !arch else !pragma_str;
      (* Inform SDAM about the different pragma we are using *)
      Ptatype.pragma_str := !pragma_str;

      if(!arch = "cellBlade") then (
        blade := true;
        arch := "cell";
      ) else if(!arch = "cellgodBlade") then (
        blade := true;
        arch := "cellgod";
      );

      (* if we are on heterogeneous architecture create two copies of the initial file *)
      if ( !isCell ) then
        spu_file := { dummyFile with fileName = (!out_name^"_func.c");};

      ppc_file := { f with fileName = (!out_name^".c");};

      (* create a call graph *)
      let callgraph = CG.computeGraph f in

      (* find tpc_decl pragmas *)
      let fspuVisitor =
        match !arch with
        | "adam" -> new Scoop_adam.findTaskDeclVisitor callgraph !ppc_file !pragma_str
        | "bddt" -> new Scoop_bddt.findTaskDeclVisitor callgraph !ppc_file !pragma_str
        | "cell" -> new Scoop_cell.findTaskDeclVisitor callgraph !ppc_file !spu_file !pragma_str
        | "cellgod" -> new Scoop_cellgod.findTaskDeclVisitor callgraph !ppc_file !spu_file !pragma_str
        | "myrmics" -> new Scoop_myrmics.findTaskDeclVisitor callgraph !ppc_file !pragma_str
        | "PAPAC" -> new Scoop_PAPAC.findTaskDeclVisitor callgraph !ppc_file !pragma_str
        | "XPPFX" -> new Scoop_XPPFX.findTaskDeclVisitor callgraph !ppc_file !pragma_str
        | _ -> E.s (unimp "Runtime \"%s\" is not supported" !arch);
      in

      let def = " "^(!cflags)^
        ( if (!stats) then " -DSTATISTICS=1" else " ")^
        ( if (!blade) then " -DBLADE=1" else " ") in
      if (!arch = "adam" || !arch = "bddt") then (
        Scoop_adam.preprocessAndMergeWithHeader_x86 !ppc_file ((!tpcIncludePath)^"/scoop/tpc_scoop.h") (def);
      ) else if !isCell then ( (* else cell/cellgod *)
        (* copy all code from file f to file_ppc *)
        let def = def^(
          if (!arch = "cellgod") then
            (" -DADAM=1")
          else
            (" -DMAX_QUEUE_ENTRIES="^(!queue_size))
        ) in

        (* Defined in scoop_util *)
        preprocessAndMergeWithHeader_cell !ppc_file ((!tpcIncludePath)^"/scoop/tpc_scoop.h") (" -DPPU=1"^(def))
                                    !tpcIncludePath;

        (* copy all typedefs and enums/structs/unions from ppc_file to spu_file
          plus the needed headers *)
        let new_types_l = List.filter is_typedef (!ppc_file).globals in
        (!spu_file).globals <- new_types_l;
        preprocessAndMergeWithHeader_cell !spu_file ((!tpcIncludePath)^"/scoop/tpc_scoop.h") (" -DSPU=1"^(def))
                                    !tpcIncludePath;
      ) else if ( !arch<>"myrmics" ) then (
        Scoop_adam.preprocessAndMergeWithHeader_x86 !ppc_file ((!tpcIncludePath)^"/"^(!arch)^"_header.h") (def);
      );

      (* Declare some globals *)
      let globals = ref [] in
      let makeGlobalVar ini n t =
        globals := GVar(makeGlobalVar n t, {init = ini;}, locUnknown)::!globals;
      in
      (match !arch with
        (* Task_element *this;
            uint32_t block_index_start
            uint64_t e_addr;
            uint64_t _tmptime; *)
        "adam" -> (
          let makeGlobalVar = makeGlobalVar None in
          let task_element_pt = TPtr((find_type !ppc_file "Task_element"), []) in
          makeGlobalVar "this_SCOOP__" task_element_pt;
          let uint32_t = (find_type !ppc_file "uint32_t") in
          let uint64_t = (find_type !ppc_file "uint64_t") in
          makeGlobalVar "block_index_start_SCOOP__" uint32_t;
          makeGlobalVar "e_addr_SCOOP__" uint64_t;
          makeGlobalVar "_tmptime1_SCOOP__" uint64_t;
          makeGlobalVar "_tmptime2_SCOOP__" uint64_t;
        )
        (* Task_element *this;
            uint32_t block_index_start
            uint64_t e_addr;
            uint64_t _tmptime; *)
        | "bddt" -> (
          let makeGlobalVar = makeGlobalVar None in
          let task_element_pt = TPtr((find_type !ppc_file "Task_element"), []) in
          makeGlobalVar "this_SCOOP__" task_element_pt;
          let uint32_t = (find_type !ppc_file "uint32_t") in
          let uint64_t = (find_type !ppc_file "uint64_t") in
          makeGlobalVar "block_index_start_SCOOP__" uint32_t;
          makeGlobalVar "e_addr_SCOOP__" uint64_t;
          makeGlobalVar "_tmptime1_SCOOP__" uint64_t;
          makeGlobalVar "_tmptime2_SCOOP__" uint64_t;
        )
        (* const int tpc_task_arguments_list[]; *)
        | "XPPFX" -> (
          makeGlobalVar (Some (SingleInit(zero))) "tpc_task_arguments_list" (TArray(TInt(IInt, [Attr("const", [])]), None, []));
        )
        | _ -> ()
      );
      add_at_top !ppc_file !globals;

      (* SDAM *)
      if ( !arch = "bddt" ||
           !arch = "adam" ||
           !arch = "myrmics" ||
           !arch = "cellgod" ||
           !arch = "PAPAC" ||
           !arch = "XPPFX" ) then
        (Ptdepa.find_dependencies f !dis_sdam);

      Cil.iterGlobals !ppc_file
        (function
          GFun(fd,_) ->
            currentFunction := fd;
            ignore(visitCilFunction (fspuVisitor :> Cil.cilVisitor) fd);
        | _ -> ()
        )
      ;

      (* copy all globals except the function declaration of "tpc_call_tpcAD65" *)
      (!ppc_file).globals <- List.filter isNotSkeleton (!ppc_file).globals;

      (* tasks  (new_tpc * old_original * args) *)
      let (_, tasks) = List.split (L.rev fspuVisitor#getTasks) in
      if (!arch = "cellgod") then (
        (!ppc_file).globals <- (make_null_task_table tasks)::((!ppc_file).globals);
        (!spu_file).globals <- (!spu_file).globals@[(make_task_table "Task_table" tasks)]
      ) else if ( !arch = "adam" || !arch = "bddt" ) then (
        (!ppc_file).globals <- ((!ppc_file).globals)@[(make_task_table "Task_table" tasks)]
      ) else if ( !arch = "myrmics" ) then (
        let main = find_function_sign !ppc_file !myrmics_main in
        let tasks = (dummyFunDec, main, [])::tasks in
        (!ppc_file).globals <- ((!ppc_file).globals)@[(make_task_table (!myrmics_main^"_task_table") tasks)]
      );

      (* execute_task is redundant in x86*)
      if !isCell then
        (!spu_file).globals <- (!spu_file).globals@[Scoop_make_exec.make_exec_func !arch !spu_file tasks];

      (* eliminate dead code *)
(*        Cfg.computeFileCFG !ppc_file;
      Deadcodeelim.dce !ppc_file;
      Cfg.computeFileCFG !spu_file;
      Deadcodeelim.dce !spu_file;*)

(*         Scoop_rmtmps.removeUnused !ppc_file; *)
      writeFile !ppc_file;
      if !isCell then
        writeFile !spu_file;
    );
    fd_post_check = true;
  }
