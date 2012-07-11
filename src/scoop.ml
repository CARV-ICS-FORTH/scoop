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
(** the task table name of myrmics runtime, default is Task_table *)
let myrmics_table = ref "Task_table"
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

    "--myrmics-table-name",
      Arg.String(fun s -> myrmics_table := s),
      " SCOOP: Specify the name of the task table for the myrmics runtime (default: Task_table)";


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

(* create 1 global list (the spe output file) *)
(** holds the processed tasks *)
let spu_tasks = ref []

(** processes recursively the arguments' info found in in() out() and
    inout() directives *)
let rec scoop_process_args typ args loc : arg_descr list =
  let attrParamToExp' = attrParamToExp !ppc_file loc in
  let arg_f = str2arg_flow typ loc in
  match args with
    (* Brand new stride syntax... *)
    (AIndex(AIndex(ACons(varname, []), ABinOp( BOr, bs_r, bs_c)), orig)::rest) ->
      (* Brand new stride syntax with optional 2nd dimension of the original array... *)
      let orig_c = 
        match orig with
          ABinOp( BOr, _, orig_c) -> orig_c
          | _ -> orig
      in
      let vi = find_scoped_var loc !currentFunction !ppc_file varname in
      let tmp_addr = Lval(var vi) in
      let size = SizeOf( getBType vi.vtype vi.vname ) in
      let tmp_bs_c = attrParamToExp' bs_c in
      (* block's row size = bs_c * sizeof(type) *)
      let tmp_bs_c = BinOp(Mult, tmp_bs_c, size, intType) in
      let tmp_bs_r = attrParamToExp' bs_r in
      let tmp_orig_c = attrParamToExp' orig_c in
      (* original array row size = orig_c * sizeof(type) *)
      let tmp_orig_c = BinOp(Mult, tmp_orig_c, size, intType) in
      let tmp_t = Stride(arg_f, tmp_orig_c, tmp_bs_r, tmp_bs_c) in
      { aname=varname; address=tmp_addr; atype=tmp_t;}::(scoop_process_args typ rest loc)
    (* handle strided (legacy) ... *) (* Check documentation for the syntax *)
    | (AIndex(AIndex(ACons(varname, []), varsize), ABinOp( BOr, var_els, var_elsz))::rest) ->
      let vi = find_scoped_var loc !currentFunction !ppc_file varname in
      let tmp_addr = Lval(var vi) in
      let tmp_size = attrParamToExp' varsize in
      let tmp_els = attrParamToExp' var_els in
      let tmp_elsz = attrParamToExp' var_elsz in
      let tmp_t = Stride(arg_f, tmp_size, tmp_els, tmp_elsz) in
      { aname=varname; address=tmp_addr; atype=tmp_t;}::(scoop_process_args typ rest loc)
    (* variable with its size *)
    | (AIndex(ACons(varname, []), varsize)::rest) ->
      let vi = find_scoped_var loc !currentFunction !ppc_file varname in
      let tmp_addr = Lval(var vi) in
      let tmp_size = 
        if (!arch <> "XPPFX" && !arch<>"PAPAC") then
          attrParamToExp !ppc_file loc varsize
        else (
          let size = SizeOf( getBType vi.vtype vi.vname ) in
          let n = attrParamToExp !ppc_file loc varsize in
          (* argument size = n * sizeof(type) *)
          BinOp(Mult, n, size, intType)
        )
      in
      let tmp_t =
        if (isScalar_v vi) then
          Scalar(arg_f, tmp_size)
        else
          Normal(arg_f, tmp_size)
      in
      { aname=varname; address=tmp_addr; atype=tmp_t;}::(scoop_process_args typ rest loc)
    (* support optional sizes example int_a would have size of sizeof(int_a) *)
    | (ACons(varname, [])::rest) ->
      let vi = find_scoped_var loc !currentFunction !ppc_file varname in
      let tmp_addr = Lval(var vi) in
      let tmp_size = SizeOf( getBType vi.vtype vi.vname ) in
      let tmp_t =
        if (isScalar_v vi) then
          Scalar(arg_f, tmp_size)
        else
          Normal(arg_f, tmp_size)
      in
      { aname=varname; address=tmp_addr; atype=tmp_t;}::(scoop_process_args typ rest loc)
    | [] -> []
    | _ -> E.s (errorLoc loc "Syntax error in #pragma %s task %s(...)\n" (!pragma_str) typ)

(** parses the #pragma css task arguments *)
let rec scoop_process pragma loc =
  match pragma with
    (AStr("highpriority")::rest) ->
      let (_, lst) = scoop_process rest loc in
      (true, lst)
    | (ACons("safe", args)::rest) ->
      (* kasas' mess here *)
      (* ignore safe tags, it's a hint for the analysis *)
      scoop_process rest loc
    (* support region r in(a,b,c) etc. *)
    | AStr("region")::(AStr(region)::(ACons(arg_typ, args)::rest)) ->
      if ( not (!arch = "adam" || !arch = "bddt") ) then (
        E.s (unimp "%a\n\tThis region syntax is not supported in %s" d_loc loc !arch);
      ) else (
        let (hp, lst) = scoop_process rest loc in
        let r_vi = find_scoped_var loc !currentFunction !ppc_file region in
        let tmp_addr = Lval(var r_vi) in
        let args_l = List.map
          (fun a -> match a with
              ACons(name, []) -> name
            | _ -> E.s (errorLoc loc "#pragma %s task region %s %s(...) should include only variable names" (!pragma_str) region arg_typ);
          ) args
        in
        let tmp_t = Region(str2arg_flow arg_typ loc, args_l) in
        (hp, { aname=region; address=tmp_addr; atype=tmp_t;}::lst)
      )
    (* support region in/out/inout(a,b,c) *)
    | AStr("region")::(ACons(arg_typ, args)::rest) ->
      if ( !arch <> "myrmics" ) then (
        E.s (unimp "%a\n\tThis region syntax is not supported in %s" d_loc loc !arch);
      ) else (
        let arg_f = str2arg_flow arg_typ loc in
        let process_regs = function
          | ACons(varname, []) ->
            let vi = find_scoped_var loc !currentFunction !ppc_file varname in
            let tmp_addr = Lval(var vi) in
            let tmp_t = Region(arg_f, [varname]) in
            { aname=varname; address=tmp_addr; atype=tmp_t;}
          | _ -> E.s (errorLoc loc "Syntax error in #pragma %s task %s(...)\n" (!pragma_str) arg_typ);
        in
        let (hp, lst) = scoop_process rest loc in
        (hp, (L.map process_regs args)@lst)
      )
    | (ACons(arg_typ, args)::rest) ->
      let (hp, lst) = scoop_process rest loc in
      (hp, (scoop_process_args arg_typ args loc)@lst)
    | [] -> (false, [])
    | _ -> E.s (errorLoc loc "Syntax error in #pragma %s task\n" (!pragma_str));

(** populates the global list of tasks [tasks] *)
class findTaskDeclVisitor cgraph = object
  inherit nopCilVisitor
  val callgraph = cgraph 
  (* visits all stmts and checks for pragma directives *)
  method vstmt (s: stmt) : stmt visitAction =
    let prags = s.pragmas in
    if (prags <> []) then (
      match (List.hd prags) with
        (* Support #pragma css ... *)
        (Attr(pr_str, rest), loc) when pr_str = !pragma_str -> (
          match rest with
          (* Support #pragma css wait on(...) *)
            [AStr("wait"); ACons("on", exps)] -> (
              if (!arch = "XPPFX") then (
                Scoop_XPPFX.make_wait_on !currentFunction !ppc_file loc exps s
              ) else if (!arch = "myrmics") then (
                Scoop_myrmics.make_wait_on !currentFunction !ppc_file loc exps s
              ) else
                DoChildren
          )
          (* Support #pragma css wait all *)
          | [AStr("wait"); AStr("all")]
          (* Support #pragma css barrier*)
          | [AStr("barrier")] -> (
            if (!arch = "myrmics") then 
              E.s (unimp "%a\n\twait all is not supported in %s" d_loc loc !arch)
            else (
              let twa = find_function_sign (!ppc_file) "tpc_wait_all" in
              let instr = Call (None, Lval (var twa), [], locUnknown) in
              let s' = {s with pragmas = List.tl s.pragmas} in
              ChangeDoChildrenPost ((mkStmt (Block (mkBlock [ mkStmtOneInstr instr; s' ]))), fun x -> x)
            )
          )
          (* Support #pragma css start *)
          | [AStr("start")]
          (* Support #pragma css start(...) *)
          | [ACons("start", [])] -> (
            if (!arch = "myrmics") then
              E.s (unimp "%a\n\tstart is not supported in %s" d_loc loc !arch)
            else (
              let ts = find_function_sign (!ppc_file) "tpc_init" in
              let instr = Call (None, Lval (var ts), [], locUnknown) in
              let s' = {s with pragmas = List.tl s.pragmas} in
              ChangeDoChildrenPost ((mkStmt (Block (mkBlock [ mkStmtOneInstr instr; s' ]))), fun x -> x)
            )
          )
          | [ACons("start", exp::rest)] -> (
            if (!arch = "myrmics") then
              E.s (unimp "%a\n\tstart is not supported in %s" d_loc loc !arch)
            else (
              let ts = find_function_sign (!ppc_file) "tpc_init" in
              let args =
                if (!arch="cell") then
                  [attrParamToExp !ppc_file loc exp]
                else if (!arch="cellgod" || !arch="PAPAC") then
                  attrParamToExp !ppc_file loc exp::[attrParamToExp !ppc_file loc (L.hd rest)]
                else (
                  match rest with
                    first::second::_ -> attrParamToExp !ppc_file loc exp::(attrParamToExp !ppc_file loc first::[attrParamToExp !ppc_file loc second])
                    | _ -> E.s (errorLoc loc "#pragma %s start takes 3 arguments" (!pragma_str))
                )
              in
              let instr = Call (None, Lval (var ts), args, locUnknown) in
              let s' = {s with pragmas = List.tl s.pragmas} in
              ChangeDoChildrenPost ((mkStmt (Block (mkBlock [ mkStmtOneInstr instr; s' ]))), fun x -> x)
            )
          )
          (* Support #pragma css finish *)
          | [AStr("finish")] -> (
            if (!arch = "myrmics") then
              E.s (unimp "%a\n\tfinish is not supported in %s" d_loc loc !arch)
            else (
              let ts = find_function_sign (!ppc_file) "tpc_shutdown" in
              let instr = Call (None, Lval (var ts), [], locUnknown) in
              let s' = {s with pragmas = List.tl s.pragmas} in
              ChangeDoChildrenPost ((mkStmt (Block (mkBlock [ mkStmtOneInstr instr; s' ]))), fun x -> x)
            )
          )
          (* Support #pragma css malloc *)
          | [AStr("malloc")] -> (
            if (!arch = "myrmics") then
              E.s (unimp "%a\n\ttpc_malloc is not supported in %s" d_loc loc !arch)
            else (
              let tm = find_function_sign (!ppc_file) "tpc_malloc" in
              match s.skind with
                  Instr(Call(Some res, Lval((Var(vi), _)), oargs, loc)::restInst) -> (


                    let instr = Call (Some res, Lval (var tm), oargs, locUnknown) in
                    ChangeTo(mkStmtOneInstr instr)
                  )
                | _ -> DoChildren
            )
          )
          (* Support #pragma css free *)
          | [AStr("free")] -> (
            if (!arch = "myrmics") then
              E.s (unimp "%a\n\ttpc_malloc is not supported in %s" d_loc loc !arch)
            else (
              let tf = find_function_sign (!ppc_file) "tpc_free" in
              match s.skind with
                  Instr(Call(_, Lval((Var(vi), _)), oargs, loc)::restInst) -> (


                    let instr = Call (None, Lval (var tf), oargs, locUnknown) in
                    ChangeTo(mkStmtOneInstr instr)
                  )
                | _ -> DoChildren
            )
          )
          (* Support #pragma css task... *)
          | AStr("task")::rest -> (
            match s.skind with
            Instr(Call(_, Lval((Var(vi), _)), oargs, loc)::restInst) -> (
              let funname = vi.vname in
              let (is_hp, args) = scoop_process rest loc in
              dbg_print debug ("Found task \""^funname^"\"");


              if !isCell then ( (* CELL *)
                let rest_f new_fd =
                  (* add arguments to the call *)
                  let call_args =
                    let expS2P = expScalarToPointer loc in
                    ref (L.rev (L.map expS2P oargs))
                  in

                  (* for each actual argument of the call find it's (pragma)
                      declared size and push it to the argument list of the
                      new call *)
                  let rec getSizeNstride = function
                    | Lval ((Var(vi),_))
                    | StartOf ((Var(vi),_)) -> (
                      try
                        let arg_desc = L.find (fun a -> (a.aname=vi.vname)) args in
                        let vsize = getSizeOfArg arg_desc in
                        call_args := vsize::!call_args;
                        if (isStrided arg_desc) then (
                          let (vels, velsz) =
                            match arg_desc.atype with
                                Stride(_, _, els, elsz) -> (els, elsz)
                              | _ -> assert false
                          in
                          call_args := velsz::!call_args;
                          call_args := vels::!call_args;
                        );
                      with Not_found ->
                        E.s (errorLoc loc "You probably forgot to add \"%s\" in the pragma directive\n" vi.vname)
                    )
                    | CastE (_, ex') -> getSizeNstride ex';
                    (* The following are not supported yet *)
                    | Const _ -> raise (Invalid_argument "Const");
                    | SizeOf _ -> raise (Invalid_argument "Sizeof");
                    | SizeOfE _ -> raise (Invalid_argument "SizeofE");
                    | SizeOfStr _ -> raise (Invalid_argument "SizeofStr");
                    | AlignOf _ -> raise (Invalid_argument "Alignof");
                    | AlignOfE _ -> raise (Invalid_argument "AlignOfE");
                    | UnOp _ -> raise (Invalid_argument "UnOp");
                    | BinOp _ -> raise (Invalid_argument "BinOp");
                    | AddrOf _ -> raise (Invalid_argument "AddrOf");
                    | _ -> raise (Invalid_argument "Uknown");
                  in
                  L.iter getSizeNstride oargs;

                  let instr = Call (None, Lval (var new_fd.svar), L.rev !call_args, locUnknown) in
                  let call = mkStmt (Instr(instr::restInst)) in
                  ChangeTo(call)
                in
                try
                  (* fast workaround *)
                  if (!arch = "cell" ) then
                    (* check if we have seen this function before *)
                    let (new_fd, _, _) = List.assoc funname !spu_tasks in
                    rest_f new_fd
                  else
                    raise Not_found
                with Not_found -> (
                  let rest_f2 var_i =
                    (* select the function to create the custom tpc_calls *)
                    let make_tpc_funcf = match !arch with
                        "cell" -> Scoop_cell.make_tpc_func
                      | _ (*"cellgod"*) -> Scoop_cellgod.make_tpc_func
                    in
                    let (new_fd, args) = make_tpc_funcf loc var_i oargs args ppc_file spu_file in
                    add_after_s !ppc_file var_i.vname new_fd;
                    spu_tasks := (funname, (new_fd, var_i, args))::!spu_tasks;
                    rest_f new_fd
                  in
                  (* try to find the function definition *)
                  try
                    (* checking for the function definition *)
                    let task = find_function_fundec_g (!ppc_file.globals) funname in
                    (* copy itself and the callees *)
                    deep_copy_function funname callgraph !spu_file !ppc_file;
                    rest_f2 task.svar
                  (* else try to find the function signature/prototype *)
                  with Not_found -> (
                    let task = find_function_sign (!ppc_file) funname in
                    rest_f2 task
                  )
                )
              ) else ( (* END CELL *)

                (* check whether all argument annotations correlate to an actual argument *)
                let check arg =
                  if ( not ((isRegion arg) || (L.exists (fun e -> ((getNameOfExp e)=arg.aname)) oargs)) )then (
                    let args_err = ref "(" in
                    List.iter (fun e -> args_err := ((!args_err)^" "^(getNameOfExp e)^",") ) oargs;
                    args_err := ((!args_err)^")");
                    E.s (errorLoc loc "#1 Argument \"%s\" in the pragma directive not found in %s" arg.aname !args_err);
                  ) in
                L.iter check args;

                let rest_f2 var_i =
                  (* select the function to create the issuer *)
                  let make_tpc_issuef = match !arch with
                      "adam" -> Scoop_adam.make_tpc_issue is_hp
                    | "bddt" -> Scoop_bddt.make_tpc_issue is_hp
                    | "XPPFX" -> Scoop_XPPFX.make_tpc_issue is_hp
                    | "PAPAC" -> Scoop_PAPAC.make_tpc_issue is_hp
                    | "myrmics" -> Scoop_myrmics.make_tpc_issue is_hp
                    | _ -> E.s (unimp "Runtime \"%s\" doesn't have a make_tpc_issue yet" !arch);
                  in
                  let (stmts, args) = make_tpc_issuef loc var_i oargs args !ppc_file !currentFunction in
                  spu_tasks := (funname, (dummyFunDec, var_i, args))::!spu_tasks;
                  ChangeTo(mkStmt (Block(mkBlock stmts)) )
                in
                (* try to find the function definition *)
                try
                  (* checking for the function definition *)
                  let task = find_function_fundec_g (!ppc_file.globals) funname in
                  rest_f2 task.svar
                (* else try to find the function signature/prototype *)
                with Not_found -> (
                  let task = find_function_sign (!ppc_file) funname in
                  rest_f2 task
                )
              )


            )
            | Block(b) -> ignore(unimp "Ignoring block pragma"); DoChildren
            | _ -> dbg_print debug "Ignoring pragma"; DoChildren
          )
          (* warn about ignored #pragma css ... directives *)
          | _ -> ignore(warnLoc loc "Ignoring #pragma %a\n" d_attr (Attr(!pragma_str, rest))); DoChildren
        )
        | (_, loc) -> dbg_print debug (loc.file^":"^(string_of_int loc.line)^" Ignoring #pragma directive"); DoChildren
    ) else 
      DoChildren
end

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
      let fspuVisitor = new findTaskDeclVisitor callgraph in

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
           !arch = "cellgod" ||
           !arch = "scc" ||
           !arch = "PAPAC" ||
           !arch = "XPPFX" ) then
        (Ptdepa.find_dependencies f !dis_sdam);

      Cil.iterGlobals !ppc_file
        (function
          GFun(fd,_) ->
            currentFunction := fd;
            ignore(visitCilFunction fspuVisitor fd);
        | _ -> ()
        )
      ;

      (* copy all globals except the function declaration of "tpc_call_tpcAD65" *)
      (!ppc_file).globals <- List.filter isNotSkeleton (!ppc_file).globals;

      (* tasks  (new_tpc * old_original * args) *)
      let (_, tasks) = List.split (L.rev !spu_tasks) in
      if (!arch = "cellgod") then (
        (!ppc_file).globals <- (make_null_task_table tasks)::((!ppc_file).globals);
        (!spu_file).globals <- (!spu_file).globals@[(make_task_table "Task_table" tasks)]
      ) else if ( !arch = "adam" || !arch = "bddt" ) then (
        (!ppc_file).globals <- ((!ppc_file).globals)@[(make_task_table "Task_table" tasks)]
      ) else if ( !arch = "myrmics" ) then (
        (!ppc_file).globals <- ((!ppc_file).globals)@[(make_task_table !myrmics_table tasks)]
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
