(****************************************************************************)
(* Copyright (c) 2010-13,                                                   *)
(*                        Foivos    Zakkak          <zakkak@ics.forth.gr>   *)
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

(** Responsible for generating code for the myrmics runtime on the
    formic architecture
    @author Foivos Zakkak, zakkak\@ics.forth.gr *)

open Cil
module SU = Scoop_util
module L  = List
module E  = Errormsg

(** the main function in the file, default is main *)
let myrmics_main = ref "main"

let options = [
  "--myrmics-main-function",
  Arg.String(fun s -> myrmics_main := s),
  " SCOOP: Specify the name of the main function for the myrmics runtime (default: main)";
]

let mkSet table i arg =
  let t = (
    match typeOfLval table with
      TArray(t', _, _) -> t'
    | t' -> t'
  ) in
  let curr = addOffsetLval (Index(integer i, NoOffset)) table in
  Set( curr, CastE(t, arg), locUnknown)

class codegen (cgraph : Callgraph.callgraph) file pragma includePath =
object (self) inherit Scoop_codegen.codegen cgraph file pragma includePath as super

  (* This is needed for SDAM *)
  val query_id           = ref 0
  (* keeps the current funcid for the new tpc_function *)
  val func_id             = ref 0
  val scoop_wait_on = "_sys_wait_on"
  val runtime       = "myrmics"

  method makeTaskTable : unit =
    (* tasks  (new_tpc * old_original * args) *)
    let (_, tasks) = List.split (L.rev found_tasks) in
    let main = SU.find_function_sign new_file !myrmics_main in
    let tasks = (dummyFunDec, main, [])::tasks in
    new_file.globals <-
      (new_file.globals)@[(SU.make_task_table (!myrmics_main^"_task_table") tasks)]

  method private make_wait_on (loc : location) (exps: attrparam list) (s: stmt)
         : stmt visitAction =
    let two = SU.find_function_sign new_file scoop_wait_on in
    let num_args = L.length exps in
    let scoop2179_args =
      try
        let args = SU.__find_local_var !SU.currentFunction "scoop2179_wo_args" in
        (* Check if the array is large enough for this wait on *)
        ( match args.vtype with
            TArray(_, Some(Const(CInt64(size, _, _))), _) ->
            if (L.length exps) > (i64_to_int size) then
              args.vtype <- TArray(voidPtrType, Some(integer num_args), []);
            | _ -> assert false;
        );
        args
      with Not_found ->
        makeLocalVar !SU.currentFunction "scoop2179_wo_args" (TArray(voidPtrType, Some(integer num_args), []))
    in
    let args = L.rev_map (SU.attrParamToExp new_file loc) exps in
    let i = ref num_args in
    let mkSet = mkSet (var scoop2179_args) in
    let init_args = L.rev_map (fun a -> decr i; mkSet !i a) args in
    let filename = Const(CStr(loc.file)) in
    let instr = Call (None, Lval (var two), [filename; integer loc.line; Lval (var scoop2179_args); integer num_args], locUnknown) in
    let s' = {s with pragmas = List.tl s.pragmas} in
    ChangeDoChildrenPost ((mkStmt (Block (mkBlock [ mkStmt (Instr(init_args)); mkStmtOneInstr instr; s' ]))), fun x -> x)

  method declareGlobals : unit = ();

  method preprocessAndMergeWithHeader flags : unit = ();

  (** Creates two arrays, one containing the arguments' addresses and another
   * containing the arguments' types the invokes _sys_spawn with those as args
   * @param loc the current file location
   * @param func_vi the varinfo of the original function
   * @param oargs the original arguments
   * @param args the arguments from the pragma directive
   * @param f the file we are modifying
   * @return the stmts that will replace the call paired with a list of numbered
   * argument descriptors
   *)
  method make_task_spawn (loc: location) (func_vi: varinfo) (oargs: exp list)
                         (args: SU.arg_descr list)
         : (stmt list * (int * SU.arg_descr) list) =

    let args_num = List.length oargs in
    let args_num_i = integer args_num in

    (* Try to find locally an array with the argument descriptors *)
    let scoop2179_args =
      try
        let args = SU.__find_local_var !SU.currentFunction "scoop2179_s_args" in
        (* Check if the array is large enough for this spawn *)
        ( match args.vtype with
          | TArray(t, Some(Const(CInt64(size, _, _))), _) ->
            if args_num > (i64_to_int size) then
              args.vtype <- TArray(t, Some(args_num_i), []);
          | _ -> assert false;
        );
        var args
      with Not_found ->
        var (makeLocalVar !SU.currentFunction "scoop2179_s_args" (TArray(voidPtrType, Some(args_num_i), [])))
    in

    let scoop2179_typs =
      try
        let typs = SU.__find_local_var !SU.currentFunction "scoop2179_t_args" in
        (* Check if the array is large enough for this spawn *)
        ( match typs.vtype with
            TArray(t, Some(Const(CInt64(size, _, _))), _) ->
            if args_num > (i64_to_int size) then
              typs.vtype <- TArray(t, Some(args_num_i), []);
            | _ -> assert false;
        );
        var typs
      with Not_found ->
        var (makeLocalVar !SU.currentFunction "scoop2179_t_args" (TArray(uintType, Some(args_num_i), [])))
    in

    let args_n, instrs =
      (* if we have arguments *)
      if (oargs <> []) then (
        let args_n = SU.number_args args oargs in

        (*       ignore(L.map (fun (i, (name, _)) ->  print_endline ("A= "^(string_of_int i)^" "^name) ) args_n); *)

        let args_n = List.sort SU.sort_args_n args_n in
        (*       ignore(L.map (fun (i, (name, _)) ->  print_endline ("B= "^(string_of_int i)^" "^name) ) args_n); *)
        incr query_id;
        let doArgument = self#doArgument scoop2179_args scoop2179_typs func_vi.vname !query_id in
        let mapped = L.flatten (List.map doArgument args_n) in
        (args_n, mapped)
      ) else ([], [])
    in

    (* spawn(taskd); *)
    let tpc_call_f = SU.find_function_sign new_file "spawn" in

    let filename = mkString loc.file in
    (* the funcid is +1 in order to skip the main function which is pushed in the task table at the end *)
    let call_args = [filename;
                     integer loc.line;
                     integer (!func_id+1);
                     Lval scoop2179_args;
                     Lval scoop2179_typs;
                     args_num_i] in
    let instrs = Call (None, Lval (var tpc_call_f), call_args, locUnknown)::instrs in

    incr func_id;
    ([mkStmt (Instr(L.rev instrs))], args_n)

  (** passes the arguments to the arrays in the correct order
   * @param targs the arguments' table
   * @param ttyps the types' table
   * @param orig_tname the original function's name
   * @param tid an id marking the query (needed by SDAM)
   * @param arg the argument we want to place
   * @return the generated Set instrs
   *)
  method private doArgument targs ttyps (orig_tname: string) (tid: int)
                 (arg: (int * SU.arg_descr) ) : instr list =
    let (i_m, arg_desc) = arg in
    let arg_addr = arg_desc.SU.address in
    let arg_type = arg_desc.SU.atype in
    let arg_name = arg_desc.SU.aname in

    let arg_type_tmp = SU.arg_type2int arg_type in
    let arg_type_tmp =
      (* TPC_BYVALUE_ARG; *)
      if (SU.isScalar arg_desc) then (
        0x9
      (* invoke isSafeArg from PtDepa to check whether this argument is a no dep *)
      (* arg_flag|TPC_SAFE_ARG; *)
      ) else if (Sdam.isSafeArg orig_tname tid arg_name) then (
        arg_type_tmp lor 0x8
      ) else (
        arg_type_tmp
      )
    in
    let arg_type_tmp =
      (* arg_flag|TPC_REGION_ARG; *)
      if (SU.isRegion arg_desc) then (
        arg_type_tmp lor 0x10
      ) else if (SU.isNTRegion arg_desc) then (
        arg_type_tmp lor 0x14
      ) else
        arg_type_tmp
    in

    [mkSet targs i_m arg_addr; mkSet ttyps i_m (integer arg_type_tmp)]

  (** parses the #pragma ... task arguments *)
  method private process_task_pragma loc pragma_args =
    let process_task_pragma = self#process_task_pragma loc in
    match pragma_args with
    | ACons("safe", args)::rest ->
      (* ignore safe tags, it's a hint for the analysis, let it handle it *)
      process_task_pragma rest
    (* support notransfer in/out/inout(a,b,c) etc. *)
    | AStr("notransfer")::(ACons(arg_typ, args)::rest) ->
      let arg_f = SU.str2arg_flow arg_typ loc in
      let process_regs = function
        | ACons(varname, []) ->
          let vi = SU.find_scoped_var loc !SU.currentFunction file varname in
          let tmp_addr = Lval(var vi) in
          let tmp_t = SU.NTRegion(arg_f, [varname]) in
          { SU.aname=varname; SU.address=tmp_addr; SU.atype=tmp_t;}
        | _ -> E.s (errorLoc loc "Syntax error in #pragma ... task %s(...)\n" arg_typ);
      in
      let lst = process_task_pragma rest in
      (L.map process_regs args)@lst
    (* support region in/out/inout(a,b,c) *)
    | AStr("region")::(ACons(arg_typ, args)::rest) ->
      let arg_f = SU.str2arg_flow arg_typ loc in
      let process_regs = function
        | ACons(varname, []) ->
          let vi = SU.find_scoped_var loc !SU.currentFunction file varname in
          let tmp_addr = Lval(var vi) in
          let tmp_t = SU.Region(arg_f, [varname]) in
          { SU.aname=varname; SU.address=tmp_addr; SU.atype=tmp_t;}
        | _ -> E.s (errorLoc loc "Syntax error in #pragma ... task %s(...)\n" arg_typ);
      in
      let lst = process_task_pragma rest in
      (L.map process_regs args)@lst
    | (ACons(arg_typ, args)::rest) ->
      let lst = process_task_pragma rest in
      (SU.scoop_process_args false file arg_typ loc args)@lst
    | [] -> []
    | _ -> E.s (errorLoc loc "Syntax error in #pragma ... task\n");

  method vstmt (s: stmt) : stmt visitAction =
    let debug = ref false in
    let prags = s.pragmas in
    if (prags <> []) then (
      match (List.hd prags) with
      (* Support #pragma ... ... *)
      | (Attr(pr_str, rest), loc) when pr_str = pragma_str -> (
        match rest with
        (* Support #pragma ... wait on(...) *)
        | [AStr("wait"); ACons("on", exps)] ->
          self#make_wait_on loc exps s
        (* Support #pragma ... task... *)
        | AStr("task")::rest -> (
          match s.skind with
          | Instr(Call(_, Lval((Var(vi), _)), oargs, loc)::restInst) -> (
            let funname = vi.vname in
            (* process the pragma ... task*)
            let args    = self#process_task_pragma loc rest in
            SU.dbg_print debug ("Found task \""^funname^"\"");

            (* check whether all argument annotations correlate to an actual argument *)
            let check arg =
              if ( not ((SU.isRegion arg) || (L.exists (fun e -> ((SU.getNameOfExp e)=arg.SU.aname)) oargs)) )then (
                let args_err = ref "(" in
                List.iter (fun e -> args_err := ((!args_err)^" "^(SU.getNameOfExp e)^",") ) oargs;
                args_err := ((!args_err)^")");
                E.s (errorLoc loc "#1 Argument \"%s\" in the pragma directive not found in %s" arg.SU.aname !args_err);
              ) in
            L.iter check args;

            (* create the spawn function *)
            let rest_f2 var_i =
              let (stmts, args) =
                self#make_task_spawn loc var_i oargs args
              in
              found_tasks <- (funname, (dummyFunDec, var_i, args))::found_tasks;
              ChangeTo(mkStmt (Block(mkBlock stmts)) )
            in
            (* try to find the function definition *)
            try
              (* checking for the function definition *)
              let task = SU.find_function_fundec_g new_file.globals funname in
              rest_f2 task.svar
            (* else try to find the function signature/prototype *)
            with Not_found -> (
              let task = SU.find_function_sign new_file funname in
              rest_f2 task
            )

          )
          | Block(b) -> ignore(unimp "Ignoring block pragma"); DoChildren
          | _ -> SU.dbg_print debug "Ignoring pragma"; DoChildren
        )
        (* warn about ignored #pragma ... ... directives *)
        | _ -> ignore(warnLoc loc "Ignoring #pragma %a\n" d_attr (Attr(pragma_str, rest))); DoChildren
      )
      | (_, loc) -> SU.dbg_print debug (loc.file^":"^(string_of_int loc.line)^" Ignoring #pragma directive"); DoChildren
    ) else
      DoChildren

  method getTasks = found_tasks
end
