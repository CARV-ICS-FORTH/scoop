(*
 *
 * Copyright (c) 2010,
 *  Foivos Zakkak        <zakkak@ics.forth.gr>
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
open Scoop_util
module L = List
module E = Errormsg

(* keeps the current funcid for the new tpc_function *)
let func_id = ref 0

(* XXX: this is for sdam *)
let querie_no = ref 0

(* a unique id for the tpc_function_* *)
let un_id = ref 0

let makeGlobalVar n t f=
  let v = makeGlobalVar n t in
  let glob = GVar(v, {init = None;}, locUnknown) in
  add_at_top f [glob];
  v

let doRegions (loc: location) (this: lval) (f: file) (args: arg_descr list )  (orig_tname: string) (tid: int): stmt list = (
  let stl = ref [] in
  let addAttribute_Task = find_function_sign f ("AddAttribute_Task") in
  let sizeOf_region_t = SizeOf(find_type f "region_t") in
  let block_size = var (find_global_var f "block_size_g") in
  let uint64_t = (find_type f "uint64_t") in
  try
    (* Go through the arguments defined in pragma *)
    L.iter (fun arg ->
      (* For the region arguments check whether they are safe and push them in DAG *)
      if (isRegion arg && not (Sdam.isSafeArg orig_tname tid arg.aname)) then (
        (*// actually ceil(arg_size/BLOCK_SZ)
          register unsigned int blocks_num = 1U + ((arg_size - 1U) / BLOCK_SZ);
          AddAttribute_Task(this, arg_addr64, arg_flag, BLOCK_SZ, blocks_num);
        *)
        let minus = (BinOp(MinusA, sizeOf_region_t, one, uint64_t)) in
        let plus = (BinOp(PlusA, one, minus, uint64_t)) in
        let div = BinOp(Div, plus, Lval block_size, uint64_t) in
        let args = [Lval this; CastE(voidPtrType, arg.address); arg_type2integer arg.atype; Lval block_size; div ] in
        let st = mkStmtOneInstr (Call (None, Lval (var addAttribute_Task), args, locUnknown)) in
        stl := st::(!stl);
      )
    ) args;
    (L.rev !stl)
   with Not_found -> []
)

(*(* Preprocess the header file <header> and merges it with f.  The
 * given header should be in the gcc include path.  Modifies f
 *) (* the original can be found in lockpick.ml *)
let preprocessAndMergeWithHeader_x86 (f: file) (header: string) (def: string)
    : unit = (
  (* //Defining _GNU_SOURCE to fix "undefined reference to `__isoc99_sscanf'" *)
  ignore (Sys.command ("echo | gcc -E -D_GNU_SOURCE "^def^" "^header^" - >/tmp/_cil_rewritten_tmp.h"));
  let add_h = Frontc.parse "/tmp/_cil_rewritten_tmp.h" () in
  let f' = Mergecil.merge [add_h; f] "stdout" in
  f.globals <- f'.globals;
)*)

(* make a tpc_ version of the function (for use on the ppc side)
 * uses the tpc_call_tpcAD65 from tpc_skeleton_tpc.c as a template
 *)
let make_tpc_issue (is_hp: bool) (loc: location) (func_vi: varinfo) (oargs: exp list)
    (args: arg_descr list) (f: file) (cur_fd: fundec) : (stmt list * (int * arg_descr) list) = (
  incr un_id;
(*   print_endline ("Creating tpc_function_" ^ func_vi.vname ^ (string_of_int !un_id)); *)
(*   let args = List.sort sort_args args in *)

  let this = var (find_global_var f "this_SCOOP__") in
  (* this->closure.funcid = (uint8_t)funcid; *)
  let this_closure = mkFieldAccess this "closure" in
  let stmts = ref [] in
  let instrs = ref [] in

  let uint64_t = (find_type f "uint64_t") in

  (* G_ppe_stats.stat_tpc_per_spe[0] += 1; *)
  let gps = var (find_global_var f "G_ppe_stats") in
  let gps_stps = mkFieldAccess gps "stat_tpc_per_spe" in
  let idxlv = addOffsetLval (Index(zero, NoOffset)) gps_stps in
  instrs := Set (idxlv, (BinOp(PlusA, Lval idxlv, one, intType)), locUnknown)::!instrs;
  (* this = AddTask(); *)
  let addTask = find_function_sign f "AddTask" in
  instrs := Call (Some this, Lval (var addTask), [], locUnknown)::!instrs;
  (* while (this == NULL)
     {
      tpr_barrier();
      this = AddTask();
     } *)
  let tpr_barrier = find_function_sign f "tpr_barrier" in
  let ins = [Call (Some this, Lval (var addTask), [], locUnknown)] in
  let ins = Call (None, Lval (var tpr_barrier), [], locUnknown)::ins in
  let wbody = [mkStmt (Instr ins)] in
  stmts := mkWhile (BinOp(Eq, Lval this, CastE(voidPtrType, zero), boolType)) wbody;
  stmts := mkStmt (Instr (List.rev !instrs))::!stmts;
  (* this->notAvailable = 1; *)
  instrs := [Set (mkFieldAccess this "notAvailable", one, locUnknown) ];
  (* TIMER_START(1); *)
  let tmptime1 = var (find_global_var f "_tmptime1_SCOOP__") in
  let rdtsc = find_function_sign f "rdtsc" in
  instrs := Call (Some tmptime1, Lval (var rdtsc), [], locUnknown)::!instrs;
  (* this->closure.funcid = (uint8_t)funcid; *)
  instrs := Set (mkFieldAccess this_closure "funcid",
    CastE(find_type f "uint8_t", integer !func_id), locUnknown)::!instrs;
  (* this->closure.total_arguments = 0 *)
  let total_arguments = mkFieldAccess this_closure "total_arguments" in
  instrs := Set (total_arguments, zero, locUnknown)::!instrs;

  (* uint32_t block_index_start *)
  let bis = var (find_global_var f "block_index_start_SCOOP__") in



  (****************************************************************************)
  (* required by doArgument *)
  let arguments = mkFieldAccess this_closure "arguments" in
  let idxlv = addOffsetLval (Index(Lval total_arguments, NoOffset)) arguments in
  let stride = mkFieldAccess idxlv "stride" in
  let size = mkFieldAccess idxlv "size" in
  let flag = mkFieldAccess idxlv "flag" in
  let eal_in = mkFieldAccess idxlv "eal_in" in
  let eal_out = mkFieldAccess idxlv "eal_out" in
  let block_size = var (find_global_var f "block_size_g") in
  let pplus = (BinOp(PlusA, Lval total_arguments, one, intType)) in

  (* Declare doArgument *)
  let doArgument (arg: exp): stmt list = (
    let arg_name = getNameOfExp arg in
    let stl = ref [] in
    let il = ref [] in

    let doSafeArg typ =
      (* this->closure.arguments[  this->closure.total_arguments ].stride=BLOCK_SZ; *)
      il := Set(stride, Lval block_size, locUnknown)::!il;
      (*  this->closure.arguments[this->closure.total_arguments].eal_in = arg_addr64;
          this->closure.arguments[this->closure.total_arguments].eal_out = arg_addr64;
          this->closure.arguments[this->closure.total_arguments].size = arg_size;*)
      il := Set(eal_in, CastE(voidPtrType, CastE(uint64_t, arg)), locUnknown)::!il;
      il := Set(eal_out, CastE(voidPtrType, CastE(uint64_t, arg)), locUnknown)::!il;
(*       il := Set(size, SizeOf( getBType vi.vtype vi.vname ), locUnknown)::!il; *)
      il := Set(size, zero, locUnknown)::!il;
      (* this->closure.arguments[  this->closure.total_arguments ].flag = IN|TPC_START_ARG|TPC_SAFE_ARG; *)
      il := Set(flag, integer typ, locUnknown)::!il;
      (* this -> closure.total_arguments++; *)
      il := Set(total_arguments, pplus, locUnknown)::!il;
      [mkStmt(Instr (L.rev !il))]
    in

    (* if it is annotated *)
    try (
      let arg_desc = L.find (fun a -> (arg_name=a.aname)) args in
      let arg_addr = arg_desc.address in
      let arg_type = arg_desc.atype in
      let arg_size = getSizeOfArg arg_desc in
    (*   print_endline ("Doing "^arg_name); *)
      let t = typeOfLval arguments in
      assert(isArrayType t);
      (* this->closure.arguments[  this->closure.total_arguments ].stride=TPC_IS_STRIDEARG(arg_flag)? va_arg(arg_list, int):0; *)
      il :=
        ( if (isStrided arg_desc) then
            Set(stride, arg_size, locUnknown)
          else
            Set(stride, Lval block_size, locUnknown)
        )::!il;

    (*  this->closure.arguments[this->closure.total_arguments].eal_in = arg_addr64;
      this->closure.arguments[this->closure.total_arguments].eal_out = arg_addr64;
      this->closure.arguments[this->closure.total_arguments].size = arg_size;
      this->closure.arguments[this->closure.total_arguments].flag = arg_flag;*)
      il := Set(eal_in, CastE(voidPtrType, arg_addr), locUnknown)::!il;
      il := Set(eal_out, CastE(voidPtrType, arg_addr), locUnknown)::!il;
      il := Set(size, arg_size, locUnknown)::!il;
    (*   il := Set(flag, integer (arg_type2int arg_type), locUnknown)::!il; *)

  (* TODO: take a look at it *)
      (*#ifdef USE_NUMA
          this->closure.arguments[this->closure.total_arguments].numa_node = -1; //check ranges in the grid
      #endif*)

      (* invoke isSafeArg from PtDepa to check whether this argument is a no dep *)
      if (Sdam.isSafeArg func_vi.vname !querie_no arg_name) then (
        (*
          if(TPC_IS_SAFEARG(arg_flag)) {
            this->closure.arguments[  this->closure.total_arguments ].flag = arg_flag|TPC_START_ARG;
            this->closure.total_arguments++;
            continue;
          }
          #define TPC_START_ARG   0x10
          #define TPC_SAFE_ARG    0x8
        *)
        (* We have to add TPC_SAFE_ARG because in SCOOP SAFE is not in arg_type *)
        il := Set(flag, integer ( (arg_type2int arg_type) lor 0x18), locUnknown)::!il;
        il := Set(total_arguments, pplus, locUnknown)::!il;
        stl := [mkStmt(Instr (L.rev !il))];
      ) else (
        (* this->closure.arguments[this->closure.total_arguments].flag = arg_flag; *)
        il := Set(flag, integer (arg_type2int arg_type), locUnknown)::!il;
        (* uint32_t block_index_start=this->closure.total_arguments; *)
        il := Set(bis, Lval total_arguments, locUnknown)::!il;

        let addAttribute_Task = find_function_sign f ("AddAttribute_Task") in
        if (isStrided arg_desc) then (
          (*  if(TPC_IS_STRIDEARG(arg_flag)){
                AddAttribute_Task(this, arg_addr64, arg_flag & ~TPC_STRIDE_ARG,
                        TPC_EXTRACT_STRIDEARG_ELEMSZ(arg_size), TPC_EXTRACT_STRIDEARG_ELEMS(arg_size));
            }*)
          let (arg_els, arg_elsz) =
            match arg_type with
                Stride(_, _, els, elsz) -> (els, elsz)
              | _ -> assert false
          in
          let args = [Lval this; CastE(voidPtrType, arg_addr); arg_type2integer arg_type; arg_elsz; arg_els] in
          let st = mkStmtOneInstr( Call (None, Lval (var addAttribute_Task), args, locUnknown) ) in
          stl := st::[mkStmt(Instr (L.rev !il))];
        ) else (
          (*this->closure.arguments[this->closure.total_arguments].stride=BLOCK_SZ;
            register unsigned int blocks_num = 1U + ((arg_size - 1U) / BLOCK_SZ);
            AddAttribute_Task(this, arg_addr64, arg_flag, BLOCK_SZ, blocks_num);*)
          il := Set(stride, Lval block_size, locUnknown)::!il;

          let minus = (BinOp(MinusA, arg_size, one, uint64_t)) in
          let plus = (BinOp(PlusA, one, minus, uint64_t)) in
          let div = BinOp(Div, plus, Lval block_size, uint64_t) in
          let args = [Lval this; CastE(voidPtrType, arg_desc.address); arg_type2integer arg_desc.atype; Lval block_size; div ] in
          let st = mkStmtOneInstr (Call (None, Lval (var addAttribute_Task), args, locUnknown)) in
          stl := st::[mkStmt(Instr (L.rev !il))];

        );
        (* this -> closure.total_arguments++; *)
        stl := mkStmtOneInstr(Set(total_arguments, pplus, locUnknown))::!stl;

        (* this->closure.arguments[ block_index_start ].flag|=TPC_START_ARG;
          tpc_common.h:20:#define TPC_START_ARG   0x10 *)
        let idxlv = addOffsetLval (Index(Lval bis, NoOffset)) arguments in
        let flag = mkFieldAccess idxlv "flag" in
        stl := mkStmtOneInstr(Set(flag, BinOp( BOr, Lval flag, integer 0x10, intType), locUnknown))::!stl;

      );
      L.rev !stl
    )
    with Not_found -> ( (* if it is not annotated *)
      try (* find if this argument belongs in some region *)
        let region = L.find
          (fun a -> match a.atype with
              Region(_, vars) -> L.exists (fun s -> s=arg_name) vars
            | _ -> false
          ) args
        in
        doSafeArg ( (arg_type2int region.atype) lor 0x18)
      with Not_found ->
        (* if the argument has no annotation check whether it's a scalar *)
        match arg with
            Lval(Var vi, NoOffset) -> (
              if (isScalar_v vi) then (
                doSafeArg 0x19
              ) else
                E.s (errorLoc loc "%s has no annotation in the #pragma css task ... and is not a scalar" arg_name);
            )
          | _ -> E.s (errorLoc loc "%s has no annotation in the #pragma css task ...  and is not a variable" arg_name);
    )
  ) in


  (****************************************************************************)

  (* if we have arguments *)
  if (oargs <> []) then (
    incr querie_no;
    let mapped = L.flatten (List.map doArgument oargs) in
    let regions = doRegions loc this f args func_vi.vname !querie_no in
    stmts := (!stmts)@(mkStmt (Instr (List.rev !instrs))::mapped)@(regions);
    instrs := [];
  );


  (*if(TPC_IS_HIGHPRIORITYARG(highpriority_arg))
    {
    this->highpriority = 1;
    }
  *)
  if (is_hp) then (
    let this_highpriority = mkFieldAccess this "highpriority" in
    let hp_set = Set(this_highpriority, one, locUnknown) in
    stmts := (mkStmtOneInstr hp_set)::(!stmts);
  );

  (* assert(this->closure.total_arguments<=MAX_ARGS); *)
  let assert_a = find_function_sign f "assert_args_SCOOP__" in
  instrs := Call (None, Lval (var assert_a), [Lval total_arguments], locUnknown)::!instrs;
  (* tpr_acquire_spinlock(&(this->spinlock)); *)
  let spinlock = mkAddrOf (mkFieldAccess this "spinlock") in
  let tpr_acquire_spinlock = find_function_sign f "tpr_acquire_spinlock" in
  instrs := Call (None, Lval (var tpr_acquire_spinlock), [spinlock], locUnknown)::!instrs;
  (* this->input_dependencies_counter -= this->backup_input_dependencies_counter; *)
  let backup_input_dependencies_counter = mkFieldAccess this "backup_input_dependencies_counter" in
  let input_dependencies_counter = mkFieldAccess this "input_dependencies_counter" in
  let sub = BinOp(MinusA, Lval input_dependencies_counter ,
                          Lval backup_input_dependencies_counter, intType) in
  instrs := Set (input_dependencies_counter, sub, locUnknown)::!instrs;
  (* this->backup_input_dependencies_counter = 0; *)
  instrs := Set (backup_input_dependencies_counter, zero, locUnknown)::!instrs;
  (* this->notAvailable = 0; *)
  instrs := Set (mkFieldAccess this "notAvailable", zero, locUnknown)::!instrs;
  (* tpr_release_spinlock(&(this->spinlock)); *)
  let tpr_release_spinlock = find_function_sign f "tpr_release_spinlock" in
  instrs := Call (None, Lval (var tpr_release_spinlock), [spinlock], locUnknown)::!instrs;
  (* G_ppe_stats.issue_ticks += TIMER_END(1); *)
  let gps_it = mkFieldAccess gps "issue_ticks" in
  let tmptime2 = var (find_global_var f "_tmptime2_SCOOP__") in
  instrs := Call (Some tmptime2, Lval (var rdtsc), [], locUnknown)::!instrs;
  let uint64_t = (find_type f "uint64_t") in
  let sub = BinOp(MinusA, Lval tmptime2 , Lval tmptime1, uint64_t) in
  let acc = BinOp(PlusA, Lval gps_it , sub, uint64_t) in
  instrs := Set (gps_it, acc, locUnknown)::!instrs;
  (* Clear_Handler(this ); *)
  let clear_Handler = find_function_sign f "Clear_Handler" in
  instrs := Call (None, Lval (var clear_Handler), [Lval this], locUnknown)::!instrs;

  stmts := !stmts@[mkStmt (Instr (L.rev !instrs))];

  incr func_id;
  (!stmts, [])
)

(* create 1 global list (the spe output file) *)
(** holds the processed tasks *)
let spu_tasks = ref []

(** parses the #pragma css task arguments *)
let rec scoop_process ppc_file loc pragma =
  let scoop_process = scoop_process ppc_file loc in
  match pragma with
    (AStr("highpriority")::rest) ->
      let (_, lst) = scoop_process rest in
      (true, lst)
    | (ACons("safe", args)::rest) ->
      (* kasas' mess here *)
      (* ignore safe tags, it's a hint for the analysis *)
      scoop_process rest
    (* support region r in(a,b,c) etc. *)
    | AStr("region")::(AStr(region)::(ACons(arg_typ, args)::rest)) ->
      let (hp, lst) = scoop_process rest in
      let r_vi = find_scoped_var loc !currentFunction ppc_file region in
      let tmp_addr = Lval(var r_vi) in
      let args_l = List.map
        (fun a -> match a with
            ACons(name, []) -> name
          | _ -> E.s (errorLoc loc "#pragma css task region %s %s(...) should include only variable names" region arg_typ);
        ) args
      in
      let tmp_t = Region(str2arg_flow arg_typ loc, args_l) in
      (hp, { aname=region; address=tmp_addr; atype=tmp_t;}::lst)
    | (ACons(arg_typ, args)::rest) ->
      let (hp, lst) = scoop_process rest in
      (hp, (scoop_process_args false ppc_file arg_typ loc args)@lst)
    | [] -> (false, [])
    | _ -> E.s (errorLoc loc "Syntax error in #pragma css task\n");

(** populates the global list of tasks [tasks] *)
class findTaskDeclVisitor (cgraph : Callgraph.callgraph) ppc_f pragma =
  object
  inherit nopCilVisitor
  val mutable spu_tasks = []
  val callgraph = cgraph
  val ppc_file = ppc_f
  val pragma_str = pragma
  (* visits all stmts and checks for pragma directives *)
  method vstmt (s: stmt) : stmt visitAction =
    let debug = ref false in
    let prags = s.pragmas in
    if (prags <> []) then (
      match (List.hd prags) with
        (* Support #pragma css ... *)
        (Attr(pr_str, rest), loc) when pr_str = pragma_str -> (
          match rest with
          (* Support #pragma css wait all *)
          | [AStr("wait"); AStr("all")]
          (* Support #pragma css barrier*)
          | [AStr("barrier")] -> (
            let twa = find_function_sign ppc_file "tpc_wait_all" in
            let instr = Call (None, Lval (var twa), [], locUnknown) in
            let s' = {s with pragmas = List.tl s.pragmas} in
            ChangeDoChildrenPost ((mkStmt (Block (mkBlock [ mkStmtOneInstr instr; s' ]))), fun x -> x)
          )
          (* Support #pragma css start *)
          | [AStr("start")]
          (* Support #pragma css start(...) *)
          | [ACons("start", [])] -> (
            let ts = find_function_sign ppc_file "tpc_init" in
            let instr = Call (None, Lval (var ts), [], locUnknown) in
            let s' = {s with pragmas = List.tl s.pragmas} in
            ChangeDoChildrenPost ((mkStmt (Block (mkBlock [ mkStmtOneInstr instr; s' ]))), fun x -> x)
          )
          | [ACons("start", exp::rest)] -> (
            let ts = find_function_sign ppc_file "tpc_init" in
            let args =
              match rest with
                first::second::_ -> attrParamToExp ppc_file loc exp::(attrParamToExp ppc_file loc first::[attrParamToExp ppc_file loc second])
                | _ -> E.s (errorLoc loc "#pragma %s start takes 3 arguments" (pragma_str))
            in
            let instr = Call (None, Lval (var ts), args, locUnknown) in
            let s' = {s with pragmas = List.tl s.pragmas} in
            ChangeDoChildrenPost ((mkStmt (Block (mkBlock [ mkStmtOneInstr instr; s' ]))), fun x -> x)
          )
          (* Support #pragma css finish *)
          | [AStr("finish")] -> (
            let ts = find_function_sign ppc_file "tpc_shutdown" in
            let instr = Call (None, Lval (var ts), [], locUnknown) in
            let s' = {s with pragmas = List.tl s.pragmas} in
            ChangeDoChildrenPost ((mkStmt (Block (mkBlock [ mkStmtOneInstr instr; s' ]))), fun x -> x)
          )
          (* Support #pragma css malloc *)
          | [AStr("malloc")] -> (
            let tm = find_function_sign ppc_file "tpc_malloc" in
            match s.skind with
            | Instr(Call(Some res, Lval((Var(vi), _)), oargs, loc)::restInst) -> (
              let instr = Call (Some res, Lval (var tm), oargs, locUnknown) in
              ChangeTo(mkStmtOneInstr instr)
            )
            | _ -> DoChildren
          )
          (* Support #pragma css free *)
          | [AStr("free")] -> (
            let tf = find_function_sign ppc_file "tpc_free" in
            match s.skind with
            | Instr(Call(_, Lval((Var(vi), _)), oargs, loc)::restInst) -> (
              let instr = Call (None, Lval (var tf), oargs, locUnknown) in
              ChangeTo(mkStmtOneInstr instr)
            )
            | _ -> DoChildren
          )
          (* Support #pragma css task... *)
          | AStr("task")::rest -> (
            match s.skind with
            Instr(Call(_, Lval((Var(vi), _)), oargs, loc)::restInst) -> (
              let funname = vi.vname in
              let (is_hp, args) = scoop_process ppc_file loc rest in
              dbg_print debug ("Found task \""^funname^"\"");

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
                let (stmts, args) =
                  make_tpc_issue is_hp loc var_i oargs args ppc_file !currentFunction
                in
                spu_tasks <- (funname, (dummyFunDec, var_i, args))::spu_tasks;
                ChangeTo(mkStmt (Block(mkBlock stmts)) )
              in
              (* try to find the function definition *)
              try
                (* checking for the function definition *)
                let task = find_function_fundec_g ppc_file.globals funname in
                rest_f2 task.svar
              (* else try to find the function signature/prototype *)
              with Not_found -> (
                let task = find_function_sign ppc_file funname in
                rest_f2 task
              )

            )
            | Block(b) -> ignore(unimp "Ignoring block pragma"); DoChildren
            | _ -> dbg_print debug "Ignoring pragma"; DoChildren
          )
          (* warn about ignored #pragma css ... directives *)
          | _ -> ignore(warnLoc loc "Ignoring #pragma %a\n" d_attr (Attr(pragma_str, rest))); DoChildren
        )
        | (_, loc) -> dbg_print debug (loc.file^":"^(string_of_int loc.line)^" Ignoring #pragma directive"); DoChildren
    ) else
      DoChildren

  method getTasks = spu_tasks
end
