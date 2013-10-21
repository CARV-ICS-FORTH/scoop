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

open Cil
module SU = Scoop_util
module L  = List
module E  = Errormsg

let options = []

let doRegions (loc: location) (this: lval) (f: file) (args: SU.arg_descr list)
              (orig_tname: string) (tid: int): stmt list = (
  let stl = ref [] in
  let addAttribute_Task = SU.find_function_sign f ("AddAttribute_Task") in
  let sizeOf_region_t = SizeOf(SU.find_type f "region_t") in
  let block_size = var (SU.find_global_var f "block_size_g") in
  let uint64_t = (SU.find_type f "uint64_t") in
  try
    (* Go through the arguments defined in pragma *)
    L.iter (fun arg ->
      (* For the region arguments check whether they are safe and push them in DAG *)
      if (SU.isRegion arg && not (Sdam.isSafeArg orig_tname tid arg.SU.aname)) then (
        (*// actually ceil(arg_size/BLOCK_SZ)
          register unsigned int blocks_num = 1U + ((arg_size - 1U) / BLOCK_SZ);
          AddAttribute_Task(this, arg_addr64, arg_flag, BLOCK_SZ, blocks_num);
        *)
        let minus = (BinOp(MinusA, sizeOf_region_t, one, uint64_t)) in
        let plus = (BinOp(PlusA, one, minus, uint64_t)) in
        let div = BinOp(Div, plus, Lval block_size, uint64_t) in
        let args = [
          Lval this;
          CastE(voidPtrType, arg.SU.address);
          SU.arg_type2integer arg.SU.atype;
          Lval block_size; div
        ] in
        let st = mkStmtOneInstr (Call (None, Lval (var addAttribute_Task), args, locUnknown)) in
        stl := st::(!stl);
      )
    ) args;
    (L.rev !stl)
   with Not_found -> []
)

class codegen (cgraph : Callgraph.callgraph) file pragma includePath =
object (self) inherit Scoop_codegen.codegen cgraph file pragma includePath as super
  val scoop_barrier       = "tpc_wait_all"
  val scoop_start         = "tpc_init"
  val scoop_finish        = "tpc_shutdown"
  val scoop_malloc        = "tpc_malloc"
  val scoop_free          = "tpc_free"
  val runtime             = "bddt"

    method declareGlobals : unit =
      (* Task_element *this;
         uint32_t block_index_start
         uint64_t e_addr;
         uint64_t _tmptime; *)
      let globals = ref [] in
      let makeGlobalVar n t =
        globals := GVar(makeGlobalVar n t, {init = None;}, locUnknown)::!globals;
      in
      let task_element_pt =
        TPtr((SU.find_type new_file "Task_element"), [])
      in
      makeGlobalVar "this_SCOOP__" task_element_pt;
      let uint32_t = (SU.find_type new_file "uint32_t") in
      let uint64_t = (SU.find_type new_file "uint64_t") in
      makeGlobalVar "block_index_start_SCOOP__" uint32_t;
      makeGlobalVar "e_addr_SCOOP__" uint64_t;
      makeGlobalVar "_tmptime1_SCOOP__" uint64_t;
      makeGlobalVar "_tmptime2_SCOOP__" uint64_t;
      SU.add_at_top new_file !globals;

    method preprocessAndMergeWithHeader flags : unit =
      SU.preprocessAndMergeWithHeader_x86 new_file
                                          (includePath)
                                         "/scoop/tpc_scoop.h"
                                          (* ((runtime)^".h") *)
                                          flags;

    method private process_task_pragma loc pragma_args =
      let process_task_pragma = self#process_task_pragma loc in
      match pragma_args with
      | (ACons("safe", args)::rest) ->
        (* ignore safe tags, it's a hint for the analysis *)
        process_task_pragma rest
      (* support region r in(a,b,c) etc. *)
      | AStr("region")::(AStr(region)::(ACons(arg_typ, args)::rest)) ->
        let lst = process_task_pragma rest in
        let r_vi = SU.find_scoped_var loc !SU.currentFunction new_file region in
        let tmp_addr = Lval(var r_vi) in
        let args_l =
          List.map (fun a ->
                    match a with
                    | ACons(name, []) -> name
                    | _ -> E.s (errorLoc loc "#pragma %s task region %s %s(...) should include only variable names" pragma_str region arg_typ);
                   ) args
        in
        let tmp_t = SU.Region(SU.str2arg_flow arg_typ loc, args_l) in
        { SU.aname=region; address=tmp_addr; atype=tmp_t;}::lst
      | (ACons(arg_typ, args)::rest) ->
        let lst = process_task_pragma rest in
        (SU.scoop_process_args false new_file arg_typ loc args)@lst
      | [] -> []
      | _ -> E.s (errorLoc loc "Syntax error in #pragma %s task\n" pragma_str);

    (** passes the arguments to the arrays in the correct order
     * @param targs the arguments' table
     * @param ttyps the types' table
     * @param orig_tname the original function's name
     * @param tid an id marking the query (needed by SDAM)
     * @param arg the argument we want to place
     * @return the generated Set instrs
     *)
    method private doArgument (args: SU.arg_descr list) (func_vi: varinfo)
                              (loc: location) (arg: exp): stmt list =
      (* targs ttyps (orig_tname: string) (tid: int) *)
      (*                               (arg: (int * SU.arg_descr) ) : instr list = *)
      (* uint32_t block_index_start *)
      let bis = var (SU.find_global_var new_file "block_index_start_SCOOP__") in
      let this = var (SU.find_global_var new_file "this_SCOOP__") in
      (* this->closure.funcid = (uint8_t)funcid; *)
      let this_closure = SU.mkFieldAccess this "closure" in
      let arguments = SU.mkFieldAccess this_closure "arguments" in
      let total_arguments = SU.mkFieldAccess this_closure "total_arguments" in
      let idxlv = addOffsetLval (Index(Lval total_arguments, NoOffset)) arguments in
      let stride = SU.mkFieldAccess idxlv "stride" in
      let size = SU.mkFieldAccess idxlv "size" in
      let flag = SU.mkFieldAccess idxlv "flag" in
      let eal_in = SU.mkFieldAccess idxlv "eal_in" in
      let eal_out = SU.mkFieldAccess idxlv "eal_out" in
      let block_size = var (SU.find_global_var new_file "block_size_g") in
      let pplus = (BinOp(PlusA, Lval total_arguments, one, intType)) in
      let arg_name = SU.getNameOfExp arg in
      let uint64_t = (SU.find_type new_file "uint64_t") in

      let stl = ref [] in
      let il = ref [] in

      let doSafeArg typ =
        (* this->closure.arguments[  this->closure.total_arguments ].stride=BLOCK_SZ; *)
        il := Set(stride, Lval block_size, locUnknown)::!il;
        (*  this->closure.arguments[this->closure.total_arguments].eal_in = arg_addr64;
          this->closure.arguments[this->closure.total_arguments].eal_out = arg_addr64;
          this->closure.arguments[this->closure.total_arguments].size = arg_size;*)
        il := Set(eal_in, CastE(voidPtrType, arg), locUnknown)::!il;
        il := Set(eal_out, CastE(voidPtrType, arg), locUnknown)::!il;
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
        let arg_desc = L.find (fun a -> (arg_name=a.SU.aname)) args in
        let arg_addr = arg_desc.SU.address in
        let arg_type = arg_desc.SU.atype in
        let arg_size = SU.getSizeOfArg arg_desc in
        (*   print_endline ("Doing "^arg_name); *)
        let t = typeOfLval arguments in
        assert(isArrayType t);
        (* this->closure.arguments[  this->closure.total_arguments ].stride=TPC_IS_STRIDEARG(arg_flag)? va_arg(arg_list, int):0; *)
        il :=
          ( if (SU.isStrided arg_desc) then
              Set(stride, arg_size, locUnknown)
            else
              Set(stride, Lval block_size, locUnknown)
          )::!il;

        (*  this->closure.arguments[this->closure.total_arguments].eal_in = arg_addr64;
      this->closure.arguments[this->closure.total_arguments].eal_out = arg_addr64;
      this->closure.arguments[this->closure.total_arguments].size = arg_size;
      this->closure.arguments[this->closure.total_arguments].flag = arg_flag;*)
        (* if it is a scalar we need an extra cast to avoid warnings for 32bit types *)
        (match arg with
         | Lval(Var vi, NoOffset) when SU.isScalar_v vi ->
           il := Set(eal_in, CastE(voidPtrType, CastE(uint64_t, arg_addr)), locUnknown)::!il;
           il := Set(eal_out, CastE(voidPtrType, CastE(uint64_t, arg_addr)), locUnknown)::!il;
           il := Set(size, arg_size, locUnknown)::!il;
         | _ ->
           il := Set(eal_in, CastE(voidPtrType, arg_addr), locUnknown)::!il;
           il := Set(eal_out, CastE(voidPtrType, arg_addr), locUnknown)::!il;
           il := Set(size, arg_size, locUnknown)::!il;
        );
        (*   il := Set(flag, integer (SU.arg_type2int arg_type), locUnknown)::!il; *)

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
          il := Set(flag, integer ( (SU.arg_type2int arg_type) lor 0x18), locUnknown)::!il;
          il := Set(total_arguments, pplus, locUnknown)::!il;
          stl := [mkStmt(Instr (L.rev !il))];
        ) else (
          (* this->closure.arguments[this->closure.total_arguments].flag = arg_flag; *)
          il := Set(flag, integer (SU.arg_type2int arg_type), locUnknown)::!il;
          (* uint32_t block_index_start=this->closure.total_arguments; *)
          il := Set(bis, Lval total_arguments, locUnknown)::!il;

          let addAttribute_Task = SU.find_function_sign new_file ("AddAttribute_Task") in
          if (SU.isStrided arg_desc) then (
            (*  if(TPC_IS_STRIDEARG(arg_flag)){
                AddAttribute_Task(this, arg_addr64, arg_flag & ~TPC_STRIDE_ARG,
                        TPC_EXTRACT_STRIDEARG_ELEMSZ(arg_size), TPC_EXTRACT_STRIDEARG_ELEMS(arg_size));
            }*)
            let (arg_els, arg_elsz) =
              match arg_type with
                SU.Stride(_, _, els, elsz) -> (els, elsz)
              | _ -> assert false
            in
            let args = [Lval this; CastE(voidPtrType, arg_addr); SU.arg_type2integer arg_type; arg_elsz; arg_els] in
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
            let args = [
              Lval this;
              CastE(voidPtrType, arg_desc.SU.address);
              SU.arg_type2integer arg_desc.SU.atype;
              Lval block_size;
              div
            ] in
            let st = mkStmtOneInstr (Call (None, Lval (var addAttribute_Task), args, locUnknown)) in
            stl := st::[mkStmt(Instr (L.rev !il))];

          );
          (* this -> closure.total_arguments++; *)
          stl := mkStmtOneInstr(Set(total_arguments, pplus, locUnknown))::!stl;

          (* this->closure.arguments[ block_index_start ].flag|=TPC_START_ARG;
          tpc_common.h:20:#define TPC_START_ARG   0x10 *)
          let idxlv = addOffsetLval (Index(Lval bis, NoOffset)) arguments in
          let flag = SU.mkFieldAccess idxlv "flag" in
          stl := mkStmtOneInstr(Set(flag, BinOp( BOr, Lval flag, integer 0x10, intType), locUnknown))::!stl;

        );
        L.rev !stl
      )
      with Not_found -> ( (* if it is not annotated *)
        try (* find if this argument belongs in some region *)
          let region = L.find
                         (fun a -> match a.SU.atype with
                                     SU.Region(_, vars) -> L.exists (fun s -> s=arg_name) vars
                                   | _ -> false
                         ) args
          in
          doSafeArg ( (SU.arg_type2int region.SU.atype) lor 0x18)
        with Not_found ->
             (* if the argument has no annotation check whether it's a scalar *)
             match arg with
               Lval(Var vi, NoOffset) -> (
               if (SU.isScalar_v vi) then (
                 doSafeArg 0x19
               ) else
                 E.s (errorLoc loc "%s has no annotation in the #pragma css task ... and is not a scalar" arg_name);
             )
             | _ -> E.s (errorLoc loc "%s has no annotation in the #pragma css task ...  and is not a variable" arg_name);
      )

    (** Generates the code to spawn a task
     * @param loc the current file location
     * @param func_vi the varinfo of the original function
     * @param oargs the original arguments
     * @param args the arguments from the pragma directive
     * @param f the file we are modifying
     * @return the stmts that will replace the call paired with a list of numbered
     * argument descriptors
     *)
    method make_task_spawn (loc: location) (func_vi: varinfo)
                                   (oargs: exp list) (args: SU.arg_descr list)
                   : (stmt list * (int * SU.arg_descr) list) =
      incr un_id;
      (*   let args = List.sort sort_args args in *)

      let this = var (SU.find_global_var new_file "this_SCOOP__") in
      (* this->closure.funcid = (uint8_t)funcid; *)
      let this_closure = SU.mkFieldAccess this "closure" in
      let stmts = ref [] in
      let instrs = ref [] in

      (* G_ppe_stats.stat_tpc_per_spe[0] += 1; *)
      let gps = var (SU.find_global_var new_file "G_ppe_stats") in
      let gps_stps = SU.mkFieldAccess gps "stat_tpc_per_spe" in
      let idxlv = addOffsetLval (Index(zero, NoOffset)) gps_stps in
      instrs := Set (idxlv, (BinOp(PlusA, Lval idxlv, one, intType)), locUnknown)::!instrs;
      (* this = AddTask(); *)
      let addTask = SU.find_function_sign new_file "AddTask" in
      instrs := Call (Some this, Lval (var addTask), [], locUnknown)::!instrs;
      (* while (this == NULL)
         {
           tpr_barrier();
           this = AddTask();
         } *)
      let tpr_barrier = SU.find_function_sign new_file "tpr_barrier" in
      let ins = [Call (Some this, Lval (var addTask), [], locUnknown)] in
      let ins = Call (None, Lval (var tpr_barrier), [], locUnknown)::ins in
      let wbody = [mkStmt (Instr ins)] in
      stmts := mkWhile (BinOp(Eq, Lval this, CastE(voidPtrType, zero), SU.boolType)) wbody;
      stmts := mkStmt (Instr (List.rev !instrs))::!stmts;
      (* this->notAvailable = 1; *)
      instrs := [Set (SU.mkFieldAccess this "notAvailable", one, locUnknown) ];
      (* TIMER_START(1); *)
      let tmptime1 = var (SU.find_global_var new_file "_tmptime1_SCOOP__") in
      let rdtsc = SU.find_function_sign new_file "rdtsc" in
      instrs := Call (Some tmptime1, Lval (var rdtsc), [], locUnknown)::!instrs;
      (* this->closure.funcid = (uint8_t)funcid; *)
      instrs := Set (SU.mkFieldAccess this_closure "funcid",
                     CastE(SU.find_type new_file "uint8_t", integer !func_id), locUnknown)::!instrs;
      (* this->closure.total_arguments = 0 *)
      let total_arguments = SU.mkFieldAccess this_closure "total_arguments" in
      instrs := Set (total_arguments, zero, locUnknown)::!instrs;

      (* if we have arguments *)
      if (oargs <> []) then (
        incr querie_no;
        let doArgument = self#doArgument args func_vi loc in
        let mapped = L.flatten (List.map doArgument oargs) in
        let regions = doRegions loc this new_file args func_vi.vname !querie_no in
        stmts := (!stmts)@(mkStmt (Instr (List.rev !instrs))::mapped)@(regions);
        instrs := [];
      );

      (*if(TPC_IS_HIGHPRIORITYARG(highpriority_arg)) {
          this->highpriority = 1;
        }
       *)
      (* if (is_hp) then ( *)
      (*   let this_highpriority = SU.mkFieldAccess this "highpriority" in *)
      (*   let hp_set = Set(this_highpriority, one, locUnknown) in *)
      (*   stmts := (mkStmtOneInstr hp_set)::(!stmts); *)
      (* ); *)

      (* assert(this->closure.total_arguments<=MAX_ARGS); *)
      let assert_a = SU.find_function_sign new_file "assert_args_SCOOP__" in
      instrs := Call (None, Lval (var assert_a), [Lval total_arguments], locUnknown)::!instrs;
      (* tpr_acquire_spinlock(&(this->spinlock)); *)
      let spinlock = mkAddrOf (SU.mkFieldAccess this "spinlock") in
      let tpr_acquire_spinlock = SU.find_function_sign new_file "tpr_acquire_spinlock" in
      instrs := Call (None, Lval (var tpr_acquire_spinlock), [spinlock], locUnknown)::!instrs;
      (* this->input_dependencies_counter -= this->backup_input_dependencies_counter; *)
      let backup_input_dependencies_counter =
        SU.mkFieldAccess this "backup_input_dependencies_counter" in
      let input_dependencies_counter =
        SU.mkFieldAccess this "input_dependencies_counter" in
      let sub = BinOp(MinusA, Lval input_dependencies_counter ,
                      Lval backup_input_dependencies_counter, intType) in
      instrs := Set (input_dependencies_counter, sub, locUnknown)::!instrs;
      (* this->backup_input_dependencies_counter = 0; *)
      instrs := Set (backup_input_dependencies_counter, zero, locUnknown)::!instrs;
      (* this->notAvailable = 0; *)
      instrs := Set (SU.mkFieldAccess this "notAvailable", zero, locUnknown)::!instrs;
      (* tpr_release_spinlock(&(this->spinlock)); *)
      let tpr_release_spinlock = SU.find_function_sign new_file "tpr_release_spinlock" in
      instrs := Call (None, Lval (var tpr_release_spinlock), [spinlock], locUnknown)::!instrs;
      (* G_ppe_stats.issue_ticks += TIMER_END(1); *)
      let gps_it = SU.mkFieldAccess gps "issue_ticks" in
      let tmptime2 = var (SU.find_global_var new_file "_tmptime2_SCOOP__") in
      instrs := Call (Some tmptime2, Lval (var rdtsc), [], locUnknown)::!instrs;
      let uint64_t = (SU.find_type new_file "uint64_t") in
      let sub = BinOp(MinusA, Lval tmptime2 , Lval tmptime1, uint64_t) in
      let acc = BinOp(PlusA, Lval gps_it , sub, uint64_t) in
      instrs := Set (gps_it, acc, locUnknown)::!instrs;
      (* Clear_Handler(this ); *)
      let clear_Handler = SU.find_function_sign new_file "Clear_Handler" in
      instrs := Call (None, Lval (var clear_Handler), [Lval this], locUnknown)::!instrs;

      stmts := !stmts@[mkStmt (Instr (L.rev !instrs))];

      incr func_id;
      (!stmts, [])

    method getTasks = found_tasks
  end
