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

let doArgument (loc: location) (this: lval) (closure: lval) (total_arguments: lval)
 (e_addr: lval) (bis: lval) (ppc_file: file) (orig_tname: string) (tid: int)
 (args: arg_descr list ) (arg: exp) : stmt list = (
  let uint32_t = (find_type ppc_file "uint32_t") in
  let uint64_t = (find_type ppc_file "uint64_t") in
  let arg_name = getNameOfExp arg in
  let stl = ref [] in
  let il = ref [] in
  let found = L.exists (fun a -> (arg_name=a.aname)) args in

  let arguments = mkFieldAccess closure "arguments" in
  let idxlv = addOffsetLval (Index(Lval total_arguments, NoOffset)) arguments in
  let stride = mkFieldAccess idxlv "stride" in
  let size = mkFieldAccess idxlv "size" in
  let flag = mkFieldAccess idxlv "flag" in
  let eal_in = mkFieldAccess idxlv "eal_in" in
  let eal_out = mkFieldAccess idxlv "eal_out" in
  let pplus = (BinOp(PlusA, Lval total_arguments, one, intType)) in

  (* if it is annotated *)
  if found then (
    let arg_desc = L.find (fun a -> (arg_name=a.aname)) args in
    let arg_addr = arg_desc.address in
    let arg_type = arg_desc.atype in
    let arg_size = getSizeOfArg arg_desc in
    let block_size = var (find_global_var ppc_file "__block_sz") in
  (*   print_endline ("Doing "^arg_name); *)
    let t = typeOfLval arguments in
    assert(isArrayType t);
    (* this->closure.arguments[  this->closure.total_arguments ].stride=TPC_IS_STRIDEARG(arg_flag)? va_arg(arg_list, int):0; *)
    il :=
      ( if (isStrided arg_desc) then
          Set(stride, arg_size, locUnknown)
        else
          Set(stride, (integer 0), locUnknown)
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
    if (Sdam.isSafeArg orig_tname tid arg_name) then (
  (*       let (Var vi, _) = arg_addr in *)
  (*       print_endline ("And it's safe "^vi.vname); *)
  (*       print_endline ("And it's safe "^arg_name); *)
      (*
        if(TPC_IS_SAFEARG(arg_flag)) {
          //this->closure.arguments[  this->closure.total_arguments ].size = arg_size;
          this->closure.arguments[  this->closure.total_arguments ].flag = arg_flag|TPC_START_ARG;
          this->closure.total_arguments++;
          continue;
        }
        #define TPC_START_ARG   0x10
        #define TPC_SAFE_ARG    0x8
      *)
  (*     il := Set(size, arg_size, locUnknown)::!il; *)
      (* this->closure.arguments[  this->closure.total_arguments ].flag = arg_flag|TPC_START_ARG|TPC_SAFE_ARG; *)
      il := Set(flag, integer ( (arg_type2int arg_type) lor 0x18), locUnknown)::!il;
  (*    let eal_in = mkFieldAccess idxlv "eal_in" in
      il := Set(eal_in, CastE(uint64_t, Lval arg_addr), locUnknown)::!il;
      let eal_out = mkFieldAccess idxlv "eal_out" in
      il := Set(eal_out, CastE(uint64_t, Lval arg_addr), locUnknown)::!il;*)
      (* this -> closure.total_arguments++; *)
      il := Set(total_arguments, pplus, locUnknown)::!il;
      stl := (*mkStmt(Continue locUnknown)::*)[mkStmt(Instr (L.rev !il))];
    ) else (

      (* this->closure.arguments[this->closure.total_arguments].flag = arg_flag; *)
      il := Set(flag, integer (arg_type2int arg_type), locUnknown)::!il;
      (* uint32_t block_index_start=this->closure.total_arguments; *)
      il := Set(bis, Lval total_arguments, locUnknown)::!il;
      il :=  Set(e_addr, CastE(uint64_t, arg_addr), locUnknown)::!il;

(*       let addAttribute_Task = find_function_sign ppc_file ("Add"^(arg_type2string arg_type)^"Attribute_Task") in *)
      let addAttribute_Task = find_function_sign ppc_file ("AddAttribute_Task") in
      if (isStrided arg_desc) then (
        (*  if(TPC_IS_STRIDEARG(arg_flag)){
            //uint32_t j;
            //uint32_t stride=this->closure.arguments[  this->closure.total_arguments ].stride ;
            uint64_t e_addr=(uint64_t)arg_addr64;
            uint32_t numElems=TPC_EXTRACT_STRIDEARG_ELEMS(arg_size);
            //for(j=0;j<numElems/*(unsigned)TPC_EXTRACT_STRIDEARG_ELEMS(arg_size)*/;j++,e_addr+=stride){
            //this->closure.arguments[  this->closure.total_arguments ].flag = arg_flag&~TPC_STRIDE_ARG;
            //this->closure.arguments[  this->closure.total_arguments ].size = TPC_EXTRACT_STRIDEARG_ELEMSZ(arg_size);
              AddAttribute_Task(this, (void * )(e_addr/*(uint32_t)arg_addr64 + stride *j*/), arg_flag&~TPC_STRIDE_ARG,TPC_EXTRACT_STRIDEARG_ELEMSZ(arg_size));
        //this->closure.total_arguments++;
            }
          }*)
        
        let (arg_els, arg_elsz) =
          match arg_type with
              Stride(_, _, els, elsz) -> (els, elsz)
            | _ -> assert false
        in
        let args = [Lval this; CastE(voidPtrType, Lval e_addr); arg_type2integer arg_type; arg_elsz; arg_els] in
        let st = mkStmtOneInstr( Call (None, Lval (var addAttribute_Task), args, locUnknown) ) in
        stl := L.rev (mkStmt(Instr (L.rev !il))::[st]);

      ) else (
        (*const uint64_t limit=((uint64_t)arg_addr64)+arg_size;
          const uint64_t aligned_limit=((uint64_t)arg_addr64)+((arg_size/BLOCK_SZ)*BLOCK_SZ);
          this->closure.arguments[this->closure.total_arguments].stride=BLOCK_SZ;
          AddAttribute_Task(this, arg_addr64, arg_flag, BLOCK_SZ, arg_size/BLOCK_SZ);
          if(limit-aligned_limit != 0) {
                  AddAttribute_Task( this, (void* )(limit), arg_flag, BLOCK_SZ, 1);
          }*)
        let limit =
          try var (__find_global_var ppc_file "limit_SCOOP__")
          with Not_found -> var (makeGlobalVar "limit_SCOOP__" uint64_t ppc_file)
        in
        let plus = (BinOp(PlusA, CastE(uint64_t, arg_addr), arg_size, uint64_t)) in
        il := Set(limit, plus, locUnknown)::!il;
        let aligned_limit =
          try var (__find_global_var ppc_file "aligned_limit_SCOOP__")
          with Not_found -> var (makeGlobalVar "aligned_limit_SCOOP__" uint64_t ppc_file)
        in
        let div = BinOp(Div, arg_size, Lval block_size, uint64_t) in
        let mul = BinOp(Mult, div, Lval block_size, uint64_t) in
        let plus = BinOp(PlusA, CastE(uint64_t, arg_addr), mul, uint64_t) in
        il := Set(aligned_limit, plus, locUnknown)::!il;

        (*this->closure.arguments[this->closure.total_arguments].stride=BLOCK_SZ;
          AddAttribute_Task( this, (void * )(e_addr), arg_flag,BLOCK_SZ);
        *)
        il := Set(stride, Lval block_size, locUnknown)::!il;
        (*let args = [Lval this; CastE(voidPtrType, Lval e_addr);
         * arg_type2integer arg_type; Lval block_size; BinOp(Div, arg_size, Lval
         * block_size, intType) ] in*)
        let args = [Lval this; CastE(voidPtrType, Lval e_addr); arg_type2integer arg_type; Lval block_size; div ] in
        let st = mkStmtOneInstr (Call (None, Lval (var addAttribute_Task), args, locUnknown)) in
        stl := L.rev (mkStmt(Instr (L.rev !il))::[st]);

        (*if(limit-aligned_limit){
          AddAttribute_Task( this, (void * )(e_addr), arg_flag,this->closure.arguments[  this->closure.total_arguments ].size);
        }*)
        let sub = (BinOp(MinusA, Lval limit, Lval aligned_limit, boolType)) in
        let ilt = ref [] in
        let args = [Lval this; CastE(voidPtrType, Lval aligned_limit); arg_type2integer arg_type; Lval block_size; one] in
        ilt := Call (None, Lval (var addAttribute_Task), args, locUnknown)::!ilt;
    (*     ilt := Set(total_arguments, pplus, locUnknown)::!ilt; *)
        let bl = mkBlock [mkStmt(Instr (L.rev !ilt))] in
        stl := (mkStmt (If(sub, bl, mkBlock [], locUnknown)))::!stl;
      );

      (* this -> closure.total_arguments++; *)
      stl := mkStmtOneInstr(Set(total_arguments, pplus, locUnknown))::!stl;

      (* this->closure.arguments[ block_index_start ].flag|=TPC_START_ARG;
        tpc_common.h:20:#define TPC_START_ARG   0x10 *)
      let idxlv = addOffsetLval (Index(Lval bis, NoOffset)) arguments in
      let flag = mkFieldAccess idxlv "flag" in
      stl := mkStmtOneInstr(Set(flag, BinOp( BOr, Lval flag, integer 0x10, intType), locUnknown))::!stl;

    );
  ) else (
    let doSafeArg typ =
      (* this->closure.arguments[  this->closure.total_arguments ].stride=0; *)
      il := Set(stride, (integer 0), locUnknown)::!il;
      (*  this->closure.arguments[this->closure.total_arguments].eal_in = arg_addr64;
          this->closure.arguments[this->closure.total_arguments].eal_out = arg_addr64;
          this->closure.arguments[this->closure.total_arguments].size = arg_size;
          this->closure.arguments[this->closure.total_arguments].flag = arg_flag;*)
      il := Set(eal_in, CastE(voidPtrType, arg), locUnknown)::!il;
      il := Set(eal_out, CastE(voidPtrType, arg), locUnknown)::!il;
(*       il := Set(size, SizeOf( getBType vi.vtype vi.vname ), locUnknown)::!il; *)
      il := Set(size, zero, locUnknown)::!il;
      (* this->closure.arguments[  this->closure.total_arguments ].flag = IN|TPC_START_ARG|TPC_SAFE_ARG; *)
      il := Set(flag, integer typ, locUnknown)::!il;
      (* this -> closure.total_arguments++; *)
      il := Set(total_arguments, pplus, locUnknown)::!il;
      stl := (*mkStmt(Continue locUnknown)::*)[mkStmt(Instr (L.rev !il))];
    in
    try (* find if this argument belongs in some region *)
      let region = L.find
        (fun a -> match a.atype with
            Region(_, vars) -> L.exists (fun s -> s=arg_name) vars
          | _ -> false
        ) args
      in
      doSafeArg ( (arg_type2int region.atype) lor 0x18)
    with Not_found -> (
      (* if the argument has no annotation check whether it's a scalar *)
      match arg with
          Lval(Var vi, NoOffset) -> (
            if (isScalar_v vi) then 
              doSafeArg 0x19
            else
              E.s (errorLoc loc "%s has no annotation in the #pragma css task ... and is not a scalar" arg_name)
          )
        | _ -> E.s (errorLoc loc "%s has no annotation in the #pragma css task ...  and is not a variable" arg_name)
    )
  );
  L.rev !stl
)

(*FIXME: Inefficient*)
let doRegions (loc: location) (this: lval) (ppc_file: file) (args: arg_descr list )  (orig_tname: string) (tid: int): stmt list = (
  let stl = ref [] in
  let ilt = ref [] in
  let closure = mkFieldAccess this "closure" in
  let arguments = mkFieldAccess closure "arguments" in
  let total_arguments = mkFieldAccess closure "total_arguments" in
  let idxlv = addOffsetLval (Index(Lval total_arguments, NoOffset)) arguments in
  let stride = mkFieldAccess idxlv "stride" in
  let addAttribute_Task = find_function_sign ppc_file ("AddAttribute_Task") in
  let sizeOf_region_t = SizeOf(find_type ppc_file "region_t") in
  let block_size = var (find_global_var ppc_file "__block_sz") in
  let uint64_t = (find_type ppc_file "uint64_t") in
  let limit =
    try var (__find_global_var ppc_file "limit_SCOOP__")
    with Not_found -> var (makeGlobalVar "limit_SCOOP__" uint64_t ppc_file)
  in
  let aligned_limit =
    try var (__find_global_var ppc_file "aligned_limit_SCOOP__")
    with Not_found -> var (makeGlobalVar "aligned_limit_SCOOP__" uint64_t ppc_file)
  in
  try
    L.iter (fun arg ->
      if (isRegion arg && not (Sdam.isSafeArg orig_tname tid arg.aname)) then (
        (*const uint64_t limit=((uint64_t)arg_addr64)+arg_size;
          const uint64_t aligned_limit=((uint64_t)arg_addr64)+((arg_size/BLOCK_SZ)*BLOCK_SZ);
          this->closure.arguments[this->closure.total_arguments].stride=BLOCK_SZ;
          AddAttribute_Task(this, arg_addr64, arg_flag, BLOCK_SZ, arg_size/BLOCK_SZ);
          if(limit-aligned_limit != 0) {
                  AddAttribute_Task( this, (void* )(limit), arg_flag, BLOCK_SZ, 1);
          }*)
        let plus = (BinOp(PlusA, CastE(uint64_t, arg.address), sizeOf_region_t, uint64_t)) in
        ilt := [Set(limit, plus, locUnknown)];
        let div = BinOp(Div, sizeOf_region_t, Lval block_size, uint64_t) in
        let mul = BinOp(Mult, div, Lval block_size, uint64_t) in
        let plus = BinOp(PlusA, CastE(uint64_t, arg.address), mul, uint64_t) in
        ilt := Set(aligned_limit, plus, locUnknown)::!ilt;

        (*this->closure.arguments[this->closure.total_arguments].stride=BLOCK_SZ;
          AddAttribute_Task( this, (void * )(e_addr), arg_flag,BLOCK_SZ);
        *)
        ilt := Set(stride, Lval block_size, locUnknown)::!ilt;
        (*let args = [Lval this; CastE(voidPtrType, Lval e_addr);
         * arg_type2integer arg_type; Lval block_size; BinOp(Div, arg_size, Lval
         * block_size, intType) ] in*)
        let args = [Lval this; CastE(voidPtrType, arg.address); arg_type2integer arg.atype; Lval block_size; div ] in
        let st = mkStmtOneInstr (Call (None, Lval (var addAttribute_Task), args, locUnknown)) in
        stl := L.rev (mkStmt(Instr (L.rev !ilt))::[st])@(!stl);

        (*if(limit-aligned_limit){
          AddAttribute_Task( this, (void * )(e_addr), arg_flag,this->closure.arguments[  this->closure.total_arguments ].size);
        }*)
        let sub = (BinOp(MinusA, Lval limit, Lval aligned_limit, boolType)) in
        let ilt = ref [] in
        let args = [Lval this; CastE(voidPtrType, Lval aligned_limit); arg_type2integer arg.atype; Lval block_size; one] in
        ilt := Call (None, Lval (var addAttribute_Task), args, locUnknown)::!ilt;
    (*     ilt := Set(total_arguments, pplus, locUnknown)::!ilt; *)
        let bl = mkBlock [mkStmt(Instr (L.rev !ilt))] in
        stl := (mkStmt (If(sub, bl, mkBlock [], locUnknown)))::!stl;
      )
    ) args;
    (L.rev !stl)
(*    let sizeOf_region_t = SizeOf(find_type ppc_file "region_t") in
    let block_size = var (find_global_var ppc_file "__block_sz") in
(*     let block_size = var (find_global_var ppc_file "__block_sz") in *)
    L.iter (fun arg ->
      if isRegion arg then (
(*         let addAttribute_Task = find_function_sign ppc_file ("Add"^(arg_type2string arg.atype)^"Attribute_Task") in *)
        let addAttribute_Task = find_function_sign ppc_file ("AddAttribute_Task") in
        (*AddAttribute_Task(this, r, TPC_IN_ARG, sizeof(region_t), sizeof(region_t)/BLOCK_SZ);*)
(*         let div = BinOp(Div, sizeOf_region_t, Lval block_size, intType) in *)
(*         let args = [Lval this; CastE(voidPtrType, arg.address); arg_type2integer arg.atype; sizeOf_region_t; div ] in *)
        let args = [Lval this; CastE(voidPtrType, arg.address); arg_type2integer arg.atype; sizeOf_region_t; one] in
        ilt := Call (None, Lval (var addAttribute_Task), args, locUnknown)::!ilt;
      )
    ) args;
    [mkStmt (Instr (L.rev !ilt))]
*) 
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
(*   (* this->closure.total_arguments = (uint8_t)arguments.size() *) *)
  (* this->closure.total_arguments = 0 *)
  let total_arguments = mkFieldAccess this_closure "total_arguments" in
  instrs := Set (total_arguments, zero, locUnknown)::!instrs;

  (* uint32_t block_index_start *)
  let bis = var (find_global_var f "block_index_start_SCOOP__") in
  (* uint64_t e_addr; *)
  let e_addr = var (find_global_var f "e_addr_SCOOP__") in
  
  (* if we have arguments *)
  if (oargs <> []) then (
    incr querie_no;
    let doArgument = doArgument loc this this_closure total_arguments e_addr bis f func_vi.vname !querie_no args in
    let mapped = L.flatten (List.map doArgument oargs) in
    stmts := (!stmts)@(mkStmt (Instr (List.rev !instrs))::mapped)@(doRegions loc this f args func_vi.vname !querie_no);
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
