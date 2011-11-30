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

  (* if it is not annotated *)
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

    (* invoke isSafeArg from PtDepa to check whether this argument is a no dep *)
    if (Sdam.isSafeArg orig_tname tid arg_name) then (
  (*       let (Var vi, _) = arg_addr in *)
  (*       print_endline ("And it's safe "^vi.vname); *)
  (*       print_endline ("And it's safe "^arg_name); *)
      (* if(TPC_IS_SAFEARG(arg_flag)){
          //uint64_t e_addr=(uint64_t) arg_addr64;
          //this->closure.arguments[  this->closure.total_arguments ].size = arg_size;
          this->closure.arguments[  this->closure.total_arguments ].flag = arg_flag|TPC_START_ARG;

          //this->closure.arguments[  this->closure.total_arguments ].eal_in  = (void* )e_addr;
          //this->closure.arguments[  this->closure.total_arguments ].eal_out = (void* )e_addr;
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

      let addAttribute_Task = find_function_sign ppc_file ("Add"^(arg_type2string arg_type)^"Attribute_Task") in
      if (isStrided arg_desc) then (
        (*  if(TPC_IS_STRIDEARG(arg_flag)){
            uint32_t j;
            uint32_t stride=this->closure.arguments[  this->closure.total_arguments ].stride ;
            uint64_t e_addr=(uint64_t)arg_addr64;
            uint32_t numElems=TPC_EXTRACT_STRIDEARG_ELEMS(arg_size);
          #ifdef UNALIGNED_ARGUMENTS_ALLOWED
            this->closure.arguments[ this->closure.total_arguments ].stride = 0;
          #endif
            for(j=0;j<numElems/*(unsigned)TPC_EXTRACT_STRIDEARG_ELEMS(arg_size)*/;j++,e_addr+=stride){
            //this->closure.arguments[  this->closure.total_arguments ].flag = arg_flag&~TPC_STRIDE_ARG;
            //this->closure.arguments[  this->closure.total_arguments ].size = TPC_EXTRACT_STRIDEARG_ELEMSZ(arg_size);
              AddAttribute_Task(this, (void * )(e_addr/*(uint32_t)arg_addr64 + stride *j*/), arg_flag&~TPC_STRIDE_ARG,TPC_EXTRACT_STRIDEARG_ELEMSZ(arg_size));
        //this->closure.total_arguments++;
            }
          }*)
        il :=  Set(e_addr, CastE(uint64_t, arg_addr), locUnknown)::!il;
        let i_var =
          try var (__find_global_var ppc_file "i_SCOOP__")
          with Not_found -> var (makeGlobalVar "i__SCOOP__" uint32_t ppc_file)
        in
        let stride_var =
          try var (__find_global_var ppc_file "stride_SCOOP__")
          with Not_found -> var (makeGlobalVar "stride_SCOOP__" uint32_t ppc_file)
        in
        il := Set(stride_var, Lval stride, locUnknown)::!il;
        if (!unaligned_args) then (
          il := Set(stride, (integer 0), locUnknown)::!il;
        );

        let ilt = ref [] in
    (*     let closure_flag = Set(flag, arg_type2integer arg_type, locUnknown) in *)
    (*     ilt := (closure_flag::!ilt; *)
    (*     ilt := Set(size, Lval block_size, locUnknown)::!ilt; *)
        let (arg_els, arg_elsz) =
          match arg_type with
              Stride(_, _, els, elsz) -> (els, elsz)
            | _ -> assert false
        in
  (* TODO integer 0 is for non reductive argument FIXME *)
        let args = [Lval this; CastE(voidPtrType, Lval e_addr); integer 0; arg_elsz] in
        ilt := Call (None, Lval (var addAttribute_Task), args, locUnknown)::!ilt;
    (*     ilt := Set(total_arguments, pplus, locUnknown)::!ilt; *)
        let start = [mkStmtOneInstr (Set(i_var, zero, locUnknown))] in
        let e_addr_plus = BinOp(PlusA, Lval e_addr, Lval stride_var, intType) in
        let j_plus = BinOp(PlusA, Lval i_var, one, intType) in
        let guard = BinOp(Le, j_plus, arg_els, boolType) in
        let next = [mkStmt( Instr ([Set(i_var, j_plus, locUnknown); Set(e_addr, e_addr_plus, locUnknown)]))] in
        let body = [mkStmt (Instr (L.rev !ilt))] in
        stl := L.rev (mkStmt(Instr (L.rev !il))::(mkFor start guard next body));

      ) else (

        (* const uint64_t limit=(((uint64_t)arg_addr64)+arg_size); *)
        let limit =
          try var (__find_global_var ppc_file "limit_SCOOP__")
          with Not_found -> var (makeGlobalVar "limit_SCOOP__" uint64_t ppc_file)
        in
        let plus = (BinOp(PlusA, CastE(uint64_t, arg_addr), arg_size, uint64_t)) in
        il := Set(limit, plus, locUnknown)::!il;

        (*#ifdef UNALIGNED_ARGUMENTS_ALLOWED
            printf("ADAM Warning: Unaligned argument\n");
            uint64_t tmp_addr=(uint64_t)arg_addr64;
            arg_addr64=((uint64_t)(tmp_addr/BLOCK_SZ))*BLOCK_SZ;
            this->closure.arguments[arg_index].stride = tmp_addr-(uint64_t)arg_addr64;
          #endif*)
        if (!unaligned_args) then (
          let printf = find_function_sign ppc_file "printf" in
          let args = [Const(CStr("ADAM Warning: Unaligned argument\n"))] in
          il := Call (None, Lval (var printf), args, locUnknown)::!il;
          let tmp_addr =
            try var (__find_global_var ppc_file "tmp_addr_SCOOP__")
            with Not_found -> var (makeGlobalVar "tmp_addr_SCOOP__" uint64_t ppc_file)
          in
          il := Set(tmp_addr, arg_addr, locUnknown)::!il;
          let div = BinOp(Div, Lval tmp_addr, Lval block_size, uint64_t) in
          let mul = BinOp(Mult, CastE(uint64_t, div), Lval block_size, voidPtrType) in
          let arg_addr_v =
            match arg_addr with
              Lval(a) -> a
              | _ -> assert false;
          in
          il := Set(arg_addr_v, CastE(voidPtrType, mul), locUnknown)::!il;
          let new_stride = BinOp(MinusA, Lval tmp_addr, CastE(uint64_t, arg_addr), intType) in
          il := Set(stride, new_stride, locUnknown)::!il;
        );

        (*for(e_addr=(uint64_t)arg_addr64;e_addr + BLOCK_SZ <= limit ;e_addr+=BLOCK_SZ){
          //this->closure.arguments[  this->closure.total_arguments ].flag = arg_flag;
          //this->closure.arguments[  this->closure.total_arguments ].size = BLOCK_SZ;
          AddAttribute_Task( this, (void * )(e_addr), arg_flag,BLOCK_SZ);
          //this -> closure.total_arguments++;
        }*)
        let ilt = ref [] in
    (*     let closure_flag = Set(flag, arg_type2integer arg_type, locUnknown) in *)
    (*     ilt := (closure_flag::!ilt; *)
    (*     ilt := Set(size, Lval block_size, locUnknown)::!ilt; *)
  (* TODO integer 0 is for non reductive argument FIXME *)
        let args = [Lval this; CastE(voidPtrType, Lval e_addr); integer 0; Lval block_size ] in
        ilt := Call (None, Lval (var addAttribute_Task), args, locUnknown)::!ilt;
    (*     ilt := Set(total_arguments, pplus, locUnknown)::!ilt; *)
        let start = [mkStmtOneInstr (Set(e_addr, CastE(uint64_t, arg_addr), locUnknown))] in
        let e_addr_plus = BinOp(PlusA, Lval e_addr, Lval block_size, intType) in
        let guard = BinOp(Le, e_addr_plus, Lval limit, boolType) in
        let next = [mkStmtOneInstr (Set(e_addr, e_addr_plus, locUnknown))] in
        let body = [mkStmt (Instr (L.rev !ilt))] in
        stl := L.rev (mkStmt(Instr (L.rev !il))::(mkFor start guard next body));

        (*if(limit-e_addr){
          //this->closure.arguments[  this->closure.total_arguments ].flag = arg_flag;
          //this->closure.arguments[  this->closure.total_arguments ].size = limit-e_addr;
          AddAttribute_Task( this, (void * )(e_addr), arg_flag,this->closure.arguments[  this->closure.total_arguments ].size);
          //this -> closure.total_arguments++;
        }*)
        let sub = (BinOp(MinusA, Lval limit, Lval e_addr, boolType)) in
        ilt := [];
    (*     ilt := [closure_flag]; *)
    (*     ilt := Set(size, sub, locUnknown)::!ilt; *)
  (* TODO integer 0 is for non reductive argument FIXME *)
        let args = [Lval this; CastE(voidPtrType, Lval e_addr); integer 0; Lval size] in
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

      (* skipping assert( (((unsigned)arg_addr64&0xF) == 0) && ((arg_size&0xF) == 0)); *)
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

let doRegions (loc: location) (this: lval) (ppc_file: file) (args: arg_descr list ) : stmt = (
  let ilt = ref [] in
  let sizeOf_region_t = SizeOf(find_type ppc_file "region_t") in
  let block_size = var (find_global_var ppc_file "__block_sz") in
  L.iter (fun arg ->
    if isRegion arg then (
      let addAttribute_Task = find_function_sign ppc_file ("Add"^(arg_type2string arg.atype)^"Attribute_Task") in
      (*AddAttribute_Task(this, r, TPC_IN_ARG, sizeof(region_t), sizeof(region_t)/BLOCK_SZ);*)
      let div = BinOp(Div, sizeOf_region_t, Lval block_size, intType) in
      let args = [Lval this; CastE(voidPtrType, arg.address); arg_type2integer arg.atype; sizeOf_region_t; div ] in
      ilt := Call (None, Lval (var addAttribute_Task), args, locUnknown)::!ilt;
    )
  ) args;
  mkStmt (Instr (L.rev !ilt))
)

(* Preprocess the header file <header> and merges it with f.  The
 * given header should be in the gcc include path.  Modifies f
 *) (* the original can be found in lockpick.ml *)
let preprocessAndMergeWithHeader_x86 (f: file) (header: string) (def: string)
    : unit = (
  (* //Defining _GNU_SOURCE to fix "undefined reference to `__isoc99_sscanf'" *)
  ignore (Sys.command ("echo | gcc -E -D_GNU_SOURCE "^def^" "^header^" - >/tmp/_cil_rewritten_tmp.h"));
  let add_h = Frontc.parse "/tmp/_cil_rewritten_tmp.h" () in
  let f' = Mergecil.merge [add_h; f] "stdout" in
  f.globals <- f'.globals;
)

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
  (* TIMER_START(1); *)
  let tmptime1 = var (find_global_var f "_tmptime1_SCOOP__") in
  let rdtsc = find_function_sign f "rdtsc" in
  instrs := Call (Some tmptime1, Lval (var rdtsc), [], locUnknown)::!instrs;
  (* this = AddTask(); *)
  let addTask = find_function_sign f "AddTask" in
  instrs := Call (Some this, Lval (var addTask), [], locUnknown)::!instrs;
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
    stmts := (mkStmt (Instr (List.rev !instrs))::mapped)@[doRegions loc this f args];
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
  (* Clear_Handler(this ); *)
  let clear_Handler = find_function_sign f "Clear_Handler" in
  instrs := Call (None, Lval (var clear_Handler), [Lval this], locUnknown)::!instrs;
  (* G_ppe_stats.issue_ticks += TIMER_END(1); *)
  let gps_it = mkFieldAccess gps "issue_ticks" in
  let tmptime2 = var (find_global_var f "_tmptime2_SCOOP__") in
  instrs := Call (Some tmptime2, Lval (var rdtsc), [], locUnknown)::!instrs;
  let uint64_t = (find_type f "uint64_t") in
  let sub = BinOp(MinusA, Lval tmptime2 , Lval tmptime1, uint64_t) in
  instrs := Set (gps_it, sub, locUnknown)::!instrs;

  stmts := !stmts@[mkStmt (Instr !instrs)];

  incr func_id;
  (!stmts, [])
)
