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

(** Responsible for generating code for the ADAM runtime on the
    Cell Processor 
    @author Foivos Zakkak, zakkak\@ics.forth.gr *)

open Cil
open Scoop_util
module E = Errormsg

(** keeps the current funcid for the new tpc_function_* *)
let func_id = ref 0


(** Creates the body of the cases for the [execute_task]'s switch statement
    @param task the task to create this case for
    @param task_info the [varinfo] of the [execute_task]'s {e task_info} formal
    argument
    @param ex_task the [varinfo] of the [execute_task]'s {e ex_task} formal
    argument
    @param args a list of [(int * arg_descr)] pairs carrying the correct
    position of each argument plus its description
    @return the body of the new case
*)
(*
    case 0:
      matrix_add_row(task_info->local[0], task_info->local[2], task_info->local[1]);
      task_info->state = EXECUTED; no need for it in every case
                                      moved it out of the swith
      break;
*)
let make_case execfun (task: varinfo) (task_info: varinfo)
              (ex_task: varinfo) (args: (int * arg_descr) list): stmt = (
  assert(isFunctionType task.vtype);
  (*TODO maybe start handling the return values of the tasks? *)
  let ret, arglopt, hasvararg, _ = splitFunctionType task.vtype in
  assert(not hasvararg);
  let argl = match arglopt with None -> [] | Some l -> l in
  let lv = mkPtrFieldAccess (var task_info) "local" in
  let t = typeOfLval lv in
  assert(isArrayType t);
  let i = ref 0 in

  let args = List.rev args in
  let arglist = List.map
    (fun (place, (name, _)) ->
      (* task_state->local[i] *)
      let idxlv = addOffsetLval (Index(integer !i, NoOffset)) lv in
      let (_, argt, _) = (List.nth argl place) in
      incr i;
      let lv = Lval (
        if (isScalar_t argt) then
          mkMem ( mkCast (Lval(idxlv)) (TPtr(argt, [])) ) NoOffset
        else
          idxlv
      ) in
      (place, mkCast lv argt )
    )
    args
  in
  let arglist = List.sort comparator arglist in
  let (_, arglist) = List.split arglist in
  mkStmt (Instr ([Call (None, Lval (var task), arglist, locUnknown)]))
)

(** Produces the required instructions to push an argument in a task descriptor
    @param i the argument's number
    @param this the {e this}'s [Cil.lval]. {e this} is defined in the skeleton
    used by {b SCOOP}
    @param bis the {e block_index_start}'s [Cil.lval]. {e block_index_start} is
    defined as local of tpc_function_* by make_tpc_func.
    @param arg a [(int * arg_descr)] pair with all the info about the arg to
    pass in the task descriptor
    @param spu_file the spu file
    @param unaligned_args flag about supporting unaligned arguments
    @param ppc_file the ppc file
    @return a statement including the produced instructions
*)
let doArgument (i: int) (this: lval) (bis: lval) (fd: fundec) (arg: (int * arg_descr) )
  (spu_file: file) (unaligned_args: bool) (ppc_file: file) : stmt = (
  let (i_m, (arg_name, (arg_type, _, _, _))) = arg in
  let closure = mkPtrFieldAccess this "closure" in
  let uint32_t = (find_type spu_file "uint32_t") in
  let arg_size = var (find_formal_var fd ("arg_size"^(string_of_int i))) in
  let actual_arg = List.nth fd.sformals i_m in
  let arg_addr = (
    if (isScalar_v actual_arg) then
      mkAddrOf( var actual_arg)
    else
      Lval( var actual_arg)
  ) in
  let il = ref [] in
  let total_arguments = mkFieldAccess closure "total_arguments" in
  let arguments = mkFieldAccess closure "arguments" in
  let t = typeOfLval arguments in
  assert(isArrayType t);
  (* this->closure.arguments[  this->closure.total_arguments ].stride=0;
     due to not supporting stride args*)
  let idxlv = addOffsetLval (Index(Lval total_arguments, NoOffset)) arguments in
  let stride = mkFieldAccess idxlv "stride" in

  let size = mkFieldAccess idxlv "size" in
  let flag = mkFieldAccess idxlv "flag" in
  let pplus = (BinOp(PlusA, Lval total_arguments, integer 1, intType)) in

  (* invoke isSafeArg from PtDepa to check whether this argument is a no dep *)
  if (Ptdepa.isSafeArg fd arg_name) then (
    (* if(TPC_IS_SAFEARG(arg_flag)){

        this->closure.arguments[  this->closure.total_arguments ].size    = arg_size;
        this->closure.arguments[  this->closure.total_arguments ].flag    = arg_flag|TPC_START_ARG;

        this->closure.arguments[  this->closure.total_arguments ].eal_in  = (uint32_t) arg_addr64;
        this->closure.arguments[  this->closure.total_arguments ].eal_out = (uint32_t) arg_addr64;
        this->closure.total_arguments++;
        continue; //We don't need continue here, we are not in a loop :)
      }
      #define TPC_START_ARG   0x10
    *)
    il := Set(size, Lval arg_size, locUnknown)::!il;
    il := Set(flag, integer ( (arg_t2int arg_type) lor 0x10), locUnknown)::!il;
    let eal_in = mkFieldAccess idxlv "eal_in" in
    il := Set(eal_in, CastE(uint32_t, arg_addr), locUnknown)::!il;
    let eal_out = mkFieldAccess idxlv "eal_out" in
    il := Set(eal_out, CastE(uint32_t, arg_addr), locUnknown)::!il;
    il := Set(total_arguments, pplus, locUnknown)::!il;
    (*stl := (*mkStmt(Continue locUnknown)::*)[mkStmt(Instr (List.rev !il))];*)
  ) else (

    (*
      #ifdef BLOCKING
        unsigned int firstBlock;
        firstBlock = Task->closure.total_arguments;
        DivideArgumentToBlocks( Task, Address, Size, Flag);
        CLOSURE.arguments[ firstBlock ].flag|=TPC_START_ARG;
      #else
        CURRENT_ARGUMENT.flag = Flag|TPC_START_ARG;
        CURRENT_ARGUMENT.size = Size;
        CURRENT_ARGUMENT.stride = 0;
        AddAttribute_Task( Task, (void* )(Address), Flag, Size, &(CURRENT_ARGUMENT));
        CLOSURE.total_arguments++;
      #endif
    *)
    if (!blocking) then (
      (* firstBlock = Task->closure.total_arguments; *)
      il := Set(bis, Lval total_arguments, locUnknown)::!il;
      (* DivideArgumentToBlocks( Task, Address, Size, Flag); *)
      let divideArgumentToBlocks = find_function_sign ppc_file "DivideArgumentToBlocks" in
      let args = [Lval this; CastE(voidPtrType, arg_addr); Lval arg_size; arg_t2integer arg_type ] in
      il := Call(None, Lval (var divideArgumentToBlocks), args, locUnknown)::!il;
      (* CLOSURE.arguments[ firstBlock ].flag|=TPC_START_ARG;
      tpc_common.h:20:#define TPC_START_ARG   0x10 *)
      let idxlv = addOffsetLval (Index(Lval bis, NoOffset)) arguments in
      let flag = mkFieldAccess idxlv "flag" in
      let bor = BinOp(BOr, Lval flag, integer 0x10, intType) in
      il := Set(flag, bor, locUnknown)::!il;
    ) else (
      (* CURRENT_ARGUMENT.flag = Flag|TPC_START_ARG; *)
      il := Set(flag, integer ( (arg_t2int arg_type) lor 0x10), locUnknown)::!il;
      (* CURRENT_ARGUMENT.size = Size; *)
      il := Set(size, Lval arg_size, locUnknown)::!il;
      (* CURRENT_ARGUMENT.stride = 0;*)
      il := Set(stride, (integer 0), locUnknown)::!il;
      (* AddAttribute_Task( Task, (void* )(Address), Flag, Size, &(CURRENT_ARGUMENT)); *)
      let addAttribute_Task = find_function_sign ppc_file "AddAttribute_Task" in
      let addrOf_args = AddrOf(idxlv) in
      let args = [Lval this; CastE(voidPtrType, arg_addr); arg_t2integer arg_type; Lval arg_size; addrOf_args ] in
      il := Call (None, Lval (var addAttribute_Task), args, locUnknown)::!il;
      (* CLOSURE.total_arguments++; *)
      il := Set(total_arguments, pplus, locUnknown)::!il;
    )
  );

  (* skipping assert( (((unsigned)arg_addr64&0xF) == 0) && ((arg_size&0xF) == 0)); *)
  mkStmt(Instr (List.rev !il));
)

(** Creates a tpc_ version of the function (for use on the ppc side)
 * uses the tpc_call_tpcAD65 from tpc_skeleton_tpc.c as a template
 * @param func_vi the varinfo of the original function
 * @param oargs the original arguments given to the annotated call
 * @param args the argument descriptions given in the annotation
 * @param ppc_file the ppc file
 * @param spu_file the spu file
 * @return the new function declaration paired with a list of numbered argument
 *         descriptors
 *)
let make_tpc_func (loc: location) (func_vi: varinfo) (oargs: exp list)
    (args: arg_descr list) (ppc_file: file ref) (spu_file: file ref)
    : (fundec * (int * arg_descr) list) = (
  print_endline ("Creating tpc_function_" ^ func_vi.vname);
  let args = List.sort sort_args (List.rev args) in
  let skeleton = find_function_fundec (!ppc_file) "tpc_call_tpcAD65" in
  let f_new = copyFunction skeleton ("tpc_function_" ^ func_vi.vname) in
  f_new.sformals <- [];
  (* set the formals to much the original function's arguments *)
  setFunctionTypeMakeFormals f_new func_vi.vtype;
  setFunctionReturnType f_new intType;
  formalScalarsToPointers f_new;
  (* create the arg_size*[, arg_elsz*, arg_els*] formals *)
  let args_num = (List.length f_new.sformals)-1 in
  assert (args_num >= 0);
  if ( args_num <> (List.length args)-1 ) then (
    ignore(E.error "%a\n\tNumber of arguments described in #pragma doesn't much the \
          number of arguments in the function declaration" d_loc loc);
    exit (1)
  );
  for i = 0 to args_num do
    let ex_arg = (List.nth oargs i) in
    let name = getNameOfExp ex_arg in
    let (_, (arg_type, _, _, _)) = List.find 
      ( fun (vname, _) -> if( vname = name) then true else false)
    args in
    ignore(makeFormalVar f_new ("arg_size"^(string_of_int i)) intType);
    if (is_strided arg_type) then (
      ignore(makeFormalVar f_new ("arg_els"^(string_of_int i)) intType);
      ignore(makeFormalVar f_new ("arg_elsz"^(string_of_int i)) intType)
    );
  done;

  let this = var (find_local_var f_new "this") in
  (* this->closure.funcid = (uint8_t)funcid; *)
  let this_closure = mkPtrFieldAccess this "closure" in
  let funcid_set = Set (mkFieldAccess this_closure "funcid",
  CastE(find_type !spu_file "uint8_t", integer !func_id), locUnknown) in
  let stmts = ref [mkStmtOneInstr funcid_set] in
  (*(* this->closure.total_arguments = (uint8_t)arguments.size() *)
  instrs := Set (mkFieldAccess this_closure "total_arguments",
  CastE(find_type !spu_file "uint8_t", integer (args_num+1)), locUnknown)::!instrs;*)

  let uint32_t = (find_type !spu_file "uint32_t") in
  (* uint32_t block_index_start *)
  let bis = var (makeLocalVar f_new "block_index_start" uint32_t) in
  
  let args_n =
  (* if we have arguments *)
  if (f_new.sformals <> []) then (
    (* volatile vector unsigned char *tmpvec   where vector is __attribute__((altivec(vector__))) *)
    let args_n = number_args args oargs in
    let i_n = ref (args_num+1) in
    let mapped = (List.map 
      (fun arg -> decr i_n; doArgument !i_n this bis f_new arg !spu_file
                  !unaligned_args !ppc_file)
      args_n) in
    stmts := mapped@(!stmts);
    args_n
  ) else [] in

  (* Foo_32412312231 is located before assert(this->closure.total_arguments<MAX_ARGS); 
    for x86*)
  let map_fun = (fun s -> Scoop_util.replace_fake_call_with_stmt s "Foo_32412312231" (List.rev !stmts)) in
  f_new.sbody.bstmts <- List.map map_fun f_new.sbody.bstmts;

  incr func_id;
  (f_new, args_n)
)
