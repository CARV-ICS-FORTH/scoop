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

(** Responsible for generating code for the ADAM runtime on the
    Cell Processor
    @author Foivos Zakkak, zakkak\@ics.forth.gr *)

open Cil
open Scoop_util
module E = Errormsg
module L = List

(** keeps the current funcid for the new tpc_function_* *)
let func_id = ref 0

(* XXX:this is for sdam *)
let querie_no = ref 0

(* a unique id for the tpc_function_* *)
let un_id = ref 0


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
  let lv = mkFieldAccess (var task_info) "local" in
  let t = typeOfLval lv in
  assert(isArrayType t);
  let i = ref 0 in

  let args = List.rev args in
  let arglist = List.map
    (fun (place, _) ->
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
  (spu_file: file) (unaligned_args: bool) (ppc_file: file)
  (orig_tname: string) (tid: int) : stmt = (
  let (i_m, arg_desc) = arg in
  let closure = mkFieldAccess this "closure" in
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
  if (Sdam.isSafeArg (*fd*) orig_tname tid arg_desc.aname) then (
    (* if(TPC_IS_SAFEARG(arg_flag)){

        this->closure.arguments[  this->closure.total_arguments ].size    = arg_size;
        this->closure.arguments[  this->closure.total_arguments ].flag    = arg_flag|TPC_START_ARG;

        this->closure.arguments[  this->closure.total_arguments ].eal_in  = (uint32_t) arg_addr64;
        this->closure.arguments[  this->closure.total_arguments ].eal_out = (uint32_t) arg_addr64;
        this->closure.total_arguments++;
        continue; //We don't need continue here, we are not in a loop :)
      }
      #define TPC_START_ARG   0x10
      #define TPC_SAFE_ARG    0x8
    *)
    il := Set(size, Lval arg_size, locUnknown)::!il;
    (* this->closure.arguments[  this->closure.total_arguments ].flag    = arg_flag|TPC_START_ARG|TPC_SAFE_ARG; *)
    il := Set(flag, integer ( (arg_type2int arg_desc.atype) lor 0x18), locUnknown)::!il;
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
      let args = [Lval this; CastE(voidPtrType, arg_addr); Lval arg_size; arg_type2integer arg_desc.atype ] in
      il := Call(None, Lval (var divideArgumentToBlocks), args, locUnknown)::!il;
      (* CLOSURE.arguments[ firstBlock ].flag|=TPC_START_ARG;
      tpc_common.h:20:#define TPC_START_ARG   0x10 *)
      let idxlv = addOffsetLval (Index(Lval bis, NoOffset)) arguments in
      let flag = mkFieldAccess idxlv "flag" in
      let bor = BinOp(BOr, Lval flag, integer 0x10, intType) in
      il := Set(flag, bor, locUnknown)::!il;
    ) else (
      (* CURRENT_ARGUMENT.flag = Flag|TPC_START_ARG; *)
      il := Set(flag, integer ( (arg_type2int arg_desc.atype) lor 0x10), locUnknown)::!il;
      (* CURRENT_ARGUMENT.size = Size; *)
      il := Set(size, Lval arg_size, locUnknown)::!il;
      (* CURRENT_ARGUMENT.stride = 0;*)
      il := Set(stride, (integer 0), locUnknown)::!il;
      (* AddAttribute_Task( Task, (void* )(Address), Flag, Size, &(CURRENT_ARGUMENT)); *)
      let addAttribute_Task = find_function_sign ppc_file "AddAttribute_Task" in
      let addrOf_args = AddrOf(idxlv) in
      let args = [Lval this; CastE(voidPtrType, arg_addr); arg_type2integer arg_desc.atype; Lval arg_size; addrOf_args ] in
      il := Call (None, Lval (var addAttribute_Task), args, locUnknown)::!il;
      (* CLOSURE.total_arguments++; *)
      il := Set(total_arguments, pplus, locUnknown)::!il;
    )
  );

  (* skipping assert( (((unsigned)arg_addr64&0xF) == 0) && ((arg_size&0xF) == 0)); *)
  mkStmt(Instr (List.rev !il))
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
    (args: arg_descr list) (ppc_file: file) (spu_file: file)
    : (fundec * (int * arg_descr) list) = (
  incr un_id;
  print_endline ("Creating tpc_function_" ^ func_vi.vname ^ (string_of_int !un_id));
  let args = List.sort sort_args (List.rev args) in
  let skeleton = find_function_fundec ppc_file "tpc_call_tpcAD65" in
  let f_new = copyFunction skeleton ("tpc_function_" ^ func_vi.vname ^ (string_of_int !un_id)) in
  f_new.sformals <- [];
  (* set the formals to much the original function's arguments *)
  setFunctionTypeMakeFormals f_new func_vi.vtype;
  setFunctionReturnType f_new intType;
  formalScalarsToPointers loc f_new;
  (* create the arg_size*[, arg_elsz*, arg_els*] formals *)
  let args_num = (List.length f_new.sformals)-1 in
  if ( args_num <> (List.length args)-1 ) then
    E.s (errorLoc loc "Number of arguments described in #pragma doesn't much the \
          number of arguments in the function declaration");
  for i = 0 to args_num do
    let ex_arg = (List.nth oargs i) in
    let name = getNameOfExp ex_arg in
    let arg_desc = List.find ( fun a -> (a.aname = name) ) args in
    ignore(makeFormalVar f_new ("arg_size"^(string_of_int i)) intType);
    if (isStrided arg_desc) then (
      ignore(makeFormalVar f_new ("arg_els"^(string_of_int i)) intType);
      ignore(makeFormalVar f_new ("arg_elsz"^(string_of_int i)) intType)
    );
  done;

  let this = var (find_local_var f_new "this") in
  (* this->closure.funcid = (uint8_t)funcid; *)
  let this_closure = mkFieldAccess this "closure" in
  let funcid_set = Set (mkFieldAccess this_closure "funcid",
  CastE(find_type spu_file "uint8_t", integer !func_id), locUnknown) in
  let stmts = ref [mkStmtOneInstr funcid_set] in
  (*(* this->closure.total_arguments = (uint8_t)arguments.size() *)
  instrs := Set (mkFieldAccess this_closure "total_arguments",
  CastE(find_type spu_file "uint8_t", integer (args_num+1)), locUnknown)::!instrs;*)

  let uint32_t = (find_type spu_file "uint32_t") in
  (* uint32_t block_index_start *)
  let bis = var (makeLocalVar f_new "block_index_start" uint32_t) in

  let args_n =
  (* if we have arguments *)
  if (f_new.sformals <> []) then (
    (* volatile vector unsigned char *tmpvec   where vector is __attribute__((altivec(vector__))) *)
    let args_n = number_args args oargs in
    let i_n = ref (args_num+1) in
    incr querie_no;
    let mapped = (List.map
      (fun arg -> decr i_n; doArgument !i_n this bis f_new arg spu_file
                  !unaligned_args ppc_file func_vi.vname !querie_no)
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

(** parses the #pragma css task arguments *)
let rec scoop_process ppc_file loc pragma =
  match pragma with
    | (ACons(arg_typ, args)::rest) ->
      let (hp, lst) = scoop_process ppc_file loc rest in
      (hp, (scoop_process_args false ppc_file arg_typ loc args)@lst)
    | [] -> (false, [])
    | _ -> E.s (errorLoc loc "Syntax error in #pragma css task\n");

(** populates the global list of tasks [tasks] *)
class findTaskDeclVisitor (cgraph : Callgraph.callgraph) ppc_f spu_f pragma =
  object
  inherit nopCilVisitor
  val mutable spu_tasks = []
  val callgraph = cgraph
  val ppc_file = ppc_f
  val spu_file = spu_f
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
                attrParamToExp ppc_file loc exp::[attrParamToExp ppc_file loc (L.hd rest)]
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

                let instr =
                  Call (None, Lval (var new_fd.svar), L.rev !call_args, locUnknown)
                in
                let call = mkStmt (Instr(instr::restInst)) in
                ChangeTo(call)
              in
              let rest_f2 var_i =
                let (new_fd, args) =
                  make_tpc_func loc var_i oargs args ppc_file spu_file
                in
                Lockutil.add_after_s ppc_file var_i.vname new_fd;
                spu_tasks <- (funname, (new_fd, var_i, args))::spu_tasks;
                rest_f new_fd
              in
              (* try to find the function definition *)
              try
                (* checking for the function definition *)
                let task = find_function_fundec_g ppc_file.globals funname in
                (* copy itself and the callees *)
                deep_copy_function funname callgraph spu_file ppc_file;
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
