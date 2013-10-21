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

(** Responsible for generating code for the TPC runtime on the
    Cell Processor
    @author Foivos Zakkak, zakkak\@ics.forth.gr *)

open Cil
module SU = Scoop_util
module E  = Errormsg
module L  = List

(** holds the TPC's SPEs queue size *)
let queue_size = ref "0"

let options = [
  "--queue-size",
  Arg.String(fun s -> queue_size := s),
  " SCOOP: Specify the queue size for Cell. Defined in the Makefile as MAX_QUEUE_ENTRIES";
]

(** Creates the body of the cases for the [execute_task]'s switch statement
    @param task the task to create this case for
    @param task_info the [varinfo] of the [execute_task]'s {e task_info} formal
    argument
    @param ex_task the [varinfo] of the [execute_task]'s {e ex_task} formal
    argument
    @param args a list of [(int * arg_descr)] pairs carrying the correct
    position of each argument plus its description
    @returns the body of the new case
*)
(*
    case 0:
      //printf("SPU: Dispatch (%p) (%d,%d,%p)\n", task_info->ls_addr,
//          task_info->state, task_info->dmatag, task_info->dmalist);
      arg1 = (float * )task_info->ls_addr;
      arg2 = (float * )((void * )arg1 + ex_task->arguments[0].size);
      arg3 = (int * )((void * )arg2 + ex_task->arguments[1].size);
      matrix_add_row(arg1, arg2, arg3);
      task_info->state = EXECUTED; no need for it in every case
                                      moved it out of the swith
      break;
*)
let make_case execfun (task: varinfo) (task_info: varinfo)
              (ex_task: varinfo) (args: (int * SU.arg_descr) list): stmt = (
  let res = ref [] in
  assert(isFunctionType task.vtype);
  let ret, arglopt, hasvararg, _ = splitFunctionType task.vtype in
  assert(not hasvararg);
  let argl = match arglopt with None -> [] | Some l -> l in
  let argaddr = makeTempVar execfun voidPtrType in
  res := Set(var argaddr, Lval (SU.mkFieldAccess (var task_info) "ls_addr"), locUnknown) :: !res;
(*  else begin
    res := Set(var argaddr, Lval (mkFieldAccess (var task_info) "local"), locUnknown) :: !res;
  end*)
  let nextaddr n stride =
    let lv = SU.mkFieldAccess (var ex_task) "arguments" in
    let t = typeOfLval lv in
    assert(isArrayType t);
    let idxlv = addOffsetLval (Index(integer n, NoOffset)) lv in
    let szlv = SU.mkFieldAccess idxlv "size" in
    let plus =
      if (stride) then
        (* next = previous + ((ex_task->arguments[pre].size >>16U)
                        *(ex_task->arguments[pre].size & 0x0FFFFU)) *)
        let els = BinOp(Shiftrt, Lval(szlv), integer 16, intType) in
        let elsz = BinOp(BAnd, Lval(szlv), integer 0x0FFFF, intType) in
        (BinOp(PlusPI, (Lval(var argaddr)), BinOp(Mult, els, elsz,intType), voidPtrType))
      else
        (* next = previous + ex_task->arguments[pre].size *)
        (BinOp(PlusPI, (Lval(var argaddr)), Lval(szlv), voidPtrType))
    in
    Set(var argaddr, plus, locUnknown);
  in
  let i = ref 0 in
  let carry = ref dummyInstr in
  let args = L.rev args in
  let arglist = List.map
    (fun (place, arg_desc) ->
      let argvar = makeTempVar execfun voidPtrType in
(*      let rec castexp atyp = match atyp with
        TInt(_, _)
        | TFloat(_, _)
        | TEnum(_, _)
        | TComp(_, _) ->
          CastE(argt, Lval(mkMem (CastE( TPtr(argt, []), Lval(var argvar))) NoOffset))
        | TNamed(_, _) -> castexp (unrollType atyp)
        | _ -> CastE(argt, Lval(var argvar))
      in*)
      let castinstr = Set(var argvar, Lval(var argaddr), locUnknown) in
      let (_, argt, _) = (List.nth argl place) in
      let advptrinstr = nextaddr !i (SU.isStrided arg_desc) in
      incr i;
      if !carry <> dummyInstr then res := !carry::!res;
      carry := advptrinstr;
      res := castinstr :: !res;
      let lv = Lval (
        if (SU.isScalar_t argt) then
          mkMem ( mkCast (Lval(var argvar)) (TPtr(argt, [])) ) NoOffset
        else
          var argvar
      ) in
      (place, mkCast lv argt)
    )
    args
  in
  let arglist = L.sort SU.comparator arglist in
  let (_, arglist) = L.split arglist in
  res := Call (None, Lval (var task), arglist, locUnknown)::!res;
  mkStmt (Instr (L.rev !res))
)

class codegen (cgraph : Callgraph.callgraph) file pragma includePath filename isblade =
object (self) inherit Scoop_codegen.codegen cgraph file pragma includePath as super

  val scoop_barrier       = "tpc_wait_all"
  val scoop_start         = "tpc_init"
  val scoop_finish        = "tpc_shutdown"
  val scoop_malloc        = "tpc_malloc"
  val scoop_free          = "tpc_free"
  val runtime             = "cell"
  (** The file for the SPE *)
  val spu_file      =
    let out_name = String.sub filename 0 ((String.length filename) -2) in
    { dummyFile with fileName = (out_name^"_func.c");};
  val isBlade = isblade

  (** Write the generated files to disk *)
  method writeFile : unit =
    SU.writeFile new_file;
    SU.writeFile spu_file;

  method makeTaskTable : unit =
    (* Cell needs a special execute_function to dispatch tasks instead
     * of a task table *)
    let (_, tasks) = List.split (L.rev found_tasks) in
    spu_file.globals <-
      spu_file.globals@[Scoop_make_exec.make_exec_func spu_file tasks make_case];

  method declareGlobals : unit = ();

  method parseFile (disableSDAM: bool) : unit =
    (* No dependencies on CELL *)
    (* Ptdepa.find_dependencies new_file disableSDAM; *)
    Cil.iterGlobals new_file
                    (function
                      | GFun(fd,_) ->
                        SU.currentFunction := fd;
                        ignore(visitCilFunction (self :> Cil.cilVisitor) fd);
                      | _ -> ()
                    );
    new_file.globals <- List.filter SU.isNotSkeleton new_file.globals;

  method preprocessAndMergeWithHeader flags : unit =
    if (!queue_size = "0") then E.s (error "No queue_size specified. Exiting!");
    let def = flags^
      " -DMAX_QUEUE_ENTRIES="^(!queue_size)^
        ( if (!SU.stats) then " -DSTATISTICS=1" else " ")^
        ( if (isBlade) then " -DBLADE=1" else " ")
    in
    SU.preprocessAndMergeWithHeader_cell new_file
                                         (includePath^"/scoop/tpc_scoop.h")
                                         (" -DPPU=1"^(def))
                                         includePath;

    (* copy all typedefs and enums/structs/unions from ppc_file to spu_file
      plus the needed headers *)
    let new_types_l = List.filter SU.is_typedef new_file.globals in
    spu_file.globals <- new_types_l;
    SU.preprocessAndMergeWithHeader_cell spu_file
                                         (includePath^"/scoop/tpc_scoop.h")
                                         (" -DSPU=1"^(def))
                                         includePath;


  (** Creates a tpc_ version of the function (for use on the ppc side)
   * uses the tpc_call_tpcAD65 from the cell's header file as a template
   * @param loc the location of the original function
   * @param func_vi the varinfo of the original function
   * @param oargs the original arguments given to the annotated call
   * @param args the argument descriptions given in the annotation
   * @return the new function declaration paired with a list of numbered argument
   *         descriptors
   *)
  method private make_task_spawn (loc: location) (func_vi: varinfo)
                                 (oargs: exp list) (args: SU.arg_descr list)
                 : (fundec * (int * SU.arg_descr) list) = (
    print_endline ("Creating tpc_function_" ^ func_vi.vname);
    let args = L.sort SU.sort_args (L.rev args) in
    let skeleton = Scoop_util.find_function_fundec new_file "tpc_call_tpcAD65" in
    let f_new = copyFunction skeleton ("tpc_function_" ^ func_vi.vname) in
    f_new.sformals <- [];
    (* set the formals to much the original function's arguments *)
    setFunctionTypeMakeFormals f_new func_vi.vtype;
    SU.setFunctionReturnType f_new intType;
    SU.formalScalarsToPointers loc f_new;
    (* create the arg_size*[, arg_elsz*, arg_els*] formals *)
    let args_num = (L.length f_new.sformals)-1 in
    if ( args_num <> (List.length args)-1 ) then
      E.s (errorLoc loc "Number of arguments described in #pragma doesn't much the \
                         number of arguments in the function declaration");
    for i = 0 to args_num do
      let ex_arg = (L.nth oargs i) in
      let name = SU.getNameOfExp ex_arg in
      let arg_desc = L.find ( fun a -> ( a.SU.aname = name) ) args in
      ignore(makeFormalVar f_new ("arg_size"^(string_of_int i)) intType);
      if (SU.isStrided arg_desc) then (
        ignore(makeFormalVar f_new ("arg_els"^(string_of_int i)) intType);
        ignore(makeFormalVar f_new ("arg_elsz"^(string_of_int i)) intType)
      );
    done;

    let avail_task = var (SU.find_local_var f_new "avail_task") in
    let instrs : instr list ref = ref [] in
    let uint8_t = SU.find_type spu_file "uint8_t" in
    (* avail_task->funcid = (uint8_t)funcid; *)
    instrs := Set (SU.mkFieldAccess avail_task "funcid",
                   mkCast (integer !func_id) uint8_t, locUnknown):: !instrs;
    (* avail_task->total_arguments = (uint8_t)arguments.size() *)
    let args_num_i = integer (args_num+1) in
    instrs := Set (SU.mkFieldAccess avail_task "total_arguments",
                   mkCast args_num_i uint8_t, locUnknown)::!instrs;

    let args_n =
      (* if we have arguments *)
      if (f_new.sformals <> []) then (
        (* volatile vector unsigned char *tmpvec   where vector is __attribute__((altivec(vector__))) *)
        let vector_uchar_p = TPtr(TInt(IUChar, [Attr("volatile", [])]), [SU.ppu_vector]) in
        let tmpvec = var (makeLocalVar f_new "tmpvec" vector_uchar_p) in
    (* struct tpc_arg_element local_arg *)
        let arg_typ = (SU.find_tcomp spu_file "tpc_arg_element") in
        (*     let arr_typ = TArray(arg_typ, Some(args_num_i), []) in *)
        let local_arg = var (makeLocalVar f_new "local_arg" arg_typ) in
        let args_n = SU.number_args args oargs in
        let i_n = ref (args_num+1) in
        let mapped = (L.map
                        (fun arg ->
                         decr i_n;
                         self#doArgument !i_n local_arg avail_task tmpvec f_new arg
                                    !Scoop_util.stats )
                        args_n) in
        instrs := (L.flatten mapped)@(!instrs);
        (*    for i = 0 to args_num do
              let ex_arg = (L.nth oargs i) in
              let name = SU.getNameOfExp ex_arg in
              let arg = L.find ( fun (vname, _) -> if( vname = name) then true else false) args in
        (* local_arg <- argument description *)
        instrs := (self#doArgument i local_arg avail_task tmpvec f_new arg
                    !Scoop_util.stats spu_file )@(!instrs);
         done;*)
        args_n
      ) else [] in

    (* insert instrs before avail_task->active = ACTIVE;
    we place a Foo_32412312231() call just above avail_task->active = ACTIVE
    to achieve that for cell *)
    f_new.sbody.bstmts <- L.map (fun s -> Lockutil.replace_fake_call s "Foo_32412312231" (L.rev !instrs)) f_new.sbody.bstmts;

    incr func_id;
    (f_new, args_n)
  )

  (** Produces the required instructions to push an argument in a task descriptor
    @param i the argument's number
    @param local_arg the {e local_arg}'s [Cil.lval]. {e local_arg} is defined
    as local of tpc_function_* by make_task_spawn.
    @param avail_task the {e avail_task}'s [Cil.lval]. {e avail_task} is defined
    in the skeleton used by {b SCOOP}
    @param tmpvec the {e tmpvec}'s [Cil.lval]. {e tmpvec} is defined
    as local of tpc_function_* by make_task_spawn.
    @param fd the tpc_function_* function declaration
    @param arg a [(int * arg_descr)] pair with all the info about the arg to
    pass in the task descriptor
    @param stats flag about generating code for statistics
    @return a statement including the produced instructions
   *)
  method private doArgument (i: int) (local_arg: lval) (avail_task: lval)
                            (tmpvec: lval) (fd: fundec)
                            (arg: (int * SU.arg_descr)) (stats: bool)
                 : instr list = (
    let (i_m, arg_desc) = arg in
    let arg_size = Lval( var (SU.find_formal_var fd ("arg_size"^(string_of_int i_m)))) in
    let actual_arg = L.nth fd.sformals i_m in
    let arg_addr = (
      if (SU.isScalar_v actual_arg) then
        mkAddrOf (var actual_arg)
      else
        Lval( var actual_arg)
    ) in
    let il = ref [] in
    (* tmpvec = (volatile vector unsigned char * )&avail_task->arguments[i]; *)

    (*if( TPC_IS_STRIDEARG(arg_flag) ) {
       arg_bytes = TPC_EXTRACT_STRIDEARG_ELEMSZ(arg_size)*TPC_EXTRACT_STRIDEARG_ELEMS(arg_size);
     } else {
       arg_bytes = arg_size;
     }
     total_bytes += ( arg_bytes<< TPC_IS_INOUTARG(arg_flag));*)
    if (stats) then (
      let total_bytes = var (SU.find_local_var fd "total_bytes") in
      (*     let arg_bytes = var (SU.find_local_var fd "arg_bytes") in *)
      let arg_bytes =
        if (SU.isStrided arg_desc) then (
          (*FIXME not sure about i here *)
          let arg_elsz = Lval( var (SU.find_formal_var fd ("arg_elsz"^(string_of_int i)))) in
          let arg_els = Lval( var (SU.find_formal_var fd ("arg_els"^(string_of_int i)))) in
          (* arg_bytes = TPC_EXTRACT_STRIDEARG_ELEMSZ(arg_size)*TPC_EXTRACT_STRIDEARG_ELEMS(arg_size); *)
          BinOp(Mult, arg_els, arg_elsz, intType)
        ) else (
          (* arg_bytes = arg_size; *)
          arg_size
        )
      in
      (* total_bytes += ( arg_bytes<< TPC_IS_INOUTARG(arg_flag)); *)
      let total_size =
        if (SU.isStrided arg_desc) then (
          BinOp(PlusA, Lval(total_bytes), BinOp(Mult, integer 2, arg_bytes, intType), intType)
        ) else (
          BinOp(PlusA, Lval(total_bytes), arg_bytes, intType)
        )
      in
      il := Set(total_bytes, total_size, locUnknown)::!il
    );
    let vector_uchar_p = TPtr(TInt(IUChar, [Attr("volatile", [])]), [SU.ppu_vector]) in
    let av_task_arg = SU.mkFieldAccess avail_task "arguments" in
    let av_task_arg_idx = addOffsetLval (Index(integer i,NoOffset)) av_task_arg in
    il := Set(tmpvec, mkCast (mkAddrOf av_task_arg_idx) (vector_uchar_p) , locUnknown)::!il;

    (*   let local_arg_idx = addOffsetLval (Index(integer i,NoOffset)) local_arg in *)
    (* local_arg.eal = (uint32_t)(arg_addr64); *)
    let eal = SU.mkFieldAccess local_arg "eal" in
    il := Set(eal, mkCast arg_addr (SU.find_type spu_file "uint32_t"), locUnknown)::!il;
    let size = SU.mkFieldAccess local_arg "size" in
    if (SU.isStrided arg_desc) then (
      (*FIXME not sure about i here *)
      let arg_elsz = Lval( var (SU.find_formal_var fd ("arg_elsz"^(string_of_int i)))) in
      let arg_els = Lval( var (SU.find_formal_var fd ("arg_els"^(string_of_int i)))) in
      (* #define TPC_BUILD_STRIDEARG(elems, elemsz)    (((elems)<<16U) | (elemsz)) *)
      (* local_arg.size = TPC_BUILD_STRIDEARG(els,elsz); *)
      let build_stride = BinOp(BOr, BinOp(Shiftlt, arg_els, (integer 16), intType), arg_elsz, intType) in
      il := Set(size, build_stride, locUnknown)::!il;
      (* local_arg.stride = arg_size; *)
      let stride = SU.mkFieldAccess local_arg "stride" in
      il := Set(stride, arg_size, locUnknown)::!il;
    ) else
      (* local_arg.size = arg_size; *)
      il := Set(size, arg_size, locUnknown)::!il;
    (* local_arg.flag = arg_flag; *)
    let flag = SU.mkFieldAccess local_arg "flag" in
    il:= Set(flag, SU.arg_type2integer arg_desc.SU.atype, locUnknown)::!il;
    (* *tmpvec = *((volatile vector unsigned char * )&local_arg); *)
    let casted_la = mkCast (mkAddrOf local_arg) vector_uchar_p in
    il := Set(mkMem (Lval(tmpvec)) NoOffset, Lval(mkMem casted_la NoOffset), locUnknown)::!il;
    !il
  )

  (** parses the #pragma ... task arguments *)
  method private process_task_pragma loc pragma_args =
    match pragma_args with
    | (ACons(arg_typ, args)::rest) ->
      let lst = self#process_task_pragma loc rest in
      (SU.scoop_process_args false new_file arg_typ loc args)@lst
    | [] -> []
    | _ -> E.s (errorLoc loc "Syntax error in #pragma css task\n");

  (* populates the global list of tasks [tasks] *)
  (** visits all stmts and checks for pragma directives *)
  method vstmt (s: stmt) : stmt visitAction =
    let debug = ref false in
    let prags = s.pragmas in
    if (prags <> []) then (
      match (List.hd prags) with
        (* Support #pragma css ... *)
        (Attr(pr_str, rest), loc) when pr_str = pragma_str -> (
          let make_func_call = SU.make_func_call new_file loc s in
          match rest with
          (* Support #pragma css wait all *)
          | [AStr("wait"); AStr("all")]
          (* Support #pragma css barrier*)
          | [AStr("barrier")] ->
            make_func_call [] scoop_barrier
          (* Support #pragma css start *)
          | [AStr("start")]
          (* Support #pragma css start(...) *)
          | [ACons("start", [])] ->
            make_func_call [] scoop_start
          | [ACons("start", exps)] ->
            make_func_call exps scoop_start
          (* Support #pragma css finish *)
          | [AStr("finish")] ->
            make_func_call [] scoop_finish
          (* Support #pragma css malloc *)
          | [AStr("malloc")] -> (
            match s.skind with
            | Instr(Call(Some res, Lval((Var(vi), _)), oargs, loc)::restInst) -> (
              let malloc = SU.find_function_sign new_file scoop_malloc in
              let instr = Call (Some res, Lval (var malloc), oargs, locUnknown) in
              ChangeTo(mkStmtOneInstr instr)
            )
            | _ -> DoChildren
          )
          (* Support #pragma css free *)
          | [AStr("free")] -> (
            match s.skind with
            | Instr(Call(_, Lval((Var(vi), _)), oargs, loc)::restInst) -> (
              let free = SU.find_function_sign new_file scoop_free in
              let instr = Call (None, Lval (var free), oargs, locUnknown) in
              ChangeTo(mkStmtOneInstr instr)
            )
            | _ -> DoChildren
          )
          (* Support #pragma css task... *)
          | AStr("task")::rest -> (
            match s.skind with
            Instr(Call(_, Lval((Var(vi), _)), oargs, loc)::restInst) -> (
              let funname = vi.vname in
              (* process the pragma ... task*)
              let args    = self#process_task_pragma loc rest in
              SU.dbg_print debug ("Found task \""^funname^"\"");

            (* check whether all argument annotations correlate to an actual argument *)
              let check arg =
                if ( not (L.exists (fun e -> ((SU.getNameOfExp e)=arg.SU.aname)) oargs) ) then (
                  let args_err = ref "(" in
                  List.iter
                    (fun e ->
                     args_err := ((!args_err)^" "^(SU.getNameOfExp e)^",") )
                    oargs;
                  args_err := ((!args_err)^")");
                  E.s (errorLoc loc "#1 Argument \"%s\" in the pragma directive not found in %s" arg.SU.aname !args_err);
                ) in
              L.iter check args;

              let rest_f new_fd =
                (* add arguments to the call *)
                let call_args =
                  let expS2P = SU.expScalarToPointer loc in
                  ref (L.rev (L.map expS2P oargs))
                in

                (* for each actual argument of the call find it's (pragma)
                    declared size and push it to the argument list of the
                    new call *)
                let rec getSizeNstride = function
                  | Lval ((Var(vi),_))
                  | StartOf ((Var(vi),_)) -> (
                    try
                      let arg_desc =
                        L.find (fun a -> (a.SU.aname=vi.vname)) args
                      in
                      let vsize = SU.getSizeOfArg arg_desc in
                      call_args := vsize::!call_args;
                      if (SU.isStrided arg_desc) then (
                        let (vels, velsz) =
                          match arg_desc.SU.atype with
                              SU.Stride(_, _, els, elsz) -> (els, elsz)
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
              try
                (* fast workaround *)
                (* check if we have seen this function before *)
                let (new_fd, _, _) = List.assoc funname found_tasks in
                rest_f new_fd
              with Not_found -> (
                let rest_f2 var_i =
                  let (new_fd, args) =
                    self#make_task_spawn loc var_i oargs args
                  in
                  Lockutil.add_after_s new_file var_i.vname new_fd;
                  found_tasks <- (funname, (new_fd, var_i, args))::found_tasks;
                  rest_f new_fd
                in
                (* try to find the function definition *)
                try
                  (* checking for the function definition *)
                  let task =
                    SU.find_function_fundec_g new_file.globals funname
                  in
                  (* copy itself and the callees *)
                  SU.deep_copy_function funname callgraph spu_file new_file;
                  rest_f2 task.svar
                (* else try to find the function signature/prototype *)
                with Not_found -> (
                  let task = SU.find_function_sign new_file funname in
                  rest_f2 task
                )
              )

            )
            | Block(b) -> ignore(unimp "Ignoring block pragma"); DoChildren
            | _ -> SU.dbg_print debug "Ignoring pragma"; DoChildren
          )
          (* warn about ignored #pragma css ... directives *)
          | _ -> ignore(warnLoc loc "Ignoring #pragma %a\n" d_attr (Attr(pragma_str, rest))); DoChildren
        )
        | (_, loc) -> SU.dbg_print debug (loc.file^":"^(string_of_int loc.line)^" Ignoring #pragma directive"); DoChildren
    ) else
      DoChildren

  method getTasks = found_tasks
end
