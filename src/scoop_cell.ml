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

(** Responsible for generating code for the TPC runtime on the
    Cell Processor
    @author Foivos Zakkak, zakkak\@ics.forth.gr *)

open Cil
open Scoop_util
module E = Errormsg
module L = List

(** keeps the current funcid for the new tpc_function *)
let func_id = ref 0

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
let make_case execfun (task: varinfo) (task_info: varinfo)
              (ex_task: varinfo) (args: (int * arg_descr) list): stmt = (
  let res = ref [] in
  assert(isFunctionType task.vtype);
  let ret, arglopt, hasvararg, _ = splitFunctionType task.vtype in
  assert(not hasvararg);
  let argl = match arglopt with None -> [] | Some l -> l in
  let argaddr = makeTempVar execfun voidPtrType in
  res := Set(var argaddr, Lval (mkFieldAccess (var task_info) "ls_addr"), locUnknown) :: !res;
(*  else begin
    res := Set(var argaddr, Lval (mkFieldAccess (var task_info) "local"), locUnknown) :: !res;
  end*)
  let nextaddr n stride =
    let lv = mkFieldAccess (var ex_task) "arguments" in
    let t = typeOfLval lv in
    assert(isArrayType t);
    let idxlv = addOffsetLval (Index(integer n, NoOffset)) lv in
    let szlv = mkFieldAccess idxlv "size" in
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
      let advptrinstr = nextaddr !i (isStrided arg_desc) in
      incr i;
      if !carry <> dummyInstr then res := !carry::!res;
      carry := advptrinstr;
      res := castinstr :: !res;
      let lv = Lval (
        if (isScalar_t argt) then
          mkMem ( mkCast (Lval(var argvar)) (TPtr(argt, [])) ) NoOffset
        else
          var argvar
      ) in
      (place, mkCast lv argt)
    )
    args
  in
  let arglist = L.sort comparator arglist in
  let (_, arglist) = L.split arglist in
  res := Call (None, Lval (var task), arglist, locUnknown)::!res;
  mkStmt (Instr (L.rev !res))
)
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

(** Produces the required instructions to push an argument in a task descriptor
    @param i the argument's number
    @param local_arg the {e local_arg}'s [Cil.lval]. {e local_arg} is defined
    as local of tpc_function_* by make_tpc_func.
    @param avail_task the {e avail_task}'s [Cil.lval]. {e avail_task} is defined
    in the skeleton used by {b SCOOP}
    @param tmpvec the {e tmpvec}'s [Cil.lval]. {e tmpvec} is defined
    as local of tpc_function_* by make_tpc_func.
    @param fd the tpc_function_* function declaration
    @param arg a [(int * arg_descr)] pair with all the info about the arg to
    pass in the task descriptor
    @param stats flag about generating code for statistics
    @param spu_file the spu file
    @return a statement including the produced instructions
*)
let doArgument (i: int) (local_arg: lval) (avail_task: lval) (tmpvec: lval) (fd: fundec)
 (arg: (int * arg_descr)) (stats: bool) (spu_file: file): instr list = (
  let (i_m, arg_desc) = arg in
  let arg_size = Lval( var (find_formal_var fd ("arg_size"^(string_of_int i_m)))) in
  let actual_arg = L.nth fd.sformals i_m in
  let arg_addr = (
    if (isScalar_v actual_arg) then
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
    let total_bytes = var (find_local_var fd "total_bytes") in
(*     let arg_bytes = var (find_local_var fd "arg_bytes") in *)
    let arg_bytes =
      if (isStrided arg_desc) then (
        (*FIXME not sure about i here *)
        let arg_elsz = Lval( var (find_formal_var fd ("arg_elsz"^(string_of_int i)))) in
        let arg_els = Lval( var (find_formal_var fd ("arg_els"^(string_of_int i)))) in
        (* arg_bytes = TPC_EXTRACT_STRIDEARG_ELEMSZ(arg_size)*TPC_EXTRACT_STRIDEARG_ELEMS(arg_size); *)
        BinOp(Mult, arg_els, arg_elsz, intType)
      ) else (
        (* arg_bytes = arg_size; *)
        arg_size
      )
    in
    (* total_bytes += ( arg_bytes<< TPC_IS_INOUTARG(arg_flag)); *)
    let total_size =
      if (isStrided arg_desc) then (
        BinOp(PlusA, Lval(total_bytes), BinOp(Mult, integer 2, arg_bytes, intType), intType)
      ) else (
        BinOp(PlusA, Lval(total_bytes), arg_bytes, intType)
      )
    in
    il := Set(total_bytes, total_size, locUnknown)::!il
  );
  let vector_uchar_p = TPtr(TInt(IUChar, [Attr("volatile", [])]), [ppu_vector]) in
  let av_task_arg = mkFieldAccess avail_task "arguments" in
  let av_task_arg_idx = addOffsetLval (Index(integer i,NoOffset)) av_task_arg in
  il := Set(tmpvec, mkCast (mkAddrOf av_task_arg_idx) (vector_uchar_p) , locUnknown)::!il;

(*   let local_arg_idx = addOffsetLval (Index(integer i,NoOffset)) local_arg in *)
  (* local_arg.eal = (uint32_t)(arg_addr64); *)
  let eal = mkFieldAccess local_arg "eal" in
  il := Set(eal, mkCast arg_addr (find_type spu_file "uint32_t"), locUnknown)::!il;
  let size = mkFieldAccess local_arg "size" in
  if (isStrided arg_desc) then (
    (*FIXME not sure about i here *)
    let arg_elsz = Lval( var (find_formal_var fd ("arg_elsz"^(string_of_int i)))) in
    let arg_els = Lval( var (find_formal_var fd ("arg_els"^(string_of_int i)))) in
    (* #define TPC_BUILD_STRIDEARG(elems, elemsz)    (((elems)<<16U) | (elemsz)) *)
    (* local_arg.size = TPC_BUILD_STRIDEARG(els,elsz); *)
    let build_stride = BinOp(BOr, BinOp(Shiftlt, arg_els, (integer 16), intType), arg_elsz, intType) in
    il := Set(size, build_stride, locUnknown)::!il;
    (* local_arg.stride = arg_size; *)
    let stride = mkFieldAccess local_arg "stride" in
    il := Set(stride, arg_size, locUnknown)::!il;
  ) else
    (* local_arg.size = arg_size; *)
    il := Set(size, arg_size, locUnknown)::!il;
  (* local_arg.flag = arg_flag; *)
  let flag = mkFieldAccess local_arg "flag" in
  il:= Set(flag, arg_type2integer arg_desc.atype, locUnknown)::!il;
  (* *tmpvec = *((volatile vector unsigned char * )&local_arg); *)
  let casted_la = mkCast (mkAddrOf local_arg) vector_uchar_p in
  il := Set(mkMem (Lval(tmpvec)) NoOffset, Lval(mkMem casted_la NoOffset), locUnknown)::!il;
  !il
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
    (args: arg_descr list) (f: file) (spu_file: file)
    : (fundec * (int * arg_descr) list) = (
  print_endline ("Creating tpc_function_" ^ func_vi.vname);
  let args = L.sort sort_args (L.rev args) in
  let skeleton = Scoop_util.find_function_fundec f "tpc_call_tpcAD65" in
  let f_new = copyFunction skeleton ("tpc_function_" ^ func_vi.vname) in
  f_new.sformals <- [];
  (* set the formals to much the original function's arguments *)
  setFunctionTypeMakeFormals f_new func_vi.vtype;
  setFunctionReturnType f_new intType;
  formalScalarsToPointers loc f_new;
  (* create the arg_size*[, arg_elsz*, arg_els*] formals *)
  let args_num = (L.length f_new.sformals)-1 in
  if ( args_num <> (List.length args)-1 ) then
    E.s (errorLoc loc "Number of arguments described in #pragma doesn't much the \
          number of arguments in the function declaration");
  for i = 0 to args_num do
    let ex_arg = (L.nth oargs i) in
    let name = getNameOfExp ex_arg in
    let arg_desc = L.find ( fun a -> ( a.aname = name) ) args in
    ignore(makeFormalVar f_new ("arg_size"^(string_of_int i)) intType);
    if (isStrided arg_desc) then (
      ignore(makeFormalVar f_new ("arg_els"^(string_of_int i)) intType);
      ignore(makeFormalVar f_new ("arg_elsz"^(string_of_int i)) intType)
    );
  done;

  let avail_task = var (find_local_var f_new "avail_task") in
  let instrs : instr list ref = ref [] in
  let uint8_t = find_type spu_file "uint8_t" in
  (* avail_task->funcid = (uint8_t)funcid; *)
  instrs := Set (mkFieldAccess avail_task "funcid",
  mkCast (integer !func_id) uint8_t, locUnknown):: !instrs;
  (* avail_task->total_arguments = (uint8_t)arguments.size() *)
  let args_num_i = integer (args_num+1) in
  instrs := Set (mkFieldAccess avail_task "total_arguments",
  mkCast args_num_i uint8_t, locUnknown)::!instrs;

  let args_n =
  (* if we have arguments *)
  if (f_new.sformals <> []) then (
    (* volatile vector unsigned char *tmpvec   where vector is __attribute__((altivec(vector__))) *)
    let vector_uchar_p = TPtr(TInt(IUChar, [Attr("volatile", [])]), [ppu_vector]) in
    let tmpvec = var (makeLocalVar f_new "tmpvec" vector_uchar_p) in
    (* struct tpc_arg_element local_arg *)
    let arg_typ = (find_tcomp spu_file "tpc_arg_element") in
(*     let arr_typ = TArray(arg_typ, Some(args_num_i), []) in *)
    let local_arg = var (makeLocalVar f_new "local_arg" arg_typ) in
    let args_n = number_args args oargs in
    let i_n = ref (args_num+1) in
    let mapped = (L.map
      (fun arg -> decr i_n; doArgument !i_n local_arg avail_task tmpvec f_new arg
                    !Scoop_util.stats spu_file )
      args_n) in
    instrs := (L.flatten mapped)@(!instrs);
(*    for i = 0 to args_num do
      let ex_arg = (L.nth oargs i) in
      let name = getNameOfExp ex_arg in
      let arg = L.find ( fun (vname, _) -> if( vname = name) then true else false) args in
        (* local_arg <- argument description *)
        instrs := (doArgument i local_arg avail_task tmpvec f_new arg
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
            let args = [attrParamToExp ppc_file loc exp] in
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
                Instr(Call(Some res, Lval((Var(vi), _)), oargs, loc)::restInst) -> (
                  let instr = Call (Some res, Lval (var tm), oargs, locUnknown) in
                  ChangeTo(mkStmtOneInstr instr)
                )
              | _ -> DoChildren
          )
          (* Support #pragma css free *)
          | [AStr("free")] -> (
            let tf = find_function_sign ppc_file "tpc_free" in
            match s.skind with
                Instr(Call(_, Lval((Var(vi), _)), oargs, loc)::restInst) -> (
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
              try
                (* fast workaround *)
                (* check if we have seen this function before *)
                let (new_fd, _, _) = List.assoc funname spu_tasks in
                rest_f new_fd
              with Not_found -> (
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
                  let task =
                    find_function_fundec_g ppc_file.globals funname
                  in
                  (* copy itself and the callees *)
                  deep_copy_function funname callgraph spu_file ppc_file;
                  rest_f2 task.svar
                (* else try to find the function signature/prototype *)
                with Not_found -> (
                  let task = find_function_sign ppc_file funname in
                  rest_f2 task
                )
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

