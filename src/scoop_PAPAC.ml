(*
 *
 * Copyright (c) 2011-2013,
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

let doArgument (taskd_args: lval) (f : file) (orig_tname: string) (tid: int)
 (loc: location) (arg: (int * arg_descr) ) : stmt list = (
  let (i_m, arg_desc) = arg in
  let arg_name = arg_desc.aname in
  let arg_addr = arg_desc.address in
  let arg_type = arg_desc.atype in
  let arg_size = getSizeOfArg arg_desc in
  let il = ref [] in

  (* taskd->args[i] *)
  let tpc_task_argument_pt = TPtr(find_type f  "tpc_task_argument", []) in
(*   let idxlv = addOffsetLval (Index(integer i, NoOffset)) arguments in *)
  let idxlv = taskd_args in
  (*  void * addr_in;
      void * addr_out;
      uint32_t type;
      uint32_t size;
      uint32_t stride;
      uint32_t element_num; *)
  (*let uint32_t = (find_type f "uint32_t") in*)
  let addr_in = mkFieldAccess idxlv "addr_in" in
  let addr_out = mkFieldAccess idxlv "addr_out" in
  let flag = mkFieldAccess idxlv "type" in
  let size = mkFieldAccess idxlv "size" in
  let stride = mkFieldAccess idxlv "stride" in
  let element_num = mkFieldAccess idxlv "element_num" in

  let arg_addr_casted = mkCast arg_addr voidPtrType in
  il := Set(addr_in, arg_addr_casted, locUnknown)::!il;
  il := Set(addr_out, arg_addr_casted, locUnknown)::!il;


  (* arg_flag|TPC_SAFE_ARG; *)
  let arg_type_tmp = arg_type2int arg_type in
  let arg_type_tmp =
    (* arg_flag|TPC_SAFE_ARG|TPC_BYVALUE_ARG; *)
    if (isScalar arg_desc) then (
      arg_type_tmp lor 0x18
    (* invoke isSafeArg from PtDepa to check whether this argument is a no dep *)
    (* arg_flag|TPC_SAFE_ARG; *)
    ) else if (Sdam.isSafeArg orig_tname tid arg_name) then (
      arg_type_tmp lor 0x8
    ) else (
      arg_type_tmp
    )
  in
  let arg_type_tmp =
    (* arg_flag|TPC_STRIDE_ARG; *)
    if (isStrided arg_desc) then (
      arg_type_tmp lor 0x4
    ) else (
      arg_type_tmp
    )
  in
  il := Set(flag, integer arg_type_tmp, locUnknown)::!il;

  if (isStrided arg_desc) then (
    let (arg_els, arg_elsz) =
      match arg_type with
          Stride(_, _, els, elsz) -> (els, elsz)
        | _ -> assert false
    in
    il := Set(size, arg_elsz, locUnknown)::!il;
    il := Set(stride, arg_size, locUnknown)::!il;
    il := Set(element_num, arg_els, locUnknown)::!il;
  ) else (
    il := Set(size, arg_size, locUnknown)::!il;
    il := Set(stride, zero, locUnknown)::!il;
    il := Set(element_num, zero, locUnknown)::!il;
  );
  il := Set(taskd_args, BinOp( PlusPI, Lval taskd_args, one, tpc_task_argument_pt) , locUnknown)::!il;

  [mkStmt (Instr (L.rev !il))]
)

(* Adds a declaration of function f_new right AFTER the first occurence
 * (declaration or definition) of the function named <f_old>. Also, the body of
 * the function f_new is added at the end of the file.
 *)
let add_wrapper (file: Cil.file) (f_old: string) (f_new: fundec) : unit =
  (*let v = new Lockutil.addFunVisitor f_old f_new in
  visitCilFile v file;*)
  let declaration = GVarDecl(f_new.svar, Cil.locUnknown) in
  let definition  = GFun(f_new, Cil.locUnknown) in
  file.globals <- declaration::(file.globals @ [definition])

(* generates the code to issue a task *)
let make_tpc_issue (is_hp: bool) (loc: location) (func_vi: varinfo) (oargs: exp list)
    (args: arg_descr list) (f: file) (cur_fd: fundec) : (stmt list * (int * arg_descr) list) = (
  incr un_id;

(*   let instrs = ref [] in *)
  let uint8_t = (find_type f "uint8_t") in
  (*let uint32_t = (find_type f "uint32_t") in*)
  let args_num = List.length oargs in
  let args_num_i = integer args_num in
  let tpc_task_descriptor_pt = TPtr(find_type f "tpc_task_descriptor", []) in
  let tpc_task_argument_pt = TPtr(find_type f "tpc_task_argument", []) in
  let taskd = var (makeTempVar cur_fd tpc_task_descriptor_pt) in
  let full_queue_res = var (makeTempVar cur_fd uint8_t) in

(*  (* const int tpc_task_arguments_list[] = {2, 3, 5, 9}; *)
  let tpc_tal = find_global_Gvar f "tpc_task_arguments_list" in
  (match tpc_tal with
     GVar(_, initi, _) -> (
      let init = initi.init in
      match init with
        Some (SingleInit _) -> initi.init <- Some (CompoundInit( intType, [(Index(integer !func_id,NoOffset), SingleInit(args_num_i))] ));
      | Some (CompoundInit(t, clist)) ->
          if (not (L.exists (fun (offset, init) ->
              match init with
                 SingleInit(a) when a=args_num_i -> true
                | _ -> false
            ) clist)
          ) then
          initi.init <- Some (CompoundInit(intType, clist@[(Index(integer !func_id,NoOffset), SingleInit(args_num_i))]));
      | None -> assert false;
    )
    | _ -> assert false;
  );*)

  (* task_desc = tpc_task_descriptor_alloc(args_num); *)
  let tpc_task_descriptor_alloc = find_function_sign f "tpc_task_descriptor_alloc" in
  let instrs = [Call (Some taskd, Lval (var tpc_task_descriptor_alloc), [args_num_i], locUnknown)] in

    (* task_desc->task = wrapper_func; *)
  let taskd_task = mkFieldAccess taskd "task" in
    (* make the wrapper id it doesn't already exist *)
  let wrapper =
    try
      find_function_fundec_g f.globals ("wrapper_SCOOP__" ^ func_vi.vname)
    with Not_found -> (
      let wrapper_t = find_function_fundec f "wrapper_SCOOP__" in
      let new_fd = copyFunction wrapper_t ("wrapper_SCOOP__" ^ func_vi.vname) in
      add_wrapper f func_vi.vname new_fd;
      new_fd
    )
  in

  let args_n = number_args args oargs in
  let args_n = List.sort sort_args_n args_n in

    (* make the wrappers body *)
  let _, arglopt, hasvararg, _ = splitFunctionType func_vi.vtype in
  assert(not hasvararg);
  let argl = match arglopt with None -> [] | Some l -> l in
  let wr_arg = var (List.hd wrapper.sformals) in
  let il = ref [] in
  let i = ref 0 in
  let doArg = function
    | ((_, t, _), (_, arg_descr)) -> (
      let ar = var (makeTempVar wrapper t) in
      il := Set(ar, mkCast (Lval (mkFieldAccess wr_arg "addr_in")) t, locUnknown)::!il;
      il := Set(wr_arg, BinOp( PlusPI, Lval wr_arg, one, tpc_task_argument_pt) , locUnknown)::!il;
      incr i;
      Lval ar
    )
  in
  let arglist = List.map doArg (L.combine argl (L.rev args_n)) in
  il := Call (None, Lval (var func_vi), arglist, locUnknown)::!il;
  wrapper.sbody <- mkBlock [mkStmt (Instr (List.rev !il))];

  let instrs = Set(taskd_task, Lval (var wrapper.svar), locUnknown)::instrs in
  (* task_desc->args = task_desc; *)
  let taskd_args = mkFieldAccess taskd "args" in
(*   instrs := Set(taskd_args, BinOp( PlusPI, Lval taskd, integer 32, tpc_task_argument_pt) , locUnknown)::!instrs;*)
  let set_argsPtr = Set(taskd_args, CastE( tpc_task_argument_pt, BinOp( PlusPI, Lval taskd, one, tpc_task_argument_pt)) , locUnknown) in
  let instrs = set_argsPtr::instrs in
(*   instrs := Set(taskd_args, Lval taskd, locUnknown)::!instrs; *)
  (* task_desc->args_no = args_num; *)
  let taskd_args_no = mkFieldAccess taskd "args_num" in
  let instrs = Set(taskd_args_no, args_num_i, locUnknown)::instrs in
  (* Leave uninitialized
     task_desc->rfu and task_desc->extras *)

  let (stmts, args_n) =
    (* if we have arguments *)
    if (oargs <> []) then (
      incr querie_no;
      let doArgument = doArgument taskd_args f func_vi.vname !querie_no loc in
      let mapped = L.flatten (List.rev_map doArgument args_n) in
      (mkStmt (Instr (L.rev instrs))::mapped, args_n)
    ) else (
      ([mkStmt (Instr (L.rev instrs))], [])
    )
  in

  (* tpc_call(taskd); *)
  let tpc_call_f = find_function_sign f "tpc_call" in
  let call = Call (None, Lval (var tpc_call_f), [Lval taskd], locUnknown) in

  let rest = mkStmt (Instr [set_argsPtr; call]) in

  (* task_desc->args = task_desc+32; *)
  let stmts = stmts@[rest] in

  (* if(isFull_Queue(task_Queues[thread_id])==0) *)
  let isfull_queue =
    let isfull_queue = find_function_sign f "isFull_Queue" in
    let task_queues =
      try var (find_global_var f "task_Queues")
      with Not_found -> E.s (errorLoc loc "task_Queues Not found");
    in
    let thread_id =
      try Lval (var (find_global_var f "thread_id"))
      with Not_found -> E.s (errorLoc loc "thread_id Not found");
    in
    let arg = Lval (addOffsetLval (Index(thread_id, NoOffset)) task_queues) in
    Call (Some full_queue_res, Lval (var isfull_queue), [arg], locUnknown)
  in

  let cond = BinOp(Eq, Lval full_queue_res, zero, boolType) in

  let else_case =
    let orig_args = List.map (fun f -> f.address) args in
    [mkStmtOneInstr (Call (None, Lval (var func_vi), orig_args, locUnknown))]
  in

  let if_stmt =
    mkStmt (If(cond, mkBlock stmts, mkBlock else_case, locUnknown))
  in

  let stmts = [mkStmtOneInstr isfull_queue; if_stmt] in

  incr func_id;
  (stmts, args_n)
)

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
    | (ACons(arg_typ, args)::rest) ->
      let (hp, lst) = scoop_process rest in
      (hp, (scoop_process_args true ppc_file arg_typ loc args)@lst)
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
          (* Support #pragma css sync*)
          | [AStr("sync")] -> (
            let ts = find_function_sign ppc_file "tpc_sync" in
            let instr = Call (None, Lval (var ts), [], locUnknown) in
            let s' = {s with pragmas = List.tl s.pragmas} in
            ChangeDoChildrenPost ((mkStmt (Block (mkBlock [ mkStmtOneInstr instr; s' ]))), fun x -> x)
          )
          (* Support #pragma css start(threads) *)
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
            | Instr(Call(_, Lval((Var(vi), _)), oargs, loc)::restInst) -> (
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
