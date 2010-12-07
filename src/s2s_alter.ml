(******************************************************************************)
(******************************************************************************)
(******************************************************************************)
(******************************************************************************)

(*
(* alternative make_tpc_func without the use of a skeleton
 *)
let make_tpc_func (f: fundec) (args: (string * arg_t * string * string * string ) list) : fundec = begin
  let f_new = emptyFunction ("tpc_function_" ^ f.svar.vname) in
  setFunctionTypeMakeFormals f_new f.svar.vtype;
  setFunctionReturnType f_new intType;
  (* TODO: push the size variables as args *)
  (*** Declare the local variables ***)
  (* unsigned int total_bytes *)
  let total_bytes = makeLocalVar f_new "total_bytes" uintType in
  (* volatile queue_entry_t *remote_entry *)
(*   let remote_entry = makeLocalVar f_new "remote_entry" (TPtr((find_type !spu_file "queue_entry_t"), [Attr("volatile", [])])) in *)
  (* volatile queue_entry_t *avail_task *)
  let avail_task = makeLocalVar f_new "avail_task" (TPtr((find_type !spu_file "queue_entry_t"), [Attr("volatile", [])])) in
  (* TODO: add the #ifdef
    #ifdef STATISTICS
      uint64_t tmptime1, tmptime2, tmptime3;
      int arg_bytes;
    #endif
  *)
  (* unsigned int *task_id_qs *)
  let task_id_qs = makeLocalVar f_new "task_id_qs" (TPtr(uintType, [])) in
  (* unsigned int task_id *)
  let task_id = makeLocalVar f_new "task_id" uintType in
  (* volatile completions_status_t *st *)
  let st = makeLocalVar f_new "st" (TPtr((find_tcomp !spu_file "completions_status_t"), [Attr("volatile", [])])) in

  (* create the arg_size*[, arg_elsz*, arg_els*] formals *)
  let args_num = (List.length f_new.sformals)-1 in
  for i = 0 to args_num do
    let (_, arg_type, _, _, _) = List.nth args i in
    ignore(makeFormalVar f_new ("arg_size"^(string_of_int i)) intType);
    if (is_strided arg_type) then begin
      ignore(makeFormalVar f_new ("arg_els"^(string_of_int i)) intType);
      ignore(makeFormalVar f_new ("arg_elsz"^(string_of_int i)) intType)
    end;
  done;
  let avail_task = var (findLocal f_new "avail_task") in
  let instrs : instr list ref = ref [] in
  (* avail_task->funcid = (uint8_t)funcid; *)
  instrs := Set (mkPtrFieldAccess avail_task "funcid",
  CastE(find_type !spu_file "uint8_t", integer !func_id), locUnknown):: !instrs;
  (* avail_task->total_arguments = (uint8_t)arguments.size() *)
  instrs := Set (mkPtrFieldAccess avail_task "total_arguments",
  CastE(find_type !spu_file "uint8_t", integer (args_num+1)), locUnknown)::!instrs;
  
  (* if we have arguments *)
  if (f_new.sformals <> []) then begin
    (* volatile vector unsigned char *tmpvec   where vector is __attribute__((altivec(vector__))) *)
    let vector_uchar_p = TPtr(TInt(IUChar, [Attr("volatile", [])]), [ppu_vector]) in
    let tmpvec = var (makeLocalVar f_new "tmpvec" vector_uchar_p) in
    (* struct tpc_arg_element local_arg *)
    let local_arg = var (makeLocalVar f_new "local_arg" (find_tcomp !spu_file "tpc_arg_element")) in
    for i = 0 to args_num do
      let arg = List.nth args i in
      (* local_arg <- argument description *)
      instrs := (doArgument i local_arg avail_task tmpvec f_new arg )@(!instrs);
    done;
  end;

  (* if (f_new.sformals <> []) then begin *)
    (* void *arg_addr64 *)
    let arg_addr64 = makeLocalVar f_new "arg_addr64" voidPtrType in
    (* unsigned int arg_size *)
    let arg_size = makeLocalVar f_new "arg_size" uintType in
    (* unsigned int arg_flag *)
    let arg_flag = makeLocalVar f_new "arg_flag" uintType in
    (* unsigned int arg_stride *)
    let arg_stride = makeLocalVar f_new "arg_stride" uintType in
    (* vector unsigned char *tmpvec   where vector is __attribute__((vector_size(8))) *)
    let tmpvec = makeLocalVar f_new "tmpvec" (TPtr(TInt(IUChar, []), [Attr("__attribute__", [ACons("vector_size", [AInt(8)])])])) in
    (* struct tpc_arg_element local_arg *)
    let local_arg = makeLocalVar f_new "local_arg" (find_tcomp !spu_file "tpc_arg_element") in
  (* end *)

  (*** Initialize local variables ***)
  let stmts = ref [] in
  (* remote_entry=NULL *)
(*   stmts := mkStmtOneInstr (Set (var remote_entry, CastE(voidPtrType ,zero), locUnknown))::!stmts; *)
  (* avail_task=NULL *)
  stmts := mkStmtOneInstr (Set (var avail_task, CastE(voidPtrType ,zero), locUnknown))::!stmts;
  (* if (f_new.sformals <> []) then begin *)
    (* arg_addr64=NULL *)
    stmts := mkStmtOneInstr (Set (var arg_addr64, CastE(voidPtrType ,zero), locUnknown))::!stmts;
    (* arg_size=0 *)
    stmts := mkStmtOneInstr (Set (var arg_size, zero, locUnknown))::!stmts;
    (* arg_flag=0 *)
    stmts := mkStmtOneInstr (Set (var arg_flag, zero, locUnknown))::!stmts;
    (* arg_stride=0 *)
    stmts := mkStmtOneInstr (Set (var arg_stride, zero, locUnknown))::!stmts;
  (* end *)
  (* TODO: Add the #ifdef
    #ifdef TPC_MULTITHREADED
      pthread_mutex_lock( &tpc_callwait_mutex );
    #endif
    READ_TIME_REG(tmptime1);
  *)
  (* get the compl_status enuminfo *)
  let compl_status_enum = find_enum !spu_file "compl_status" in
  (* get the entry_status enuminfo *)
  let entry_status_enum = find_enum !spu_file "entry_status" in
  (* get the s_available_spe varinfo *)
  let g_max_spes = find_global_var !spu_file "G_max_spes" in
  (* get the s_available_spe varinfo *)
  let s_available_spe = find_global_var !spu_file "s_available_spe" in
  (* get the task_queue_tail varinfo *)
  let task_queue_tail = find_global_var !spu_file "task_queue_tail" in
  (* get the task_queue_tail varinfo *)
  let compl_queue = find_global_var !spu_file "compl_queue" in
  (* get the g_task_current_id varinfo *)
  let g_task_current_id = find_global_var !spu_file "g_task_current_id" in
  (* get the g_task_current_id varinfo *)
  let g_task_id_queue = find_global_var !spu_file "g_task_id_queue" in
  (* get the task_queue varinfo *)
  let task_queue = find_global_var !spu_file "task_queue" in
  (* create the while body *)
  let w_body = ref [] in
  (* create the [s_available_spe][task_queue_tail[s_available_spe]] offset *)
  let big_offset = Index(Lval(var s_available_spe), Index(Lval((Var(task_queue_tail) , Index(Lval(var s_available_spe), NoOffset))), NoOffset)) in
  (* st = &compl_queue[s_available_spe][task_queue_tail[s_available_spe]] *)
  w_body := mkStmtOneInstr (Set (var st, AddrOf((Var compl_queue, big_offset)) , locUnknown))::!w_body;
  let st_status = mkPtrFieldAccess (var st) "status" in
  (* st->status = WAITING; break; *)
  let bthen = mkBlock [ mkStmtOneInstr (Set (st_status, Const(CEnum(zero, "WAITING", compl_status_enum)), locUnknown) ); mkStmt (Break locUnknown) ] in
  (* s_available_spe = (s_available_spe+1) % G_max_spes; *)
  let next_spe_s = mkStmtOneInstr(Set (var s_available_spe, BinOp(Mod, BinOp(PlusA, Lval(var s_available_spe), one, intType), Lval(var g_max_spes), intType), locUnknown) ) in
  let belse = mkBlock [ next_spe_s ] in
  (*
    if(st->status == COMPLETED) {
      st->status = WAITING;
      break;
    } else {
      s_available_spe = (s_available_spe+1) % G_max_spes;
    }
  *)
  w_body := mkStmt(If(BinOp(Eq, Lval(st_status), Const(CEnum(one, "COMPLETED", compl_status_enum)), boolType), bthen, belse, locUnknown))::!w_body;
  (* push while in stmts *)
  stmts := mkStmt(Loop( mkBlock (L.rev !w_body), locUnknown, None, None))::!stmts;
  (* g_task_current_id[s_available_spe] *)
  let gtc_indexed = (Var g_task_current_id, Index(Lval(var s_available_spe), NoOffset)) in
  (** task_id = g_task_current_id[s_available_spe]++; **)
  (* task_id = g_task_current_id[s_available_spe]; *)
  stmts := mkStmtOneInstr(Set (var task_id, Lval(gtc_indexed), locUnknown))::!stmts;
  (* g_task_current_id[s_available_spe]++; *)
  stmts := mkStmtOneInstr(Set (gtc_indexed, BinOp(PlusA, Lval(gtc_indexed), one, intType), locUnknown))::!stmts;
  (* task_id_qs = &g_task_id_queue[s_available_spe][task_queue_tail[s_available_spe]]; *)
  stmts := mkStmtOneInstr(Set (var task_id_qs, AddrOf((Var g_task_id_queue, big_offset)) , locUnknown))::!stmts;
  (** task_id = (task_id & 0x0FFFFFFF) | (s_available_spe << 28); **)
  (* (task_id & 0x0FFFFFFF) *)
  let lbs = BinOp(BAnd, Lval(var task_id), Const(CInt64(Int64.of_string "0x0FFFFFFF", IInt, None)), intType) in
  (* (s_available_spe << 28) *)
  let rbs = BinOp(Shiftlt, Lval(var s_available_spe), Const(CInt64(Int64.of_int 28, IInt, None)), intType) in
  (* task_id = lbs | rbs; *)
  stmts := mkStmtOneInstr(Set (var task_id, BinOp(BOr, lbs, rbs, intType), locUnknown))::!stmts;
  (* *task_id_qs = task_id; *)
  stmts := mkStmtOneInstr(Set ((mkMem (Lval(var task_id_qs)) NoOffset), Lval(var task_id), locUnknown))::!stmts;
  (* TODO: READ_TIME_REG(tmptime2); *)
  (* avail_task = &task_queue[s_available_spe][task_queue_tail[s_available_spe]]; *)
  stmts := mkStmtOneInstr(Set (var avail_task, AddrOf((Var task_queue, big_offset)) , locUnknown))::!stmts;
  (* avail_task->funcid = (uint8_t)funcid; *)
  stmts := mkStmtOneInstr(Set (mkPtrFieldAccess (var avail_task) "funcid", CastE(find_type !spu_file "uint8_t", Const(CInt64(Int64.of_int !func_id, IInt, None))), locUnknown))::!stmts;
  (* avail_task->total_arguments = (uint8_t)arguments.size() *)
  stmts := mkStmtOneInstr(Set (mkPtrFieldAccess (var avail_task) "total_arguments", CastE(find_type !spu_file "uint8_t", Const(CInt64(Int64.of_int (List.length f_new.sformals), IInt, None))), locUnknown))::!stmts;
  (* total_bytes=0; *)
  stmts := mkStmtOneInstr(Set (var total_bytes, zero, locUnknown))::!stmts;

  (* TODO: complete code depending on arguments *)

  (* avail_task->active = ACTIVE *)
  stmts := mkStmtOneInstr(Set (mkPtrFieldAccess (var avail_task) "active", Const(CEnum(one, "ACTIVE", entry_status_enum)), locUnknown))::!stmts;
  (* TODO:
    READ_TIME_REG(tmptime3);
    #ifdef STATISTICS
      G_ppe_stats.stat_tpc_per_spe[s_available_spe] += 1;
      G_ppe_stats.bytes_per_spe[s_available_spe] += total_bytes;
      G_ppe_stats.stalled_ticks += (tmptime2 - tmptime1);
      G_ppe_stats.issue_ticks += (tmptime3 - tmptime2);
    #endif
    #ifdef TPC_MULTITHREADED
      pthread_mutex_unlock( &tpc_callwait_mutex );
    #endif
  *)
  (** task_queue_tail[s_available_spe] = c % MAX_QUEUE_ENTRIES; **)
  (* task_queue_tail[s_available_spe] *)
  let tqt_indexed = (Var task_queue_tail, Index(Lval(var s_available_spe), NoOffset)) in
  (* (task_queue_tail[s_available_spe]+1) *)
  let tqt_plus1 = BinOp(PlusA, Lval(tqt_indexed), one, intType) in
  (* tqt_indexed = tqt_plus1 % MAX_QUEUE_ENTRIES; *)
  stmts := mkStmtOneInstr(Set (tqt_indexed, BinOp(Mod, tqt_plus1, Const(CInt64(Int64.of_int 1(* FIXME: MAX_QUEUE_ENTRIES *), IInt, None)), intType), locUnknown))::!stmts;
  (* s_available_spe = (s_available_spe+1) % G_max_spes; *)
  stmts := next_spe_s::!stmts;
  (* return task_id; *)
  stmts := mkStmt (Return (Some (Lval (var task_id)), locUnknown))::!stmts;
  (* reverse the stmt list and put it in the body *)
  f_new.sbody <- mkBlock (L.rev !stmts);
  incr func_id;
  f_new
end
*)
