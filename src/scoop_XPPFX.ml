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

class codegen (cgraph : Callgraph.callgraph) file pragma includePath =
object (self) inherit Scoop_codegen.codegen cgraph file pragma includePath as super
  val scoop_barrier       = "tpc_sync"
  val scoop_start         = "tpc_init"
  val scoop_finish        = "tpc_shutdown"
  val scoop_malloc        = "tpc_malloc"
  val scoop_free          = "tpc_free"
  val runtime             = "nesting"

  method declareGlobals : unit =
    ignore(SU.makeGlobalVar "tpc_task_arguments_list" (Some (SingleInit(zero)))
                     (TArray(TInt(IInt, [Attr("const", [])]), None, []))
                     new_file);

  method private doArgument (taskd_args: lval) (orig_tname: string) (tid: int)
                            (loc: location) (arg: (int * SU.arg_descr) ) : stmt list =
    let (i_m, arg_desc) = arg in
    let arg_name = arg_desc.SU.aname in
    let arg_addr = arg_desc.SU.address in
    let arg_type = arg_desc.SU.atype in
    let arg_size = SU.getSizeOfArg arg_desc in
    let il = ref [] in

    (* taskd->args[i] *)
    let tpc_task_argument_pt = TPtr(SU.find_type new_file  "tpc_task_argument", []) in
    (*   let idxlv = addOffsetLval (Index(integer i, NoOffset)) arguments in *)
    let idxlv = taskd_args in
    (*  void * addr_in;
      void * addr_out;
      uint32_t type;
      uint32_t size;
      uint32_t stride;
      uint32_t element_num; *)
    let addr_in     = SU.mkFieldAccess idxlv "addr_in"     in
    let addr_out    = SU.mkFieldAccess idxlv "addr_out"    in
    let flag        = SU.mkFieldAccess idxlv "type"        in
    let size        = SU.mkFieldAccess idxlv "size"        in
    let stride      = SU.mkFieldAccess idxlv "stride"      in
    let element_num = SU.mkFieldAccess idxlv "element_num" in

    il := Set(addr_in, CastE(voidPtrType, arg_addr), locUnknown)::!il;
    il := Set(addr_out, CastE(voidPtrType, arg_addr), locUnknown)::!il;


    (* arg_flag|TPC_SAFE_ARG; *)
    let arg_type_tmp = SU.arg_type2int arg_type in
    let arg_type_tmp =
      (* arg_flag|TPC_SAFE_ARG|TPC_BYVALUE_ARG; *)
      if (SU.isScalar arg_desc) then (
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
      if (SU.isStrided arg_desc) then (
        arg_type_tmp lor 0x4
      ) else (
        arg_type_tmp
      )
    in
    il := Set(flag, integer arg_type_tmp, locUnknown)::!il;

    if (SU.isStrided arg_desc) then (
      let (arg_els, arg_elsz) =
        match arg_type with
          SU.Stride(_, _, els, elsz) -> (els, elsz)
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

  (** Generates the code to spawn a task
   * @param loc the current file location
   * @param func_vi the varinfo of the original function
   * @param oargs the original arguments
   * @param args the arguments from the pragma directive
   * @param new_file the file we are modifying
   * @return the stmts that will replace the call paired with a list of numbered
   * argument descriptors
   *)
  method make_task_spawn (loc: location) (func_vi: varinfo)
                         (oargs: exp list) (args: SU.arg_descr list)
         : (stmt list * (int * SU.arg_descr) list) =
    incr un_id;

    let instrs = ref [] in
    let args_num = List.length oargs in
    let args_num_i = integer args_num in
    let tpc_task_descriptor_pt =
      TPtr(SU.find_type new_file "tpc_task_descriptor", []) in
    let tpc_task_argument_pt   =
      TPtr(SU.find_type new_file "tpc_task_argument", []) in
    let taskd = var (makeTempVar !SU.currentFunction tpc_task_descriptor_pt) in

    (* const int tpc_task_arguments_list[] = {2, 3, 5, 9}; *)
    let tpc_tal = SU.find_global_Gvar new_file "tpc_task_arguments_list" in
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
    );

    (* task_desc = tpc_task_descriptor_alloc(args_num); *)
    let tpc_task_descriptor_alloc = SU.find_function_sign new_file "tpc_task_descriptor_alloc" in
    instrs := Call (Some taskd, Lval (var tpc_task_descriptor_alloc), [args_num_i], locUnknown)::!instrs;

    (* task_desc->task = wrapper_func; *)
    let taskd_task = SU.mkFieldAccess taskd "task" in
    (* make the wrapper id it doesn't already exist *)
    let wrapper =
      try
        SU.find_function_fundec_g new_file.globals ("wrapper_SCOOP__" ^ func_vi.vname)
      with Not_found -> (
        let wrapper_t = SU.find_function_fundec new_file "wrapper_SCOOP__" in
        let new_fd = copyFunction wrapper_t ("wrapper_SCOOP__" ^ func_vi.vname) in
        Lockutil.add_after_s new_file func_vi.vname new_fd;
        new_fd
      )
    in
    (* make the wrappers body *)
    let _, arglopt, hasvararg, _ = splitFunctionType func_vi.vtype in
    assert(not hasvararg);
    let argl = match arglopt with None -> [] | Some l -> l in
    let wr_arg = var (List.hd wrapper.sformals) in
    let il = ref [] in
    let doArg = function
      | (name, t, attr) -> (
        let ar = var (makeTempVar wrapper t) in
        il := Set(ar, mkCast (Lval (SU.mkFieldAccess wr_arg "addr_in")) t, locUnknown)::!il;
        il := Set(wr_arg, BinOp( PlusPI, Lval wr_arg, one, tpc_task_argument_pt) , locUnknown)::!il;
        Lval ar
      )
    in
    let arglist = List.map doArg argl in
    il := Call (None, Lval (var func_vi), arglist, locUnknown)::!il;
    wrapper.sbody <- mkBlock [mkStmt (Instr (List.rev !il))];

    instrs := Set(taskd_task, Lval (var wrapper.svar), locUnknown)::!instrs;
    (* task_desc->args = task_desc; *)
    let taskd_args = SU.mkFieldAccess taskd "args" in
    (*   instrs := Set(taskd_args, BinOp( PlusPI, Lval taskd, integer 32, tpc_task_argument_pt) , locUnknown)::!instrs; *)
    instrs := Set(taskd_args, CastE( tpc_task_argument_pt, BinOp( PlusPI, Lval taskd, one, tpc_task_argument_pt)) , locUnknown)::!instrs;
    (*   instrs := Set(taskd_args, Lval taskd, locUnknown)::!instrs; *)
    (* task_desc->args_no = args_num; *)
    let taskd_args_no = SU.mkFieldAccess taskd "args_num" in
    instrs := Set(taskd_args_no, args_num_i, locUnknown)::!instrs;
    (* Leave uninitialized
     task_desc->rfu and task_desc->extras *)
    let stmts = ref [] in

    let args_n =
      (* if we have arguments *)
      if (oargs <> []) then (
        let args_n = SU.number_args args oargs in

        (*       ignore(L.map (fun (i, (name, _)) ->  print_endline ("A= "^(string_of_int i)^" "^name) ) args_n); *)

        let args_n = List.sort SU.sort_args_n args_n in
        (*       ignore(L.map (fun (i, (name, _)) ->  print_endline ("B= "^(string_of_int i)^" "^name) ) args_n); *)
        incr querie_no;
        let doArgument = self#doArgument taskd_args func_vi.vname !querie_no loc in
        let mapped = L.flatten (List.rev_map doArgument args_n) in
        stmts := mkStmt (Instr (L.rev !instrs))::mapped;
        instrs := [];
        args_n
      ) else []
    in

    (* task_desc->args = task_desc+32; *)
    instrs := Set(taskd_args, CastE(tpc_task_argument_pt, BinOp( PlusPI, Lval taskd, one, tpc_task_argument_pt)) , locUnknown)::!instrs;
    (* tpc_call(taskd); *)
    let tpc_call_f = SU.find_function_sign new_file "tpc_call" in
    instrs := Call (None, Lval (var tpc_call_f), [Lval taskd], locUnknown)::!instrs;

    stmts := !stmts@[mkStmt (Instr(L.rev !instrs))];

    incr func_id;
    (!stmts, args_n)

  method getTasks = found_tasks
end
