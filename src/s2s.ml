(*
 *
 * Copyright (c) 2010, 
 *  Polyvios Pratikakis <polyvios@ics.forth.gr>
 *  Foivos Zakkak	<zakkak@ics.forth.gr>
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

open Pretty
open Cil
module E = Errormsg
module H = Hashtbl
module S = Str
module L = List

let debug = ref false
let out_name = ref "final"

let options =
  [
    "--debug-tpctool",
      Arg.Set(debug),
      " Print debugging information.";

    "--out-name",
      Arg.String(fun s -> out_name := s),
      " Specify the output files' prefix. (default: final) will produce final.c and final_func.c";
  ]


type spu_task =
  string * arg list
and arg =
    In
  | Out
  | InOut

(* FIXME: in_file should refer to a file not a list*)
(* create 2 lists (the ppc output file and the tasks list) and create a ref to the input file*)
let spu_tasks, ppc_glist, in_file, func_id = ref [], ref [], ref Cil.dummyFile , ref 0;

(* find the function definition of variable "name" in file f *)
exception Found_fundec of fundec
let find_function_fundec (f: file) (name: string) : fundec =
  let findit = function
    | GFun(fd, _) when fd.svar.vname = name -> raise (Found_fundec fd)
    | _ -> ()
  in
  try
    Cil.iterGlobals f findit;
    raise Not_found
  with Found_fundec v -> v

(* find the (first) typedef for type "name" in file f *)
exception Found_type of typ
let find_type (f: file) (name: string) : typ =
  let findit = function
    | GType(ti, _) when ti.tname = name -> raise (Found_type (TNamed(ti, [])))
    | _ -> ()
  in
  try
    Cil.iterGlobals f findit;
    raise Not_found
  with Found_type t -> t

let find_tcomp (f: file) (name: string) : typ =
  let findit = function
    | GCompTag(ci, _) when ci.cname = name -> raise (Found_type (TComp(ci, [])))
    | _ -> ()
  in
  try
    Cil.iterGlobals f findit;
    raise Not_found
  with Found_type t -> t

(* make a tpc_ version of the function (for use on the ppc side) *)
let make_tpc_func (f: fundec) : global = begin
  print_endline ("Creating tpc_function_" ^ f.svar.vname);
  let f_new = Cil.emptyFunction ("tpc_function_" ^ f.svar.vname) in
  Cil.setFunctionTypeMakeFormals f_new f.svar.vtype;
  (* TODO: push the size variables as args *)
  (*** Declare the local variables ***)
  (* unsigned int total_bytes *)
  let total_bytes = Cil.makeLocalVar f_new "total_bytes" Cil.uintType in
  (* volatile queue_entry_t *remote_entry *)
  let remote_entry = Cil.makeLocalVar f_new "remote_entry" (TPtr((find_type !in_file "queue_entry_t"), [Attr("volatile", [])])) in
  (* volatile queue_entry_t *avail_task *)
  let avail_task = Cil.makeLocalVar f_new "avail_task" (TPtr((find_type !in_file "queue_entry_t"), [Attr("volatile", [])])) in
  (* TODO: add the #ifdef
    #ifdef STATISTICS
      uint64_t tmptime1, tmptime2, tmptime3;
      int arg_bytes;
    #endif
  *)
  (* unsigned int *task_id_qs *)
  let task_id_qs = Cil.makeLocalVar f_new "task_id_qs" (TPtr(Cil.uintType, [])) in
  (* unsigned int task_id *)
  let task_id = Cil.makeLocalVar f_new "task_id" Cil.uintType in
  (* if (f_new.sformals <> []) then begin *)
    (* void *arg_addr64 *)
    let arg_addr64 = Cil.makeLocalVar f_new "arg_addr64" voidPtrType in
    (* unsigned int arg_size *)
    let arg_size = Cil.makeLocalVar f_new "arg_size" Cil.uintType in
    (* unsigned int arg_flag *)
    let arg_flag = Cil.makeLocalVar f_new "arg_flag" Cil.uintType in
    (* unsigned int arg_stride *)
    let arg_stride = Cil.makeLocalVar f_new "arg_stride" Cil.uintType in
    (* vector unsigned char *tmpvec   where vector is __attribute__((vector_size(8))) *)
    let tmpvec = Cil.makeLocalVar f_new "tmpvec" (TPtr(TInt(IUChar, []), [Attr("__attribute__", [ACons("vector_size", [AInt(8)])])])) in
    (* struct tpc_arg_element local_arg *)
    let local_arg = Cil.makeLocalVar f_new "local_arg" (find_tcomp !in_file "tpc_arg_element") in
  (* end *)

  (*** Initialize local variables ***)
  let stmts = ref [] in
  (* remote_entry=NULL *)
  stmts := Cil.mkStmtOneInstr (Set (var remote_entry, CastE(voidPtrType ,zero), locUnknown))::!stmts;
  (* avail_task=NULL *)
  stmts := Cil.mkStmtOneInstr (Set (var avail_task, CastE(voidPtrType ,zero), locUnknown))::!stmts;
  (* arg_addr64=NULL *)
  stmts := Cil.mkStmtOneInstr (Set (var arg_addr64, CastE(voidPtrType ,zero), locUnknown))::!stmts;
  (* arg_size=0 *)
  stmts := Cil.mkStmtOneInstr (Set (var arg_size, zero, locUnknown))::!stmts;
  (* arg_flag=0 *)
  stmts := Cil.mkStmtOneInstr (Set (var arg_flag, zero, locUnknown))::!stmts;
  (* arg_stride=0 *)
  stmts := Cil.mkStmtOneInstr (Set (var arg_stride, zero, locUnknown))::!stmts;

  (* TODO: complete code *)

  (* return task_id; *)
  stmts := Cil.mkStmt (Return (Some (Lval (var task_id)), locUnknown))::!stmts;
  f_new.sbody <- mkBlock !stmts;
  GFun (f_new, locUnknown)
end

(* populates the global list of spu tasks [spu_tasks] *)
class findSPUDeclVisitor = object
  inherit nopCilVisitor
  (* visits all stmts and checks for pragma directives *)
  method vstmt (s: stmt) : stmt visitAction =
    let prags = s.pragmas in
    if (prags <> []) then begin
      print_endline "We 've got pragmas here";
      match (List.hd prags) with 
	(Attr("tpc", args), _) -> begin
	  let args' =
	    List.map (fun arg -> match arg with
		ACons(varname, ACons("in", [])::ACons(varsize, [])::[]) -> In
	      | ACons(varname, ACons("out", [])::ACons(varsize, [])::[]) -> Out
	      | ACons(varname, ACons("inout", [])::ACons(varsize, [])::[]) -> InOut
	      | _ -> ignore(E.error "impossible"); assert false
	    ) args in
	  match s.skind with 
	    Instr(Call(_, Lval((Var(vi), _)), _, _)::_) -> begin
	      print_endline "CALL";
	      let funname = vi.vname in
	      print_endline ("Found task \""^funname^"\"");
	      try
		(* check if we have seen this function before *)
		let ( _, new_fd) = List.assoc funname !spu_tasks in
		(* TODO: add arguments to the call *)
		let instr = Call (None, Lval (var new_fd.svar), [], locUnknown) in
		let call = Cil.mkStmtOneInstr instr in
		ChangeTo(call)
	      with Not_found -> begin
		let task = find_function_fundec (!in_file) funname in
		let new_tpc = make_tpc_func task in
		(* FIXME: Warning P: this pattern-matching is not exhaustive. *)
		let GFun(new_fd, _) = new_tpc in
		ppc_glist := new_tpc::(!ppc_glist);
		spu_tasks := (funname, (args', new_fd))::!spu_tasks;
		(* TODO: add arguments to the call *)
		let instr = Call (None, Lval (var new_fd.svar), [], locUnknown) in
		let call = Cil.mkStmtOneInstr instr in
		ChangeTo(call)
	      end
	    end
	    | Block(b) -> print_endline "Ignoring block pragma"; DoChildren
	    | _ -> print_endline "Ignoring pragma"; DoChildren
	end
	| _ -> print_endline "Unrecognized pragma"; DoChildren
    end else
      DoChildren
end

(* Make the execute_func function that branches on the task id and
 * calls the actual task function on the spe *)
let make_exec_func (f: file) (tasks: fundec list) : global = begin
  (* make the function *)
  let exec_func = Cil.emptyFunction "execute_task" in
  (* make "queue_entry_t * volatile  ex_task" *)
  let arg1 = Cil.makeFormalVar exec_func "ex_task" (TPtr((find_type f "queue_entry_t"), [Attr("volatile", [])])) in
  (* make "tpc_spe_task_state_t task_info" *)
  (* FIXME: Warning S: this expression should have type unit. *)
  (Cil.makeFormalVar exec_func "task_info" (find_type f "tpc_spe_task_state_t"));
  (* make an int variable for the return value *)
  let lexit = Cil.makeLocalVar exec_func "exit" Cil.intType in
  (* make a switch statement with one case per task starting from zero *)
  let switchcases = List.map
    (fun task ->
      let c = Case (integer !func_id, locUnknown) in
      incr func_id;
      let instr = Call (None, Lval (var task.svar), [], locUnknown) in
      let call = Cil.mkStmtOneInstr instr in
      call.labels <- [c];
      [call; mkStmt (Break locUnknown)]
    )
    tasks
  in
  let cases = List.map List.hd switchcases in
  (* let one = 1*)
  let one = Const(CInt64(Int64.one, IInt, None)) in
  (* make stmt exit=1; *)
  let assignment = Cil.mkStmtOneInstr (Set (var lexit, one, locUnknown)) in
  (* set it's label to default *)
  assignment.labels <- [Default(locUnknown)];
  (* append the new stmt to the switch *)
  let switchcases2 = (List.append (List.flatten switchcases) [assignment; mkStmt (Break locUnknown)]) in
  (* make stmt exit=0; *)
  let exit0 = Cil.mkStmtOneInstr (Set (var lexit, zero, locUnknown)) in
  (* make return exit; *)
  let retstmt = Cil.mkStmt (Return (Some (Lval (var lexit)), locUnknown)) in
  (* the case expression of the switch statement (switch(expr)) *)
  let expr = Lval (Lockutil.mkPtrFieldAccess (var arg1) "funcid") in
  let switchstmt = Cil.mkStmt (
    Switch ( expr , Cil.mkBlock switchcases2, cases, locUnknown)
  ) in
  (* the function body: exit = 0; switch (taskid); return exit; *)
  exec_func.sbody <- mkBlock [exit0; switchstmt; retstmt];
  GFun (exec_func, locUnknown)
end

(* write an AST (list of globals) into a file *)
let writeFile f fname globals = begin
  let file = { f with
    fileName = fname;
    globals = globals;
  } in
  let oc = open_out fname in
  Rmtmps.removeUnusedTemps file;
  dumpFile defaultCilPrinter oc fname file;
  close_out oc
end

(* Preprocess the header file <header> and merges it with f.  The
 * given header should be in the gcc include path.  Modifies f
 *) (* the original can be found in lockpick.ml *)
let preprocessAndMergeWithHeader (f: file) (header: string) : unit = begin
  (* FIXME: what if we move arround the executable? *)
  (* FIXME: Warning S: this expression should have type unit. *)
  Sys.command ("echo | gcc -E -DCIL=1 -I./include/ppu -I./include/spu -include tpc_s2s.h -include "^(header)^" - >/tmp/_cil_rewritten_tmp.h");
  let add_h = Frontc.parse "/tmp/_cil_rewritten_tmp.h" () in
  let f' = Mergecil.merge [add_h; f] "stdout" in
  f.globals <- f'.globals;
end

(* Checks if <g> is *not* the function declaration of "main"  *)
let isNotMain (g: global) : bool = match g with
    GFun({svar = vi}, _) when (vi.vname = "main") -> false
  | _ -> true


let feature : featureDescr = 
  { fd_name = "findspucode";
    fd_enabled = ref true;
    fd_description = "find all pragmas declaring spu tasks";
    fd_extraopt = options;
    fd_doit = 
    (function (f: file) -> 
      (* get the input file for global use *)
      in_file := f;
      (* find tpc_decl pragmas *)
      let fspuVisitor = new findSPUDeclVisitor in

      (* create a global list (the spu output file) *)
      let spu_glist = ref [] in

      (* copy all code from file f to file_ppc *)
      preprocessAndMergeWithHeader f "ppu_intrinsics.h";
      preprocessAndMergeWithHeader f "include/tpc_common.h";
      preprocessAndMergeWithHeader f "include/tpc_spe.h";
      ppc_glist := f.globals;
      visitCilFileSameGlobals fspuVisitor f;
      (* copy all code from file f to file_spe plus the needed headers*)
      preprocessAndMergeWithHeader f "spu_intrinsics.h";
      preprocessAndMergeWithHeader f "spu_mfcio.h";
      preprocessAndMergeWithHeader f "include/tpc_common.h";
      preprocessAndMergeWithHeader f "include/tpc_spe.h";
      (* copy all globals except the function declaration of "main" *)
      spu_glist := List.filter isNotMain f.globals;

      let tasks : fundec list = List.map
        (fun (name, _) -> find_function_fundec f name)
        !spu_tasks
      in
      spu_glist := List.append !spu_glist [(make_exec_func f tasks)];
      writeFile f (!out_name^".c") !ppc_glist;
      writeFile f (!out_name^"_func.c") !spu_glist;
      );
    fd_post_check = true;
  } 

