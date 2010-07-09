(*
 *
 * Copyright (c) 2010, 
 * polyvios@ics.forth.gr
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
let spu_filename = ref "spu-task-code.c"

let options =
  [
    "--debug-tpctool",
      Arg.Set(debug),
      " Print debugging information.";

    "--out-spu",
      Arg.String(fun s -> spu_filename := s),
      " Specify where the spu code should go. (default: spu-task-code.c)";
  ]


type spu_task =
  string * arg list
and arg =
    In
  | Out
  | InOut

let spu_tasks = ref [];

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

(* populates the global list of spu tasks [spu_tasks] *)
class findSPUDeclVisitor = object
  inherit nopCilVisitor
  method vglob (g: global) : global list visitAction = 
    match g with 
      GPragma(Attr("tpc", [ACons(funname, args)]), loc) -> begin
        let args' =
        List.map (fun arg -> match arg with
            AStr("in") -> In
          | AStr("out") -> Out
          | AStr("inout") -> InOut
          | _ -> ignore(E.error "impossible"); assert false
        ) args in
        spu_tasks := (funname, args')::!spu_tasks;
        SkipChildren
      end

    | _ -> SkipChildren
end

(* make a tpc_ version of the function (for use on the ppc side) *)
let make_tpc_func (f: fundec) : global = begin
  let f_new = Cil.emptyFunction ("tpc_func_" ^ f.svar.vname) in
  (* TODO: add function call *)
  GFun (f_new, locUnknown)
end

(* Make the execute_func function that branches on the task id and
 * calls the actual task function on the spe *)
let make_exec_func (f: file) (tasks: fundec list) : global = begin
  (* make the function *)
  let exec_func = Cil.emptyFunction "execute_task" in
  (* make an int argument called "taskid" *)
  let arg1 = Cil.makeFormalVar exec_func "ex_task" Cil.intType (*(TPtr((find_type f "queue_entry_t"), []))*) in
  (* make an int argument called "taskid" *)
  let arg2 = Cil.makeFormalVar exec_func "task_info" Cil.intType in
  (*let arg2 = Cil.makeFormalVar exec_func "y" Cil.intType, false, [] in*)
  (* make an int variable for the return value *)
  let lexit = Cil.makeLocalVar exec_func "exit" Cil.intType in
  (* make a switch statement with one case per task starting from zero *)
  let i = ref 0 in
  let switchcases = List.map
    (fun task ->
      let c = Case (integer !i, locUnknown) in
      incr i;
      let instr = Call (None, Lval (var task.svar), [], locUnknown) in
      let call = Cil.mkStmtOneInstr instr in
      call.labels <- [c];
      [call; mkStmt (Break locUnknown)]
    )
    tasks
  in
  (* TODO: fix default statement to abort *)
  let cases = List.map List.hd switchcases in
  let exit0 = Cil.mkStmtOneInstr (Set (var lexit, zero, locUnknown)) in
  let retstmt = Cil.mkStmt (Return (Some (Lval (var lexit)), locUnknown)) in
  let switchstmt = Cil.mkStmt (
    Switch (Lval (var arg1), Cil.mkBlock (List.flatten switchcases), cases, locUnknown)
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
  dumpFile defaultCilPrinter oc fname file;
  close_out oc
end

let feature : featureDescr = 
  { fd_name = "findspucode";
    fd_enabled = ref true;
    fd_description = "find all pragmas declaring spu tasks";
    fd_extraopt = [];
    fd_doit = 
    (function (f: file) -> 
      (* find tpc_decl pragmas *)
      let fspuVisitor = new findSPUDeclVisitor in
      visitCilFileSameGlobals fspuVisitor f;

      (* create 2 global lists (the 2 output files) *)
      let ppc_glist, spu_glist = ref [], ref [] in

      (* copy all code from file f to file_ppc *)
      ppc_glist := f.globals;
      (* FIXME: change that text input to merger*)
      spu_glist := GText( "#include <stdio.h>\n"
            ^"#include <spu_intrinsics.h>\n"
            ^"#include <spu_mfcio.h>\n"
            ^"#include \"include/tpc_common.h\"\n"
            ^"#include \"include/tpc_spe.h\"\n\n")
	::f.globals;
      let tasks : fundec list = List.map
        (fun (name, _) ->
          let task = find_function_fundec f name in
          ppc_glist := (make_tpc_func task)::(!ppc_glist);
          task
        )
        !spu_tasks
      in
      spu_glist := (make_exec_func f tasks) :: !spu_glist;
(*       print_endline (L.hd(S.split (S.regexp ".c") f.fileName)); *)
      writeFile f "foofafa1.c" !ppc_glist;
      writeFile f "foofafa2.c" !spu_glist;
      );
    fd_post_check = true;
  } 

