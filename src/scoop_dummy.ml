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

(** Responsible for generating code for the myrmics runtime on the
    formic architecture
    @author Foivos Zakkak, zakkak\@ics.forth.gr *)

open Cil
module SU = Scoop_util
module L  = List
module E  = Errormsg

(** Dummy string option *)
let dummy_str = ref ""
(** Dummy Boolean option *)
let dummy_flag1 = ref false
(** Dummy Boolean option *)
let dummy_flag2 = ref true

let options = [
  "--dummy-print",
  Arg.String(fun s -> dummy_str := s),
  " SCOOP: This is a dummy option, just for demo purposes";

  "--dummy-enable",
  Arg.Set(dummy_flag1),
  " SCOOP: This is a dummy option, just for demo purposes";

  "--dummy-disable",
  Arg.Clear(dummy_flag1),
  " SCOOP: This is a dummy option, just for demo purposes";
]

class codegen (cgraph : Callgraph.callgraph) file pragma includePath =
object (self) inherit Scoop_codegen.codegen cgraph file pragma includePath as super

  (* a unique id for unique variable naming *)
  val un_id               = ref 0
  (* this is needed for SDAM *)
  val query_id            = ref 0
  (* keeps the current funcid for the new tpc_function *)
  val func_id             = ref 0

  (** The function name that implements wait_on *)
  val scoop_wait_on       = "dummy_wait_on"
  (** The function name that implements barrier *)
  val scoop_barrier       = "dummy_syncall"
  (** The function name that implements start *)
  val scoop_start         = "dummy_init"
  (** The function name that implements finish *)
  val scoop_finish        = "dummy_finish"
  (** The function name that implements malloc *)
  val scoop_malloc        = "dummy_alloc"
  (** The function name that implements free *)
  val scoop_free          = "dummy_free"
  (** The runtime name *)
  val runtime             = "dummy"

  (** Declare any globals needed by the code generator *)
  method declareGlobals : unit =
    let globals = ref [] in
    let uint64_t = (SU.find_type new_file "uint64_t") in
    let make_global_var ini n t =
      globals := GVar(makeGlobalVar n t, {init = ini;}, locUnknown)::!globals;
    in
    make_global_var None "_time1_SCOOP__" uint64_t;
    make_global_var None "_time2_SCOOP__" uint64_t;
    SU.add_at_top new_file !globals;

  (** Preprocesses the runtime header file and merges it with new_file. *)
  method preprocess_and_merge_header flags : unit =
    SU.preprocess_and_merge_header_x86 new_file
                                        (includePath)
                                        ((runtime)^".h")
                                        flags;
    SU.preprocess_and_merge_header_x86 new_file
                                        (includePath)
                                        ("lib.h")
                                        flags;
    SU.preprocess_and_merge_header_x86 new_file
                                        (includePath)
                                        ("main.h")
                                        flags;

  (** Generates the code to spawn a task
   * containing the arguments' types the invokes _sys_spawn with those as args
   * @param loc the current file location
   * @param func_vi the varinfo of the original function
   * @param oargs the original arguments
   * @param args the arguments from the pragma directive
   * @param f the file we are modifying
   * @return the stmts that will replace the call paired with a list of numbered
   * argument descriptors
   *)
  method make_task_spawn (loc: location) (func_vi: varinfo) (oargs: exp list)
                         (args: SU.arg_descr list)
         : (stmt list * (int * SU.arg_descr) list) =
    incr un_id;

    let args_num = List.length oargs in
    let args_num_i = integer args_num in

    (* Create an array with the argument descriptors *)
    let scoop2179_args =
        var (makeLocalVar !SU.current_function ("scoop_args"^(string_of_int !un_id))
                          (TArray(voidPtrType, Some(args_num_i), [])))
    in

    let args_n, instrs =
      (* if we have arguments *)
      if (oargs <> []) then (
        let args_n = SU.number_args args oargs in
        let args_n = List.sort SU.sort_args_n args_n in
        incr query_id;
        let doArgument = self#doArgument scoop2179_args func_vi.vname !query_id in
        let mapped = L.flatten (List.map doArgument args_n) in
        (args_n, mapped)
      ) else ([], [])
    in

    (* spawn(funcid, args, num_args); *)
    let spawn_f = SU.find_function_sign new_file "spawn" in

    let call_args = [integer !func_id;
                     Lval scoop2179_args;
                     args_num_i] in
    let instrs = Call (None, Lval (var spawn_f), call_args, locUnknown)::instrs in

    incr func_id;
    ([mkStmt (Instr(L.rev instrs))], args_n)

  (** passes the arguments to the arrays in the correct order
   * @param targs the arguments' table
   * @param orig_tname the original function's name
   * @param tid an id marking the query (needed by SDAM)
   * @param arg the argument we want to place
   * @return the generated Set instrs
   *)
  method private doArgument targs (orig_tname: string) (tid: int)
                 (arg: (int * SU.arg_descr) ) : instr list =
    let (i_m, arg_desc) = arg in
    let arg_addr = arg_desc.SU.address in
    let arg_type = arg_desc.SU.atype in
    let arg_name = arg_desc.SU.aname in

    if (SU.is_scalar arg_desc) then (
      (* do something for scalars *)
    (* invoke isSafeArg from PtDepa to check whether this argument is a no dep *)
    ) else if (Sdam.isSafeArg orig_tname tid arg_name) then (
      (* do something for safe arguments *)
    );

    let mkSet table i arg =
      let t = (
        match typeOfLval table with
          TArray(t', _, _) -> t'
        | t' -> t'
      ) in
      let curr = addOffsetLval (Index(integer i, NoOffset)) table in
      Set( curr, CastE(t, arg), locUnknown)
    in

    [mkSet targs i_m arg_addr]

  (* IF YOU NEED FURTHER CUSTOMIZATION PLEASE REFER TO
  scoop_codegen.ml FOR OTHER OVERRIDABLE METHODS *)

  method getTasks = found_tasks
end
