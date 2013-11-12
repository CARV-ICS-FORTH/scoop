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

(** Generic code generation class. To be inherited by runtime specific
    code generators *)
class virtual codegen : Callgraph.callgraph -> Cil.file -> string -> string ->
  object inherit Cil.nopCilVisitor
    val callgraph     : Callgraph.callgraph
    val new_file      : Cil.file
    val pragma_str    : string
    val scoop_wait_on : string
    val scoop_barrier : string
    val scoop_start   : string
    val scoop_finish  : string
    val scoop_malloc  : string
    val scoop_free    : string
    val runtime       : string
    val includePath   : string
    val mutable found_tasks :
      ( string * (Cil.fundec * Cil.varinfo * ( int * Scoop_util.arg_descr ) list )) list

    (** Write the generated file to disk *)
    method write_file : unit
    (** Creates a task table (a tale with all tasks with the funcid as
     * index) and adds it to the file to be generated *)
    method makeTaskTable : unit
    (** Parse file to find task spawns and dependencies between arguments *)
    method parseFile : bool -> unit
    (** Declare any globals needed by the code generator *)
    method declareGlobals : unit
    (** Preprocesses the runtime header file and merges it with new_file. *)
    method preprocess_and_merge_header : string -> unit
    (** Generates the code to spawn a task *)
    method virtual make_task_spawn : Cil.location -> Cil.varinfo -> Cil.exp list
                                     -> Scoop_util.arg_descr list
                                     -> (Cil.stmt list * (int * Scoop_util.arg_descr) list)
    (* populates the global list of tasks [tasks] *)
    (** visits all stmts and checks for pragma directives *)
    method vstmt : Cil.stmt -> Cil.stmt Cil.visitAction
    method getTasks :
             ( string * (Cil.fundec * Cil.varinfo * ( int * Scoop_util.arg_descr ) list )) list
  end
