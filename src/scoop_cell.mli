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

val make_case : Cil.fundec -> Cil.varinfo -> Cil.varinfo -> Cil.varinfo ->
    ((int * Scoop_util.arg_descr) list) -> Cil.stmt

class findTaskDeclVisitor : Callgraph.callgraph -> Cil.file -> Cil.file ->
  string -> object
    inherit Cil.nopCilVisitor
    val mutable spu_tasks :
      ( string * (Cil.fundec * Cil.varinfo * ( int * Scoop_util.arg_descr ) list )) list
    val callgraph : Callgraph.callgraph
    val ppc_file : Cil.file
    val spu_file : Cil.file
    val pragma_str : string
    (* visits all stmts and checks for pragma directives *)
    method vstmt : Cil.stmt -> Cil.stmt Cil.visitAction
    method getTasks : ( string * (Cil.fundec * Cil.varinfo * ( int * Scoop_util.arg_descr ) list )) list
  end
