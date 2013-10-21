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

val options : (string * Arg.spec * string) list

val make_case : Cil.fundec -> Cil.varinfo -> Cil.varinfo -> Cil.varinfo ->
    ((int * Scoop_util.arg_descr) list) -> Cil.stmt

class codegen : Callgraph.callgraph -> Cil.file -> string -> string -> string
                -> bool ->
  object
    inherit Scoop_codegen.codegen
    method make_task_spawn : Cil.location -> Cil.varinfo -> Cil.exp list
                             -> Scoop_util.arg_descr list
                             -> (Cil.stmt list * (int * Scoop_util.arg_descr) list)
  end
