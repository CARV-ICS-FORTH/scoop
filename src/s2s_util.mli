(*
 *
 * Copyright (c) 2010, 
 *  Foivos Zakkak        <zakkak@ics.forth.gr>
 *  Polyvios Pratikakis <polyvios@ics.forth.gr>
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


(******************************************************************************)
(*                          Types                                             *)
(******************************************************************************)

type arg_t =
    In
  | Out
  | InOut
  | SIn
  | SOut
  | SInOut

and arg_descr = (string * (arg_t * Cil.exp * Cil.exp * Cil.exp))

(******************************************************************************)
(*                          Globals                                           *)
(******************************************************************************)

(* define the ppu_vector *)
val ppu_vector : Cil.attribute
val voidType : Cil.typ
val intType : Cil.typ
val uintType : Cil.typ
val longType : Cil.typ
val ulongType : Cil.typ
val charType : Cil.typ
val boolType : Cil.typ
val currentFunction : Cil.fundec ref
val stats : bool ref


(******************************************************************************)
(*                          Search Functions                                  *)
(******************************************************************************)


(* find the function definition of variable <name> in file f *)
val find_function_fundec : Cil.file -> string -> Cil.fundec

(* searches a global list for a function definition with name <name> *)
val find_function_fundec_g : Cil.global list -> string -> Cil.fundec

(* find the function signature for <name> function *)
val find_function_sign : Cil.file -> string -> Cil.varinfo

(* find the (first) typedef for type "name" in file f *)
val find_type : Cil.file -> string -> Cil.typ

(* find the struct or union named struct/union <name> *)
val find_tcomp : Cil.file -> string -> Cil.typ

(* find the variable named <name> in file <f> *)
val find_global_var : Cil.file -> string -> Cil.varinfo

(* find the variable named <name> in the formals of <fd> *)
val find_formal_var : Cil.fundec -> string -> Cil.varinfo

(* find the variable named <name> in the locals of <fd> *)
val find_local_var : Cil.fundec -> string -> Cil.varinfo

(* find the variable named <name> in fundec <fd>
   else look if it's a global of file <f> *)
val find_scoped_var : Cil.fundec -> Cil.file -> string -> Cil.varinfo

(* find the enum named <name> in file f *)
val find_enum : Cil.file -> string -> Cil.enuminfo

(* find the variable named <name> in fd's locals *)
val findLocal : Cil.fundec -> string -> Cil.varinfo


(******************************************************************************)
(*                                Converters                                  *)
(******************************************************************************)


(* Converts the strings describing the argument type to arg_t *)
val translate_arg : string -> bool -> arg_t

(* Converts the arg_t to the corresponding (as defined in tpc_common.h)
 * integer expretion
 *)
val arg_t2integer : arg_t -> Cil.exp

(* Converts the arg_t to the corresponding (as defined in tpc_common.h) int *)
val arg_t2int : arg_t -> int


(******************************************************************************)
(*                         Copy Function                                      *)
(******************************************************************************)

(* recursively copies a function definition and all it's callees
   from the ppc_file to the spu_file *)
val deep_copy_function : string -> Callgraph.callgraph -> Cil.file -> Cil.file -> unit

(******************************************************************************)
(*                         AttrParam to Expression                            *)
(******************************************************************************)

(* Convert an attribute into an expression, if possible. Otherwise raise 
 * NotAnExpression *)
val attrParamToExp : Cil.attrparam -> ?currFunction:Cil.fundec -> Cil.file -> Cil.exp

(******************************************************************************)
(*                               GETTERS                                      *)
(******************************************************************************)

(* returns the argument type from an argument description 
  (string * arg_t * string * string *string ) *)
val get_arg_type : (string * (arg_t * Cil.exp * Cil.exp * Cil.exp )) -> arg_t

(* change the return type of a function *)
val setFunctionReturnType : Cil.fundec -> Cil.typ -> unit

(* returns the compiler added variables of the function *)
val get_tpc_added_formals : Cil.fundec -> Cil.fundec -> Cil.varinfo list


(******************************************************************************)
(*                                   LOOP                                     *)
(******************************************************************************)

(*
 *   for(i=0; i<N; ++i);
 *)

(* returns i=0 *)
val get_loop_lower : Cil.stmt -> Cil.stmt -> Cil.exp

(* returns i<N *)
val get_loop_condition : Cil.stmt -> Cil.exp

(* returns ++i *)
val get_loop_successor : Cil.stmt -> Cil.exp


(******************************************************************************)
(*                             FILE handling                                  *)
(******************************************************************************)

(* write an AST (list of globals) into a file *)
val writeNewFile : Cil.file -> string -> Cil.global list -> unit

(* write out file <f> *)
val writeFile : Cil.file -> unit


(******************************************************************************)
(*                                BOOLEAN                                     *)
(******************************************************************************)

(* function that checks if an exp uses an indice *)
val uses_indice : Cil.exp -> bool

(* check if an arguments type is stride *)
val is_strided : arg_t -> bool

(* check if an arguments type is out *)
val is_out_arg : arg_t -> bool

(* function that checks if a stmt is tagged with a #pragma tpc... *)
val tpc_call_with_arrray : Cil.stmt -> bool

(* Checks if <g> is *not* the function declaration of "main"  *)
val isNotMain : Cil.global -> bool

(* Checks if <g> is *not* the function declaration of "tpc_call_tpcAD65"  *)
val isNotSkeleton : Cil.global -> bool

(* Checks if <g> is a typedef, enum, struct or union *)
val is_typedef : Cil.global -> bool


(******************************************************************************)
(*                          Constructors                                      *)
(******************************************************************************)

(* for a struct instance creates the struct.field *)
val mkFieldAccess : Cil.lval -> string -> Cil.lval

(* for a struct instance   pointer creates the struct->field *)
val mkPtrFieldAccess : Cil.lval -> string -> Cil.lval

(* Defines the Task_table for the spu file *)
val make_task_table :
    (Cil.fundec * Cil.varinfo * arg_descr list) list -> Cil.global

(* Defines the Task_table for the ppu file *)
val make_null_task_table :
    (Cil.fundec * Cil.varinfo * arg_descr list) list -> Cil.global


(******************************************************************************)
(*                                 MISC                                       *)
(******************************************************************************)

val replace_fake_call_with_stmt : Cil.stmt -> string -> Cil.stmt list -> Cil.stmt

val preprocessAndMergeWithHeader_cell : Cil.file -> string -> string ->
    string -> string -> unit
