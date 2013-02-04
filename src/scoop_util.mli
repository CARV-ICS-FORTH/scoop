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
(*
type arg_t =
    In
  | Out
  | InOut
  | TIn
  | TOut
  | TInOut
  | SIn
  | SOut
  | SInOut

and arg_descr = (string * (Cil.exp * arg_t * Cil.exp * Cil.exp * Cil.exp))*)


(* The argument description, extracted by the annotations *)
type arg_descr =
  {
    mutable aname: string;    (* The arguments' name *)
    mutable address: Cil.exp;     (* The arguments' address *)
    mutable atype: arg_type;  (* The argument's type *)
  }

(* The arguments' type *)
and arg_type =
  | Scalar of arg_flow * Cil.exp (* Scalar arguments only need their size (maybe
                                    not) *)
  | Stride of arg_flow * Cil.exp * Cil.exp * Cil.exp
                              (* Stride args have three sizes
                                1. stride size
                                2. number of elements
                                3. size of a single element
                              *)
  | Normal of arg_flow * Cil.exp (* Normal arguments only need their size *)
  | Region of arg_flow * string list (* Region arguments include all the
                                        arguments of the region *)
  | NTRegion of arg_flow * string list (** No transfer arguments include all the
                                           arguments of the region that should
                                           not be transfered *)

(* The arguments' data flow *)
and arg_flow =
  | IN
  | OUT
  | INOUT

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
val unaligned_args : bool ref
val blocking : bool ref


(******************************************************************************)
(*                                BOOLEAN                                     *)
(******************************************************************************)

(* Function that checks if an exp uses an index *)
val uses_index : Cil.exp -> bool

(* Check if an arguments type is stride *)
val isStrided : arg_descr -> bool

(* Check if an arguments type is scalar *)
val isScalar : arg_descr -> bool

(* Check if an argument is region *)
val isRegion : arg_descr -> bool

(* Check if an argument is a no transfer region *)
val isNTRegion : arg_descr -> bool

(* Check if an arguments type is out *)
val isOut : arg_descr -> bool

(* Check if an arguments type is in *)
val isIn : arg_descr -> bool

(* Function that checks if a stmt is tagged with a #pragma tpc... *)
val tpc_call_with_arrray : Cil.stmt -> bool

(* Check if <g> is *not* the function declaration of "main"  *)
val isNotMain : Cil.global -> bool

(* Check if <g> is *not* the function declaration of "tpc_call_tpcAD65"  *)
val isNotSkeleton : Cil.global -> bool

(* Check if <g> is a typedef, enum, struct or union *)
val is_typedef : Cil.global -> bool

(* Check if <t> is a scalar *)
val isScalar_t : Cil.typ -> bool

(* Check if <vi> is a scalar *)
val isScalar_v : Cil.varinfo -> bool


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

(* find the global variable named {e name} in the globals of {e f}*)
val find_global_Gvar : Cil.file -> string -> Cil.global

(* find the variable named <name> in file <f> *)
val find_global_var : Cil.file -> string -> Cil.varinfo

(* find the variable named <name> in file <f> *)
val __find_global_var : Cil.file -> string -> Cil.varinfo

(* find the variable named <name> in the formals of <fd> *)
val find_formal_var : Cil.fundec -> string -> Cil.varinfo

(* find the variable named <name> in the locals of <fd> *)
val find_local_var : Cil.fundec -> string -> Cil.varinfo

(* find the variable named <name> in the locals of <fd> *)
val __find_local_var : Cil.fundec -> string -> Cil.varinfo

(* find the variable named <name> in fundec <fd>
   else look if it's a global of file <f> *)
val find_scoped_var : Cil.location -> Cil.fundec -> Cil.file -> string ->
  Cil.varinfo

(* find the enum named <name> in file f *)
val find_enum : Cil.file -> string -> Cil.enuminfo


(******************************************************************************)
(*                                Converters                                  *)
(******************************************************************************)

(** Takes an expression and changes it if it's a scalar to its address
    @param e the expression to get the address of
    @return the new expression (& old_expression)
 *)
val expScalarToPointer : Cil.location -> Cil.exp -> Cil.exp

(** Takes a function declaration and changes the types of its scalar formals to
    pointers
    @param f the function declaration to change
 *)
val formalScalarsToPointers : Cil.location -> Cil.fundec -> unit

(* Converts the strings describing the argument type to arg_flow *)
val str2arg_flow : string -> Cil.location -> arg_flow

(* Converts the arg_t to the corresponding (as defined in tpc_common.h) int *)
val arg_type2int : arg_type -> int

(* Converts the arg_t to the corresponding string IN/OUT/INOUT *)
val arg_type2string : arg_type -> string

(* Checks if tag is data annotation *)
val is_dataflow_tag : string -> bool

(* Converts the arg_t to the corresponding (as defined in tpc_common.h)
 * integer expretion
 *)
val arg_type2integer : arg_type -> Cil.exp

(******************************************************************************)
(*                         Copy Function                                      *)
(******************************************************************************)

(* recursively copies a function definition and all it's callees
   from the ppc_file to the spu_file *)
val deep_copy_function : string -> Callgraph.callgraph -> Cil.file -> Cil.file
-> unit

(******************************************************************************)
(*                         AttrParam to Expression                            *)
(******************************************************************************)

(* Convert an attribute into an expression, if possible. Otherwise raise
 * NotAnExpression *)
val attrParamToExp : Cil.file -> Cil.location -> ?currFunction:Cil.fundec ->
    Cil.attrparam -> Cil.exp

(******************************************************************************)
(*                               GETTERS                                      *)
(******************************************************************************)

(* change the return type of a function *)
val setFunctionReturnType : Cil.fundec -> Cil.typ -> unit

(*(* returns the compiler added variables of the function *)
val get_tpc_added_formals : Cil.fundec -> Cil.fundec -> Cil.varinfo list*)

(* returns the name of the variable in the expration *)
val getNameOfExp : Cil.exp -> string

(* gets the basetype of a type *)
val getBType : Cil.typ -> string -> Cil.typ

(* returns the arg_flow of {e arg} *)
val getFlowOfArg : arg_descr -> arg_flow

(* returns the expression with the size of of {e arg} *)
val getSizeOfArg : arg_descr -> Cil.exp

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
(*                          Constructors                                      *)
(******************************************************************************)

(* for a struct instance creates the struct.field *)
val mkFieldAccess : Cil.lval -> string -> Cil.lval

(* Defines the Task_table for the spu file *)
val make_task_table : string ->
    (Cil.fundec * Cil.varinfo * (int * arg_descr) list) list -> Cil.global

(* Defines the Task_table for the ppu file *)
val make_null_task_table :
    (Cil.fundec * Cil.varinfo * (int * arg_descr) list) list -> Cil.global

(******************************************************************************)
(*                                 MISC                                       *)
(******************************************************************************)

val replace_fake_call_with_stmt : Cil.stmt -> string -> Cil.stmt list ->
  Cil.stmt

(** Comparators for use with [List.sort] *)
val comparator : (int * Cil.exp) -> (int * Cil.exp) -> int

val sort_args : arg_descr -> arg_descr -> int

val sort_args_n : (int*arg_descr) -> (int*arg_descr) -> int

val sort_args_n_inv : (int*arg_descr) -> (int*arg_descr) -> int

(** assigns to each argument description its place in the original argument list
 *)
val number_args : arg_descr list -> Cil.exp list -> (int * arg_descr) list

(** Preprocesses a header file and merges it with a file. *)
val preprocessAndMergeWithHeader_cell : Cil.file -> string -> string ->
    string -> unit

(** Prints {e msg} if {e flag} is true *)
val dbg_print : bool ref -> string -> unit

(** Adds a list of globals right BEFORE the first function definition *)
val add_at_top : Cil.file -> Cil.global list -> unit

(******************************************************************************)
(*                             Argument processor                             *)
(******************************************************************************)

(** processes recursively the arguments' info found in in() out() and
    inout() directives *)
val scoop_process_args : bool -> Cil.file -> string -> Cil.location ->
  Cil.attrparam list -> arg_descr list
