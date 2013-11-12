(****************************************************************************)
(* Copyright (c) 2010-13,                                                   *)
(*                        Foivos    Zakkak          <zakkak@ics.forth.gr>   *)
(*                        Polyvios  Pratikakis      <polyvios@ics.forth.gr> *)
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
val current_function : Cil.fundec ref
val stats : bool ref
val unaligned_args : bool ref


(******************************************************************************)
(*                                BOOLEAN                                     *)
(******************************************************************************)

(* Function that checks if an exp uses an index *)
val uses_index : Cil.exp -> bool

(* Check if an arguments type is stride *)
val is_strided : arg_descr -> bool

(* Check if an arguments type is scalar *)
val is_scalar : arg_descr -> bool

(* Check if an argument is region *)
val is_region : arg_descr -> bool

(* Check if an argument is a no transfer region *)
val is_NT_region : arg_descr -> bool

(* Check if an arguments type is out *)
val is_out : arg_descr -> bool

(* Check if an arguments type is in *)
val is_in : arg_descr -> bool

(* Function that checks if a stmt is tagged with a #pragma tpc... *)
val tpc_call_with_arrray : Cil.stmt -> bool

(* Check if <g> is *not* the function declaration of "main"  *)
val is_not_main : Cil.global -> bool

(* Check if <g> is *not* the function declaration of "tpc_call_tpcAD65"  *)
val is_not_skeleton : Cil.global -> bool

(* Check if <g> is a typedef, enum, struct or union *)
val is_typedef : Cil.global -> bool

(* Check if <t> is a scalar *)
val is_scalar_t : Cil.typ -> bool

(* Check if <vi> is a scalar *)
val is_scalar_v : Cil.varinfo -> bool


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
val scalar_exp_to_pointer : Cil.location -> Cil.exp -> Cil.exp

(** Takes a function declaration and changes the types of its scalar formals to
    pointers
    @param f the function declaration to change
 *)
val formals_to_pointers : Cil.location -> Cil.fundec -> unit

(* Converts the strings describing the argument type to arg_flow *)
val arg_flow_of_string : string -> Cil.location -> arg_flow

(* Converts the arg_t to the corresponding (as defined in tpc_common.h) int *)
val int_of_arg_type : arg_type -> int

(* Converts the arg_t to the corresponding string IN/OUT/INOUT *)
val string_of_arg_type : arg_type -> string

(* Checks if tag is data annotation *)
val is_dataflow_tag : string -> bool

(* Converts the arg_t to the corresponding (as defined in tpc_common.h)
 * integer expretion
 *)
val integer_exp_of_arg_type : arg_type -> Cil.exp

(******************************************************************************)
(*                         Copy Function                                      *)
(******************************************************************************)

(* recursively copies a function definition and all it's callees
   from the ppc_file to the spu_file *)
val deep_copy_function : string -> Callgraph.callgraph -> Cil.file -> Cil.file
                         -> unit

(******************************************************************************)
(*                         Function call generator                            *)
(******************************************************************************)
val make_func_call : Cil.file -> Cil.location -> Cil.stmt -> Cil.attrparam list
                     -> string -> Cil.stmt Cil.visitAction

(******************************************************************************)
(*                         AttrParam to Expression                            *)
(******************************************************************************)

(* Convert an attribute into an expression, if possible. Otherwise raise
 * NotAnExpression *)
val attrparam_to_exp : Cil.file -> Cil.location -> ?currFunction:Cil.fundec ->
                       Cil.attrparam -> Cil.exp

(******************************************************************************)
(*                               GETTERS                                      *)
(******************************************************************************)

(* change the return type of a function *)
val set_function_return_type : Cil.fundec -> Cil.typ -> unit

(*(* returns the compiler added variables of the function *)
val get_tpc_added_formals : Cil.fundec -> Cil.fundec -> Cil.varinfo list*)

(* returns the name of the variable in the expration *)
val get_name_of_exp : Cil.exp -> string

(* gets the basetype of a type *)
val get_basetype : Cil.typ -> string -> Cil.typ

(* returns the arg_flow of {e arg} *)
val get_arg_flow : arg_descr -> arg_flow

(* returns the expression with the size of of {e arg} *)
val get_arg_size : arg_descr -> Cil.exp

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
val write_new_file : Cil.file -> string -> Cil.global list -> unit

(* write out file <f> *)
val write_file : Cil.file -> unit

(******************************************************************************)
(*                          Constructors                                      *)
(******************************************************************************)

(* for a struct instance creates the struct.field *)
val make_field_access : Cil.lval -> string -> Cil.lval

(* Defines the Task_table for the spu file *)
val make_task_table : string ->
    (Cil.fundec * Cil.varinfo * (int * arg_descr) list) list -> Cil.global

(* Defines the Task_table for the ppu file *)
val make_null_task_table :
    (Cil.fundec * Cil.varinfo * (int * arg_descr) list) list -> Cil.global

val make_global_var : string -> Cil.init option -> Cil.typ -> Cil.file -> Cil.varinfo

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

(** Preprocesses a header file and merges it with a file (x86). *)
val preprocess_and_merge_header_x86 : Cil.file -> string -> string -> string
     -> unit

(** Preprocesses a header file and merges it with a file (cell). *)
val preprocess_and_merge_header_cell : Cil.file -> string -> string ->
    string -> unit

(** Prints {e msg} if {e flag} is true *)
val debug_print : bool ref -> string -> unit

(** Adds a list of globals right BEFORE the first function definition *)
val add_at_top : Cil.file -> Cil.global list -> unit

(******************************************************************************)
(*                             Argument processor                             *)
(******************************************************************************)

(** processes recursively the arguments' info found in in() out() and
    inout() directives *)
val scoop_process_args : bool -> Cil.file -> string -> Cil.location ->
  Cil.attrparam list -> arg_descr list
