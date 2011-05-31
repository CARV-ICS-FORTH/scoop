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

(** Includes all the generic functions (utilities) *)

open Cil
module E = Errormsg
module H = Hashtbl
module L = List
module CG = Callgraph


(******************************************************************************)
(*                          Types                                             *)
(******************************************************************************)

(** the argument type supported by the annotations *)
type arg_t =
    In
  | Out
  | InOut
  | SIn
  | SOut
  | SInOut

(** the argument description, extracted by the annotations *)
and arg_descr = (string * (arg_t * exp * exp * exp))

(******************************************************************************)
(*                          Globals                                           *)
(******************************************************************************)

(** define the ppu_vector *)
let ppu_vector = Attr("altivec", [ACons("vector__", [])])

(** define void *)
let voidType = TVoid([])
(** define int *)
let intType = TInt(IInt,[])
(** define uint *)
let uintType = TInt(IUInt,[])
(** define long *)
let longType = TInt(ILong,[])
(** define ulong *)
let ulongType = TInt(IULong,[])
(** define char *)
let charType = TInt(IChar, [])
(** define bool *)
let boolType = TInt(IBool, [])

(** keeps the function we are in at the current point (scoping) *)
let currentFunction = ref dummyFunDec

(** keeps whether stats are enabled or not *)
let stats = ref false
(** keeps whether we want unaligned arguments or not *)
let unaligned_args = ref false
(** keeps whether we want blocking of arguments or not *)
let blocking = ref false


(******************************************************************************)
(*                          Search Functions                                  *)
(******************************************************************************)

(** Exception returning the found function declaration *)
exception Found_fundec of fundec
(** searches a global list for a function definition with name {e name}
    @param g the global list to search in
    @param name the name of the function to search
    @raise Not_found when there is no function declaration with name
      {e name} in {e g}
    @return the Cil.fundec of the function named {e name} *)
let find_function_fundec_g (g: global list) (name: string) : fundec =
  let findit = function
    | GFun(fd, _) when fd.svar.vname = name -> raise (Found_fundec fd)
    | _ -> ()
  in
  try
    List.iter findit g;
    raise Not_found
  with Found_fundec v -> v

(** find the function definition of variable {e name} in file {e f}
    @param f the file to look in
    @param name the name of the function to search
    @raise Not_found when there is no function declaration with name
      {e name} in {e f}
    @return the Cil.fundec of the function named {e name}
 *)
let find_function_fundec (f: file) (name: string) : fundec =
  find_function_fundec_g f.globals name

(** Exception returning the found function signature *)
exception Found_sign of varinfo
(** find the function signature for {e name} function in file {e f}
    @param f the file to look in
    @param name the name of the function to search
    @raise Not_found when there is no function signature with name
      {e name} in {e f}
    @return the Cil.varinfo of the function named {e name}
 *)
let find_function_sign (f: file) (name: string) : varinfo =
  let findit = function
    | GVarDecl(vi, _) when vi.vname = name -> raise (Found_sign vi)
    | _ -> ()
  in
  try
    iterGlobals f findit;
    raise Not_found
  with Found_sign v -> v

(** Exception returning the found type *)
exception Found_type of typ
(** find the {b first} typedef for type {e name} in file {e f}
    @param f the file to look in
    @param name the name of the type to search
    @raise Not_found when there is no type with name {e name} in {e f}
    @return the Cil.typ of the function named {e name}
 *)
let find_type (f: file) (name: string) : typ =
  let findit = function
    | GType(ti, _) when ti.tname = name -> raise (Found_type (TNamed(ti, [])))
    | _ -> ()
  in
  try
    iterGlobals f findit;
    raise Not_found
  with Found_type t -> t

(** find the struct or union named struct/union {e name} in file {e f}
    @param f the file to look in
    @param name the name of the struct/union to search
    @raise Not_found when there is no struct or union with name {e name} in {e f}
    @return the Cil.typ of the function named {e name}
 *)
let find_tcomp (f: file) (name: string) : typ =
  let findit = function
    | GCompTag(ci, _) when ci.cname = name -> raise (Found_type (TComp(ci, [])))
    | _ -> ()
  in
  try
    iterGlobals f findit;
    raise Not_found
  with Found_type t -> t

(** Exception returning the found varinfo *)
exception Found_var of varinfo
(** find the global variable named {e name} in file {e f} 
    @param f the file to look in
    @param name the name of the global variable to search
    @raise Not_found when there is no global variable with name {e name} in {e f}
    @return the Cil.varinfo of the global variable {e name}
 *)
let find_global_var (f: file) (name: string) : varinfo =
  let findit = function
    | GVarDecl(vi, _) when vi.vname = name -> raise (Found_var vi)
    | GVar(vi, _, _) when vi.vname = name -> raise (Found_var vi)
    | _ -> ()
  in
  try
    iterGlobals f findit;
    raise Not_found
  with Found_var v -> v

(** find the variable named {e name} in the formals of {e fd} 
    @param fd the function declaration to look in
    @param name the name of the formal variable to search
    @raise Not_found when there is no formal variable with name {e name} in {e fd}
    @return the Cil.varinfo of the formal variable {e name}
 *)
let find_formal_var (fd: fundec) (name: string) : varinfo =
  let findit = function
    | vi when vi.vname = name -> raise (Found_var vi)
    | _ -> ()
  in
  try
    List.iter findit fd.sformals;
    raise Not_found
  with Found_var v -> v

(** find the variable named {e name} in the locals of {e fd}  
    @param fd the function declaration to look in
    @param name the name of the local variable to search
    @raise Not_found when there is no local variable with name {e name} in {e fd}
    @return the Cil.varinfo of the local variable {e name}
 *)
let find_local_var (fd: fundec) (name: string) : varinfo =
  let findit = function
    | vi when vi.vname = name -> raise (Found_var vi)
    | _ -> ()
  in
  try
    List.iter findit fd.slocals;
    raise Not_found
  with Found_var v -> v

(** find the variable named {e name} in fundec {e fd}
   else look if it's a global of file {e f}  
    @param fd the function declaration to look in
    @param f the file to look in
    @param name the name of the formal variable to search
    @raise Not_found when there is no variable with name {e name} in {e fd} of {e f}
    @return the Cil.varinfo of the variable {e name}
 *)
let find_scoped_var (fd: fundec) (f: file) (name: string) : varinfo =
  try
    find_local_var fd name
  with Not_found -> 
      ( try
        find_formal_var fd name
      with Not_found -> 
          ( try
            find_global_var f name
          with Not_found -> (
              raise Not_found)
          )
      )

(** Exception returning the found enum *)
exception Found_enum of enuminfo
(** find the enum named {e name} in file {e f}
    @param f the file to look in
    @param name the name of the enum to search
    @raise Not_found when there is no enum with name {e name} in {e fd} of {e f}
    @return the Cil.enuminfo of the enum {e name}
 *)
let find_enum (f: file) (name: string) : enuminfo =
  let findit = function
    | GEnumTag(ei, _) when ei.ename = name -> raise (Found_enum ei)
    | _ -> ()
  in 
  try
    iterGlobals f findit;
    raise Not_found
  with Found_enum ei -> ei



(******************************************************************************)
(*                                Converters                                  *)
(******************************************************************************)


(** Converts the {e arg} describing the argument type to arg_t
    @param arg the string (in/out/inout/input/output) describing the type of the argument
    @param strided flag showing whether it is a strided argument
    @return the corresponding arg_t
 *)
let translate_arg (arg: string) (strided: bool) : arg_t =
  match arg with
      "in" when strided -> SIn
    | "out" when strided -> SOut
    | "inout" when strided -> SInOut
    | _  when strided -> ignore(E.error "Only in/out/inout are allowed"); assert false
    | "in" (* legacy *)
    | "input" -> In
    | "out" (* legacy *)
    | "output" -> Out
    | "inout" -> InOut
    | _ -> ignore(E.error "Only input/output/inout are allowed"); assert false

(** Maps the arg_t to a number as defined by the TPC headers
    @return the corrensponding number *)
let arg_t2int = function
    | In -> 1
    | Out -> 2
    | InOut -> 3
    | SIn -> 5
    | SOut -> 6
    | SInOut -> 7

(** Maps the arg_t to ints as defined by the TPC headers
    @return the corrensponding int *)
let arg_t2integer = function
    | t -> integer (arg_t2int t)


(******************************************************************************)
(*                         Copy Function                                      *)
(******************************************************************************)

(** recursively copies a function definition and all it's callees
   from the {e ppc_file} to the {e spu_file}
    @param func the name of the function to be copied
    @param callgraph the callgraph of the program
    @param spu_file the spu file
    @param ppc_file the ppc file
*)
let rec deep_copy_function (func: string) (callgraph: CG.callgraph)
      (spu_file: file) (ppc_file: file)= (
    (* TODO copy globals? *)

    (* First copy the Callees *)
    let cnode: CG.callnode = H.find callgraph func in
    let nodeName = function
      | CG.NIVar (v, _) -> v.vname
      | CG.NIIndirect (n, _) -> n
    in
    let deep_copy _ (n: CG.callnode) : unit =
      let name = nodeName n.CG.cnInfo in
      deep_copy_function name callgraph spu_file ppc_file
    in
    Inthash.iter deep_copy cnode.CG.cnCallees;
    
    (* now copy current *)
    try
      (* First check whether the function is defined *)
      let new_fd = GFun(find_function_fundec (ppc_file) func, locUnknown) in
      spu_file.globals <- spu_file.globals@[new_fd];
      (* if not check whether we have a signature *)
    with Not_found -> ignore(find_function_sign (ppc_file) func);
)

(******************************************************************************)
(*                               GETTERS                                      *)
(******************************************************************************)

(* change the return type of a function *)
let setFunctionReturnType (f: fundec) (t: typ) : unit = begin
  match unrollType f.svar.vtype with
    TFun (_, Some args, va, a) -> 
      f.svar.vtype <- TFun(t, Some args, va, a);
    | _ -> assert(false);
end

(* returns the compiler added variables of the function *)
let get_tpc_added_formals (new_f: fundec) (old_f: fundec) : varinfo list = begin
  List.filter 
    (fun formal -> 
        List.exists 
          (fun formal2 -> formal <> formal2 )
          old_f.sformals
    )
    new_f.sformals
end

let getCompinfo = function
  (* unwrap the struct *)
    TComp (ci, _) -> ci
  (* if it's not a struct, die. too bad. *)
  | _ -> assert false

let rec getNameOfExp = function
  Lval ((Var(vi),_))
  | AddrOf ((Var(vi),_))
  | StartOf ((Var(vi),_)) -> vi.vname
  | CastE (_, ex)
  | AlignOfE ex
  | SizeOfE ex -> getNameOfExp ex
  (* The following are not supported yet *)
  | Const _ -> raise (Invalid_argument "Const");
  | SizeOf _ -> raise (Invalid_argument "Sizeof");
  | SizeOfStr _ -> raise (Invalid_argument "SizeofStr");
  | AlignOf _ -> raise (Invalid_argument "Alignof");
  | UnOp _ -> raise (Invalid_argument "UnOp");
  | BinOp _ -> raise (Invalid_argument "BinOp");
  | _ -> raise (Invalid_argument "Uknown");


(******************************************************************************)
(*                                   LOOP                                     *)
(******************************************************************************)

exception CouldntGetLoopLower of stmt 
let get_loop_lower (s: stmt) (prevstmt: stmt) : exp =
  match (prevstmt).skind with
      Instr(il) -> begin
        match (L.hd (L.rev il)) with
          Set(_, e, _) -> e
        | _ -> raise (CouldntGetLoopLower s)
      end
    | _ -> raise (CouldntGetLoopLower s)

exception CouldntGetLoopUpper of stmt 
let get_loop_condition (s: stmt) : exp =
  match s.skind with
      Loop(b, _, _, _) -> begin
        match (L.hd b.bstmts).skind with
            If(ec, _, _, _) -> ec
(*           | If(UnOp(LNot, ec, _), _, _, _) -> ec *)
          | _ -> raise (CouldntGetLoopUpper s)
      end
    | _ -> raise (CouldntGetLoopUpper s)

exception CouldntGetLoopSuccesor of stmt 
let get_loop_successor (s: stmt) : exp =
  match s.skind with
      Loop(b, _, _, _) -> begin
        let lstmt = L.hd (L.rev b.bstmts) in
          match lstmt.skind with
            Instr(il) -> begin
              match (L.hd (L.rev il)) with
                Set(_, e, _) -> e
              | _ -> raise (CouldntGetLoopSuccesor s)
            end
          | _ -> raise (CouldntGetLoopSuccesor s)
      end
    | _ -> raise (CouldntGetLoopSuccesor s)


(******************************************************************************)
(*                          Constructors                                      *)
(******************************************************************************)

let mkFieldAccess lv fieldname =
  let lvt = Cil.typeOfLval lv in
  let ci = getCompinfo (unrollType lvt) in
  let field = getCompField ci fieldname in
  addOffsetLval (Field (field, NoOffset)) lv

let mkPtrFieldAccess lv fieldname =
  let lvt = Cil.typeOfLval lv in
  (* get the type *)
  let lvtf = match lvt with
    | TPtr(ty, _) -> ty
    | _ -> assert false
  in
  let ci = getCompinfo (unrollType lvtf) in
  let field = getCompField ci fieldname in
  addOffsetLval (Field (field, NoOffset)) (mkMem (Lval lv) NoOffset)

(* Defines the Task_table for the spu file *)
let make_task_table (tasks : (fundec * varinfo * (int * arg_descr) list) list) : global = (
  let etype = TPtr( TFun(TVoid([]), None, false, []), []) in
  let type' = TArray (etype, None, []) in
  let vi = makeGlobalVar "Task_table" type' in
  let n = L.length tasks in
  let init' = 
      let rec loopElems acc i =
        if i < 0 then acc
        else (
          let (_, task_vi, _) = L.nth tasks i in
          loopElems ( (Index(integer i, NoOffset), SingleInit(Lval (Var(task_vi), NoOffset))) :: acc) (i - 1)
        )
      in
      CompoundInit(etype, loopElems [] (n - 1))
  in
  let ii = {init = Some init'} in
  GVar(vi, ii, locUnknown)
)

(* Defines the Task_table for the ppu file *)
let make_null_task_table (tasks : (fundec * varinfo * (int * arg_descr) list) list) : global = (
  let etype = TPtr( TFun(TVoid([]), None, false, []), []) in
  let type' = TArray (etype, None, []) in
  let vi = makeGlobalVar "Task_table" type' in
  let n = L.length tasks in
  let init' = 
      let rec loopElems acc i = 
        if i < 0 then acc
        else loopElems ((Index(integer i, NoOffset), makeZeroInit etype) :: acc) (i - 1) 
      in
      CompoundInit(etype, loopElems [] (n - 1))
  in
  let ii = {init = Some init'} in
  GVar(vi, ii, locUnknown)
)


(******************************************************************************)
(*                         AttrParam to Expression                            *)
(******************************************************************************)

(* Convert an attribute into an expression, if possible. Otherwise raise 
 * NotAnExpression *)
exception NotAnExpression of attrparam
let attrParamToExp (a: attrparam) ?(currFunction: fundec = !currentFunction) (ppc_file: file) : exp= 
  let rec subAttr2Exp (a: attrparam) : exp= begin
    match a with
        AInt(i) -> integer i                    (** An integer constant *)
      | AStr(s) -> Const(CStr s)                (** A string constant *)
      | ACons(name, []) ->                      (** An id *)
        Lval (Var (find_scoped_var currFunction ppc_file name) , NoOffset)
      (* We don't support function calls as argument size *)
      (*| ACons(name, args) ->                    (** A function call *)*)
      | ASizeOf(t) -> SizeOf t                  (** A way to talk about types *)
      | ASizeOfE(a) -> SizeOfE (subAttr2Exp a)
      | AAlignOf(t) -> AlignOf t
      | AAlignOfE(a) -> AlignOfE (subAttr2Exp a)
      | AUnOp(op, a) -> UnOp(op, subAttr2Exp a, intType) (* how would i know what type to put? *)
      | ABinOp(op, a, b) -> BinOp(op, subAttr2Exp a,
                                    subAttr2Exp b, intType) (* same as above *)
      | ADot(a, s) -> begin                    (** a.foo **)
        let predot = subAttr2Exp a in
        match predot with Lval(v) ->
            Lval (mkFieldAccess v s)
          | _ -> raise (NotAnExpression a)
      end
      | AStar(a) -> Lval(mkMem (subAttr2Exp a) NoOffset) (** * a *)
      | AAddrOf(a) -> begin                                 (** & a **)
        let ar = subAttr2Exp a in
        match ar with Lval(v) ->
            mkAddrOf v
          | _ -> raise (NotAnExpression a)
      end
      | AIndex(a, i) -> begin                               (** a1[a2] *)
        let arr = subAttr2Exp a in
        match arr with Lval(v) ->
            Lval(addOffsetLval (Index(subAttr2Exp i, NoOffset)) v)
          | _ -> raise (NotAnExpression a)
      end
      (* not supported *)
  (*    | AQuestion of attrparam * attrparam * attrparam (** a1 ? a2 : a3 **)*)
      | _ -> raise (NotAnExpression a)
  end in
  subAttr2Exp a


(******************************************************************************)
(*                             FILE handling                                  *)
(******************************************************************************)

(* write an AST (list of globals) into a file *)
let writeNewFile f fname globals = begin
  let file = { f with
    fileName = fname;
    globals = globals;
  } in
  let oc = open_out fname in
  Rmtmps.removeUnusedTemps file;
  dumpFile defaultCilPrinter oc fname file;
  close_out oc
end

(* write out file {e f} *)
let writeFile f = begin
  let oc = open_out f.fileName in
  Rmtmps.removeUnusedTemps f;
  dumpFile defaultCilPrinter oc f.fileName f;
  close_out oc
end


(******************************************************************************)
(*                                BOOLEAN                                     *)
(******************************************************************************)

(* function that checks if an exp uses an indice *)
let uses_indice (e: exp) : bool =
  match (e) with 
    (BinOp(PlusPI, _, _, _))
    | (BinOp(IndexPI, _, _, _))
    | (Lval(_, Index(_, _))) -> true
    | _ -> false

(* check if an arguments type is stride *)
let is_strided (arg: arg_t) : bool =
   match arg with
    | SIn
    | SOut
    | SInOut -> true
    | _ -> false

(* check if an arguments type is out *)
let is_out_arg (arg: arg_t) : bool =
   match arg with
    | Out
    | InOut
    | SOut
    | SInOut -> true
    | _ -> false

(* function that checks if a stmt is tagged with a #pragma tpc... *)
let tpc_call_with_arrray (st: stmt) : bool =
  if (st.pragmas <> []) then begin
    match (L.hd st.pragmas) with
      (Attr("css", _), _) -> begin
        match (st.skind)  with
          Instr(Call(_, _, args, _)::_) -> L.exists uses_indice args
          | _ -> false
      end
      | _ -> false
  end else
    false

(* Checks if {e g} is *not* the function declaration of "main"  *)
let isNotMain (g: global) : bool = match g with
    GFun({svar = vi}, _) when (vi.vname = "main") -> false
  | _ -> true

(* Checks if {e g} is *not* the function declaration of "tpc_call_tpcAD65"  *)
let isNotSkeleton (g: global) : bool = match g with
    GFun({svar = vi}, _) when (vi.vname = "tpc_call_tpcAD65") -> false
  | _ -> true

(* Checks if {e g} is a typedef, enum, struct or union *)
let is_typedef (g: global) : bool = match g with
    GType(_, _)
  | GCompTag(_, _)
  | GCompTagDecl(_, _)
  | GEnumTag(_, _)
  | GEnumTagDecl(_, _) -> true
  | GVarDecl(vi, _) when vi.vstorage=Extern -> true
  | _ -> false

let isCompType = function
    TComp _ -> true
  | _ -> false

let isScalar (vi: varinfo) =  match vi.vtype with
    TVoid _
  | TPtr _
  | TArray _
  | TFun _ -> false
  | _ -> true
  

(******************************************************************************)
(*                                 MISC                                       *)
(******************************************************************************)

class changeStmtVisitor (s: stmt) (name: string) (stl: stmt list) : cilVisitor =
  object (self)
    inherit nopCilVisitor
    method vstmt (s: stmt) = match s.skind with
      (*Instr(Call(_, Lval((Var(vi), _)), _, _)::res) when vi.vname = name ->
        ChangeTo (mkStmt (Block (mkBlock (stl@[mkStmt (Instr(res))]))))*)
      Instr(instrs) ->
        let first = ref [] in
        let second = ref [] in
        let found = ref false in
        let iter_fun = (
          fun i -> match i with
            Call(_, Lval((Var(vi), _)), _, _) when vi.vname = name -> found := true;
          | _ -> if (!found) then (second := i::!second) else (first := i::!first);
        ) in
        L.iter iter_fun instrs;
        if (!found = false) then
          DoChildren
        else
          ChangeTo (mkStmt (Block (mkBlock (mkStmt (Instr(L.rev !first))::stl@[mkStmt (Instr(L.rev !second))]))))
    | _ -> DoChildren
  end

let replace_fake_call_with_stmt (s: stmt) (fake: string) (stl: stmt list) =
  let v = new changeStmtVisitor s fake stl in
  visitCilStmt v s

(** Comparator for use with [List.sort] *)
let comparator (a: (int * exp)) (b: (int * exp)) : int =
  let (a_i, _) = a in
  let (b_i, _) = b in
  if (a_i = b_i) then 0
  else if (a_i > b_i) then 1
  else (-1)


(** Comparator for use with [List.sort], 
    takes an arg_descr list and sorts it according to the type of the arguments
    input @ inout @ output *)
let sort_args (a: arg_descr) (b: arg_descr) : int = 
  let (_, (arg_typa, _, _, _)) = a in
  let (_, (arg_typb, _, _, _)) = b in
    (* if they are equal *)
    if (arg_typa = arg_typb) then 0
    (* if a is Out *)
    else if (arg_typa = Out || arg_typa = SOut) then 1
    (* if b is Out *)
    else if (arg_typb = Out || arg_typb = SOut) then (-1)
    (* if neither are Out and a is In *)
    else if (arg_typa = In || arg_typa = SIn) then (-1)
    else 1

(** assigns to each argument description its place in the original
          argument list *)
let number_args (args: arg_descr list) (oargs: exp list) : (int*arg_descr) list =
  L.map (fun arg ->
      let (name, _) = arg in
      let i = ref 0 in
      ignore(L.exists (fun e ->
        let ename=getNameOfExp e in
        if (ename=name) then
          true
        else (
          incr i;
          false
        )
      ) oargs);
      (!i, arg)
  ) args

(* Preprocess the header file <header> and merges it with f.  The
 * given header should be in the gcc include path.  Modifies f
 *) (* the original can be found in lockpick.ml *)
let preprocessAndMergeWithHeader_cell (f: file) (header: string) (def: string)
    (arch: string) (incPath: string) : unit = (
  (* //Defining _GNU_SOURCE to fix "undefined reference to `__isoc99_sscanf'" *)
  ignore (Sys.command ("echo | ppu32-gcc -E "^def^" -I"^incPath^"/ppu -I"^incPath^"/spu "^header^" - >/tmp/_cil_rewritten_tmp.h"));
  let add_h = Frontc.parse "/tmp/_cil_rewritten_tmp.h" () in
  let f' = Mergecil.merge [add_h; f] "stdout" in
  f.globals <- f'.globals;
)