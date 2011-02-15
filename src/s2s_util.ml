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


open Cil
module E = Errormsg
module H = Hashtbl
module L = List
module CG = Callgraph


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

and arg_descr = (string * (arg_t * exp * exp * exp))

(******************************************************************************)
(*                          Globals                                           *)
(******************************************************************************)

(* define the ppu_vector *)
let ppu_vector = Attr("altivec", [ACons("vector__", [])])

let voidType = TVoid([])
let intType = TInt(IInt,[])
let uintType = TInt(IUInt,[])
let longType = TInt(ILong,[])
let ulongType = TInt(IULong,[])
let charType = TInt(IChar, [])
let boolType = TInt(IBool, [])


(******************************************************************************)
(*                          Search Functions                                  *)
(******************************************************************************)


(* searches a global list for a function definition with name <name> *)
exception Found_fundec of fundec
let find_function_fundec_g (g: global list) (name: string) : fundec =
  let findit = function
    | GFun(fd, _) when fd.svar.vname = name -> raise (Found_fundec fd)
    | _ -> ()
  in
  try
    List.iter findit g;
    raise Not_found
  with Found_fundec v -> v

(* find the function definition of variable <name> in file f *)
let find_function_fundec (f: file) (name: string) : fundec =
  find_function_fundec_g f.globals name

(* find the function signature for <name> function *)
exception Found_sign of varinfo
let find_function_sign (f: file) (name: string) : varinfo =
  let findit = function
    | GVarDecl(vi, _) when vi.vname = name -> raise (Found_sign vi)
    | _ -> ()
  in
  try
    iterGlobals f findit;
    raise Not_found
  with Found_sign v -> v

(* find the (first) typedef for type "name" in file f *)
exception Found_type of typ
let find_type (f: file) (name: string) : typ =
  let findit = function
    | GType(ti, _) when ti.tname = name -> raise (Found_type (TNamed(ti, [])))
    | _ -> ()
  in
  try
    iterGlobals f findit;
    raise Not_found
  with Found_type t -> t

(* find the struct or union named struct/union <name> *)
let find_tcomp (f: file) (name: string) : typ =
  let findit = function
    | GCompTag(ci, _) when ci.cname = name -> raise (Found_type (TComp(ci, [])))
    | _ -> ()
  in
  try
    iterGlobals f findit;
    raise Not_found
  with Found_type t -> t

(* find the variable named <name> in file <f> *)
exception Found_var of varinfo
let find_global_var (f: file) (name: string) : varinfo =
  let findit = function
    | GVarDecl(vi, _) when vi.vname = name -> raise (Found_var vi)
    | GVar(vi, _, _) when vi.vname = name -> raise (Found_var vi)
    | _ -> ()
  in
  try
    iterGlobals f findit;
    ignore(E.warn  "\"%s\" is not globally defined in %s\n" name f.fileName);
    raise Not_found
  with Found_var v -> v

(* find the variable named <name> in the formals of <fd> *)
let find_formal_var (fd: fundec) (name: string) : varinfo =
  let findit = function
    | vi when vi.vname = name -> raise (Found_var vi)
    | _ -> ()
  in
  try
    List.iter findit fd.sformals;
    ignore(E.warn  "\"%s\" is not a formal of %s\n" name fd.svar.vname);
    raise Not_found
  with Found_var v -> v

(* find the variable named <name> in the locals of <fd> *)
let find_local_var (fd: fundec) (name: string) : varinfo =
  let findit = function
    | vi when vi.vname = name -> raise (Found_var vi)
    | _ -> ()
  in
  try
    List.iter findit fd.slocals;
    ignore(E.warn "\"%s\" is not a local of %s" name fd.svar.vname);
    raise Not_found
  with Found_var v -> v

(* find the variable named <name> in fundec <fd>
   else look if it's a global of file <f> *)
let find_scoped_var (fd: fundec) (f: file) (name: string) : varinfo =
  try
    find_local_var fd name
  with Not_found -> 
      ( try
        ignore(E.warn "Now looking in formals");
        find_formal_var fd name
      with Not_found -> 
          ( try
            ignore(E.warn "Now looking in globals");
            find_global_var f name
          with Not_found -> (
              ignore(E.warn "\"%s\" is not accessible from %s" name fd.svar.vname);
              raise Not_found)
          )
      )

(* find the enum named <name> in file f *)
exception Found_enum of enuminfo
let find_enum (f: file) (name: string) : enuminfo =
  let findit = function
    | GEnumTag(ei, _) when ei.ename = name -> raise (Found_enum ei)
    | _ -> ()
  in 
  try
    iterGlobals f findit;
    raise Not_found
  with Found_enum ei -> ei

(* find the variable named <name> in fd's locals *)
let findLocal (fd: fundec) (name: string) : varinfo =
  let findit = function
    | vi when vi.vname = name -> raise (Found_var vi)
    | _ -> ()
  in
  try
    List.iter findit fd.slocals;
    ignore(E.error "\"%s\" is not a local of %s" name fd.svar.vname);
    raise Not_found
  with Found_var v -> v



(******************************************************************************)
(*                                Converters                                  *)
(******************************************************************************)


(* Converts the strings describing the argument type to arg_t *)
let translate_arg (arg: string) (strided: bool) : arg_t =
  if (strided) then
    match arg with
        "in" -> SIn
      | "out" -> SOut
      | "inout" -> SInOut
      | _ -> ignore(E.error "Only in/out/inout are allowed"); assert false
  else
    match arg with
        "in" (* legacy *)
      | "input" -> In
      | "out" (* legacy *)
      | "output" -> Out
      | "inout" -> InOut
      | _ -> ignore(E.error "Only input/output/inout are allowed"); assert false


let arg_t2integer = function
    | In -> integer 1
    | Out -> integer 2
    | InOut -> integer 3
    | SIn -> integer 5
    | SOut -> integer 6
    | SInOut -> integer 7

let arg_t2int = function
    | In -> 1
    | Out -> 2
    | InOut -> 3
    | SIn -> 5
    | SOut -> 6
    | SInOut -> 7


(******************************************************************************)
(*                         Copy Function                                      *)
(******************************************************************************)

(* recursively copies a function definition and all it's callees
   from the ppc_file to the spu_file *)
let rec deep_copy_function (task: string) (callgraph: CG.callgraph)
      (spu_file: file) (ppc_file: file)= begin
    (* TODO copy globals? *)

    (* First copy the Callees *)
    let cnode: CG.callnode = H.find callgraph task in
    let nodeName (n: CG.nodeinfo) : string =
      match n with
        CG.NIVar (v, _) -> v.vname
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
      let new_fd = GFun(find_function_fundec (ppc_file) task, locUnknown) in
      spu_file.globals <- spu_file.globals@[new_fd];
      (* if not check whether we have a signature *)
    with Not_found -> ignore(find_function_sign (ppc_file) task);
end

(******************************************************************************)
(*                               GETTERS                                      *)
(******************************************************************************)

(* returns the argument type from an argument description 
  (string * arg_t * string * string *string ) *)
let get_arg_type (arg: (string * (arg_t * exp * exp * exp ))) : arg_t =
  match arg with
    (_, (arg_type ,_ ,_ ,_)) -> arg_type

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


(******************************************************************************)
(*                         AttrParam to Expression                            *)
(******************************************************************************)

(* Convert an attribute into an expression, if possible. Otherwise raise 
 * NotAnExpression *)
exception NotAnExpression of attrparam
let attrParamToExp (a: attrparam) (currentFunction: fundec) (ppc_file: file) : exp= 
  let rec subAttr2Exp (a: attrparam) : exp= begin
    match a with
        AInt(i) -> integer i                    (** An integer constant *)
      | AStr(s) -> Const(CStr s)                (** A string constant *)
      | ACons(name, []) ->                      (** An id *)
        Lval (Var (find_scoped_var currentFunction ppc_file name) , NoOffset)
      (* We don't support function calls as argument size *)
      (*| ACons(name, args) ->                    (** A function call *)
        let args' = L.map (fun a -> attrParamToExp a) args in
        let instr = Call (None, Lval (var find_function_sign ppc_file name), args', locUnknown) in
        let call = mkStmtOneInstr instr in
        Lval (var (find_scoped_var !currentFunction ppc_file name) , NoOffset)*)
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

(* write out file <f> *)
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

(* Checks if <g> is *not* the function declaration of "main"  *)
let isNotMain (g: global) : bool = match g with
    GFun({svar = vi}, _) when (vi.vname = "main") -> false
  | _ -> true

(* Checks if <g> is *not* the function declaration of "tpc_call_tpcAD65"  *)
let isNotSkeleton (g: global) : bool = match g with
    GFun({svar = vi}, _) when (vi.vname = "tpc_call_tpcAD65") -> false
  | _ -> true

(* Checks if <g> is a typedef, enum, struct or union *)
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