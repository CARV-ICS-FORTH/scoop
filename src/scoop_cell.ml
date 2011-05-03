(*
 *
 * Copyright (c) 2010, 
 *  Foivos Zakkak        <zakkak@ics.forth.gr>
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
open Scoop_util
module E = Errormsg
module L = List

(* keeps the current funcid for the new tpc_function *)
let func_id = ref 0

let doArgument (i: int) (local_arg: lval) (avail_task: lval) (tmpvec: lval) (fd: fundec)
 (arg: arg_descr) (stats: bool) (spu_file: file): instr list = (
  let arg_size = Lval( var (find_formal_var fd ("arg_size"^(string_of_int i)))) in
  let actual_arg = List.nth fd.sformals i in
  let arg_addr = (
    if (isScalar actual_arg) then
      AddrOf( var actual_arg)
    else
      Lval( var actual_arg)
  ) in
  let arg_type = get_arg_type arg in
  let il = ref [] in
  (* tmpvec = (volatile vector unsigned char * )&avail_task->arguments[i]; *)
  if (stats) then (
    let total_bytes = var (find_local_var fd "total_bytes") in
(*     let arg_bytes = var (find_local_var fd "arg_bytes") in *)
    let arg_bytes =
      if (is_strided arg_type) then (
        let arg_elsz = Lval( var (find_formal_var fd ("arg_elsz"^(string_of_int i)))) in
        let arg_els = Lval( var (find_formal_var fd ("arg_els"^(string_of_int i)))) in
        (* arg_bytes = TPC_EXTRACT_STRIDEARG_ELEMSZ(arg_size)*TPC_EXTRACT_STRIDEARG_ELEMS(arg_size); *)
        BinOp(Mult, arg_els, arg_elsz, intType)
      ) else (
        (* arg_bytes = arg_size; *)
        arg_size
      )
    in
    (* total_bytes += ( arg_bytes<< TPC_IS_INOUTARG(arg_flag)); *)
    let total_size = 
      if (is_out_arg arg_type) then (
        BinOp(PlusA, Lval(total_bytes), BinOp(Mult, integer 2, arg_bytes, intType), intType)
      ) else (
        BinOp(PlusA, Lval(total_bytes), arg_bytes, intType)
      )
    in
    il := Set(total_bytes, total_size, locUnknown)::!il
  );
  let vector_uchar_p = TPtr(TInt(IUChar, [Attr("volatile", [])]), [ppu_vector]) in
  let av_task_arg = mkPtrFieldAccess avail_task "arguments" in
  let av_task_arg_idx = addOffsetLval (Index(integer i,NoOffset)) av_task_arg in
  il := Set(tmpvec, CastE(vector_uchar_p, AddrOf(av_task_arg_idx)) , locUnknown)::!il;

  (*TODO: if !stats then
     if( TPC_IS_STRIDEARG(arg_flag) ) {
       arg_bytes = TPC_EXTRACT_STRIDEARG_ELEMSZ(arg_size)*TPC_EXTRACT_STRIDEARG_ELEMS(arg_size);
     } else {
       arg_bytes = arg_size;
     }
     total_bytes += ( arg_bytes<< TPC_IS_INOUTARG(arg_flag));*)

  (* local_arg.eal = (uint32_t)(arg_addr64); *)
  let eal = mkFieldAccess local_arg "eal" in
  il := Set(eal, CastE(find_type spu_file "uint32_t", arg_addr), locUnknown)::!il;
  let size = mkFieldAccess local_arg "size" in
  if (is_strided arg_type) then (
    let arg_elsz = Lval( var (find_formal_var fd ("arg_elsz"^(string_of_int i)))) in
    let arg_els = Lval( var (find_formal_var fd ("arg_els"^(string_of_int i)))) in
    (* #define TPC_BUILD_STRIDEARG(elems, elemsz)    (((elems)<<16U) | (elemsz)) *)
    (* local_arg.size = TPC_BUILD_STRIDEARG(els,elsz); *)
    let build_stride = BinOp(BOr, BinOp(Shiftlt, arg_els, (integer 16), intType), arg_elsz, intType) in
    il := Set(size, build_stride, locUnknown)::!il;
    (* local_arg.stride = arg_size; *)
    let stride = mkFieldAccess local_arg "stride" in
    il := Set(stride, arg_size, locUnknown)::!il;
  ) else
    (* local_arg.size = arg_size; *)
    il := Set(size, arg_size, locUnknown)::!il;
  (* local_arg.flag = arg_flag; *)
  let flag = mkFieldAccess local_arg "flag" in
  il:= Set(flag, arg_t2integer arg_type, locUnknown)::!il;
  (* *tmpvec = *((volatile vector unsigned char * )&local_arg); *)
  let casted_la = CastE(vector_uchar_p, AddrOf(local_arg)) in
  il := Set(mkMem (Lval(tmpvec)) NoOffset, Lval(mkMem casted_la NoOffset), locUnknown)::!il;
  !il
)

(* make a tpc_ version of the function (for use on the ppc side)
 * uses the tpc_call_tpcAD65 from tpc_skeleton_tpc.c as a template
 *)
let make_tpc_func (func_vi: varinfo) (oargs: exp list)
    (args: (string * (arg_t * exp * exp * exp )) list)
    (f: file ref) (spu_file: file ref) : fundec = begin
  print_endline ("Creating tpc_function_" ^ func_vi.vname);
  let skeleton = Scoop_util.find_function_fundec (!f) "tpc_call_tpcAD65" in
  let f_new = copyFunction skeleton ("tpc_function_" ^ func_vi.vname) in
  f_new.sformals <- [];
  (* set the formals to much the original function's arguments *)
  setFunctionTypeMakeFormals f_new func_vi.vtype;
  setFunctionReturnType f_new intType;
  (* create the arg_size*[, arg_elsz*, arg_els*] formals *)
  let args_num = (List.length f_new.sformals)-1 in
  if ( args_num > (List.length args) ) then (
    ignore(E.error "Number of arguments described in #pragma doesn't much the\
          number of arguments in the function declaration");
    assert false
  );
  for i = 0 to args_num do
    let (_, (arg_type, _, _, _)) = List.nth args i in
    ignore(makeFormalVar f_new ("arg_size"^(string_of_int i)) intType);
    if (is_strided arg_type) then (
      ignore(makeFormalVar f_new ("arg_els"^(string_of_int i)) intType);
      ignore(makeFormalVar f_new ("arg_elsz"^(string_of_int i)) intType)
    );
  done;

  let avail_task = var (findLocal f_new "avail_task") in
  let instrs : instr list ref = ref [] in
  (* avail_task->funcid = (uint8_t)funcid; *)
  instrs := Set (mkPtrFieldAccess avail_task "funcid",
  CastE(find_type !spu_file "uint8_t", integer !func_id), locUnknown):: !instrs;
  (* avail_task->total_arguments = (uint8_t)arguments.size() *)
  instrs := Set (mkPtrFieldAccess avail_task "total_arguments",
  CastE(find_type !spu_file "uint8_t", integer (args_num+1)), locUnknown)::!instrs;
  
  (* if we have arguments *)
  if (f_new.sformals <> []) then (
    (* volatile vector unsigned char *tmpvec   where vector is __attribute__((altivec(vector__))) *)
    let vector_uchar_p = TPtr(TInt(IUChar, [Attr("volatile", [])]), [ppu_vector]) in
    let tmpvec = var (makeLocalVar f_new "tmpvec" vector_uchar_p) in
    (* struct tpc_arg_element local_arg *)
    let local_arg = var (makeLocalVar f_new "local_arg" (find_tcomp !spu_file "tpc_arg_element")) in
    for i = 0 to args_num do
      let ex_arg = (List.nth oargs i) in
      let name = getNameOfExp ex_arg in
      let arg = List.find ( fun (vname, _) -> if( vname = name) then true else false) args in
        (* local_arg <- argument description *)
        instrs := (doArgument i local_arg avail_task tmpvec f_new arg 
                    !Scoop_util.stats !spu_file )@(!instrs);
    done;
  );

  (* insert instrs before avail_task->active = ACTIVE;
    we place a Foo_32412312231() call just above avail_task->active = ACTIVE
    to achieve that for cell *)
  f_new.sbody.bstmts <- List.map (fun s -> Lockutil.replace_fake_call s "Foo_32412312231" (L.rev !instrs)) f_new.sbody.bstmts;

  incr func_id;
  f_new
end