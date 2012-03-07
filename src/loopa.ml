open Cil
open Printf
open Lockutil
open Barrierstate
open Sdam
open Int64
module E = Errormsg
module RD = Reachingdefs

let debug = ref false

let options = [
  "--debug-loopa",
  Arg.Set(debug),
  "SDAM-Simple loop analysis: debugging output.";
]

(*let eval_step = (function*)
(*		Const(CInt64(v, _, _)) -> ignore(E.log "step is an integer constant (%Ld)\n" v);*)
(*	| Const(CChr(_)) -> ignore(E.log "step is a character constant\n");*)
(*	| Const(CReal(_)) -> ignore(E.log "step is a float constant\n");*)
(*	| Const(CEnum(_)) -> ignore(E.log "step is an enumeration\n");*)
(*	| _ -> ignore(E.log "SDAM: Loop step too compilcated...\n");*)
(* ) *)

(** analyzes the instruction in the body of the loop and returns the index variable
		and step expression.  If the analysis fails then it return None. If the loop index
		is referenced or modified  then the analysis  fails
		@param body_stmts the list of statements in the loop body
		@return Some loop descriptor for the current loop, or None if index is modified 
			or did not match pattern
*)
let getLoopIndex (body_stmts: stmt list) : loop_descr option = 
	let last_stmt = (List.hd (List.rev body_stmts)) in 
	let rec get_loop_index' il = (
		match il with 
			Instr(Set((Var(vi), NoOffset), e, _)::[]) -> ( 
				if !debug then ignore(E.log "Found index %s\n" vi.vname);
				if(vi.vreferenced || vi.vaddrof) then None
				else (
					let rec isModified = (function
							[] -> false
						| (stmt::rest) -> (
							let rec isModified' il = (
								match il with 
								Instr([]) -> false
							| Instr(_::[]) -> false (* last stmt should be ignored *)
							|	Instr(Set((Var(vi'), NoOffset), _, _)::_) when vi.vname == vi'.vname -> true
							| Instr(_::rest) -> isModified' (Instr rest) 
							| _ -> false 
							) 
							in
							if !debug then ignore(E.log "Checking stmt:%a\n" d_stmt stmt);		 
							if(isModified' stmt.skind) then (
								if !debug then ignore(E.log "il is modified\n"); 
								true
							)
							else isModified	rest
						)
					)
					in			
					if(isModified body_stmts) then None
					else Some (make_loop_descr vi e) 
				)				
			)
		| Instr(_::[]) -> if !debug then ignore(E.log "Loop pattern did not match\n"); None
		|	Instr(_::rest) -> get_loop_index' (Instr rest)
		| _ -> if !debug then ignore(E.log "Loop pattern did not match\n"); None
	) in get_loop_index' last_stmt.skind

(** find reaching defs, and return the array descriptor if argument is or refers
		to an array variable.  Return None if argument is not such a variable or
		there are more than one reaching definitions
		@param loc the location of the arument in the file
		@param currSid the current sid used by reaching_defs module
		@return the array descriptor of the array tha reaches the argument variable or None
		 if variable does not reach an array or has multiple reaching definitions a the given
		 program point
*)
let getArrayDescr (vi: varinfo) (loc: location) currSid : array_descr option = 
	try (
		let reaching_defs = RD.getRDs currSid in	
		let (_, _, iosh) = getSome reaching_defs in
		let ios =	(Inthash.find iosh vi.vid) in 
		if !debug then (
		  ignore(E.log "argument:%s-%d\n" vi.vname vi.vid);	
		  (*  ignore(E.log "RDs table_size=%d\n" (Inthash.length iosh));*)
		  (*  Inthash.iter (fun a b -> ignore(E.log "vi:%d\n" a);) iosh;*)
		);
		if ((RD.IOS.cardinal ios) <= 1) then (
			let elmt = RD.IOS.choose ios in (* there should be only one element *)
		 	(match elmt with
		      Some i ->  if not(isSome (RD.getSimpRhs i)) then None else
					(
						let r = getSome (RD.getSimpRhs i) in
						(match r with
							RD.RDExp e -> if !debug then (ignore(E.log "Argument %s reaches expr:%a\n" vi.vname d_exp e););
							(match e with 
								AddrOf(Var(va), Index(index, NoOffset)) -> Some (make_array_descr va index)
							|	BinOp(IndexPI, Lval(Var(va), NoOffset), index, _) -> Some (make_array_descr va index)
							|	BinOp(PlusPI, Lval(Var(va), NoOffset), index, _) -> Some (make_array_descr va index)
							| _ -> (
								if !debug then ignore(E.log "Argument %s does not reach an array, no analysis possible\n" vi.vname);
								None	
								) 
							) 
						| RD.RDCall i -> (
							if !debug then ignore(E.log "Argument %s reaches a function call, no analysis possible\n" vi.vname); 
							None
							)
						)
					)
		    | None -> (
		    	if !debug then ignore(E.log "No reaching definitions\n"); 
		    	None
		    )
      )
		)
		else (
			if !debug then ignore(E.log "More than one reaching defs\n"); (* TODO:check if all paths are monotonal instead *)
			None
		)
	) 
	with Not_found -> (
		(* zakkak workaround *)
		if !debug then ignore(E.log "Inthash.find failed\n");
		None
	)
									
(** returns the expression by which the loop index is increased/decreased
		@param loop_d the descriptor of the loop whose step we seek
		@return the expression by which the loop index is increased/decreased
		@exception failure if loop index expression is too complicated for the analysis to handle
*)
let get_loop_step (loop_d: loop_descr) : exp = 
	if !debug then ignore(E.log "Index is %s with exp:%a\n" loop_d.l_index_info.vname d_exp loop_d.l_index_exp);
	let vi = loop_d.l_index_info in
	match loop_d.l_index_exp with
		BinOp(PlusA, Lval(Var(vi'), _), step, _) when (vi' = vi) -> ( 
			if !debug then ignore(E.log "Loop step increases by %a\n" d_exp step);
			step 
		) 
	| BinOp(MinusA, Lval(Var(vi'), _), step, _) when (vi' = vi) -> (
			if !debug then ignore(E.log "Loop step decreases by %a\n" d_exp step);
			step
		)
	| CastE(_, BinOp(PlusA, CastE(_, Lval(Var(vi'), _)), step, _)) when vi == vi' -> (
			if !debug then ignore(E.log "Loop step increases by %a\n" d_exp step);
			step
		)
	| CastE(_, BinOp(MinusA, CastE(_, Lval(Var(vi'), _)), step, _)) when vi == vi' -> (
			if !debug then ignore(E.log "Loop step decreases by %a\n" d_exp step);
			step
		)
	| _ -> if !debug then ignore(E.log "Loop index too compilcated...\n"); raise (Failure "Loop index too compilcated...\n")

(** returns the varinfo of the array index variable
		@param array_d the array descriptor whose index we seek
		@return the varinfo of the array index variables
		@exception failure when index is too complicated for the analysis to handle 
*)
let get_array_index_info (array_d: array_descr) : varinfo =
		match  array_d.a_index_exp with
			Lval(Var(vi), NoOffset) -> vi
		|	_ -> if !debug then ignore(E.log "Array index too complicated\n"); raise (Failure "Index too complicated\n")

(** checks if lv is a variable
			@param lv the lhost of an lvalue
			@return true if lv is a variable, else false
*)
let isVar (lv: lhost) : bool =
	match lv with 
		Var(_) -> true
	| _ -> false

(** returns the varinfo of lv
			@param lv the lhost of an lvalue
			@return the varinfo of lv
			@exception raise failure if lv is not a variable
*)	
let getVar (lv: lhost) : varinfo =
	match lv with 
		Var(vi) -> vi
	| _ -> raise (Failure "")
		
(** comares the sizes of the the task argument consumed by the task with the
		loop step.
		@param vi the variable info of the task argument
		@param arg_e_size the size epxression of the argument
		@param l_step the step expression of the loop
		@return true is sizes are equal, false otherwize
*)
let size_equal vi arg_e_size l_step = 
	let e_size = arg_e_size in
	if !debug then ignore(E.log "Comparing e_size:%a and l_step:%a\n" d_exp e_size d_exp l_step);
	if e_size == l_step then true
	else (
		(* if sizes differ, but l_step is a const, then check if e_size == l_size*sizeof(elmnt) *)
		match l_step with
				SizeOf(t) -> (
					match e_size with 
						SizeOf(t') when t == t' -> true
					|	_ -> false
				)
			| Const(CInt64(1L, _, _)) -> (
				if !debug then ignore(E.log "matched Const(1) with %a\n" d_exp l_step); 
				match e_size with
					SizeOf(t) -> (
						if !debug then ignore(E.log "SizeOf(t)->... vtype=%a-type=%a\n" d_type vi.vtype d_type t);
						match vi.vtype with 
							TPtr(t', _) when t == t' -> true
						| TArray(t', _, _) when t == t' -> true
						| _ when t == vi.vtype -> true  
						| _ -> false
					)
				| Const(c) -> false
				|	_ -> false 
				)
			| Const(CInt64(v, _, _)) -> (
					match e_size with
					BinOp(Mult, Const(CInt64(v', _, _)), SizeOf(t), res_t) -> (
						if !debug then ignore(E.log "const*sizeof(x) | e:%a - e':%a\n" d_exp l_step d_exp e_size);
						match vi.vtype with 
							TPtr(t', _) when ((compare v v') == 0 && t == t') -> true
						| _ when ((compare v v') == 0 && t == vi.vtype) -> true  
						| _ -> false
					) 
				|	_ -> false
			)
			| Lval(lh, _) -> (
					match e_size with
					BinOp(Mult, Lval(lh', _), SizeOf(t), res_t) -> (
						if(not (isVar lh) || not (isVar lh')) then false
						else (
							let li = getVar lh in
							let li' = getVar lh' in
							if(li.vaddrof || li'.vaddrof) then false
							else (
								if !debug then ignore(E.log "lval*sizeof(x) | e:%a - e':%a\n" d_exp l_step d_exp e_size);
								match vi.vtype with 
									TPtr(t', _) when (li  == li'  && t == t') -> true
								| _ when (li  == li' && t == vi.vtype) -> true  
								| _ -> false
							)
						)
					) 
					| _ -> false
			)
		| _ -> false 
	)

(**	checks if an argument that refers to an array is self dependent by analyzing 
		the array's and loop's indexes
		@param arg the descriptor of the argument that we want to check
		@return true if argument is not self dependent
*)
let array_bounds_safe (arg: arg_descr) : bool =
	(* cannot handle strided args *)
	if arg.strided then false
	else (
		try (
			if (not (isSome arg.loop_d)) then raise (Failure "Loop cannot be analyzed\n");		
			if (not (isSome arg.array_d)) then raise (Failure "Argument does is not or does not refer to an array\n");
			let loop_d = getSome arg.loop_d in
			let array_d = getSome arg.array_d in
			(* 1. check if array index is more complicated than *[i] *)
			(* 2. check if array index matches loop index *)
			let a_index_var = get_array_index_info array_d in
			let l_index_var = loop_d.l_index_info in
			if (a_index_var.vname == l_index_var.vname) then (
				if !debug then ignore(E.log "Loop index matches array index\n");
				(* 3. check if loop step matches array element size *)
				let loop_step = get_loop_step loop_d in
				if(size_equal arg.arginfo arg.argsize loop_step) then (
					if !debug then ignore(E.log "Array bounds safe\n");
					true
				)
				else (
					if !debug then ignore(E.log "Array bounds not safe\n");
					false
			 	) 
			)
			else
				false
		)
		with _ -> (
			if !debug then ignore(E.log "Array bounds analysis inconclusive\n"); 
			false
		)
	)

