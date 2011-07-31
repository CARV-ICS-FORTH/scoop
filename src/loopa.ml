open Cil
open Printf
open Lockutil
open Barrierstate
open Sdam
open Int64
module E = Errormsg
module RD = Reachingdefs

let debug = ref true

let aaaInfoHT : (arg_descr, array_loop_descr) Hashtbl.t = Hashtbl.create 100

let curr_index = ref None 

let addElmt arg ald = 
	let (argname, _) = arg in
	ignore(E.log "adding argument %s\n" argname);
(*	if(isSome lpi) then ignore(E.log "is indeed some\n");*)
	Hashtbl.add aaaInfoHT arg ald

let eval_step = (function
		Const(CInt64(v, _, _)) -> ignore(E.log "step is an integer constant (%Ld)\n" v);
	| Const(CChr(_)) -> ignore(E.log "step is a character constant\n");
	| Const(CReal(_)) -> ignore(E.log "step is a float constant\n");
	| Const(CEnum(_)) -> ignore(E.log "step is an enumeration\n");
	| _ -> ignore(E.log "SDAM: Loop step too compilcated...\n");
)

let get_loop_step vi e = 
begin
	if !debug then (
		ignore(E.log "loop index var is %s\n" vi.vname);
		ignore(E.log "BinOp: %a\n" d_exp e);
	);
	match e with
		BinOp(PlusA, Lval(Var(vi'), _), step, _) when (vi' = vi) -> ( 
			ignore(E.log "increase by %a\n" d_exp step); 
			curr_index := Some (vi, step)
		) 
	| BinOp(MinusA, Lval(Var(vi'), _), step, _) when (vi' = vi) -> (
			if !debug then ignore(E.log "decrease by %a\n" d_exp step);
			curr_index := Some (vi, step)		
		)
	| _ -> if !debug then ignore(E.log "SDAM: Loop index too compilcated...\n"); curr_index := None
end

let get_loop_index body_stmts =
	let last_stmt = (List.hd (List.rev body_stmts)) in 
	let rec get_loop_index' il = (
		match il with 
			Instr(Set((Var(vi), NoOffset), e, _)::[]) -> ( 
				(if !debug then ignore(E.log "Found index %s\n" vi.vname););
				if(vi.vreferenced) then None (* FIXME: this does not seem to work... *)
				else (
					let rec isModified = function
						[] -> false
					| (stmt::rest) -> (
						let rec isModified' il =
							match il with 
							Instr([]) -> false
						| Instr(_::[]) -> false (* last stmt should be ignored *)
						|	Instr(Set((Var(vi'), NoOffset), _, _)::_) when vi.vname == vi'.vname -> (
							ignore(E.log "why am i here? instr\n");
							true
						)
						| Instr(_::rest) -> isModified' (Instr rest) 
						| _ -> false
						in
						if !debug then ignore(E.log "Checking stmt:%a\n" d_stmt stmt);		 
						if(isModified' stmt.skind) then (ignore(E.log "il is modified\n"); true)
						else isModified	rest
					)
					in			
					if(isModified body_stmts) then None
					else Some(vi, e)
				)				
			)
		| Instr(_::[]) -> (if !debug then ignore(E.log "Loop pattern did not match\n");); None
		|	Instr(_::rest) -> get_loop_index' (Instr rest)
		| _ -> (if !debug then ignore(E.log "Loop pattern did not match\n");); None
	) in get_loop_index' last_stmt.skind

let process_call_actuals acts loc currSid task_d loop_d = begin
		List.iter (fun acts -> 
			match acts with
				Lval(Var(vi), _) -> (
					let reaching_defs = RD.getRDs currSid in	
					let (_, _, iosh) = getSome reaching_defs in
					let ios =	(Inthash.find iosh vi.vid) in 
					if !debug then (
						ignore(E.log "argument:%s-%d\n" vi.vname vi.vid);	
(*						ignore(E.log "RDs table_size=%d\n" (Inthash.length iosh));*)
(*						Inthash.iter (fun a b -> ignore(E.log "vi:%d\n" a);) iosh;*)
					);
					if ((RD.IOS.cardinal ios) <= 1) then (
						RD.IOS.iter (function
													Some i ->  let r = getSome (RD.getSimpRhs i) in
													(match r with
														RD.RDExp e -> if !debug then (ignore(E.log "Argument %s reaches expr:%a\n" vi.vname d_exp e););
															(match e with 
																AddrOf(Var(va), Index(index, NoOffset)) -> addElmt (vi.vname, task_d) ((va, index), loop_d);
															| _ -> if !debug then ignore(E.log "Argument %s does not reach an array, no analysis possible\n" vi.vname); 
															); 
													| RD.RDCall i -> if !debug then ignore(E.log "Argument %s reaches a function call, no analysis possible\n" vi.vname);
													);
												| None -> if !debug then ignore(E.log "No reaching definitions\n"); ) ios;
					)
					else (if !debug then ignore(E.log "More than one reaching defs\n"); ); (* TODO:check if all paths are monotonal instead *)
				);
			| _ -> ignore(warnLoc loc "SDAM:Actual expr too complicated, no analysis possible\n");
		) acts; 
end

(* get the variable name of array index *)
let get_array_index array_d : varinfo =
		let (_, index_expr) = array_d in
		match  index_expr with
			Lval(Var(vi), NoOffset) -> vi
		|	_ -> if !debug then ignore(E.log "Array index too complicated\n"); raise (Failure "Index too complicated\n")

let get_loop_step vi loop_i : exp = 
	let (_, e) = loop_i in
	if !debug then ignore(E.log "Index is %s with exp:%a\n" vi.vname d_exp e);
	(* we only handle index+1 and index-1 *)
	match e with
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
	
	
let get_array_elmt_size arg_elmnt_size : exp =
	let e_size = arg_elmnt_size in
	if !debug then ignore(E.log "array element size=%a\n" d_exp e_size);
	arg_elmnt_size

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
		| _ -> false 
	)

let array_bounds_safe arg1 arg2 : bool =
	(* only do this check if argument is compared to itself *)
	let (argname1, var_inf1, arg_elmnt_size, (id1, taskname1, scope1)) = arg1 in
	let (argname2, _, _, (id2, taskname2, _)) = arg2 in 
	if !debug then ignore(E.log "Checking array index of argument %s\n" argname1);
	if(argname1 != argname2 || id1 != id2 || taskname1 != taskname2) then (
		if !debug then ignore(E.log "Arg1 =/= Arg2\n");
		false (* TODO: maybe expand to deal with simple cases of diff arguments *)
	)
	else (
		try (
			let (array_d, loop_d) = Hashtbl.find aaaInfoHT (argname1, (id1, taskname1, scope1)) in
			if (not (isSome loop_d)) then (
				if !debug then ignore(E.log "Argument is not in a loop\n");
				true (* We return no self-dep, since argument is not in loop, only valid if we check with itself *)	
			)
			else (
				let (_, loop_index) = getSome loop_d in
				if(not (isSome loop_index)) then (
					if !debug then ignore(E.log "Loop index is modified\n");
					false		
				)
				else (				
					(* 1. check if array index is more complicated than *[i] *)
					(* 2. check if array index matches loop index *)
					if !debug then ignore(E.log "Loop index is not modified\n");
					let a_index = get_array_index array_d in
					let (l_index_var, _) = getSome loop_index in
					if (a_index.vname == l_index_var.vname) then (
						if !debug then ignore(E.log "Loop index matches array index\n");
						(* 3. check if loop step matches array element size *)
						let loop_step = get_loop_step l_index_var (getSome loop_index) in
						let array_elmt_size = get_array_elmt_size arg_elmnt_size in
						if(size_equal var_inf1 array_elmt_size loop_step) then (
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
			)
		)
		with _ -> if !debug then ignore(E.log "Array bounds analysis inconclusive\n"); false
	)

