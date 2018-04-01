open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

let evalOperation (stack, (state, input, output)) operation = match ((stack, (state, input, output)), operation) with
	| (((x :: y :: stack), (state, input, output)), (BINOP op)) -> ((Expr.eval state (Expr.Binop (op, Expr.Const y, Expr.Const x))) :: stack, (state, input, output))
	| ((stack, (state, input, output)), (CONST c)) -> (c :: stack, (state, input, output))
	| ((stack, (state, c :: input, output)), READ) -> (c :: stack, (state, input, output))
	| ((x :: stack, (state, input, output)), WRITE) -> (stack, (state, input, output @ [x]))
	| ((stack, (state, input, output)), (LD x)) -> ((state x) :: stack, (state, input, output))
	| ((v :: stack, (state, input, output)), (ST x)) -> (stack, (Expr.update x v state, input, output))


	
(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
*)                         
let rec eval config program = match program with
	| [] -> config
	| op :: rest -> eval (evalOperation config op) rest

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i = let (_, (_, _, o)) = eval ([], (Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)

let rec compileExpression expression = match expression with
	| Expr.Const c                 -> [CONST c]
	| Expr.Var x                   -> [LD x]
    | Expr.Binop (operation, l, r) -> (compileExpression l) @ (compileExpression r) @ [BINOP operation]

let rec compile x = match x with
	| Stmt.Read x -> [READ; ST x]
	| Stmt.Write x -> compileExpression x @ [WRITE] 
	| Stmt.Assign(x, e) -> compileExpression e @ [ST x]
	| Stmt.Seq(e1, e2) -> compile e1 @ compile e2
