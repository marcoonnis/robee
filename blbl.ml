type ide = string;;

type exp =
	Eint of int 
      | Ebool of bool 
      | Echar of char
      | Empty
      | Cons of exp * exp
      | First of exp
      | Rest of exp
      | Den of ide
      | Prod of exp * exp
      | Sum of exp * exp
      | Diff of exp * exp
      | Mod of exp * exp
      | Div of exp * exp
      | Lessint of exp * exp
      | Eqint of exp * exp
      | Iszero of exp
      | Lesschar of exp * exp
      | Eqchar of exp * exp
      | Isempty of exp
      | Or of exp * exp
      | And of exp * exp
      | Not of exp
      | Ifthenelse of exp * exp * exp
      | Let of (ide * exp) list * exp
      | Fun of ide list * exp
      | Apply of exp * exp list;;

type dval =
	Int of int
      | Bool of bool
      | Char of char
      | ListaVuota
      | Lista of dval * dval
      | Function of exp;;

let rec searchEnv i env = match env with
	[] -> failwith "ERROR Il nome della variabile non e' nell'ambiente"
      | (nome, valore) :: tl when i = nome -> valore
      | (nome, valore) :: tl -> searchEnv i tl;;

(** Questa la tengo solo nel caso la nuova implementazione non vada bene

let rec addEnv i e env = match env with
	[] -> [(i, e)]
      | (nome, valore) :: tl when i = nome -> (i, e) :: tl
      | (nome, valore) :: tl -> (nome, valore) :: (addEnv i e env);;

**)

let rec semRec exp env = match exp with
	Eint (n) -> Int (n)
      | Ebool (b) -> Bool (b)
      | Echar (c) -> Char (c)
      | Empty -> ListaVuota
     (* | Cons (e1, e2) -> 
      | First (l) ->
      | Rest (l) -> *)
      | Den (i) -> searchEnv i env
      | Prod (n1, n2) -> opInt ( * ) (semRec n1 env) (semRec n2 env)
      | Sum (n1, n2) -> opInt ( + ) (semRec n1 env) (semRec n2 env)
      | Diff (n1, n2) -> opInt ( - ) (semRec n1 env) (semRec n2 env)
      | Mod (n1, n2) -> opInt ( mod ) (semRec n1 env) (semRec n2 env)
      | Div (n1, n2) -> opInt ( / ) (semRec n1 env) (semRec n2 env)
      | Lessint (n1, n2) -> opIntBool ( < ) (semRec n1 env) (semRec n2 env)
      | Eqint (n1, n2) -> opIntBool ( = ) (semRec n1 env) (semRec n2 env)
      | Iszero (n) -> opIntBool ( = ) (semRec n env) (Int (0))
      | Lesschar (c1, c2) -> opCharBool ( < ) (semRec c1 env) (semRec c2 env)
      | Eqchar (c1, c2) -> opCharBool ( = ) (semRec c1 env) (semRec c2 env)
      (* | Isempty (l) -> (semRec l env) = ListaVuota *)
      | Or (b1, b2) -> opBool ( || ) (semRec b1 env) (semRec b2 env)
      | And (b1, b2) -> opBool ( && ) (semRec b1 env) (semRec b2 env)
      | Not (b) -> opBoolNot (semRec b env)
      (* | Ifthenelse (b, e1, e2) -> if (semRec b env) then (semRec e1 env) else (semRec e2 env) *)
      | Let (i, e) -> semRec e (addEnv i  env)
     (* | Fun (i::tl, e) ->
      | Apply (e1, e2::tl) -> *)
and opInt op n1 n2 = match (n1, n2) with
	(Int (n3), Int (n4)) -> Int (op n3 n4)
      | _ -> failwith "ERROR Mi aspettavo due Int"
and opIntBool op n1 n2 = match (n1, n2) with
	(Int (n3), Int (n4)) -> Bool (op n3 n4)
      | _ -> failwith "ERROR Mi aspettavo due Int"
and opCharBool op c1 c2 = match (c1, c2) with
	(Char (c3), Char (c4)) -> Bool (op c3 c4)
      | _ -> failwith "ERROR Mi aspettavo due Char"
and opBool op b1 b2 = match (b1, b2) with
	(Bool (b3), Bool (b4)) -> Bool (op b3 b4)
      | _ -> failwith "ERROR Mi aspettavo due Bool"
and opBoolNot b = match b with
	Bool (b1) -> Bool (not b1)
      | _ -> failwith "ERROR Mi aspettavo un Bool"
and addEnv i env = match i with
	[] -> env
      | (i, e) :: tl -> match env with
		[] -> [(i, (semRec e env))]
	      | (nome, valore) :: tl when i = nome -> (i, (semRec e env)) :: tl
	     (* | (nome, valore) :: tl -> (nome, valore) :: (addEnv i env)*);;;;

let sem exp = semRec exp [];;

sem (Let(["x",Eint 1],Den("x")));;
