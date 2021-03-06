(************************************************************)
(*                          BASE                            *)
(************************************************************)
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
| Apply of exp * exp list
| Try of exp * ide * exp
| Raise of ide;;



 
(************************************************************)
(*                           ENVIRONMENT                    *)
(************************************************************)


type eval = None 
          | Int of int 
          | Bool of bool
          | Char of char
          | List of eval * eval
          | VoidList 
          | Funval of efun
and efun = exp;;

(* The function find, given a predicate p: 'a     bool,
a function f : 'a     eval and a list, finds the first element
of the list (if any) that satisfies the predicate. *)
let rec find p f = function
    [] -> None
  | x::l -> if p x then f x else find p f l;;

(* The function bind binds an identifier x to a value v in the given environment. *)
 let rec bind x v = function
    [] -> [(x,v)]
  | (y,v')::l when x=y -> (x,v)::l
  | (y,v')::l -> (y,v')::(bind x v l);;

(* The function applyenv searches the environment for the value bound to the identifier x. *)
let applyenv env x = (find (fun (y,v) -> x=y) (fun (y,v) -> v)) env;;
let emptyenv () = [];;

type env = (ide*eval) list;;

(************************************************************)
(*                       FUNCTIONS                          *)
(************************************************************)
(* type check principale*)
let type_check (x,y) =
  match x with
    | "int" ->
      (match y with 
      | Int(a) -> true
      | _ -> false)
    | "bool" ->
      (match y with 
      | Bool(a) -> true
      | _ -> false)
    | "char" ->
      (match y with 
      | Char(a) -> true
      | _ -> false)
    | "list" ->
      (match y with
      | VoidList -> true
      | List (a,b) -> true
      | _ -> false)
    | _ -> failwith ("invalid type");;

(* multiplicazione *)
let mul (x,y) =
  if type_check ("int",x) && type_check("int",y) 
  then
    (match (x,y) with
    | (Int(a), Int(b)) -> Int(a*b)
    | _ -> failwith("error"))
  else failwith ("type error mul");;

(* addizione *)
let add(x,y) =
  if type_check("int",x) && type_check("int",y) 
  then 
    (match (x,y) with
    | (Int(a), Int(b)) -> Int(a+b)
    | _ -> failwith("error"))
  else failwith ("type error add");;

(* sottrazione*)
let sub (x,y) =
  if type_check("int",x) && type_check("int",y) 
  then 
    (match (x,y) with
    | (Int(a), Int(b)) -> Int(a-b)
    | _ -> failwith("error"))
  else failwith ("type error sub");;

(* modulo *)
let modulo(x,y) =
  if type_check("int",x) && type_check("int",y) 
  then 
    (match (x,y) with
    | (Int(a), Int(b)) -> Int(a mod b)
    | _ -> failwith("error"))
  else failwith ("type error mod");;



(* divisione *)
let divisione (x,y) =
  if type_check("int",x) && type_check("int",y) 
  then 
    (match (x,y) with
    | (Int(a), Int(b)) -> Int(a / b)
    | _ -> failwith("error"))
  else failwith ("type error div");;

(* lessint *)
let lessint (x,y) =
  if type_check("int",x) && type_check("int",y) 
  then 
    (match (x,y) with
    | (Int(a), Int(b)) when Int(a)<Int(b) -> Int(a)
    | (Int(a), Int(b)) -> Int(b)
    | _ -> failwith("error"))
  else failwith ("type error lessint");;

(* eqint *)
let eqint (x,y) =
  if type_check("int",x) && type_check("int",y) 
  then 
    (match (x,y) with
    | (Int(a), Int(b)) when Int(a)=Int(b) -> Bool(true)
    | _ -> Bool(false))
  else failwith ("type error eqint");;

(* iszero *)
let iszero x =
  if type_check("int",x) 
  then 
    (match x with
    | Int(a) -> Bool(a=0)
    | _ -> failwith("error"))

else failwith ("type error iszero");;

(* lesschar *)
let lesschar (x,y) =
  if type_check("char",x) && type_check("char",y) 
  then 
    (match (x,y) with
    | (Char(a), Char(b)) when Char(a)<Char(b) -> Char(a)
    | (Char(a), Char(b)) -> Char(b)
    | _ -> failwith("error"))
  else failwith ("type error lesschar");;

(* eqchar *)
let eqchar (x,y) =
  if type_check("char",x) && type_check("char",y) 
  then 
    (match (x,y) with
    | (Char(a), Char(b)) when Char(a)=Char(b) -> Bool(true)
    | _ -> Bool(false))
  else failwith ("type error eqchar");;

(*Isempty*)
 let isempty x = 
 	(match x with
 	VoidList -> Bool true
 | List _ -> Bool false
 | _ -> failwith "Era attesa una lista");;
 

(* or*)
let or_f (x,y) =
  if type_check("bool",x) && type_check("bool",y) 
  then 
    (match (x,y) with
    | (Bool(a), Bool(b)) -> Bool(a || b)
    | _ -> failwith("error"))
  else failwith ("type error or");;

(* and*)
let and_f (x,y) =
  if type_check("bool",x) && type_check("bool",y) 
  then 
    (match (x,y) with
    | (Bool(a), Bool(b)) -> Bool(a && b)
    | _ -> failwith("error"))
  else failwith ("type error and");;

(* not*)
let not_f x =
  if type_check("bool",x) 
  then 
    (match x with
    | Bool(a) -> Bool(not a)
    | _ -> failwith("error"))
  else failwith ("type error not");;


let cons (x,y) = if type_check("list",y)
  then 
    (match (x,y) with
    | (_,VoidList) -> List(x,VoidList)
    | (Int(u),List(z,w)) -> 
        if type_check("int",z)
        then List(Int(u), List(z,w))
        else failwith "type list error"
    | (Bool(u),List(z,w)) ->
        if type_check("bool",z)
        then List(Bool(u),List(z,w))
        else failwith "type list error"
    | (Char(u),List(z,w)) ->
        if type_check("char",z) 
        then List(Char(u),List(z,w))
        else failwith "type list error"
    | _ -> failwith("error"))
  else failwith ("type error");;

exception WrongBindlist;;
let rec bindlist2 (r,il,el) =
  match (il,el) with
    ([],[]) -> r
  | i::il1, e::el1 -> bindlist2((bind i e r), il1, el1)
  | _ -> raise WrongBindlist;;


(************************************************************)
(*                          SEM_EAGER                       *)
(************************************************************)

let rec sem_eager (e:exp) (r:env) = match e with
    | Eint(n) -> (Int(n))
    | Ebool(b) -> (Bool(b))
    | Echar(c) -> (Char(c))
    | Empty -> (VoidList)
    | Cons(a,b) -> cons((sem_eager a r),(sem_eager b r))             
    | Den(i) ->applyenv r i
    | Prod(a,b) -> (mul((sem_eager a r), (sem_eager b r)))
    | Sum(a,b) -> (add((sem_eager a r), (sem_eager b r)))
    | Diff(a,b)  -> (sub((sem_eager a r), (sem_eager b r)))
    | Mod(a,b) -> (modulo((sem_eager a r), (sem_eager b r)))
    | Div(a,b) -> (divisione((sem_eager a r), (sem_eager b r)))
    | Lessint(a,b) -> (lessint((sem_eager a r), (sem_eager b r)))
    | Eqint(a,b) -> (eqint((sem_eager a r), (sem_eager b r)))
    | Iszero(a) -> (iszero((sem_eager a r)))
    | Lesschar(a,b) -> (lesschar((sem_eager a r), (sem_eager b r)))
    | Eqchar(a,b) -> (eqchar((sem_eager a r), (sem_eager b r) ))
    | Isempty(a) -> (isempty(sem_eager a r))
    | Or(a,b) ->  (or_f((sem_eager a r), (sem_eager b r)))
    | And(a,b) ->  (and_f((sem_eager a r), (sem_eager b r)))
    | Not(a) -> (not_f((sem_eager a r)))
    | Ifthenelse(a,b,c) -> 
            let g = ( (sem_eager a r)) in
            if type_check("bool",g) then
               (if g = Bool(true) 
               then ((sem_eager b r))
               else ((sem_eager c r)))
            else failwith ("wrong guard")
 (*   | Let(i,e1,e2) -> sem_eager e2 (bind i (sem_eager e1 r) r) *)
    | Let(l,b) -> (sem_eager b (bindList l r))
    | Fun(i,a) -> makefun(Fun(i,a))
    | Apply (a,b) -> let r' = applyf(a, (sem_eagerlist b r),r) in
        (applyfun((sem_eagerlist b r'), (sem_eager a r'), r))
| Try (e1,id,e2) ->funtry(e1,id,e2) r
 | Raise d -> ((applyenv r d))  (* considerato Raise come un Den per leggere l'ide  dall'ambiente*) 
and applyf ((a:exp),(b:eval list),(r:env)) = match a with
    Fun(ii,aa) -> bindlist2(r,ii,b)
  | Den(i) -> (match (applyenv r i) with  Funval(Fun(ii,aa)) -> (bindlist2(r,ii,b)))
  | _ -> failwith "No"

and bindList l r = match l with
    [] -> r
  | (x,a)::tl -> bindList tl (bind x ((sem_eager a r)) r)
  |_->failwith"error"

and sem_eagerlist el r = match el with
	  | [] -> []
	  | e::el1 -> ((sem_eager e r))::(sem_eagerlist el1 r)
	  |_-> failwith"error"
and makefun (a:exp) =
      (match a with
      |	Fun(ii,aa) -> Funval(a)
      |	_ -> failwith ("Non-functional object"))

and applyfun ((ev2:eval list),(ev1:eval),(r:env)) =
      ( match ev1 with
      | Funval(Fun(ii,aa)) -> sem_eager aa (bindlist2(r,ii,ev2))
      | _ -> failwith ("attempt to apply a non-functional object"))  


(************************************************************)
(*                        ECCEZIONI                         *)
(************************************************************)

and funtry(e1,id,e2) r=match e1,e2 with

|Eint n,_-> Int n
|_,Eint n->Int n
|Ebool b,_->Bool b
|_,Ebool b->Bool b
|Echar c,_->Char c
|_,Echar c->Char c
|Prod(a,b),_ -> (match a,b with
    Raise(i),_->if id=i then sem_eager (Prod(e2,b)) r else failwith"unbound exception"
  |_,Raise(i)-> if id=i then sem_eager (Prod(a,e2)) r else failwith"unbound exception")
| Sum(a,b),_ -> (match a,b with
    Raise(i),_->if id=i then sem_eager (Sum(e2,b)) r else failwith"unbound exception"
  |_,Raise(i)-> if id=i then sem_eager (Sum(a,e2)) r else failwith"unbound exception")
| Diff(a,b),_  -> (match a,b with
    Raise(i),_->if id=i then sem_eager (Diff(e2,b)) r else failwith"unbound exception"
  |_,Raise(i)-> if id=i then sem_eager (Diff(a,e2)) r else failwith"unbound exception")
| Mod(a,b),_ -> (match a,b with
    Raise(i),_->if id=i then sem_eager (Mod(e2,b)) r else failwith"unbound exception"
  |_,Raise(i)-> if id=i then sem_eager (Mod(a,e2)) r else failwith"unbound exception")
| Div(a,b) ,_-> (match a,b with
    Raise(i),_->if id=i then sem_eager (Div(e2,b)) r else failwith"unbound exception"
  |_,Raise(i)-> if id=i then sem_eager (Div(a,e2)) r else failwith"unbound exception")
| Lessint(a,b),_ ->(match a,b with
    Raise(i),_->if id=i then sem_eager (Lessint(e2,b)) r else failwith"unbound exception"
  |_,Raise(i)-> if id=i then sem_eager (Lessint(a,e2)) r else failwith"unbound exception")
| Eqint(a,b),_ -> (match a,b with
    Raise(i),_->if id=i then sem_eager (Eqint(e2,b)) r else failwith"unbound exception"
  |_,Raise(i)-> if id=i then sem_eager (Eqint(a,e2)) r else failwith"unbound exception")
| Iszero(a),_->(match a with
    Raise(i)->if id=i then sem_eager (Iszero(e2)) r else failwith"unbound exception")
|Lesschar(a,b) ,_->(match a,b with
    Raise(i),_->if id=i then sem_eager (Lesschar(e2,b)) r else failwith"unbound exception"
  |_,Raise(i)-> if id=i then sem_eager (Lesschar(a,e2)) r else failwith"unbound exception")
| Eqchar(a,b),_ ->(match a,b with
    Raise(i),_->if id=i then sem_eager (Eqchar(e2,b)) r else failwith"unbound exception"
  |_,Raise(i)-> if id=i then sem_eager (Eqchar(a,e2)) r else failwith"unbound exception")
| Or(a,b) ,_-> (match a,b with
    Raise(i),_->if id=i then sem_eager (Or(e2,b)) r else failwith"unbound exception"
  |_,Raise(i)-> if id=i then sem_eager (Or(a,e2)) r else failwith"unbound exception")
| And(a,b),_ -> (match a,b with
    Raise(i),_->if id=i then sem_eager (And(e2,b)) r else failwith"unbound exception"
  |_,Raise(i)-> if id=i then sem_eager (And(a,e2)) r else failwith"unbound exception")
| Not(a),_ -> (match a with
    Raise(i)->if id=i then sem_eager (Not(e2)) r else failwith"unbound exception")
| Ifthenelse(a,b,c),_ -> (match b,c with
    Raise(i),_->if id=i then sem_eager (Ifthenelse(a,e2,c)) r else failwith"unbound exception"
  |_,Raise(i)-> if id=i then sem_eager (Ifthenelse(a,b,e2)) r else failwith"unbpund exception")
      
|Try(a,b,c),_-> funtry(a,b,c) r
|Let(l,b),_->(try sem_eager b r with _->funtry(b,id,e2) r 
  |_->failwith"raise not found")
|Fun(i,e),_->sem_eager e1 r
|Apply(a,b),_->(match a with
    Fun(l,e)->sem_eager e1 r      
  |_->failwith"first argument is not a function ")
|_->sem_eager e1 r
      



(*
(************************************************************)
(*                           TYPE_INF                       *)
(************************************************************)
and type_inf ((e:exp),(r:env)) = match e with
    Eint(n) -> Tint
  | Ebool(b) -> Tbool
  | Echar(c) -> Tchar
  | Empty -> Tlist (gentide())
  | Cons (a,b) -> Tlist (type_inf(a,r))
  | Den(i) -> let rec typ_den e =
      (match e with
           Int(n) -> Tint
         | Bool(b) -> Tbool
         | Char(c) -> Tchar
         | VoidList -> Tlist(gentide())
         | List(a,b) -> typ_den(a))
    in typ_den (applyenv r i)
  | Iszero(a) -> Tbool
  | Eqint(a,b) -> Tbool
  | Lesschar(a,b) -> Tbool
  | Lessint(a,b) -> Tbool
  | Eqchar(a,b) -> Tbool
  | Prod(a,b) -> Tint
  | Sum(a,b) -> Tint
  | Diff(a,b) -> Tint
  | Mod(a,b) -> Tint
  | Div(a,b) -> Tint
  | And(a,b) -> Tbool
  | Or(a,b) -> Tbool
  | Not(a) -> Tbool
  | Ifthenelse(a,b,c) -> if fst (sem_eager a r) = Bool(true) 
    then type_inf(b,r)
    else type_inf(c,r)
  | Let(l,b) -> type_inf(b,r)
  | Fun(i,a) -> Tfun(typ_list i r , type_inf(a,r))
  | Apply(a,b) -> 
      (match type_inf(a,r) with
           Tfun(l,e) -> e
         | _ -> failwith "first argument is not a function")
| Raise id -> gentide()
 | Try (e0,id,e1) -> type_inf (e0,r)
and typ_list l r = match l with
    [] -> []
  | hd::tl -> type_inf((Den(hd)),r)::(typ_list tl r)
  |_->failwith "error"
;;
*)

(************************************************************)
(*                          SEM_LAZY                        *)
(************************************************************)

let rec sem_lazy (e:exp) (r:env) = match e with
   
  Apply (a,b) ->(match a with
    Fun (e0,e1) ->(match e1 with
      Den (i)->sem_eager e r
    |Eint n-> Int n
    |Ebool b-> Bool b
    |Echar c->Char c
    |Sum(g,h)->(match g,h with
	Den(i),Den(l)-> sem_eager e r
      | Den (i),_->sem_eager e r
      |_,Den (i)->sem_eager e r)
	  
    |Diff(g,h)->(match g,h with
	Den(i),Den(l)-> sem_eager e r
      |Den (i),_->sem_eager e r
      |_,Den (i)->sem_eager e r)
    |Prod(g,h)->(match g,h with
	Den(i),Den(l)-> sem_eager e r
      |Den(i),Den(l)->sem_eager e r
      |Den( i),_->sem_eager e r
      |_,Den (i)->sem_eager e r)
    |Div(g,h)->(match g,h with
	Den(i),Den(l)-> sem_eager e r
      |Den (i),Eint n->if n=0 then None else sem_eager e r
      |Eint n,Den( i)-> sem_eager e r)
    |Mod(g,h)->(match g,h with
	Den(i),Den(l)-> sem_eager e r 
 |Den (i),_->sem_eager e r
      |_,Den (i)->sem_eager e r)
    |Lessint(g,h)->(match g,h with
        Den(i),Den(l)->sem_eager e r
      |Den (i),_->sem_eager e r
      |_,Den (i)->sem_eager e r)
    |Eqint(g,h)->(match g,h with
        Den(i),Den(l)->sem_eager e r
      |Den(i),_->sem_eager e r
      |_,Den (i)->sem_eager e r)
    |Lesschar (g,h)->(match g,h with
	Den(i),Den(l)-> sem_eager e r
      |Den(i),_->sem_eager e r
      |_,Den (i)->sem_eager e r)
    |Eqchar (g,h)->(match g,h with
	
        Den(i),Den(l)->sem_eager e r  
      |Den (i),_->sem_eager e r
      |_,Den (i)->sem_eager e r)
    |Iszero a-> match a with Den (i)->(sem_eager e r)
      |Not a-> match a with Den (i)->sem_eager e r
	|And (g,h)->(match g,h with
            Den(i),Den(l)->sem_eager e r
          |Den (i),_->sem_eager e r
          |_,Den (i)->sem_eager e r)
	|Or (g,h)->(match g,h with
	    Den(i),Den(l)->sem_eager e r     
	  |Den (i),_->sem_eager e r
          |_,Den (i)->sem_eager e r)
	|Ifthenelse(a,b,c)->sem_eager e r
	|_->let w = sem_eager e1 r in (match w with
	    (Int(n)) -> (Int(n))
	  | (Bool(b)) -> (Bool(b))
	  | (Char(c)) -> (Char(c))
          |_->None )))
(*|Raise(i)->sem_eager e r*)
|_->sem_eager e r ;;


(************************************************************)
(*                        TEST                              *)
(************************************************************)
sem_eager (Let([("x",Eint(1));
("f",Fun([],Den("x")));
("g",Fun(["y"],Let([("x",Eint(2))],
Apply(Den("y"),[]))))],
Let([("x",Eint(3))],Apply(Den("g"),[Den("f")])))) (emptyenv());;
