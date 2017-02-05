
(*TESTO PROGETTO*)

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



 

(*CREAZIONE AMBIENTE (ENV)*)



type eval = None 
          | Int of int 
          | Bool of bool
          | Char of char
          | List of eval * eval
          | VoidList 
          | Funval of efun
and efun = exp;;

(* trova col predicato p la funzione f *)
let rec find p f = function
    [] -> None
  | x::l -> if p x then f x else find p f l;;

(* lega identificatori e valori *)
 let rec bind x v = function
    [] -> [(x,v)]
  | (y,v')::l when x=y -> (x,v)::l
  | (y,v')::l -> (y,v')::(bind x v l);;

(* cerca l' env giusto e lo applica *)
let applyenv env x = (find (fun (y,v) -> x=y) (fun (y,v) -> v)) env;;
let emptyenv () = [];;

type env = (ide*eval) list;;


(*FUNZIONI DA RICHIAMARE NEI VARI SEM*)

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

(* first *)
let first x = match x with 
	List(x,y) -> x 
	|_ -> failwith ("error");;

(* rest *)
let rest x = match x with 
	List(x,y) -> y 
	|_ -> failwith ("error");;

(* cons *)
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




(*SEM_EAGER*)


let rec sem_eager (e:exp) (r:env) = match e with
    | Eint(n) -> (Int(n))
    | Ebool(b) -> (Bool(b))
    | Echar(c) -> (Char(c))
    | Empty -> (VoidList)
    | Cons(a,b) -> cons((sem_eager a r),(sem_eager b r))  
    | First(a) -> (first(sem_eager a r))
    | Rest(a) -> (rest(sem_eager a r)) 
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
 
    | Let(l,b) -> (sem_eager b (bindList l r))
    | Fun(i,a) -> makefun(Fun(i,a))
    | Apply (a,b) -> let r' = applyf(a, (sem_eagerlist b r),r) in
        (applyfun((sem_eagerlist b r'), (sem_eager a r'), r))
(*| Try (e1,id,e2) ->funtry(e1,id,e2) r
 | Raise d -> ((applyenv r d))  *) 
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
      | _ -> failwith ("attempt to apply a non-functional object"))  ;;

(*SEM_TRY*)

let rec sem_try (e:exp) (r:env) = match e with
    | Eint(n) -> (Int(n))
    | Ebool(b) -> (Bool(b))
    | Echar(c) -> (Char(c))
    | Empty -> (VoidList)
    | Cons(a,b) -> cons((sem_try a r),(sem_try b r))  
    | First(a) -> (first(sem_try a r))
    | Rest(a) -> (rest(sem_try a r)) 
    | Den(i) ->applyenv r i
    | Prod(a,b) -> (mul((sem_try a r), (sem_try b r)))
    | Sum(a,b) -> (add((sem_try a r), (sem_try b r)))
    | Diff(a,b)  -> (sub((sem_try a r), (sem_try b r)))
    | Mod(a,b) -> (modulo((sem_try a r), (sem_try b r)))
    | Div(a,b) -> (divisione((sem_try a r), (sem_try b r)))
    | Lessint(a,b) -> (lessint((sem_try a r), (sem_try b r)))
    | Eqint(a,b) -> (eqint((sem_try a r), (sem_try b r)))
    | Iszero(a) -> (iszero((sem_try a r)))
    | Lesschar(a,b) -> (lesschar((sem_try a r), (sem_try b r)))
    | Eqchar(a,b) -> (eqchar((sem_try a r), (sem_try b r) ))
    | Isempty(a) -> (isempty(sem_try a r))
    | Or(a,b) ->  (or_f((sem_try a r), (sem_try b r)))
    | And(a,b) ->  (and_f((sem_try a r), (sem_try b r)))
    | Not(a) -> (not_f((sem_try a r)))
    | Ifthenelse(a,b,c) -> 
            let g = ( (sem_try a r)) in
            if type_check("bool",g) then
               (if g = Bool(true) 
               then ((sem_eager b r))
               else ((sem_eager c r)))
            else failwith ("wrong guard")
 
    | Let(l,b) -> (sem_try b (bindList l r))
    | Fun(i,a) -> makefun(Fun(i,a))
    | Apply (a,b) -> let r' = applyf(a, (sem_eagerlist b r),r) in
        (applyfun((sem_eagerlist b r'), (sem_try a r'), r))
| Try (e1,id,e2) ->funtry(e1,id,e2) r
 | Raise d -> ((applyenv r d))   

(*ECCEZIONI*)

and funtry(e1,id,e2) r=match e1,e2 with

|Eint n,_-> Int n
|_,Eint n->Int n
|Ebool b,_->Bool b
|_,Ebool b->Bool b
|Echar c,_->Char c
|_,Echar c->Char c
|First(a),_->(match a with Raise(i)->if id=i then sem_try (First(e2)) r else failwith"unbound exception")
|Rest(a),_->(match a with Raise(i)->if id=i then sem_try (Rest(e2)) r else failwith"unbound exception")
|Prod(a,b),_ -> (match a,b with
    Raise(i),_->if id=i then sem_try (Prod(e2,b)) r else failwith"unbound exception"
  |_,Raise(i)-> if id=i then sem_try (Prod(a,e2)) r else failwith"unbound exception")
| Sum(a,b),_ -> (match a,b with
    Raise(i),_->if id=i then sem_try (Sum(e2,b)) r else failwith"unbound exception"
  |_,Raise(i)-> if id=i then sem_try (Sum(a,e2)) r else failwith"unbound exception")
| Diff(a,b),_  -> (match a,b with
    Raise(i),_->if id=i then sem_try (Diff(e2,b)) r else failwith"unbound exception"
  |_,Raise(i)-> if id=i then sem_try (Diff(a,e2)) r else failwith"unbound exception")
| Mod(a,b),_ -> (match a,b with
    Raise(i),_->if id=i then sem_try (Mod(e2,b)) r else failwith"unbound exception"
  |_,Raise(i)-> if id=i then sem_try (Mod(a,e2)) r else failwith"unbound exception")
| Div(a,b) ,_-> (match a,b with
    Raise(i),_->if id=i then sem_try (Div(e2,b)) r else failwith"unbound exception"
  |_,Raise(i)-> if id=i then sem_try (Div(a,e2)) r else failwith"unbound exception")
| Lessint(a,b),_ ->(match a,b with
    Raise(i),_->if id=i then sem_try (Lessint(e2,b)) r else failwith"unbound exception"
  |_,Raise(i)-> if id=i then sem_try (Lessint(a,e2)) r else failwith"unbound exception")
| Eqint(a,b),_ -> (match a,b with
    Raise(i),_->if id=i then sem_try (Eqint(e2,b)) r else failwith"unbound exception"
  |_,Raise(i)-> if id=i then sem_try (Eqint(a,e2)) r else failwith"unbound exception")
| Iszero(a),_->(match a with
    Raise(i)->if id=i then sem_try (Iszero(e2)) r else failwith"unbound exception")
|Lesschar(a,b) ,_->(match a,b with
    Raise(i),_->if id=i then sem_try (Lesschar(e2,b)) r else failwith"unbound exception"
  |_,Raise(i)-> if id=i then sem_try (Lesschar(a,e2)) r else failwith"unbound exception")
| Eqchar(a,b),_ ->(match a,b with
    Raise(i),_->if id=i then sem_try (Eqchar(e2,b)) r else failwith"unbound exception"
  |_,Raise(i)-> if id=i then sem_try (Eqchar(a,e2)) r else failwith"unbound exception")
|Isempty(a),_->(match a with Raise(i)->if id=i then sem_try (Isempty(e2)) r else failwith"unbound exception")
| Or(a,b) ,_-> (match a,b with
    Raise(i),_->if id=i then sem_try (Or(e2,b)) r else failwith"unbound exception"
  |_,Raise(i)-> if id=i then sem_try (Or(a,e2)) r else failwith"unbound exception")
| And(a,b),_ -> (match a,b with
    Raise(i),_->if id=i then sem_try (And(e2,b)) r else failwith"unbound exception"
  |_,Raise(i)-> if id=i then sem_try (And(a,e2)) r else failwith"unbound exception")
| Not(a),_ -> (match a with
    Raise(i)->if id=i then sem_try (Not(e2)) r else failwith"unbound exception")
| Ifthenelse(a,b,c),_ -> (match b,c with
    Raise(i),_->if id=i then sem_try (Ifthenelse(a,e2,c)) r else failwith"unbound exception"
  |_,Raise(i)-> if id=i then sem_try (Ifthenelse(a,b,e2)) r else failwith"unbpund exception")
      
|Try(a,b,c),_-> funtry(a,b,c) r
|Let(l,b),_->(try sem_try b r with _->funtry(b,id,e2) r 
  |_->failwith"raise not found")
|Fun(i,e),_->sem_try e1 r
|Apply(a,b),_->(match a with
    Fun(l,e)->sem_try e1 r      
  |_->failwith"first argument is not a function ")
|_->sem_try e1 r;;
      



(*SEM_LAZY*)


let rec sem_lazy (e:exp) (r:env) = match e with
   
  Apply (a,b) ->(match a with
    Fun (e0,e1) ->(match e1 with
      Den (i)->sem_eager e r
    |Eint n-> Int n
    |Ebool b-> Bool b
    |Echar c->Char c
    |First a -> match a with Den(i) -> (sem_eager e r)
    |Rest a -> match a with Den(i) -> (sem_eager e r)
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
    |Isempty a -> match a with | Den(i) -> (sem_eager e r)
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
|Raise(i)->sem_eager e r
|_->sem_eager e r ;;




(*SEM*)

let rec sem (e:exp) (r:env) = match e with
    | Eint(n) -> (Int(n))
    | Ebool(b) -> (Bool(b))
    | Echar(c) -> (Char(c))
    | Empty -> (VoidList)
    | Cons(a,b) -> cons((sem a r),(sem b r))  
    | First(a) -> (first(sem a r))
    | Rest(a) -> (rest(sem a r)) 
    | Den(i) ->applyenv r i
    | Prod(a,b) -> (mul((sem a r), (sem b r)))
    | Sum(a,b) -> (add((sem a r), (sem b r)))
    | Diff(a,b)  -> (sub((sem a r), (sem b r)))
    | Mod(a,b) -> (modulo((sem a r), (sem b r)))
    | Div(a,b) -> (divisione((sem a r), (sem b r)))
    | Lessint(a,b) -> (lessint((sem a r), (sem b r)))
    | Eqint(a,b) -> (eqint((sem a r), (sem b r)))
    | Iszero(a) -> (iszero((sem a r)))
    | Lesschar(a,b) -> (lesschar((sem a r), (sem b r)))
    | Eqchar(a,b) -> (eqchar((sem a r), (sem b r) ))
    | Isempty(a) -> (isempty(sem a r))
    | Or(a,b) ->  (or_f((sem a r), (sem b r)))
    | And(a,b) ->  (and_f((sem a r), (sem b r)))
    | Not(a) -> (not_f((sem a r)))
    | Ifthenelse(a,b,c) -> 
            let g = ( (sem a r)) in
            if type_check("bool",g) then
               (if g = Bool(true) 
               then ((sem b r))
               else ((sem c r)))
            else failwith ("wrong guard")
 
    | Let(l,b) -> (sem b (bindList l r))
    | Fun(i,a) -> makefun(Fun(i,a))
    | Apply (a,b) -> let r' = applyf(a, (sem_eagerlist b r),r) in
        (applyfun((sem_eagerlist b r'), (sem_eager a r'), r))
;;



(*TEST*)

sem_eager (Let([("x",Eint(1));
("f",Fun([],Den("x")));
("g",Fun(["y"],Let([("x",Eint(2))],
Apply(Den("y"),[]))))],
Let([("x",Eint(3))],Apply(Den("g"),[Den("f")])))) (emptyenv());;
