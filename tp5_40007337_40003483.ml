(* Un type somme pour repr ́esenter les operations (i.e les operateurs binaires) *)
type top = Plus| Moins | Fois | Div;;
(* Le type somme pour representer les fonctions usuelles *)
type tfonc = Sin | Cos | Ln | Exp | Rac | Opp;;
(* Un type recursif pour representer des expressions algebriques *)
type texpr =
| Const of float
| X
| Op of texpr * top * texpr
| F of tfonc * texpr
| P of texpr * int;;


(* Exercice 1 *)

(* 7x^3 - 4x + 8  *)
let ex1a = Op(Op(Op(Const(7.0), Fois, P(X, 3)), Moins, Op(Const(4.0), Fois, X)), Plus, Const(8.0));;

(* (1+sin(3x))/(1−cos(x))  *)
let ex1b = Op(Op(Const(1.0), Plus, F(Sin, Op(Const(3.0), Fois, X))), Div, Op(Const(1.0), Moins, F(Cos, X)));;

(* 1 - (x^2)/2 + (x^3)/3 - (x^4)/4 *)
let ex1c = Op(Op(Op(Const(1.0), Moins, Op(P(X, 2), Div, Const(2.0))), Plus, Op(P(X, 3), Div, Const(3.0))), Moins, Op(P(X, 4), Div, Const(4.0)));;

(* ln(1 + sqrt(x^2 + 3)) *)
let ex1d = F(Ln, F(Rac, Op(P(X, 2), Plus, Const(3.0))));;


(* Exercice 2 *)

(* texpr -> string *)
let rec stringinf_of_texpr = function (expr) -> 
  match expr with
    (* cas de base : l'expression est une constante ou une inconnu x*)
  | Const c -> string_of_float(c)
  | X -> "x"
    (*cas récursif:
      si c'est une opération entre u et v on rappelle la fonction pour u et pour v afin d'obtenir leur stringification
      et on concatène à l'opérateur et au parenthèses *)
  | Op (u, o, v) -> 
    (match u, o, v with
    | _, Plus, _ ->   "(" ^ stringinf_of_texpr(u) ^ " + " ^ stringinf_of_texpr(v) ^ ")"
    | _, Moins, _ ->  "(" ^ stringinf_of_texpr(u) ^ " - " ^ stringinf_of_texpr(v) ^ ")"
    | _, Fois, _ ->   "(" ^ stringinf_of_texpr(u) ^ " * " ^ stringinf_of_texpr(v) ^ ")"
    | _, Div, _ ->    "(" ^ stringinf_of_texpr(u) ^ " / " ^ stringinf_of_texpr(v) ^ ")"
    )
    (* même chose pour x si c'est de la forme f(x) *)
  | F (f, e) -> 
    (match f with
    | Sin -> "sin" ^ stringinf_of_texpr(e)
    | Cos -> "cos" ^ stringinf_of_texpr(e)
    | Ln ->  "ln" ^ stringinf_of_texpr(e)
    | Exp -> "e^" ^ stringinf_of_texpr(e)
    | Rac -> "√" ^ stringinf_of_texpr(e)
    | Opp -> "-" ^ stringinf_of_texpr(e)
    )
    (* ou x^n *)
  | P (e, i) -> "(" ^ stringinf_of_texpr(e) ^ "^" ^ string_of_int(i) ^ ")"
;;
(* 3x + 25 *)
let e1 = Op( Op(Const(3.0), Fois, X), Plus, Const(25.0)) ;;

(* √((x^2) + 3) *)
let e2 = F(Rac, Op( P(X, 2), Plus, Const(3.0)));;

(* test de la fonction avec les expressions e1 et e2 de l'exemple du sujet et deux qui viennent de l'exercice 1 *)
stringinf_of_texpr(e1);;
stringinf_of_texpr(e2);;
stringinf_of_texpr(ex1a);;
stringinf_of_texpr(ex1d);;


(* Exercie 3 *)

(* texpr -> string *)
let rec stringpref_of_texpr = function (expr) ->
  (* même chose que la version infixée sauf que l'on place les opérateurs au début des expressions stringifiés *)
  match expr with
  | Const c -> string_of_float(c)
  | X -> "x"
  | Op (u, o, v) -> 
    (match u, o, v with
    | _, Plus, _ ->   "+ " ^ stringpref_of_texpr(u) ^ " " ^ stringpref_of_texpr(v)
    | _, Moins, _ ->  "- " ^ stringpref_of_texpr(u) ^ " " ^ stringpref_of_texpr(v) 
    | _, Fois, _ ->   "* " ^ stringpref_of_texpr(u) ^ " " ^ stringpref_of_texpr(v)
    | _, Div, Const(0.) -> "><(((°> " ^ stringpref_of_texpr(u)
    | _, Div, _ ->    "/ " ^ stringpref_of_texpr(u) ^ " " ^ stringpref_of_texpr(v)
    )
  | F (f, e) -> 
    (match f with
    | Sin -> "sin " ^ stringpref_of_texpr(e)
    | Cos -> "cos " ^ stringpref_of_texpr(e)
    | Ln ->  "ln " ^ stringpref_of_texpr(e)
    | Exp -> "e^ " ^ stringpref_of_texpr(e)
    | Rac -> "√ " ^ stringpref_of_texpr(e)
    | Opp -> "- " ^ stringpref_of_texpr(e)
    )
  | P (e, i) -> "^ " ^ stringpref_of_texpr(e) ^ " " ^ string_of_int(i) 
;;

(* test de la fonction avec les expressions e1 et e2 de l'exemple et deux qui viennent de l'exercice 1 *)
stringpref_of_texpr(e1);;
stringpref_of_texpr(e2);;
stringpref_of_texpr(ex1a);;
stringpref_of_texpr(ex1d);;


(* Exercice 4 *)

(* texpr * float -> float *)
let rec evaltexpr = fun (expr, x) -> 
  match expr with
    (* cas de base : l'expression est une constante c ou x *)
  | Const c -> c
  | X -> x
    (* cas récursif : l'expression est composé d'une ou plusieurs opérations sur des constantes : 
    on évalu chaque opération et sous-opération *)
  | Op (u, o, v) -> 
    (match u, o, v with
    | _, Plus, _ ->  evaltexpr(u, x) +. evaltexpr(v, x)
    | _, Moins, _ -> evaltexpr(u, x) -. evaltexpr(v, x)
    | _, Fois, _ ->  evaltexpr(u, x) *. evaltexpr(v, x)
    | _, Div, _ ->   evaltexpr(u, x) /. evaltexpr(v, x)
    )
  | F (f, e) -> 
    (match f with
    | Sin -> sin(evaltexpr(e, x))
    | Cos -> cos(evaltexpr(e, x))
    | Ln ->  log1p(evaltexpr(e, x)) (* log1p(x) est ln(x) en ocaml *)
    | Exp -> exp(evaltexpr(e, x))
    | Rac -> sqrt(evaltexpr(e, x))
    | Opp -> -1.0 *. evaltexpr(e, x) (* opposé de x = -1 * x*)
    )
  | P (e, i) -> evaltexpr(e, x) ** float_of_int(i) (* evaltexpr retourne un float donc on doit convertir i *)
;;

(* test de l'evaluation des expressions comme dans l'exemple *)
evaltexpr(e1, 7.2);;
evaltexpr(e2, 2.0);;
evaltexpr(e2, 1.0);;


(* Exercice 5 *)

(* texpr -> texpr *)
let rec derive = function (expr) -> 
  match expr with
  (* cste' = 0 et x' = 1 *)
  | Const c -> Const(0.)
  | X -> Const(1.)

  | Op (u, o, v) -> 
    (match u, o, v with
    (* (u + v)' ou (u - v)' = u' + v' ou u' - v'*)
    | _, Plus, _ | _, Moins, _ -> Op(derive(u), o, derive(v))
    (* (u * v)' *)
    | _, Fois, _ -> Op(Op(derive(u), o, v), Plus, Op(u, o, derive(v)))
    (* (u / v)' *)
    | _, Div, _ ->  Op(Op(Op(derive(u), o, v), Moins, Op(u, o, derive(v))), Div, P(v, 2))
    )
  | F (f, u) -> 
    (match f with
    | Sin -> Op(derive(u), Fois, F(Cos, u))
    | Cos -> Op(F(Opp, derive(u)), Fois, F(Sin, u)) (* -u' *sin(u): la fonction opposée est bien utile ici *)
    | Ln ->  Op(derive(u), Div, u)
    | Exp -> Op(derive(u), Fois, F(Exp, u))
    | Rac -> Op(derive(u), Div, Op(Const(2.), Fois, F(Rac, u)))
    | Opp -> F(Opp, derive(u))
    )
    (* ici on converti i en float car le type Const ne prend que des float *)
  | P (u, n) -> Op(Const(float_of_int(n)), Fois, Op(derive(u), Fois, P(u, n - 1)))
;;

(* test de la dérivation de e1 et e2 
pour e1 l'ordre n'est pas le même que dans l'exemple mais les expression sont les égales (u'v + uv' au lieu de uv' u'v) *)
derive e1;;
stringinf_of_texpr(e1);;
stringinf_of_texpr(derive e1);;
derive e2;;
stringinf_of_texpr(derive e2);;


(* Exercice 6 *)

(* texpr -> texpr *)
let rec simplif = function (expr) ->
  let rec s(expr) =
    match expr with
    | Op (u, o, v) ->
      (match u, o, v with

        (* u = u+0 = u-0 = u*1 = u/1 *)
      | _, Plus, Const(0.)
      | _, Moins, Const(0.) 
      | _, (Fois | Div), Const(1.) 
        -> simplif(u)
      
        (* v = 0+v = 1*v (!= 0-v, 1/v,) *)
      | Const(0.), Plus, _ 
      | Const(1.), Fois, _ 
        -> simplif(v)

        (* 0 = 0*x = 0/x = x-x = x*0 *)
      | Const(0.), (Fois | Div), _ 
      | X, Moins, X 
      | _, Fois, Const(0.) 
        -> Const(0.)

        (* cste _ cste1 = cste2 *)
      | Const(_), _, Const(_) -> Const(evaltexpr(expr, 0.))

        (* u - v = 0 si u = v ou si leur simplification est la même *)
      | _, Moins, _ ->
        if (u = v || simplif(u) = simplif(v))
        then Const(0.) 
        else Op(simplif(u), o, simplif(v))
      
        (* cas par défaut pour une operation, on essaie de simplifier les operations à l'interieur de celle ci *)
      | _ -> Op(simplif(u), o, simplif(v)) 
      )
      (* on tente de simplifier x dans x^n ou f(x)*)
    | F (f, u) -> F(f, simplif(u))
    | P (u, n) -> P(simplif(u), n)

      (* cas de base, a rien pu simplifier on renvoi l'expression inchangée *)
    | _ -> expr
  in
    (* si on a pu simplifier l'expression ce n'est plus la même, on essaie encore de simplifier jusqu'à ce que l'expression soit inchangée*)
    let r = s(expr)
      in if(r = expr) then r else s(r)


(* Différents tests de la fonction simplif avec les dérivation de e1 et e2 de l'exercice 2 
et 2 autres nouvelles expressions: *)

(* √((1x^2) + (3 + 0)) *)
let e3 = F(Rac, Op( P(Op(X, Fois, Const(1.)), 2), Plus, Op(Const(3.0), Plus, Const(0.))));;

(* (3 + x) - (3 + x) *)
let e4 = Op(Op(Const(3.), Plus, X), Moins, Op(Const(3.), Plus, X));;

stringinf_of_texpr(derive e1);;
stringinf_of_texpr(simplif(derive e1));;

stringinf_of_texpr(derive e2);;
stringinf_of_texpr(simplif(derive e2));;

stringinf_of_texpr(e3);;
stringinf_of_texpr(simplif(e3));;

stringinf_of_texpr(e4);;
stringinf_of_texpr(simplif(e4));;

(* un test de stringpref_of_texpr avec une division par 0 *)
stringpref_of_texpr(Op(Const(1.), Div, Const(0.)));;