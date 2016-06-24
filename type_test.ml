type typ =
  [`One of int
  | `Two of int]
;;


type typ1 =
  | One of int
  | Two of int
;;

let f0 t=
  match t with
  | One i -> t
;;

type typ2 =
  | One of int
  | Three of string
;;

(* 
   OK. Actually labels with same name are allowed. The previous label will be covered.
   At this point, the lable One will be recognized as typ2 (in inference).
*)
  
(*
   Q1: What is type for a function when there is only one constructor?
*)
let f1 t =
  match t with
  | One i -> "This is one"
;;

let f1' = function
    One i -> `One 1
;;

let f2 t =
  match t with
  | Two i -> ();;


  
let f (t:typ) =
  match t with
  | `One i -> "This is one"

;;
print_endline (f (`One 1))
;;
  
(* Q2: Is constructor always necessary? *)
type q2t1 =
  QT1
| QT2
;;

(* Equlity is only allowed like this *)
type q2t2 =
  q2t1

type q2t2 =
  | Q2T1 of q2t1
  | Q2T3
;;
    
(* The following is not allowed *)
(* type q2t2 = *)
(*   q2t1 *)
(* | QT3 *)


(* The most common senario of using polymophic variant *)
type typ3 =
  Ten of int
| Eleven of int
;;
let f3 t =
  match t with
    `T1 t1 ->
    (
      match t1 with
      | Ten _ -> 10
      | Eleven _ -> 11
    )
  | `T2 t2 ->
     (
       match t2 with
       | One t21 -> 21
       | Three t22 -> 22
     )
;;

type t1 = String of string | Int of int | Bool of bool | List of t1 list
type t2 = String of string | Int of int | Other

let simplify x =
  match x with
      String s -> String s
    | Int n -> Int n
    (* | Bool _ *)
    (* | List _ -> Other *)
  
