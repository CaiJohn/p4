(* Put your solution to part 1 in this file *)

(* A bitvector set is implemented as a pair (range, mems), where

   range is an array containing the possible members of the set, where
         the element at range.(i) is represented by bit i in mems
   mems is an array of bits representing membership in the set, where
         1 = is in the set and 0 = is not in the set

   For example,

   ([|'a', 'b', 'c'|], [|1|]) represents the set {'a'}
   ([|'a', 'b', 'c'|], [|5|]) represents the set {'a', 'c'}

  Do not change this type.
*)

(*

USE THIS TYPE FOR bvset:

type 'a bvset = ('a,int) Hashtbl.t * (int,'a) Hashtbl.t * int array
*)


(* Below here is a non-bitvector implementation of sets, in case you
   want to skip this part of the project and get started on the other
   parts. Ultimately, you should comment out this code and replace it
   with your own bvset implementation, using the right type.

   Note that this implementation doesn't do range checking, but yours
   should.*)

type 'a bvset = 'a list * 'a list (* all elts, cur elts *)

let mkempty all = (all, [])

let insert x (all, elts) =
  if List.mem x elts then (all, elts) else (all, x::elts)

let remove x (all, elts) =
  (all, List.filter (fun y -> x <> y) elts)

let mem x (all, elts) = List.mem x elts

let mems (all, elts) = elts

let union (all1, elts1) (all2, elts2) =
  (all1, (List.fold_left (fun l x -> if List.mem x l then l else x::l) elts1 elts2))

let inter (all1, elts1) (all2, elts2) =
  (all1, List.fold_left (fun l x -> if List.mem x elts2 then x::l else l) [] elts1)

let diff (all1,elts1) (all2,elts2) =
  (all1, List.fold_left (fun l x -> if List.mem x elts2 then l else x::l) [] elts1)

let negate (all,elts) = diff (all,all) (all,elts)

let empty (all, elts) = elts = [] (* not required in interface *)

let equal s1 s2 =
  (empty (diff s1 s2)) && (empty (diff s2 s1))
