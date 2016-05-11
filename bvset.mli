type 'a bvset (* a bit vector set containing elements of type 'a *)

(* Return an empty set that ranges over the elements in the list. *)
val mkempty : 'a list -> 'a bvset

(* Add an element to a set, or do nothing if elt already present.
   Raises an exception if elt not in range of set. *)
val insert : 'a -> 'a bvset -> 'a bvset

(* Remove an element to a set, or do nothing if elt not present.
   Raises an exception if elt not in range of set. *)
val remove : 'a -> 'a bvset -> 'a bvset

(* Return true iff element in set. Raises an exception if elt not in
   range of set. *)
val mem : 'a -> 'a bvset -> bool

(* Return a list of all members of the set. *)
val mems : 'a bvset -> 'a list

(* Set union. Raises an exception if sets not derived from same
   mkempty call. *)
val union : 'a bvset -> 'a bvset -> 'a bvset

(* Set intersection. Raises an exception if sets not derived from same
   mkempty call. *)
val inter : 'a bvset -> 'a bvset -> 'a bvset

(* diff a b returns a - b. Raises an exception if sets not derived
   from same mkempty call. *)
val diff : 'a bvset -> 'a bvset -> 'a bvset

(* negate a returns 1 - a, where 1 is the set of all elements. Thus,
   negate (mkempty l) returns top. *)
val negate : 'a bvset -> 'a bvset

(* Return true iff sets are equal. Raises an exception if sets not
   derived from same mkempty call. *)
val equal : 'a bvset -> 'a bvset -> bool
