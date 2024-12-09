(** b_trees simulation for AP3 TP1 @author Samy Abbes *)

(** type for polymorphic binary tree implemented with a sum type *)
type 'a b_trees


(** bt_isempty *)
val bt_isemptys : 'a b_trees -> bool
(** THIS FUNCTION CHECKS IF A BINARY TREE IS EMPTY OR NOT *)

(** bt_empty*)
val bt_emptys : unit -> 'a b_trees
(** THIS FUNCTION CREATES AN EMPTY BINARY TREE *)

(** bt_rootings *)
val bt_rootings : 'a * 'a b_trees * 'a b_trees -> 'a b_trees
(** THIS FUNCTION CREATES A BINARY TREE WITH A ROOT AND TWO SONS *)


(** the root *)
val bt_roots : 'a b_trees -> 'a
(** THIS FUNCTION RETURNS THE ROOT VALUE OF A BINARY TREE *)

(** the right son *)
val bt_rights : 'a b_trees -> 'a b_trees
(** THIS FUNCTION RETURNS THE RIGHT SON OF A BINARY TREE *)

(** the left son *)
val bt_lefts : 'a b_trees -> 'a b_trees
(** THIS FUNCTION RETURNS THE LEFT SON OF A BINARY TREE *)


(** The size of a binary tree*)
val size :'a b_trees -> int
(** THIS FUNCTION RETURNS THE SIZE OF A BINARY TREE *)

(** the max of two elements *)
val max : 'a * 'a -> 'a
(** THIS FUNCTION RETURNS THE MAX OF TWO ELEMENTS, IT IS USED IN THE COMPUTATION OF THE HEIGHT *)

(** the height of a binary tree *)
val  height : 'a b_trees -> int
(** THIS FUNCTION RETURNS THE HEIGHT OF A BINARY TREEE *)

(** the display of a binary tree *)
val btree_to_string : int b_trees -> string
(** THIS FUNCTION DISPLAYS A BINARY TREE IN A FORM A A STRING *)
