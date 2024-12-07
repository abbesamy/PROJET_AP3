(** b_trees simulation for AP3 TP1 @author Samy Abbes *)

(** type for polymorphic binary tree *)
type 'a b_trees


(** bt_isempty *)
val bt_isemptys : 'a b_trees -> bool
(** THIS FUNCTION VERIFY IF A BINARY TREE IS EMPTY OR NOT *)

(** bt_empty*)
val bt_emptys : unit -> 'a b_trees
(** THIS FUNCTION CREATE AN EMPTY BINARY TREE *)

(** Null b_tree *)
val bt_rootings : 'a * 'a b_trees * 'a b_trees -> 'a b_trees

(** THIS FUNCTION CREATE AN EMPTY BINARY TREE *)


(** the root *)
val bt_roots : 'a b_trees -> 'a
(** THIS FUNCTION RETURN THE ROOT VALUE OF A BINARY TREE *)

(** the right son *)
val bt_rights : 'a b_trees -> 'a b_trees
(** THIS FUNCTION RETURN THE RIGHT SON OF A BINARY TREE *)

(** the left son *)
val bt_lefts : 'a b_trees -> 'a b_trees
(** THIS FUNCTION RETURN THE LEFT SON OF A BINARY TREE *)





