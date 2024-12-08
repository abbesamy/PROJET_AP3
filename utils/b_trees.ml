
(** type for polymorphic binary tree implemented with a sum type *)
type 'a b_trees =
 | NULL
 | NODE of 'a * 'a b_trees * 'a b_trees
;;

(** bt_emptys *)
(** THIS FUNCTION CREATES AN EMPTY BINARY TREE *)
let bt_emptys() : 'a b_trees =
   let bt : 'a b_trees = NULL in
   bt
;;

(** the empty check *)
(** THIS FUNCTION CHECKS IF A BINARY TREE 'bt' IS EMPTY OR NOT *)
let bt_isemptys(bt : 'a b_trees) : bool =
  bt = NULL
;;

(** the rooting of a binary tree *)
(** THIS FUNCTION CREATES A BINARY TREE FROM A ROOT 'v' AND TWO SONS 'btl' & 'btr' *)
let bt_rootings(v, btl, btr : 'a * 'a b_trees * 'a b_trees) : 'a b_trees =
  let bt : 'a b_trees = NODE (v, btl, btr) in
  bt
;;


(** the root *)
(** THIS FUNCTION RETURNS THE ROOT VALUE OF A BINARY TREE 'bt' *)
let bt_roots(bt : 'a b_trees) : 'a =
   match bt with
    | NULL -> failwith "erreur : arbre vide"
    | NODE(v,btl,btr) -> v
;;


(** the left son *)
(** THIS FUNCTION RETURNS THE LEFT SON OF A BINARY TREE 'bt' *)
let bt_lefts(bt : 'a b_trees) : 'a b_trees =
   match bt with
    | NULL -> NULL
    | NODE(v,btl,btr) -> btl
;;

(** the right son *)
(** THIS FUNCTION RETURNS THE RIGHT SON OF A BINARY TREE 'bt' *)
let bt_rights(bt : 'a b_trees) : 'a b_trees =
   match bt with
    | NULL -> NULL
    | NODE(v,btl,btr) -> btr
;;


(** The size of a binary tree*)
(** THIS FUNCTION RETURNS THE SIZE OF A BINARY TREE 'bt' *)
let rec size(bt : 'a b_trees) : int =
if (bt_isemptys(bt))
then 0
else 1 + size(bt_rights(bt)) + size(bt_lefts(bt))
;;

(** the max of two elements *)
(** THIS FUNCTION RETURNS THE MAX OF TWO ELEMENTS 'x' & 'y', IT IS USED IN THE COMPUTATION OF THE HEIGHT OF A BINARY TREE *)
let max(x, y : 'a * 'a) : 'a =
if x < y
then y
else x
;;


(** the height of a binary tree *)
(** THIS FUNCTION RETURNS THE HEIGHT OF A BINARY TREEE 'bt' *)
let rec height(bt : 'a b_trees) : int =
if (bt_isemptys(bt_lefts(bt)) && bt_isemptys(bt_rights(bt)))
then 0
else (1 + max(height(bt_lefts(bt)), height(bt_rights(bt))))
;;


(** The display of a binary tree *)
(** THIS FUNCTION DISPLAYS A BINARY TREE 'bt' IN THE FORM OF A STRING *)
let rec btree_to_string(bt : 'a b_trees) : string =
if bt_isemptys(bt)
then "a_vide()"
else " (" ^ string_of_int(bt_roots(bt)) ^ ", " ^ btree_to_string(bt_lefts(bt)) ^ ", " ^ btree_to_string(bt_rights(bt)) ^ ") "
;;
