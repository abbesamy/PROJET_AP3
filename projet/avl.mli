(** IMPORTS *)
open Bst;;


(** TYPE DEFINITION *)

type 'a avl = ('a * int) bst;;


(** PART 1 :  IMPLEMENTATION OF THE AVLs AND UTILITY FUNCTION *)


(** THIS FUNCTION RETURNS THE HEIGHT OF AN AVL *)
val avl_get_node_height : 'a avl -> int

(** THIS FUNCTION COMPUTES THE UNBALANCE OF AN AVL 'avl' *)
val avl_compute_unbalance : 'a avl -> int

(**  THIS FUNCTION CREATES AN AVL FROM A ROOT AND TWO SONS *)
val avl_rooting : ('a * int) * 'a avl * 'a avl -> 'a avl

(**  THIS FUNCTION UPDATES THE HEIGHT OF AN AVL  *)
val avl_new_node_height : 'a avl -> 'a avl


(****************** ROTATIONS  *********************)

(** RIGHT ROTATION *)
val r_rot : 'a avl -> 'a avl

(** LEFT ROTATION *)
val l_rot : 'a avl -> 'a avl

(** RIGHT of left ROTATION *)
val lr_rot : 'a avl -> 'a avl

(** LEFT of right ROTATION *)
val rl_rot : 'a avl -> 'a avl



(*******************  REBALANCE, DELETION AND INSERTION   *****************)


(** THIS FUNCTION REBALANCES AN AVL, IT IS USED AFTER THE INSERTIONS *)
val avl_rebalance : 'a avl -> 'a avl

(** THIS FUNCTION DELETES THE MAXIMUM VAL OF AN AVL *)
val avl_delete_max : 'a avl -> 'a avl

(** THIS FUNCTION RETURNS THE VALUE OF THE ROOT OF AN AVL *)
val avl_get_val_node : 'a avl -> 'a

(**  THIS FUNCTION RETURNS THE MAXIMUM VAL OF AN AVL *)
val avl_max_val : 'a avl -> 'a

(** THIS FUNCTION DELETES AN ELEMENT FROM AN AVL *)
val avl_delete_val : 'a * 'a avl -> 'a avl


(** THIS FUNCTION INSERTS AN ELEMENT IN AN AVL *)
val avl_insert_val : 'a * 'a avl -> 'a avl

(** THIS FUNCTION CHECKS IF AN ELEMENT IS PRESENT IN AN AVL  *)
val seek_avl : 'a * 'a avl -> bool




(**  --------------------- PART 2 : RANDOM CREATIONS----------------  *)

(** THIS FUNCTION IS USED TO GENERATE AN INT LIST OF SIZE 'size'  *)

(** THIS FUNCTION IS USED IN generate_rand_list TO GENERATE AN INT LIST *)
val generate_rand_list_aux : int * int list -> int list

(** THIS FUNCTION GENERATES AN INT LIST *)
val generate_rand_list : int -> 'a list


(** AUXILLIARY FUNCTION USED TO CREATE AN AVL FROM AN 'a LIST  *)
val avl_rnd_create_aux : 'a list * 'a avl -> 'a avl

(** THIS FUNCTION CREATES AN AVL FROM AN 'a LIST *)
val avl_rnd_create : 'a list -> 'a avl

(** THIS FUNCTION DISPLAYS AN AVL IN A FORM OF A STRING, IT IS USED FOR TESTING *)
val avl_to_string : 'a avl -> string
