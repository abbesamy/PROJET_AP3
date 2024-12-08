open B_trees;;


(** type for  a binary search tree (BST) implemented with the module B_trees *)
type 'a bst = 'a B_trees.b_trees


(** checks the presence of a value in a bst *)
val bst_seek : 'a bst * 'a -> bool
(** THIS FUNCTION CHECHKS IF AN ELEMENT IS PRESENT IN A BST (RETURNS TRUE) OR NOT (RETURNS FALSE) *)


(** insertion of a value *)
val bst_linsert : 'a * 'a bst -> 'a bst
(** THIS FUNCTIONS INSERTS AN ELEMENT IN A BST *)


(** auxiliary function for bst_lbuild *)
val bst_lbuild_aux : 'a list * 'a bst -> 'a bst
(** USED FOR THE INSERTION OF EACH ELEMENT OF A LIST IN A BST *)


(** BST Building *)
val bst_lbuild : 'a list -> 'a bst
(** THIS FUNCTION BUILD A BST FROM A LIST OF ELEMENTS *)


(** the max of a bst *)
val bst_max : 'a bst -> 'a
(** THIS FUNCTION RETURNS THE MAX OF BST *)


(** the deletion of an element*)
val bst_delete :  'a * 'a bst -> 'a bst
(** THIS FUNCTIONS DELETES AN ELEMENT FROM A BST *)


(** auxiliary function for bst_to_list *)
val bst_to_list_aux : 'a bst * 'a list -> 'a list
(** THIS FUNCTION ADD EACH LEAF OF BST INTO AN ASCENDING ORDERED LIST *)


(** List building from a BST *)
val bst_to_list : 'a bst -> 'a list
(** THIS FUNCTION BUILD A LIST FROME A BST *)


(** BST cutting *)
val bst_cut : 'a bst * 'a -> 'a bst * 'a bst
(** THIS FUNCTION CUTS A BST FROM ONE OF HIS ELEMENTS *)


(** BST check *)
val bst_isBst : 'a bst -> bool
(** THIS FUNCTION CHECKS IF A TREE IS A BST OR NOT *)


(** root insertion *)
val bst_rinsert : 'a bst * 'a -> 'a bst
(** THIS FUNCTION ADDS AN ELEMENT TO A BST AS A ROOT *)
