open B_trees;;

type 'a bst = 'a B_trees.b_trees

val bst_seek : 'a bst * 'a -> bool

val bst_linsert : 'a * 'a bst -> 'a bst

val bst_lbuild_aux : 'a list * 'a bst -> 'a bst

val bst_lbuild : 'a list -> 'a bst

val bst_max : 'a bst -> 'a

val bst_delete :  'a * 'a bst -> 'a bst

val bst_isBst : 'a bst -> bool

val bst_rinsert : 'a bst * 'a -> 'a bst
