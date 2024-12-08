(* Interface du module *)
open Bst;;

(* Définition du type my_t comme étant un entier *)
type my_t = int

(* Fonction pour générer une liste de nombres aléatoires de taille 'size' *)
val generate_rand_list : int -> my_t list

(* Crée un arbre binaire de recherche (BST) à partir d'une liste générée aléatoirement *)
val bst_rnd_create : int -> int bst
