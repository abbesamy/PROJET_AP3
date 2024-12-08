(*#directory "../utils";;

#load "bst.cmo";;*)
open Bst;;

(* Définition du type my_t comme étant un entier *)
type my_t = int;;

(* Initialisation du générateur de nombres aléatoires *)
Random.self_init ();;

(* Définition de la borne supérieure pour les nombres aléatoires générés *)
let max : int = 50;;

(* Fonction pour générer la liste de nombres aléatoires *)
let rec generate_rand_list_aux (len, list: int * my_t list) : my_t list =
  if len = 0 
  then list  
  else
    let rand_int : my_t = Random.int max in  
    let current_list : my_t list = rand_int :: list in  
    generate_rand_list_aux (len - 1, current_list)  
;;
let generate_rand_list (size: int) : my_t list =
  generate_rand_list_aux (size, [])  
;;

(* Crée un arbre binaire de recherche à partir d'une liste générée aléatoirement. *)
let bst_rnd_create(len: int): int bst =
  let new_list: int list = generate_rand_list(len) in
  bst_lbuild(new_list)
;;