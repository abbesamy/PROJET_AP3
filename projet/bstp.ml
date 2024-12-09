(*#directory "../utils";;*)
(*#load "bst.cmo";;*)
(*#load "b_trees.cmo";;*)
open B_trees;;
open Bst;;





(* ___ Exercice 1, question 1. ___ *)

(* Type de données utilisé pour les éléments du BST. *)
type my_t = int;;

(* Initialisation aléatoire. *)
Random.self_init;;

(* Limite supérieure pour les nombres aléatoires. *)
let up_bound : int = 100;;

(* Génère une liste aléatoire d'entiers.*)
let rec generate_rand_list_aux(size, l : int * my_t list) : my_t list =
  if size = 0
  then l
  else
    let num : my_t = Random.int up_bound in
    let newl : my_t list = num::l in
    generate_rand_list_aux(size - 1, newl)
;;
let generate_rand_list(size : int) : my_t list = 
 generate_rand_list_aux(size, [])
;;

(* Crée un arbre binaire de recherche à partir d'une liste aléatoire. *)
let bst_rnd_create(len: int): int bst =
  let new_list: int list = generate_rand_list(len) in
  bst_lbuild(new_list)
;;





(* ___ Exercice 1, question 2. ___ *)

(* Fonction pour calculer la valeur maximale entre deux entiers. *)
let max(x, y: int * int): int =
  if x > y
  then x
  else y
;;

(* Fonction pour calculer la hauteur d'un arbre binaire de recherche. *)
let rec hauteur (tree: 'a bst) : int =
  if bt_isemptys(tree) 
  then 0  
  else
    let left_height = hauteur(bt_lefts(tree)) in
    let right_height = hauteur(bt_rights(tree)) in
    1 + max(left_height, right_height)  
;;

(* Fonction pour calculer le déséquilibre d'un arbre binaire de recherche. *)
let bst_imbalance (tree: 'a bst) : int =
  if bt_isemptys(tree) 
  then 0
  else
    let left_height = hauteur(bt_lefts(tree)) in
    let right_height = hauteur(bt_rights(tree)) in
    abs(left_height - right_height)
;;

(* Fonction pour estimer le déséquilibre moyen d'arbres binaires de recherche. *)
let estimate_average_imbalance (num_trees: int) : float =
  let rec compute_average (remaining_trees, total_imbalance: int * int) : float =
    if remaining_trees = 0 
    then float_of_int(total_imbalance) /. float_of_int(num_trees)
    else
      let tree = bst_rnd_create(100) in 
      let imbalance = bst_imbalance(tree) in
      compute_average (remaining_trees - 1, total_imbalance + imbalance)
  in
  compute_average (num_trees, 0)
;;

(* Calcul moyenne pour 100 000 arbres binaires de recherches aléatoires. *)
let average_imbalance = estimate_average_imbalance(10000);;
print_endline ("Le déséquilibre moyen pour 10 000 arbres généré avec des listes aléatoires est : " ^ string_of_float average_imbalance);;





(* ___ Exercice 1, question 3. ___ *)

let generate_random_ordered_list (size, max_value : int * int) : int list =
  let rec generate_elements remaining_elements accumulated_list =
    if remaining_elements = 0 then accumulated_list
    else
      let random_value = Random.int (max_value + 1) in
      generate_elements (remaining_elements - 1) (random_value :: accumulated_list)
  in
  let unsorted_list = generate_elements size [] in
  List.sort compare unsorted_list 
;;

let generate_random_ordered_descending_list (size, max_value : int * int) : int list =
  let rec generate_elements remaining_elements accumulated_list =
    if remaining_elements = 0 then accumulated_list
    else
      let random_value = Random.int (max_value + 1) in
      generate_elements (remaining_elements - 1) (random_value :: accumulated_list)
  in
  let unsorted_list = generate_elements size [] in
  List.sort (fun x y -> compare y x) unsorted_list  
;; 

(* Fonction pour calculer le déséquilibre moyen pour des arbres générés à partir de listes ordonnées. *)
let estimate_average_imbalance_ordered (num_trees, size, max_value : int * int * int) : float =
  let rec compute_average (remaining_trees, total_imbalance: int * int) : float =
    if remaining_trees = 0 
    then float_of_int(total_imbalance) /. float_of_int(num_trees)
    else
      let ordered_list = generate_random_ordered_list (size, max_value) in
      let tree = bst_lbuild(ordered_list) in
      let imbalance = bst_imbalance tree in
      compute_average (remaining_trees - 1, total_imbalance + imbalance)
  in
  compute_average (num_trees, 0)
;;

(* Fonction pour calculer le déséquilibre moyen pour des arbres générés à partir de listes ordonnées décroissantes. *)
let estimate_average_imbalance_descending (num_trees, size, max_value : int * int * int) : float =
  let rec compute_average (remaining_trees, total_imbalance: int * int) : float =
    if remaining_trees = 0 
    then float_of_int(total_imbalance) /. float_of_int(num_trees)
    else
      let descending_list = generate_random_ordered_descending_list (size, max_value) in
      let tree = bst_lbuild(descending_list) in
      let imbalance = bst_imbalance tree in
      compute_average (remaining_trees - 1, total_imbalance + imbalance)
  in
  compute_average (num_trees, 0)
;;

(* Utilisation : *)
let average_imbalance_ordered = estimate_average_imbalance_ordered (10000, 100, 100);;
print_endline ("Le déséquilibre moyen pour 10 000 arbres avec des listes ordonnées par ordre croissant est : " ^ string_of_float average_imbalance_ordered);;

let average_imbalance_descending = estimate_average_imbalance_descending (10000, 100, 100);;
print_endline ("Le déséquilibre moyen pour 10 000 arbres avec des listes ordonnées par ordre décroissantes est : " ^ string_of_float average_imbalance_descending);;