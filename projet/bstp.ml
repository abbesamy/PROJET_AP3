(* #directory "../utils";; *)
(* #load "bst.cmo";; *)
(* #load "b_trees.cmo";; *)
open B_trees;;
open Avl;;
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

(*EXERCICE 2 : 2.2)*)
(* 1. Définissez une fonction avl_rnd_create qui crée des arbres AVL à partir de suites
d’entiers aléatoires et vérifiez expérimentalement que les opérations de recherche,
 d’insertion et de suppression ont bien une complexité en Θ(log n) où n est la taille de
l’arbre. *)

let rec check_complexity(arb, nodelist, s, d, i : 'a avl *int list *int*int*int):int*int*int =
  match nodelist with
  | [] -> (s, d, i) 
  | hd :: tl ->
    reset_calls_bst_seek(); (*remise à 0 du compteur du nombre d'appels de la fonction bst_seek(utilisé aussi pour la recherce dans les avl)*)
    avl_seek(hd, arb);
    let seek_count = get_nb_calls_bst_seek() in (*obtenir le nombre d'appels de la fonction bst_seek*)

    reset_calls_avl_delete_val();
    avl_delete_val(hd, arb);
    let delete_count = get_nb_calls_avl_delete_val() in

    reset_calls_avl_insert_val();
    avl_insert_val(hd, avl_delete_val(hd,arb));
    let insert_count = get_nb_calls_avl_insert_val() in

    check_complexity(arb, tl, s + seek_count, d + delete_count, i + insert_count)
;;

let average_calls_per_node (arb, nodelist : 'a avl * 'a list) : float*float*float =
  let (total_seek, total_delete, total_insert) = check_complexity(arb, nodelist, 0, 0, 0) in
  let n = float_of_int (List.length nodelist) in
  (float_of_int total_seek /. n, float_of_int total_delete /. n, float_of_int total_insert /. n)
;;


let rec process_sizes(sizes : int list) : unit =
  match sizes with
  | [] -> ()
  | size :: rest ->
    let (arb, nodelist) = avl_rnd_create_withListNodes(size) in
    let (avg_seek, avg_delete, avg_insert) = average_calls_per_node (arb, nodelist) in
    Printf.printf 
      "Taille: %d, Nombre d'appels moyens - Recherche: %f, Suppression: %f, Insertion: %f\n"
      size avg_seek avg_delete avg_insert;
    process_sizes (rest) 
;;


let sizes = [10; 100; 1000; 10000] in
process_sizes sizes;;



(* 2. En créant des arbres AVL avec des suites de nombres entiers qui contiennent des sous
suites ordonnées de longueur variable (2), estimez le nombre moyen de rotations qui sont
effectuées pour garder l’arbre équilibré. Comment ce nombre évolue-t-il en fonction de
la taille de l’arbre ? *)


let rec generate_ordered_sublist(n, k : int *int) : int list  =
  if k = 0 then [] else n :: generate_ordered_sublist (n + 1, k - 1)
;;

let rec generate_random_list (n : int) : int list =
  if n = 0 then [] else Random.int 1000 :: generate_random_list (n - 1)
;;

let generate_test_list (size ,  k : int * int) : int list =
  if k > size then failwith "Sous-suite  trop grande pour la taille de la liste";
  let ordered_part = generate_ordered_sublist (1, k) in
  let random_part = generate_random_list (size - k) in
  ordered_part @ random_part
;;

let rec insert_and_count_rotations (arb, lst, total_rotations : 'a avl * int list * int) : int =
  match lst with
  | [] -> total_rotations
  | hd :: tl ->
    reset_calls_rotations(); 
    let arb = avl_insert_val(hd, arb) in 
    let current_rotations = get_nb_calls_rotations() in
    insert_and_count_rotations(arb, tl, (total_rotations + current_rotations))
;;

let average_rotations_per_node (size, k : int * int) : float =
  let test_list = generate_test_list(size, k) in
  let arb = bt_emptys() in 
  let total_rotations = insert_and_count_rotations (arb, test_list, 0) in
  float_of_int (total_rotations) /. float_of_int (size)
;;



let rec process_k_values (sizes, k_values : int list*int list) : unit =
  match sizes with
  | [] -> () 
  | size :: rest_sizes ->
    let rec process_sizes (k_values : int list) : unit =
      match k_values with
      | [] -> () 
      | k :: rest_k_values ->
        let avg_rotations = average_rotations_per_node (size, k) in
        Printf.printf 
        "Taille: %d, Sous-suite ordonnée: %d, Rotations moyennes: %f\n"
          size k avg_rotations;
        process_sizes (rest_k_values) 
    in
    process_sizes (k_values); 
    process_k_values (rest_sizes, k_values) 

let sizes = [100; 1000; 10000;100000];;
let k_values = [2; 5; 10; 50];;
process_k_values (sizes, k_values);;
