#directory "../utils";;
#load "bst.cmo";;
#load "b_trees.cmo";;
open B_trees;;
open Bst;;


type my_t = int;;

Random.self_init;;

let up_bound : int = 50;;

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

let bst_rnd_create(len: int): int bst =
  let new_list: int list = generate_rand_list(len) in
  bst_lbuild(new_list)
;;
