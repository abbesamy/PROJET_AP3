(** IMPORTS *)

#load "bst.cmo";;
open Bst;;


(** TYPE DEFINITION *)

type 'a avl = ('a * int) bst;;


(** --------------PART 1 : IMPLEMENTATION OF THE AVLs AND UTILITY FUNCTION--------------- *)


(** THIS FUNCTION RETURNS THE HEIGHT OF AN AVL 'avl' *)
let avl_get_node_height(avl : 'a avl) : int =
  if bt_isemptys(avl) then 0
  else
    let (rval,des : 'a * int) = bt_roots(avl) in
    des
;;

(** THIS FUNCTION COMPUTES THE UNBALANCE OF AN AVL 'avl' *)
let avl_compute_unbalance(avl : 'a avl) : int =
  if bt_isemptys(avl)
  then 0
  else avl_get_node_height(bt_lefts(avl)) - avl_get_node_height(bt_rights(avl))
;;

(**  THIS FUNCTION CREATES AN AVL FROM A ROOT 'r' AND TWO SONS 'g' and 'd' *)
let rec avl_rooting(r, g, d : ('a * int) * 'a avl * 'a avl) : 'a avl =
  bt_rootings(r, g, d)
;;

(**  THIS FUNCTION UPDATES THE HEIGHT OF AN AVL 'avl' *)
let avl_new_node_height(avl : 'a avl) : 'a avl =
  if bt_isemptys(avl)
  then avl
  else
    let ((rval, des), g, d : ('a * int) * 'a avl * 'a avl) = (bt_roots(avl), bt_lefts(avl), bt_rights(avl)) in
    let new_h : int = 1 + max(avl_get_node_height(g), avl_get_node_height(d)) in (** new height computing *)
    bt_rootings((rval, new_h), g, d)
;;


(****************** ROTATIONS  *********************)

(** RIGHT ROTATION *)
let r_rot(avl : 'a avl) : 'a avl =
  if (bt_isemptys(avl) || bt_isemptys(bt_lefts(avl)))
  then failwith "empty avl or empty left avl in r_rot"
  else
    let (rootval, lroot : ('a * int) * ('a * int)) = bt_roots(avl), bt_roots(bt_lefts(avl))in (** the roots to move *)
    let (lavl_l, ravl_l, ravl : 'a avl * 'a avl * 'a avl) = (** the avls to move *)
      bt_lefts(bt_lefts(avl)), bt_rights(bt_lefts(avl)), bt_rights(avl) in
    avl_new_node_height(avl_rooting(lroot, (** new root *)
                                    lavl_l, (** new left son *)
                                    avl_new_node_height(avl_rooting(rootval, ravl_l, ravl)))) (** new right son *)
;;

(** LEFT ROTATION *)
let l_rot(avl : 'a avl) : 'a avl =
  if (bt_isemptys(avl) || bt_isemptys(bt_rights(avl)))
  then failwith "empty avl or empty right avl in l_rot"
  else
    let (rootval, rroot : ('a * int) * ('a * int)) = bt_roots(avl), bt_roots(bt_rights(avl))in (** the roots to move *)
    let (lavl_r, ravl_r, lavl : 'a avl * 'a avl * 'a avl) = (** the avls to move *)
      bt_lefts(bt_rights(avl)), bt_rights(bt_rights(avl)), bt_lefts(avl) in
    avl_new_node_height(avl_rooting(rroot, (** new root*)
                                    avl_new_node_height(avl_rooting(rootval, lavl, lavl_r)), (** new left son *)
                                    ravl_r)) (** new right son *)
;;

(** RIGHT of left ROTATION *)
let lr_rot(avl : 'a avl) : 'a avl =
  let (r, g, d) = bt_roots(avl), bt_lefts(avl), bt_rights(avl) in
  r_rot(avl_rooting(r, l_rot(g), d)) (** left rotation + right rotation *)
;;

(** LEFT of right ROTATION *)
let rl_rot(avl : 'a avl) : 'a avl =
  let (r, g, d) = bt_roots(avl), bt_lefts(avl), bt_rights(avl) in
  l_rot(avl_rooting(r, g, r_rot(d))) (** right rotation + left rotation *)
;;


(*******************  REBALANCE, DELETION AND INSERTION   *****************)


(** THIS FUNCTION REBALANCES AN AVL 'avl', IT IS USED AFTER THE INSERTIONS *)
let avl_rebalance( avl : 'a avl) : 'a avl =
  let unbalance : int = avl_compute_unbalance(avl) in
  if (unbalance = 0 || unbalance = -1 || unbalance = 1) (** if the avl is not unbalanced *)
  then avl
  else
    if unbalance = 2 || unbalance = -2 (** if the avl is unbalanced *)
    then
      (
        if unbalance = 2 (** case 1 : unbalance = 2 *)
        then
          (
            if avl_compute_unbalance(bt_lefts(avl)) = 1
            then r_rot(avl)
            else lr_rot(avl) (** case = -1 *)
          )
        else  (** case 2 : unbalance = -2 *)
          (
            if avl_compute_unbalance(bt_rights(avl)) = -1
            then l_rot(avl)
            else rl_rot(avl) (** case = 1 *)
          )
      )
        (**  normally we should never get here  *)
    else failwith "unbalance value can't be handled int avl_rebalance"
;;


(** THIS FUNCTION DELETES THE MAXIMUM VAL OF AN AVL 'avl' *)
 let rec avl_delete_max(avl : 'a avl) : 'a avl =
  if bt_isemptys(avl)
  then failwith "empty avl in avl_delete_max"
  else
    if bt_isemptys(bt_rights(avl))
    then bt_lefts(avl)
    else avl_rebalance(avl_new_node_height(avl_rooting( (** rebalance + height updating + rooting *)
                                               bt_roots(avl), (** new root *)
                                               bt_lefts(avl), (** new left son *)
                                               avl_delete_max(bt_rights(avl))))) (** new right son *)
;;

(** THIS FUNCTION RETURNS THE VALUE OF THE ROOT OF AN AVL 'avl' *)
let avl_get_val_node(avl : 'a avl) : 'a =
  let (node, h : 'a * int) = bt_roots(avl) in
  node
;;


(**  THIS FUNCTION RETURNS THE MAXIMUM VAL OF AN AVL 'avl' *)
let rec avl_max_val(avl : 'a avl) : 'a =
  if bt_isemptys(avl) then failwith "empty avl in avl_max_val"
  else
    if bt_isemptys(bt_rights(avl)) then avl_get_val_node(avl)
    else
      avl_max_val(bt_rights(avl))
;;

(** THIS FUNCTION DELETES THE ELEMENT WITH THE VALUE 'v' FROM AN AVL 'avl' *)
let rec avl_delete_val(v, avl : 'a * 'a avl) : 'a avl =
  if bt_isemptys(avl)
  then failwith "empty avl in avl_delete_val"
  else
    let ((node, h), g, d) : (('a * int) * 'a avl * 'a avl) = bt_roots(avl), bt_lefts(avl), bt_rights(avl) in
    if v < node
    then avl_rebalance( (** rebalance *)
             avl_new_node_height( (** height updating *)
                 avl_rooting( (** rooting *)
                     (node, h), (** root *)
                     avl_delete_val(v, g), (** deletion here : left son *) 
                     d))) (** right son *)
    else
      if v > node
      then avl_rebalance(
               avl_new_node_height(
                   avl_rooting((node, h),
                               g,
                               avl_delete_val(v, d)))) (** deletion here : right son *)
      else (** ca v = node *)
        if bt_isemptys(d) (**deletion of the root *)
        then g
        else
          if bt_isemptys(g) (**deletion of the root *)
          then d
          else avl_rebalance( (** deletion of the root with the rebalance of the left son *)
                   avl_new_node_height(
                       avl_rooting(
                           (avl_max_val(g), h),
                           avl_delete_max(g),
                           d)))
;;

(** THIS FUNCTION INSERTS THE ELEMENT WITH THE VALUE 'v' IN AN AVL 'avl' *)
let rec avl_insert_val(v, avl : 'a * 'a avl) : 'a avl =
  if bt_isemptys(avl)
  then avl_rooting((v, 0), bt_emptys(), bt_emptys())
  else
    let ((node, h), g, d) = bt_roots(avl), bt_lefts(avl), bt_rights(avl) in
    if v < node
    then avl_rebalance( (** rebalance *)
             avl_new_node_height( (** height updating *)
                 avl_rooting(  (** root *)
                     (node, h), (** left son *)
                     avl_insert_val(v, g), (** insertion here : left son *)
                     d))) (** right son *)
    else
      if v > node
      then avl_rebalance(  (** rebalance *)
               avl_new_node_height( (** height updating *)
                   avl_rooting( (** rooting *)
                       (node, h), (** root *)
                       g, (** left son *)
                       avl_insert_val(v, d)))) (** insertion here : right son *)
      else avl (** case v = node *)
;;


(** THIS FUNCTION CHECKS IF AN ELEMENT WITH THE VALUE 'elem' IS PRESENT IN AN AVL 'avl' *)
(** THE CODE OF THE FUNCTION BST_SEEK WAS ADAPTED  *)
let rec avl_seek (elem, avl : 'a * 'a avl) : bool =
  bst_seek(avl, elem)
;;




(** ---------- PART 2 : RANDOM CREATIONS  --------------*)

(** VARIABLE USED AS BOUND IN RAND.INT  *)
let up_bound : int = 200;;

(** INITIALISATION OF THE GENERATOR OF RANDOM INTs  *)
Random.self_init;;


(** THIS FUNCTION IS USED IN generate_rand_list TO GENERATE AN INT LIST OF SIZE 'size'  *)
let rec generate_rand_list_aux(size, l : int * int list) : int list =
  if size = 0
  then l
  else
    let num : int = Random.int up_bound in
    let newl : int list = num::l in
    generate_rand_list_aux(size - 1, newl)
;;

(** THIS FUNCTION GENERATES AN INT LIST OF SIZE 'size' *)
let generate_rand_list(size : int) : 'a list =
 generate_rand_list_aux(size, [])
;;



(** AUXILLIARY FUNCTION USED TO CREATE AN AVL 't' FROM AN 'a LIST 'l'  *)
let rec avl_rnd_create_aux(l, t : 'a list * 'a avl) : 'a avl =
  if l = []
  then avl_rebalance(t)
  else
    avl_rnd_create_aux(List.tl(l), avl_insert_val(List.hd(l), t))
;;

(** THIS FUNCTION CREATES AN AVL FROM AN 'a LIST 'l' *)
let avl_rnd_create(len : int) : 'a avl =
  let l : int list = generate_rand_list(len) in
  let t : 'a avl = avl_rooting((List.hd(l), 0), bt_emptys(), bt_emptys()) in
  avl_rnd_create_aux(List.tl(l), t)
;;

(** THIS FUNCTION DISPLAYS AN AVL 'avl' IN A FORM OF A STRING, IT IS USED FOR TESTING *)
let rec avl_to_string(avl : 'a avl) : string =
  if bt_isemptys(avl)
  then "avl_vide()"
  else
    " ("^ "(" ^ string_of_int(avl_get_val_node(avl)) ^", " ^ string_of_int(avl_get_node_height(avl)) ^ " ) "^ ", " ^ avl_to_string(bt_lefts(avl)) ^ ", " ^ avl_to_string(bt_rights(avl)) ^ ") "
;;


(** WE TESTED AND TRACED THE FUNCTIONS avl_seek, avl_delete_val and avl_insert_val AND WE SAW THAT THE COMPLEXITY OF THE OPERATION IS INDEED IN LOG(n) WHERE n IS THE SIZE OF THE AVL *)


