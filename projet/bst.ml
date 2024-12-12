(* #directory "../utils";; *)

(* #load "b_trees.cmo";; *)
open B_trees;;

(** type for a binary search tree implemented with the module B_trees *)
type 'a bst = 'a B_trees.b_trees;;


(* Variables pour l'etude de la complexité *)
let nb_calls_bst_seek: int ref = ref 0;;

let get_nb_calls_bst_seek () : int = !nb_calls_bst_seek;;
let reset_calls_bst_seek () : unit = 
  nb_calls_bst_seek := 0
;;

(** checks the presence of a  value in a bst *)
(** THIS FUNCTION CHECHKS IF AN ELEMENT 'v' IS PRESENT IN A BST 'abr' (RETURNS TRUE) OR NOT (RETURNS FALSE) *)
let rec bst_seek(abr, v : ('a * int) bst * 'a) : bool =
  nb_calls_bst_seek := !nb_calls_bst_seek + 1;
  if bt_isemptys(abr)
  then false
  else
    let (x, h :'a * int) = bt_roots(abr) in
    if (v = x)
    then true
    else
      if (v < x)
      then bst_seek(bt_lefts(abr), v)
      else bst_seek(bt_rights(abr), v)
;;



(** insertion of a value *)
(** THIS FUNCTIONS INSERTS AN ELEMENT 'v' IN A BST 't' *)
let rec bst_linsert(v, t : 'a * 'a bst) : 'a bst =
  if bt_isemptys(t)
  then bt_rootings(v, bt_emptys(), bt_emptys())
  else
    let r : 'a = bt_roots(t) in
    if v = r
    then t
    else
      if v < r
      then bt_rootings(r, bst_linsert(v, bt_lefts(t)), bt_rights(t))
      else bt_rootings(r, bt_lefts(t), bst_linsert(v, bt_rights(t)))
;;


(** auxiliary function for bst_lbuild *)
(** USED FOR THE INSERTION OF EACH ELEMENT OF A LIST 'l' IN A BST 'abr' *)
let rec bst_lbuild_aux(l, abr : 'a list * 'a bst) : 'a bst =
  if l = []
  then abr
  else
    let fst : 'a = List.hd(l) in
    let res : 'a bst = bst_linsert(fst, abr) in bst_lbuild_aux(List.tl(l), res)
;;


(** BST Building *)
(** THIS FUNCTION BUILD A BST FROM A LIST 'l' OF ELEMENTS *)
let bst_lbuild(l : 'a list) : 'a bst =
  if l = []
  then bt_emptys()
  else bst_lbuild_aux(l, bt_emptys())
;;


(** the max of a bst *)
(** THIS FUNCTION RETURNS THE MAX OF BST 't' *)
let rec bst_max(t : 'a bst) : 'a =
  if bt_isemptys(t) then failwith "error : no max in empty tree"
  else if bt_isemptys(bt_rights(t)) then bt_roots(t)
  else bst_max(bt_rights(t))
;;


(** the deletion of an element*)
(** THIS FUNCTIONS DELETES AN ELEMENT 'v' FROM A BST 't' *)
let rec bst_delete(v, t : 'a * 'a bst) : 'a bst =
  if bt_isemptys(t) then failwith "error : empty bst !"
  else let r : 'a = bt_roots(t) in
       if v < r
       then bt_rootings(bt_roots(t), bst_delete(v, bt_lefts(t)), bt_rights(t))
       else
         if v > r
         then bt_rootings(bt_roots(t), bt_lefts(t), bst_delete(v, bt_rights(t)))
         
         else if bt_isemptys(bt_rights(t)) then bt_lefts(t)
         else if bt_isemptys(bt_lefts(t)) then bt_rights(t)
         
         else let max : 'a = bst_max(bt_lefts(t)) in
              bt_rootings(max, bst_delete(max, bt_lefts(t)), bt_rights(t))
;;


(** auxiliary function for bst_to_list *)
(** THIS FUNCTION ADD EACH LEAF OF BST 'abr' INTO AN ASCENDING ORDERED LIST 'l' *)
let rec bst_to_list_aux(abr, l : 'a bst * 'a list) : 'a list =
  if bt_isemptys(abr) then l
  else
    let r = bt_roots(abr) and
        res : 'a list = bst_to_list_aux(bt_rights(abr), l)
    in
    bst_to_list_aux(bt_lefts(abr), r::res)
;;


(** List building from a BST *)
(** THIS FUNCTION BUILD A LIST FROME A BST 'abr' *)
let bst_to_list(abr : 'a bst) : 'a list =
    if bt_isemptys(abr) then []
    else
      bst_to_list_aux(abr, [])
;;


(** BST cutting *)
(** THIS FUNCTION CUTS A BST 'abr' FROM ONE OF HIS ELEMENTS 'v' *)
let rec bst_cut (abr, v : 'a bst * 'a) : 'a bst * 'a bst =
    if bt_isemptys(abr) then (bt_emptys(), bt_emptys())
    else
      let r : 'a = bt_roots(abr) in
      if v < r
      then
        let (g, d : 'a bst * 'a bst) = bst_cut (bt_lefts(abr), v) in
        (g, bt_rootings(bt_roots(abr), d, bt_rights(abr)))
      else
        let (g, d : 'a bst * 'a bst) = bst_cut (bt_rights(abr), v) in
        (bt_rootings(bt_roots(abr), bt_lefts(abr), g), d)
;;


(** root insertion *)
(** THIS FUNCTION ADDS AN ELEMENT 'elem' TO A BST 'abr' AS A ROOT *)
let bst_rinsert(abr, elem : 'a bst * 'a) : 'a bst =
  let (lg, ld : 'a bst * 'a bst) = bst_cut(abr, elem) in
  bt_rootings(elem, lg, ld)
;;


(** BST check *)
(** THIS FUNCTION CHECKS IF A TREE 'bst' IS A BST OR NOT *)
let rec bst_isBst(bst : 'a bst) : bool =
  if (bt_isemptys(bst))
  then true
  else
    if(bt_isemptys(bt_rights(bst)) && bt_isemptys(bt_lefts(bst)))
    then true
    else
      let r : 'a = bt_roots(bst) in
      if bt_isemptys(bt_rights(bst)) then (r > bt_roots(bt_lefts(bst)) && bst_isBst(bt_lefts(bst)))
      else
        if bt_isemptys(bt_lefts(bst)) then (r < bt_roots(bt_rights(bst)) && bst_isBst(bt_rights(bst)))
        else
         (r > bt_roots(bt_lefts(bst)) && bst_isBst(bt_lefts(bst)) && r < bt_roots(bt_rights(bst)) && bst_isBst(bt_rights(bst)))
;;
