(*#directory "../utils";;

#load "b_trees.cmo";;*)
open B_trees;;

type 'a bst = 'a B_trees.b_trees;;

let rec bst_seek(abr, v : 'a bst * 'a) : bool =
  if bt_isemptys(abr)
  then false
  else
    let (x :'a) = bt_roots(abr) in
    if (v = x)
    then true
    else
      if (v < x)
      then bst_seek(bt_lefts(abr), v)
      else bst_seek(bt_rights(abr), v)
;;

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


let rec bst_lbuild_aux(l, abr : 'a list * 'a bst) : 'a bst =
  if l = []
  then abr
  else
    let fst : 'a = List.hd(l) in
    let res : 'a bst = bst_linsert(fst, abr) in bst_lbuild_aux(List.tl(l), res)
;;


let bst_lbuild(l : 'a list) : 'a bst =
  if l = []
  then bt_emptys()
  else bst_lbuild_aux(l, bt_emptys())
;;


let rec bst_max(t : 'a bst) : 'a =
  if bt_isemptys(t) then failwith "error : no max in empty tree"
  else if bt_isemptys(bt_rights(t)) then bt_roots(t)
  else bst_max(bt_rights(t))
;;


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


let rec bst_to_list_aux(abr, l : 'a bst * 'a list) : 'a list =
  if bt_isemptys(abr) then l
  else
    let r = bt_roots(abr) and
        res : 'a list = bst_to_list_aux(bt_rights(abr), l)
    in
    bst_to_list_aux(bt_lefts(abr), r::res)
;;


let bst_to_list(abr : 'a bst) : 'a list =
    if bt_isemptys(abr) then []
    else
      bst_to_list_aux(abr, [])
;;


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

let bst_rinsert(abr, elem : 'a bst * 'a) : 'a bst =
  let (lg, ld : 'a bst * 'a bst) = bst_cut(abr, elem) in
  bt_rootings(elem, lg, ld)
;;

let rec bst_to_string(abr : 'a bst ) : string =
   if bt_isemptys(abr)
   then "a_vide()"
   else " (" ^ string_of_int(bt_roots(abr)) ^ ", " ^ bst_to_string(bt_lefts(abr)) ^ ", " ^ bst_to_string(bt_rights(abr)) ^ ") "
;;


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


