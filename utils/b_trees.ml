type 'a b_trees =
 | NULL
 | NODE of 'a * 'a b_trees * 'a b_trees
;;

let bt_emptys() : 'a b_trees =
   let bt : 'a b_trees = NULL in
   bt
;;

let bt_isemptys(bt : 'a b_trees) : bool =
  bt = NULL
;;

let bt_rootings(v, btl, btr : 'a * 'a b_trees * 'a b_trees) : 'a b_trees =
  let bt : 'a b_trees = NODE (v, btl, btr) in
  bt
;;

let bt_roots(bt : 'a b_trees) : 'a = 
   match bt with
    | NULL -> failwith "erreur : arbre vide"
    | NODE(v,btl,btr) -> v
;;

let bt_lefts(bt : 'a b_trees) : 'a b_trees = 
   match bt with
    | NULL -> NULL
    | NODE(v,btl,btr) -> btl
;;

let bt_rights(bt : 'a b_trees) : 'a b_trees = 
   match bt with
    | NULL -> NULL
    | NODE(v,btl,btr) -> btr
;;

