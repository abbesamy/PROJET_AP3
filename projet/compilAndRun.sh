rm *.cmo 2> error
rm *.cmi 2> error
rm run 2> error
cp ../utils/*.cmi . 2> error
cp ../utils/*.cmo . 2> error 
ocamlc -c "bst.mli"
ocamlc -c "bst.ml"
ocamlc -c "avl.mli"
ocamlc -c "avl.ml"
ocamlc -c "bstp.mli"
ocamlc -c "bstp.ml"
ocamlc -o run b_trees.cmo bst.cmo avl.cmo bstp.cmo 
./run
# ocaml bstp.ml
# rm *.cmo 2> error
# rm *.cmi 2> error