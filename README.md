Il y a ici le module B_trees des arbre binaires implantées avec un type somme ainsi que le module Bst du TP4 
avec toutes les fonction nécessaires pour faire la partie des ABR du projet, comme ca on aura tous la meme base pour travailler sur le projet 
et ne pas se mélanger.
On ajoutera les modules et fonctions dont on aura besoin au fur et à mesure de l'avancement.

il faut compiler avec -I ../utils/ pour ajouter le module B_trees qui se trouve dans un repertoire différents que Bst (organisation Ze3ma)
code : ocamlc -I ../utils/ -c bst.mli
       ocamlc -I ../utils/ -c bst.ml
Et il faut ajouter un "open B_trees" dans bst.mli pour que ca compile normalement. Celui dans le .ml ne suffit pas car les répertoires sont différents.
