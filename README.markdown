# Une implémentation de l'algorithme de Shor

Vous trouverez ci-joint une implémentation de l'algorithme de Shor
que j'ai écrite dans le cadre de mes TIPE en OCaml.

## Liste des fichiers

 - main.ml : fichier principal fait des vérifications, et autres 
interactions avec l'utilisateur.
 - arithmetik.ml : toutes les fonctions arithmétiques dont la 
recherche de l'ordre.
 - quantum.ml : implémentation de registre quantique.
 - matrixFactory.ml : module de matrice générique.
 - log.ml : gestion des entrées et sorties utilisateurs, spécifie
un mode de deboggage ou non (très simplet).

## Makefile

Pour générer l'exécutable il suffit de faire un :
    
    make

suivit d'un :

    ./shor

pour exécuter l'algorithme, noter toutefois qu'il est aussi possible
de rentrer directement les deux nombres choisis directement en entrée :

    ./shor 15 8

Par exemple pour factoriser 15 quand le nombre dont on cherche l'ordre 
est 8.
