# Minilucy

- Projet MPRI pour le cours [systèmes synchrones](https://www.di.ens.fr/~pouzet/cours/synchrone/).
- Groupe : Émilien Lemaire et Paul Patault
- Enseignants : T. Bourke et M. Pouzet
- Sujet : [minilucy.pdf](./pdf/minilucy.pdf)

## Usage

### Dépendances

- opam (`bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"`)
- bibliothèque GoblintCil : arbre de syntaxe abstraite de `C`
- système de build `dune` pour ocaml
- générateur de parseur `menhir`

Installation :
```
$ opam switch create minilucy 4.14.0 --yes
$ eval $(opam env --switch=minilucy)
$ opam install dune menhir goblint-cil --yes
$ dune build
```

### Utilisation par exemple

- Suite `3n+1` :
  ```
  $ make
  $ ./minilucy.exe demo/101-syracuse.lus main0 -no-nl
  $ gcc demo/101-syracuse.c
  $ ./a.out 13
  ```

- Reset :
  ```
  $ ./minilucy.exe demo/11-reset.lus main0 -no-nl
  $ gcc demo/11-reset.c
  $ echo "0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0" | ./a.out
  ```

### Utilisation générique

Commande générique :
```
$ make
$ ./minilucy.exe [file.lus] [OPTIONS] [main-node]
```
où `OPTIONS` :
- `-v` | `-verbose` : verbose (affiches les ast successifs)
- `-parse-only` : arrêt après le parsing
- `-type-only` : arrêt après le typage
- `-automaton-only` : arrêt après la compilation des automates
- `-clock-only` : arrêt après la vérification des horloges
- `-norm-only` : arrêt après la normalisation
- `-sched-only` : arrêt après le scheduling
- `-imp-only` : arrêt après la traduction vers `imp`
- `-c-only` : arrêt après la traduction vers `C`
- `-no-sleep` : n'ajoute pas la fonction `sleep(1)` dans le main du fichier `C`
- `-no-nl` : n'ajoute pas de `\n` dans le `printf` du `while` du main du fichier `C`

Sinon, `make` fabriquera un exécutable dans le répertoire principal, utilisable ensuite avec :
```
$ make
$ cd examples
$ ../minilucy.exe [file].lus -v [main-node] [options...]
```

Gestion des tests :
```
$ make test --always-make    # si sortie vide alors les tests passent
$ make promote -i            # mise à niveau de l'ensemble des tests
```

## Travail réalisé

- `when`, `merge` (sur ADT simples), `reset`, `automates` : explications de fonctionnement présentées dans les slides (cf. [slides.pdf](./tex/slides.pdf))

- ajouter `{% set const-main %}` en tête de code pour que les arguments du main soient constants
  (on les demande plus à chaque tour du `while` dans le main du code c généré) mais seulement en
  entrée de programme

- Phases de compilation
  + `Parsed` -> `Parsed`:
    - dé-sucrage des automates et des `print`
  + `Parsed` -> `Typed`:
    - vérification de la cohérence du typage explicite qui apparait dans le code source
  + `Typed` -> `Normalised`:
    - toutes les expressions sont "dé-inlinées" dans des variables locales aux noeuds
  + `Normalised` -> `Clocked`:
    - vérifications de la cohérence des horloges (cf. [Biernacki, Colaco, Hamon, Pouzet](https://www.di.ens.fr/~pouzet/bib/lctes08a.pdf))
  + `Clocked` -> `Scheduled`:
    - mise en ordre des équations dans le noeud en fonction des liens de dépendances
    - dans le même temps, analyse de causalité
  + `Scheduled` -> `Imp`:
    - traduction intermédiaire vers un AST de langage impératif avant la truduction en C
    - création des mémoires des noeuds, fonctions d'initialisation des mémoires
  + `Imp` -> `C`:
    - nous utilisons un l'ast C de la bibliothèque Goblint.Cil
    - le retour de la fonction `main` du code lustre initial est affiché à chaque tour d'un `while (1)`
    - gestion des arguments du noeud main en lustre :
      + s'il contient des arguments, ceux-ci seront demandés en entrée à chaque tour du `while`,
        sauf si l'on ajout le commentaire magique `{% set const-main %}` qui demandera alors uniquement
        les arguments sur la ligne de commande en entrée du programme
      + on pourra alors donner en entrée du programme un flux de données par
        la commande `$ echo "0 0 0 0 1 0 0 0 1 " | ./a.out` pour envoyer au programme
        `a.out` le flux de données `0 0 0 0 1 0 0 0 1` par exemple.

- Ajout d'une fonction `print` dans lustre
