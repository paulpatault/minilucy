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

### Utilisation

Commande générique :
```
$ dune exec src/minilucy.exe -- [file.lus] [OPTIONS] [main-node]
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

Exemple :
```
$ dune exec src/minilucy.exe -- examples/ex005.lus -automaton-only -v main1
```

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

- Automates (non imbriqués)

- ADT simple (sans paramètres)

- `when`, `merge`, (`reset` ?)

- ajouter `{% set const-main %}` en tête de code pour que les arguments du main soient constants
  (on les demande plus à chaque tour du `while` dans le main du code c généré) mais seulement en
  entrée de programme

- Print (types simples (pas ADT)) dans lustre : TODO tuples

- Phases de compilation
  + `Parsed` -> `Parsed`:
    - dé-sucrage des automates et des `print`
  + `Parsed` -> `Typed`:
    - TODO
  + `Typed` -> `Normalised`:
    - toutes les expressions sont "dé-inlinées" dans des variables locales aux noeuds
    - ...
  + `Normalised` -> `Clocked`:
    - vérifications des horloges (analyse à la typage (cf. article du sujet))
    - ...
  + `Clocked` -> `Scheduled`:
    - mise en ordre des équations dans le noeud en fonction des liens de dépendances
    - ...
  + `Scheduled` -> `Imp`:
    - traduction intermédiaire vers un AST de langage impératif avant la truduction en C
    - ...
  + `Imp` -> `C`:
    - le retour de la fonction `main` du code lustre initial est affiché à chaque tour du `while (1)`
    - nous utilisons un l'ast C de la bibliothèque Goblint.Cil
    - si le main n'a pas besoin de mémoire interne, pas besoin d'en fabriquer une pour rien (cf ex008)
    - ...

- TODO Suite
