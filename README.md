# Minilucy

- Projet MPRI pour le cours [systèmes synchrones](https://www.di.ens.fr/~pouzet/cours/synchrone/).
- Groupe : Émilien Lemaire et Paul Patault
- Enseignants : T. Bourke et M. Pouzet
- Sujet : [minilucy.pdf](./pdf/minilucy.pdf)

## TODO

- pseudo-rapport du travial réalisé dans le README
  - expliquer le schéma de compilation
  - parler des features
     - clocks
     - reset
     - merge
     - automaton (niveau 1 pour l'instant)
     - ..

- check [https://github.com/paulpatault/minilucy/blob/automaton/TODO.md]

- base
  - [ ] compile simple : WIP

- features avancées
  - [ ] reset
  - [ ] merge : WIP

## Usage

### Dépendances

- opam (`bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"`)
- bibliothèque Goblint : interface avec `C`
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

Exemple :
```
$ dune exec src/minilucy.exe -- examples/ex005.lus -automaton-only -v main
```

## Travail réalisé

- TODO
