## TODO

- pseudo-rapport du travial réalisé dans le README
  + expliquer le schéma de compilation
  + parler des features :
    - [x] clocks
    - [x] reset
    - [x] merge
    - [x] automaton (niveau 1 pour l'instant)
    - [x] ajout print en dur
    - [x] ..

- base
  - [x] compile simple : `pre`, `->`, `fby`
  - [ ] meilleur `imp` printer

- features avancées
  - [x] `reset` : WIP PR #12
  - [x] `merge` : WIP PR #7
  - [x] automaton
  - [x] option : lecture input main non constant ? (ne pas utiliser argc/v de C mais un fichier ?)
  - [x] WIP: ajouter des exemples négatifs stylés

- alt
  - [x] ajouter un print sur les var de sorties du `main` dans le `while (1)`
    - presque ok : ajouter pour les enum/mémoires/...

  - [ ] automate :
    - [x] plusieurs sorties
    - [ ] "Automate d'automate" : cf. ex006.lus
    - [ ] sortes automates : `until-then`, `unless-continue`, `unless-then`

  - [x] meilleur imp printer

  - [ ] étendre `base_ty_to_format_string` et `typ_to_format_string` aux adts/...

  - [x] main avec float

- meilleurs tests
  - [x] tests positifs
  - [x] tests négatifs
  - [x] automatisation de la vérification des tests dans le makefile
