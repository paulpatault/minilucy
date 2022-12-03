## TODO

- pseudo-rapport du travial réalisé dans le README
  + expliquer le schéma de compilation
  + parler des features :
    - clocks
    - reset
    - merge
    - automaton (niveau 1 pour l'instant)
    - ajout print en dur
    - ..

- base
  - [x] compile simple : `pre`, `->`, `fby`
  - [ ] meilleur `imp` printer

- features avancées
  - [ ] `reset`
  - [x] `merge` : WIP PR #7
  - [x] automaton
  - [ ] option : lecture input main non constant ? (ne pas utiliser argc/v de C mais un fichier ?)
  - [ ] ajouter des exemples négatifs stylés

* ajouter un print sur les var de sorties du `main` dans le `while (1)`
  - presque ok : ajouter pour les enum/mémoires/...

* ajouter `reset`

* automate :
  - "Automate d'automate" : cf. ex006.lus
  - sortes automates : `until-then`, `unless-continue`, `unless-then`

* meilleur imp printer

* étendre `base_ty_to_format_string` et `typ_to_format_string` aux adts/...

* main avec float
