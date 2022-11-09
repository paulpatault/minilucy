* Faire un `imp_ast` (cf. [Imp_ast](https://github.com/emilienlemaire/MiniLustreLLVM/blob/main/src/target/imp.ml)) 
    pour pouvoir avoir la mémoire des nœuds
* Faire la normalisation avant le `imp_ast`
* Faire les clocks
* Ajouter merge int et float.
* Ajouter merge ADT simple

* GOAL automaton :
```
node aux (lo, hi : int) returns (x: int);
let
  automaton
  | Await ->
      x = 0 -> pre x + 1;
      unless (x <= lo) then Run
  | Run ->
      x = 0 -> pre x - 1;
      unless (x >= hi) then Await
  end
tel
```

to
```
type t_aux = Await | Run

node aux (lo, hi : int) returns (x: int);
local s: t_aux init Await
let
  x = merge s
    (Await -> x = 0 -> pre x + 1)
    (Run   -> x = 0 -> pre x - 1) ;

  s = merge s
    (Await -> if (x <= lo) then Run else Await)
    (Run   -> if (x >= hi) then Await else Run) ;
tel
```

