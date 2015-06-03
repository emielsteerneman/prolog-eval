module Test where

import Types

simple = [
	Clause (Predicate "a0" []) [],
    Clause (Predicate "b0" []) [],
    Clause (Predicate "c0" []) [Clause (Predicate "a0" []) [],Clause (Predicate "b0" []) []]
    ]

royalfamily = [Clause (Predicate "woman" ["juliana"]) [],
    Clause (Predicate "woman" ["beatrix"]) [],
    Clause (Predicate "woman" ["margriet"]) [],
    Clause (Predicate "woman" ["irene"]) [],
    Clause (Predicate "woman" ["Christina"]) [],
    Clause (Predicate "man" ["bernhard"]) [],
    Clause (Predicate "mother" ["juliana","beatrix"]) [],
    Clause (Predicate "mother" ["juliana","margriet"]) [],
    Clause (Predicate "mother" ["juliana","irene"]) [],
    Clause (Predicate "father" ["bernhard","beatrix"]) [],
    Clause (Predicate "father" ["bernhard","margriet"]) [],
    Clause (Predicate "child" ["K","O"]) [Clause (Predicate "mother" ["O","K"]) []],
    Clause (Predicate "child" ["K","O"]) [Clause (Predicate "father" ["O","K"]) []],
    Clause (Predicate "son" ["Z","O"]) [Clause (Predicate "child" ["Z","O"]) [], Clause (Predicate "man" ["Z"]) []]
    ]
    
    -- How to not
    -- Clause (Predicate "sister" ["X","Y"]) [Clause (Predicate "child" ["X","O"]) [], Clause (Predicate "woman" ["X"] [],Clause (Predicate "child" ["Y","O"]) [],Clause ],


    
