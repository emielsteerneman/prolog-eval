module Test where

import Types

simple = [
	Clause (Atom "a" []) [],
    Clause (Atom "b" []) [],
    Clause (Atom "c" []) [Atom "a" [], Atom "b" [], Atom "d" []],
	Clause (Atom "d" []) [Atom "e" [], Atom "f" []]
    ]
royalfamily :: [Clause]
royalfamily = [
	-- FACTS
	Clause (Atom "woman" ["juliana"]) [],
    Clause (Atom "woman" ["beatrix"]) [],
    Clause (Atom "woman" ["margriet"]) [],
    Clause (Atom "woman" ["irene"]) [],
    Clause (Atom "woman" ["Christina"]) [],
    Clause (Atom "man" ["bernhard"]) [],
    Clause (Atom "mother" ["juliana","beatrix"]) [],
    Clause (Atom "mother" ["juliana","margriet"]) [],
    Clause (Atom "mother" ["juliana","irene"]) [],
    Clause (Atom "father" ["bernhard","beatrix"]) [],
    Clause (Atom "father" ["bernhard","margriet"]) []
	
	-- Atoms
	{-
    Clause (Atom "child" ["K","O"]) [
		Clause (Atom "mother" ["O","K"]) []
	],
    Clause (Atom "child" ["K","O"]) [
		Clause (Atom "father" ["O","K"]) []
	],
    Clause (Atom "son" ["Z","O"]) [
		Clause (Atom "child" ["Z","O"]) [], 
		Clause (Atom "man" ["Z"]) []
	]
	-}
	]
    
    -- How to not
    -- Clause (Atom "sister" ["X","Y"]) [Clause (Atom "child" ["X","O"]) [], Clause (Atom "woman" ["X"] [],Clause (Atom "child" ["Y","O"]) [],Clause ],


    
