module Test where

import Types

simple = [
	Clause (Atom "a" []) [],
    Clause (Atom "b" []) [],
    Clause (Atom "c" []) [Atom "a" [], Atom "b" [], Atom "d" []],
	Clause (Atom "d" []) [Atom "e" [], Atom "f" []],
	--Clause (Atom "d" []) [],
	
	
	-- Clause (Atom "p" [Const "a", Const "b"]) [],
	Clause (Atom "p" [Const "c", Const "d"]) [],
	
	Clause (Atom "q" [Const "c", Const "d"]) [],
	-- Clause (Atom "q" [Const "e", Const "f"]) [],
	
	-- Clause (Atom "s" [Const	"a", Const "b"]) [],
	-- Clause (Atom "s" [Const	"d", Const "z"]) [],
	
	Clause (Atom "r1" [Var "X", Var "Y"]) [Atom "p" [Var "X", Var "Y"]],
	-- Clause (Atom "r1" [Var "X", Var "Y"]) [Atom "q" [Var "X", Var "Y"]],
	
	Clause (Atom "r2" [Var "X", Var "Y"]) [Atom "q" [Var "X", Var "Y"]],
	
	Clause (Atom "r" [Var "X", Var "Y"]) [Atom "r1" [Var "X", Var "Y"], Atom "r2" [Var "X", Var "Y"]]
	--Clause (Atom "r" [Var "X", Var "Y"]) [Atom "p" [Const "c", Var "Y"], Atom "q" [Const "c", Var "Y"]]
    ]
	
royalfamily :: [Clause]
royalfamily = [
	-- FACTS
	Clause (Atom "woman" 	[Const "juliana"]) [],
    Clause (Atom "woman" 	[Const "beatrix"]) [],
    Clause (Atom "woman" 	[Const "margriet"]) [],
    Clause (Atom "woman" 	[Const "irene"]) [],
    Clause (Atom "woman" 	[Const "Christina"]) [],
    Clause (Atom "man" 		[Const "bernhard"]) [],
	Clause (Atom "man" 		[Const "henk"]) [],
    Clause (Atom "mother" 	[Const "juliana",Const "beatrix"]) [],
    Clause (Atom "mother" 	[Const "juliana",Const "margriet"]) [],
    Clause (Atom "mother" 	[Const "juliana",Const "irene"]) [],
    Clause (Atom "father" 	[Const "bernhard",Const "beatrix"]) [],
    Clause (Atom "father" 	[Const "bernhard",Const "margriet"]) [],
	Clause (Atom "father" 	[Const "bernhard",Const "henk"]) [],
	
    Clause (Atom "child" 	[Var "K",Var "O"]) 	[(Atom "mother" [Var "O",Var "K"])],
    Clause (Atom "child" 	[Var "K",Var "O"]) 	[(Atom "father" [Var "O",Var "K"])],
    Clause (Atom "son" 		[Var "Z",Var "O"])	[(Atom "child"  [Var "Z",Var "O"]), (Atom "man" [Var "Z"])]
	]
    
    -- How to not
    -- Clause (Atom "sister" ["X","Y"]) [Clause (Atom "child" ["X","O"]) [], Clause (Atom "woman" ["X"] [],Clause (Atom "child" ["Y","O"]) [],Clause ],


    
