module Test where

import Types
	
royalfamily :: [Clause]
royalfamily = [
	Clause (Atom "woman" 	[Var "juliana"]) [],
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
	
    Clause (Atom "child" 	[Var "K",Var "O"]) 	[(Atom "mother" [Var "O",Var "K"])],
    Clause (Atom "child" 	[Var "K",Var "O"]) 	[(Atom "father" [Var "O",Var "K"])],
    Clause (Atom "son" 		[Var "Z",Var "O"])	[(Atom "child"  [Var "Z",Var "O"]), (Atom "man" [Var "Z"])]
	]

prog = [
	Clause (Atom "o"	[Const "o1", Const "b"]) [],
	Clause (Atom "o"	[Const "o2", Const "d"]) [],
	Clause (Atom "p"	[Const "p1", Const "c", Const "b"]) [],
	Clause (Atom "q"	[Const "q1", Const "c", Const "b"]) [],
	Clause (Atom "q"	[Const "q1", Const "c", Const "d"]) [],
	
	Clause (Atom "r"	[Var "X", Var "O"]) [Atom "o" [Var "O", Var "Y"], Atom "p" [Const "P1", Var "X", Var "Y"], Atom "q" [Const "Q1", Var "X", Var "Y"]]
	]    
