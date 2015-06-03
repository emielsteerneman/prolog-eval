data Clause = Clause Predicate [Clause] deriving (Show)

data Predicate = Predicate String [String] deriving (Show)

c = [
	Clause (Predicate "a" []) [],
	Clause (Predicate "b" []) [
		Clause(Predicate "a" []) [], 
		Clause(Predicate "c" []) [
			Clause (Predicate "d" []) [],
			Clause (Predicate "e" []) []
		]
	],		
	Clause (Predicate "c" []) [
		Clause (Predicate "d" []) [],
		Clause (Predicate "e" []) []
	],
	Clause (Predicate "d" []) [],
	Clause (Predicate "e" []) []
	]

evalProp
	
eval c@(Clause p@(Predicate name vars) children) = 
	truth
	where
		truth = all (==True) $ map (\child -> eval child) children