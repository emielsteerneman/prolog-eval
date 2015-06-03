import Test
import Types
import Debug.Trace

p = prove (Atom "c" []) simple

prove atom@(Atom name vars) program = gc atom program

gc :: Atom -> Program -> [Atom]
gc atom@(Atom name vars) program
	| clauses == [] = [atom]
	| otherwise = foldl (++) [] $ map(\clause -> e clause program) clauses
	where
		clauses = filter (\c@(Clause (Atom aname vars) atoms) -> name == aname) program

e :: Clause -> Program -> [Atom]
e c@(Clause (Atom name args) atoms) program = 
	foldl (++) [] $ map (\atom -> gc atom program) atoms


