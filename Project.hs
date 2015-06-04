import Test
import Types
import Debug.Trace

t1 = evalProp (Atom "c" []) simple
t2 = evalProp (Atom "q" [Const "a"]) simple
t3 = evalProp (Atom "son" [Const "henk", Const "bernhard"]) royalfamily
t4 = evalProp (Atom "son" [Const "henk", Const "henk"]) royalfamily

-- empty if true, returns a path if false
evalProp atom@(Atom name vars) program = checkAtom atom program

-- proves a child. Three possibilities: the atom is a fact, the atom is a rule, the atom doesn't exist
-- son(henk, bernard)
checkAtom :: Atom -> Program -> [Atom]
checkAtom atom@(Atom name vars) program
	-- | clausesName == [] = [atom]			-- This atom does not exist anywhere in the LHS, thus it is false
	| isAFact = []							-- This atom is a fact, thus it is true
	| allRules == [] = [atom]				-- It is no fact and there are no rules, so the atom doesn't exist in the RHS
	| elem [] provenRules = []				-- There is a clause which returns True
	| otherwise = atom : (provenRules !! 0) -- returns this atom and the first of provenClause
	where
-- 	Bool			Is this atom a fact?     Same atom        No children
		isAFact = any (\(Clause _atom ac) -> atom == _atom && ac == []) program
-- 	[Clause]		Get all rules from this atom				    Same name		 Same amount of vars			Has children
		allRules = filter (\(Clause _atom@(Atom _name _vars) ac) -> name == _name && length vars == length _vars && ac /= []) program
-- 	[Atom]			For each new rule: prove it by calling proveClause
		provenRules = map(\clause -> proveClause atom clause program) allRules

-- Combines the result of proving all children
-- Atom: son(henk, bernard)   Clause: son(O, Z) :- child(Z, O), man(Z)		Program: all clauses
proveClause :: Atom -> Clause -> Program -> [Atom]
proveClause atom@(Atom name args) clause@(Clause atomC@(Atom nameC argsC) children) program = 
	prove
	where
-- [(Term, Term)] 	returns [(O -> henk), (K -> bernard)]
		mapping = createMapping args argsC
-- [Atom]			replaces all variables with constants. child(Z, O) -> Son(bernard, henk)
		newChildren = swapAtoms children mapping
-- [Atom]			Prove each atom of newChildren by entering it in checkAtom
		prove = foldl (++) [] $ map (\a -> checkAtom a program) newChildren
		
{-		
evalClause :: Clause -> Program -> [Atom]
evalClause c@(Clause (Atom name args) atoms) program = 
	foldl (++) [] $ map (\atom@(Atom aname vars) -> trace ("Proving " ++ name ++ " -> " ++ aname) (gc atom program)) atoms
		
		clausesName = filter (\c@(Clause a@(Atom aname avars) atoms) -> name == aname) program  		-- Get all clauses with the same name on the LHS
		truthTable = map(\clause -> e clause program) clausesName										-- Prove all the clauses we got from the LHS

		
swapClause :: Atom -> Clause -> [Atom]		
swapClause atom@(Atom name vars) clause@(Clause (Atom cname cvars) children) = 
	Clause (Atom )newChildren
	where
		mapping = createMapping vars cvars
		newChildren = swapAtoms children mapping


	
b = [Atom "b" [Var "X", Var "Y"], Atom "c" [Var "Y", Var "X"], Atom "d" [Var "X", Var "X"]]
c = Clause(Atom "a" [Var "X", Var "Y"]) [Atom "b" [Var "X", Var "Y"], Atom "c" [Var "Y", Var "X"]]
d = Atom "a" [Const "var1", Const "var2"]


vars = [Const "a", Const "b"]
avars= [Var "X", Var "Y"]
m = createMapping avars vars
-}		

-- 				 Const     Var
createMapping :: [Term] -> [Term] -> [(Term, Term)]
createMapping [] [] = []
createMapping (x:xs) (y:ys) = (y, x) : createMapping xs ys

-- for each atom, get all the vars and swap them with swapVars
swapAtoms :: [Atom] -> [(Term, Term)] -> [Atom]
swapAtoms atoms mapping = map (\(atom@(Atom name vars)) -> Atom name (swapVars vars mapping)) atoms
-- for each var, swap it using the mapping
swapVars :: [Term] -> [(Term, Term)] -> [Term]
swapVars listOfVars mapping = map (\(var@(Var v)) -> getSwap var mapping) listOfVars
-- var -> const
getSwap :: Term -> [(Term, Term)] -> Term		
getSwap var mapping = snd $ (filter ((==var).fst) mapping) !! 0

	
isAFact atom program = any (\(Clause _atom ac) -> atom == _atom && ac == []) program
w = Atom "p" [Const "a"]
{-	
	FACT if it has no children
	get all with same name
		check if it has no children -> FACT
			if variables match -> True
		if it has children
			replace variables
			
-}









			