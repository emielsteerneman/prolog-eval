import Test
import Types
import Debug.Trace

{-
t1 = evalProp (Atom "c" []) simple
t2 = evalProp (Atom "b" []) simple
t3 = evalProp (Atom "q" [Const "a"]) simple
t4 = evalProp (Atom "son" [Const "henk", Const "bernhard"]) royalfamily
t5 = evalProp (Atom "son" [Const "henk", Const "henk"]) royalfamily

-- empty if true, returns a path if false
evalProp atom@(Atom name vars) program = checkAtom atom program


-- proves a child. Three possibilities: the atom is a fact, the atom is a rule, the atom doesn't exist
-- son(henk, bernard)
checkAtom :: Atom -> Program -> Result
checkAtom atom@(Atom name vars) program
	| isAFact = Result True []				-- True  This atom is a fact, thus it is true
	| allRules == [] = [atom]				-- False It is no fact and there are no rules, so the atom doesn't exist in the RHS
	| elem [] provenRules = []				-- True  There is a clause which returns True
	| otherwise = atom : (provenRules !! 0) -- False returns this atom and the first of provenClause
	where
-- 	Bool			Is this atom a fact?     Same atom        No children
		isAFact = any (\(Clause _atom ac) -> atom == _atom && ac == []) program
-- 	[Clause]		Get all rules from this atom				    Same name		 Same amount of vars			Has children
		allRules = filter (\(Clause _atom@(Atom _name _vars) ac) -> name == _name && length vars == length _vars && ac /= []) program
-- 	[Atom]			For each new rule: prove it by calling proveClause
		provenRules = map(\clause -> proveClause atom clause program) allRules

-- Combines the result of proving all children
-- Atom: son(henk, bernard)   Clause: son(O, Z) :- child(Z, O), man(Z)		Program: all clauses
proveClause :: Atom -> Clause -> Program -> Result
proveClause atom@(Atom name args) clause@(Clause atomC@(Atom nameC argsC) children) program = 
	prove
	where
-- [(Term, Term)] 	returns [(O -> henk), (K -> bernard)]
		mapping = createMapping args argsC
-- [Atom]			replaces all variables with constants. child(Z, O) -> child(bernard, henk)
		newChildren = swapAtoms children mapping
-- [Atom]			Prove each atom of newChildren by entering it in checkAtom
		prove = foldl (++) [] $ map (\a -> checkAtom a program) newChildren
-}

{- == MAPPING == -}		
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





evalAtom atom@(Atom name args) program = return
	where
		atomFact = evalFact atom program			-- Is this atom a fact?
		return 
			| atomFact /= NoFact = atomFact		-- Atom is a fact!
			| otherwise = Wer

			
			
w = evalRule        (Atom "r" [Var "X", Var "Y"]) simple
x = evalSingleRule	(Atom "r" [Const "a", Var "B"]) (Clause (Atom "r" [Var "X", Var "Y"]) [Atom "p" [Var "X", Var "Y"], Atom "q" [Var "X", Var "Y"]]) simple	
--           Clause (Atom "p" [Const "a", Const "b"]) [],
--  		 Clause (Atom "p" [Var   "J", Var "K"]) [],
--			 Clause (Atom "p" [Var   "G", Const "h"]) [],
--			 Clause (Atom "q" [Var   "K", Const "b"]) [],
	
evalRule atom@(Atom name args) program = return
	where
		allRules = filter (\(Clause _atom@(Atom _name _args) children) -> name == _name && length args == length _args && children /= []) program
		return = allRules
		forAll = map (\clause -> evalSingleRule atom clause program) allRules

--evalSingleRule :: Atom -> Clause -> Program -> Result
evalSingleRule atom@(Atom name args) clause@(Clause atomC@(Atom nameC argsC) children) program = return
	where
		mapping = createMapping args argsC
		return = proveAll
		subs = swapAtoms children mapping
		proveAll = map (\sub -> evalAtom sub program) subs
	
{-	
proveClause :: Atom -> Clause -> Program -> Result
proveClause atom@(Atom name args) clause@(Clause atomC@(Atom nameC argsC) children) program = 
	prove
	where
-- [(Term, Term)] 	returns [(O -> henk), (K -> bernard)]
		mapping = createMapping args argsC
-- [Atom]			replaces all variables with constants. child(Z, O) -> child(bernard, henk)
		newChildren = swapAtoms children mapping
-- [Atom]			Prove each atom of newChildren by entering it in checkAtom
		prove = foldl (++) [] $ map (\a -> checkAtom a program) newChildren

-}

	


		
			
evalFact :: Atom -> Program -> Result
evalFact atom@(Atom name args) program = return
	where
		return
			| allFacts == [] = NoFact
			| subsNoNone==[] = NoFact
			| otherwise = Fact subsNoNone
		allFacts = filter (\(Clause _atom@(Atom _name _args) ac) -> (length args) == (length _args) && name == _name && ac == []) program
		subs 	 = map (\fact -> compareAtoms atom (getAtom fact)) allFacts
		subsNoNone=filter (\sub -> sub /= NoSub) subs


compareAtoms :: Atom -> Atom -> Sub
compareAtoms (Atom t1 args1) (Atom t2 args2) = result
	where 
		compare = compareArgs args1 args2
		result
			| (Sub []) == compare = NoSub
			| otherwise = compare


compareArgs :: [Term] -> [Term] -> Sub
compareArgs [] [] = Sub []
compareArgs a1@(x:xs) a2@(y:ys) = return
	 where
		compare	= compareArg x y
		next	= compareArgs xs ys
		return
			| compare == NoSub = NoSub
			| next    == NoSub = NoSub
			| otherwise = Sub (listComp ++ listNext)
		(Sub listComp) = compare
		(Sub listNext) = next
		

compareArg :: Term -> Term -> Sub
compareArg t1@(Var v1)   t2@(Var v2)   = Sub [(t1, Anything)]--VAR TO VAR		= ANYTHING
compareArg t1@(Var v1)   t2@(Const v2) = Sub [(t1, t2)]		-- VAR TO CONST		= VAR -> CONST
compareArg t1@(Const v1) t2@(Var v2) = Sub []				-- CONST TO VAR		= []
compareArg t1@(Const v1) t2@(Const v2)						-- CONST TO CONST	= [] || NoSub
	| t1 == t2 = Sub []
	| otherwise = NoSub


getAtom :: Clause -> Atom
getAtom c = a where (Clause a _) = c
		

		
-- ==== TESTS		
f1 = evalFact (Atom "p" [Const "X", Const "b"]) simple
f2 = evalFact (Atom "p" [Var "X", Const "b"]) simple
	
fs0 = compareAtoms	(Atom "a" [Const "x"]) (Atom "b" [Const "x"])
fs1 = compareAtoms	(Atom "a" [Var "W", Var "X"  , Const "Y", Const "Z"])
					(Atom "b" [Var "w", Const "x", Var "y"  , Const "Z"])	
		
-- x = [0,1,2,3]
-- y =   [1,2,3,4]
-- z =     [2,3,4,5]		
		
-- q = [x, y, z]		

-- f [x] = x
-- f (x:y:xs) = f (z:xs)
	-- where
		-- z = filter(\n -> elem n y) x
		
		
		
		



			