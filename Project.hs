module Project where

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


wer = evalAtom (Atom "r1" [Var "X", Var "Y"]) simple

evalAtom :: Atom -> Program -> Result
evalAtom atom@(Atom name args) program = return
	where
		atomFact = evalFact atom program			-- Is this atom a fact?
		ruleFact = evalRule atom program
		return 
			| atomFact /= NoFact = atomFact		-- Atom is a fact!
			| ruleFact /= NoFact = ruleFact 
			| otherwise = NoFact
			
			
r1 = evalRule        (Atom "r1" [Var "X", Var "Y"]) simple
r2 = evalRule        (Atom "r2" [Var "X", Var "Y"]) simple
r  = evalRule        (Atom "r" [Var "X", Var "Y"]) simple


-- x = evalSingleRule	(Atom "r" [Const "a", Var "B"]) (Clause (Atom "r" [Var "X", Var "Y"]) [Atom "p" [Var "X", Var "Y"], Atom "q" [Var "X", Var "Y"]]) simple	
-- x = evalSingleRule	(Atom "r" [Var "A", Var "B"]) (Clause (Atom "r" [Var "X", Var "Y"]) [Atom "s" [Var "X", Var "Y"]]) simple	
-- z = evalSingleRule	(Atom "r" [Var "A", Var "B"]) (Clause (Atom "r" [Var "X", Var "Y"]) [Atom "p" [Var "X", Var "Y"], Atom "q" [Var "X", Var "Y"]]) simple	


--           Clause (Atom "p" [Const "a", Const "b"]) [],
--			 Clause (Atom "p" [Var   "G", Const "h"]) [],
--			 Clause (Atom "q" [Var   "K", Const "b"]) [],

evalRule :: Atom -> Program -> Result	
evalRule atom@(Atom name args) program = return
	where
		-- get all rules
		allRules = filter (\(Clause _atom@(Atom _name _args) children) -> name == _name && length args == length _args && children /= []) program
		-- prove all rules by entering them into evalSingleRule
		proveRules = map (\clause -> evalSingleRule atom clause program) allRules
		-- isFact if there is any rule that is a Fact
		isFact = any (\rule -> rule /= NoFact) proveRules
		-- merge = mergeSubs forAll
		mergeSubs = foldl (++) [] $ map (\(Fact sub) -> sub) proveRules
		
		return 
			| isFact = Fact mergeSubs
			| otherwise = NoFact
		-- = trace("proveRules: " ++ show(proveRules) ++ "\n") $ Fact mergeSubs
		
		
{-
	Try to prove all the facts. Every fact returns a Fact [Sub]
	Fact [
		Sub [(Var "X",Const "a"),(Var "Y",Const "b")],
		Sub [(Var "X",Const "c"),(Var "Y",Const "d")]
	]
	Fact [
		Sub [(Var "X",Const "c"),(Var "Y",Const "d")],
		Sub [(Var "X",Const "e"),(Var "Y",Const "f")]
	]
-}		



-- z = evalSingleRule (Atom "r" [Var "q", Var "Y"]) (Clause (Atom "r" [Var "X", Var "Y"]) [Atom "sdf" [Var "Y", Var "X"], Atom "q" [Var "X", Var "Y"]]) simple

{-
[
Fact [
	Sub [(Var "X",Const "a"),(Var "Y",Const "b")],
	Sub [(Var "X",Const "c"),(Var "Y",Const "d")]
	],
Fact [
	Sub [(Var "X",Const "c"),(Var "Y",Const "d")],
	Sub [(Var "X",Const "e"),(Var "Y",Const "f")]
	]
]
-}

evalSingleRule :: Atom -> Clause -> Program -> Result		
evalSingleRule atom@(Atom name args) clause@(Clause atomC@(Atom nameC argsC) children) program = return
	where
		-- [Atom]	swap all arguments of the children
		newChildren			= swapAtoms children $ createMapping args argsC
		-- [Result] Prove every child
		proveNewChildren	= map (\child -> evalAtom child program) newChildren
		-- [[Sub]] if any child return NoFact, then the clause is NoFact
		isFact 				= notElem NoFact proveNewChildren
		-- Turn [Fact [sub1], Fact [sub2], Fact [sub3]] into  [[sub1], [sub2], [sub3]]
		subsList 			= map (\(Fact subs) -> subs) proveNewChildren
		-- Check if any sub makes all rules True
		mergeSubsList = mergeSubs subsList
		-- [[sub1], [sub2], [sub3]] -> Fact [[sub1], [sub2], [sub3]]
		myFact = Fact mergeSubsList
		return 
			| isFact == False = NoFact
			| otherwise = myFact
		
mergeFacts [] = []
mergeFacts (x:xs) = [list] ++ mergeFacts xs
	where
		(Fact list) = x		
		
mergeSubs [x] = x
mergeSubs (x:y:z) = mergeSubs (add : z)
	where
		add = mergeSub x y
		
x' = [Sub [(Var "X",Const "a"),(Var "Y",Const "b")], Sub [(Var "A",Const "a"),(Var "B",Const "b")], Sub [(Var "C",Const "c"),(Var "D",Const "d")]]
y' = [Sub [(Var "Y",Const "b"),(Var "X",Const "a")], Sub [(Var "B",Const "b"),(Var "A",Const "a")], Sub [(Var "Q",Const "q"),(Var "R",Const "r")]]		

mergeSub :: [Sub] -> [Sub] -> [Sub]
mergeSub [] _ = []
mergeSub l1@(x:xs) l2 = add ++ mergeSub xs l2
	where
		add
			| subInSubs x l2 = [x]
			| otherwise = []


-- sub in list of subs
subInSubs :: Sub -> [Sub] -> Bool
subInSubs sub subs = any (\_sub -> subsEqual sub _sub) subs

x = Sub [(Var "X",Const "a"),(Var "Y",Const "b")]
y = Sub [(Var "Y",Const "b"),(Var "X",Const "a")]		
	
subsEqual :: Sub -> Sub -> Bool	
subsEqual s1 s2 = subsEqual' x y where (Sub x, Sub y) = (s1, s2)

subsEqual' :: [(Term, Term)] -> [(Term, Term)] -> Bool		
subsEqual' [] _ = True		
subsEqual' l1@(x:xs) l2 = (elem x l2) && (subsEqual' xs l2)
		
q = subsEqual x y	



		
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
			| allFacts == [] = NoFact			-- The atom doesn't exist on the RHS
			| args == [] = Fact []		-- The atom does exist, and has no arguments, making it correct
			| subsNoNone== [(Sub [])] = Fact [] 
			| subsNoNone==[] = NoFact
			| otherwise = Fact subsNoNone		-- 
		allFacts = filter (\(Clause _atom@(Atom _name _args) ac) -> (length args) == (length _args) && name == _name && ac == []) program
		subs 	 = map (\fact -> compareAtoms atom (getAtom fact)) allFacts
		subsNoNone=filter (\sub -> sub /= NoSub) subs		-- remove all NoSub, leaving only the correct rules for swapping


compareAtoms :: Atom -> Atom -> Sub
compareAtoms (Atom t1 args1) (Atom t2 args2) = compare
	where 
		compare = compareArgs args1 args2
		-- result
			-- | (Sub []) == compare = NoSub
			-- | otherwise = compare


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
f0 = evalFact (Atom "a" []) simple	
f1 = evalFact (Atom "q" [Var "X", Var "Y"]) simple
f2 = evalFact (Atom "p" [Const "a", Var "Y"]) simple
f3 = evalFact (Atom "p" [Const "a", Const "b"]) simple
f4 = evalFact (Atom "p" [Const "b", Var "Y"]) simple

	
fs0 = compareAtoms	(Atom "a" [Const "x"])
					(Atom "b" [Const "x"])
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
		
		
		
		

		
		
		
		
		
		
	
		
		
		
		
		
		
		
		


			