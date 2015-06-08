module Project where

import Test
import Types
import Debug.Trace
import Data.List
import Pretty

{-
	Emiel Steerneman	1499262
	Olaf Haalstra		1482041
-}


w = query (Atom "r" [Var "X", Var "O"]) prog

-- ENTRY
query atom program = evalAtom atom program

evalAtom :: Atom -> Program -> Result
evalAtom atom@(Atom name args) program = trace("\nevalAtom " ++ show(atom)) $ return
	where
		atomFact = evalFact atom program			-- Is this atom a constant?
		ruleFact = evalRule atom program			-- Is this atom a rule?
		return 
			| atomFact /= NoFact = trace("\t" ++ show(atom) ++ " == " ++ show(atomFact)) $ atomFact		-- The atom is constant and a fact
			| ruleFact /= NoFact = trace("\t" ++ show(atom) ++ " == " ++ show(atomFact)) $ ruleFact 	-- The atom is a rule and a fact
			| otherwise = trace("\t" ++ show(atom) ++ " == NoFact") $ NoFact				-- The atom is not a fact

evalRule :: Atom -> Program -> Result	
evalRule atom@(Atom name args) program = trace("evalRule " ++ show(atom) ++ " " ++ show(allRules)) $ return
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

evalSingleRule :: Atom -> Clause -> Program -> Result		
evalSingleRule atom@(Atom name args) clause@(Clause atomC@(Atom nameC argsC) children) program = 
	-- trace ("termsList: " ++ show(termsList)) $ return
	return
	where
		-- [Atom]	swap all arguments of the children
		newChildren			= swapAtoms children $ createMapping args argsC
		-- [Result] Prove every child
		evalChildren		= map (\child -> evalAtom child program) newChildren
		-- if any child returns NoFact, then the clause is NoFact
		isFact 				= notElem NoFact evalChildren
		-- Turn [Fact [sub1], Fact [sub2], Fact [sub3]] into  [[sub1], [sub2], [sub3]]
		
		-- wer = mergeChildren evalChildren
		
		subsList			= map (\(Fact sub) -> sub) evalChildren
		-- subList				= map (\(Sub list) -> list) subsList
		-- termsList			= map (\(Sub list) -> list) subList
		-- Check if any sub makes all rules True
		mergeSubsList = trace("\n\nsubsList: " ++ show(subsList)) $ mergeSubs subsList
		-- mergeSubsList = mergeSubs subsList
		-- mergeTermsList = mergeTerms termsList
		
		-- [[sub1], [sub2], [sub3]] -> Fact [[sub1], [sub2], [sub3]]
		myFact = Fact mergeSubsList
		return 
			| isFact == False = NoFact
			| otherwise = myFact



{- ======= CODE USED FOR SUBSTITUTION ======= -}			
createMapping :: [Term] -> [Term] -> [(Term, Term)]
createMapping [] [] = []
createMapping (x:xs) (y:ys) = (y, x) : createMapping xs ys

swapAtoms :: [Atom] -> [(Term, Term)] -> [Atom]
swapAtoms atoms mapping = map (\(atom@(Atom name vars)) -> Atom name (swapVars vars mapping)) atoms

swapVars :: [Term] -> [(Term, Term)] -> [Term]
swapVars listOfVars mapping = map (\var -> getSwap var mapping) listOfVars
--trace("swapVars: " ++ show(listOfVars) ++ ", " ++ show(mapping)) $ 

getSwap :: Term -> [(Term, Term)] -> Term		
getSwap var mapping = result 
-- getSwap var mapping = trace ("getSwap " ++ show(mapping) ++ " -> " ++ show(var) ++ " -> " ++ show(_filter) ++ " -> " ++ show(result)) $ result 
	where 
		_filter = (filter ((==var).fst) mapping)
		result
			| _filter == [] = var
			| otherwise = snd $ _filter !! 0
			
			
			
			
			
{- ======= CODE USED FOR SELECTING WHICH SUBSTITUTION ARE TRUE FOR EVERY RULE ======= -}					
mergeSubs :: [[Sub]] -> [Sub]			
mergeSubs [x] = trace ("\nmergeSubs1: " ++ show(x)) $ x
mergeSubs (x:y:z) = trace("\nmergeSubs: \n\t" ++ show(x) ++ " -> \n\t" ++ show(y) ++ "\nresult: " ++ "\n\t" ++ show(add)) $ mergeSubs (add : z)
	where
		add = mergeSub x y

mergeSub :: [Sub] -> [Sub] -> [Sub]
mergeSub [] _ = []
mergeSub l1@(x:xs) l2 = add ++ mergeSub xs l2
	where
		add
			| subInSubs x l2 = [x]
			| otherwise = []


-- Is the sub of the first rule in the sub of the second rule? If so, then keep the rule, else discard it
subInSubs :: Sub -> [Sub] -> Bool
subInSubs sub subs = any (\_sub -> subsEqual sub _sub) subs

subsEqual :: Sub -> Sub -> Bool	

subsEqual s1 s2 = subsEqual' x y where (Sub x, Sub y) = (s1, s2)

subsEqual' :: [(Term, Term)] -> [(Term, Term)] -> Bool		
subsEqual' [] _ = True		
subsEqual' l1@(x:xs) l2 = (elem x l2) && (subsEqual' xs l2)




{- ======= CODE USED CHECKING IF THE ATOM IS A CONSTANT ======= -}					
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
		subsNoNone=trace("\nevalFact " ++ show(atom) ++ "\n\tallFacts: " ++ show(allFacts) ++ "\n\tArgs: " ++ show(args) ++ "\n\tSubs: " ++ show(subs)) $ filter (\sub -> sub /= NoSub) subs		-- remove all NoSub, leaving only the correct rules for swapping

		
		
		
{- ======= CODE USED FOR COMPARING ARGUMENTS ======= -}			
compareAtoms :: Atom -> Atom -> Sub
compareAtoms (Atom t1 args1) (Atom t2 args2) = compareArgs args1 args2

compareArgs :: [Term] -> [Term] -> Sub
compareArgs [] [] = Sub []
compareArgs a1@(x:xs) a2@(y:ys) = trace("compareAtoms " ++ show(a1) ++ " " ++ show(a2) ++ " -> " ++ show(return)) $ return
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
compareArg t1@(Var v1)   t2@(Var v2)   = Sub [(t1, Anything)]	-- VAR TO VAR		= ANYTHING
compareArg t1@(Var v1)   t2@(Const v2) = Sub [(t1, t2)]			-- VAR TO CONST		= VAR -> CONST
compareArg t1@(Const v1) t2@(Var v2) = Sub []					-- CONST TO VAR		= []
compareArg t1@(Const v1) t2@(Const v2)							-- CONST TO CONST	= [] || NoSub
	| t1 == t2 = Sub []
	| otherwise = NoSub


getAtom :: Clause -> Atom
getAtom c = a where (Clause a _) = c





l = 
	[
		[
			Sub	[(Var "O", Const "o1"), (Var "Y", Const "b")], 
			Sub	[(Var "O", Const "o2"),	(Var "Y", Const "d")] 
		],
		[ 
			Sub[(Var "X", Const "c")]
		],
		[ 
			Sub[(Var "X", Const "c"), (Var "Y", Const "b")] 
		]
	]



e = mergeChildren l
mergeChildren (c1:c2:cs) = wer
	where
		t1 = map (\(Sub list) -> list) c1
		t2 = map (\(Sub list) -> list) c2
		wer = mergeTerms [t1, t2]
		
		
getTerms (Sub list) = list

		
x = [	 [(Var "O",Const "o1"),(Var "Y",Const "d")],  [(Var "O",Const "o2"),(Var "Y",Const "b")]]
y = [	 [(Var "X",Const "c"),(Var "Y",Const "b")],  [(Var "X",Const "c"),(Var "Y",Const "e")],  [(Var "X",Const "d"),(Var "Y", Const "d")]]
z = [	 [(Var "Z",Const "z1"),(Var "Y",Const "b")],  [(Var "Z",Const "z2"),(Var "Y",Const "d")]]

t = mergeTerms [x, y, z]
r = conflictingSub [(Var "O",Const "O1"),(Var "Y",Const "D")] [(Var "X",Const "C"),(Var "Y",Const "B")]

mergeTerms :: [[[(Term, Term)]]] -> [[(Term, Term)]]
mergeTerms [l] = l
mergeTerms (l1:l2:ls) = mergeTerms(merge : ls)
	where
		merge = [(nub (x ++ y)) | x <- l1, y <- l2, not (conflictingSub x y)]

conflictingSub s1 s2 = [] /= [(x, y) | x@(x1, x2) <- s1, y@(y1, y2) <- s2, x1 == y1, x2 /= y2 && notElem Anything [x2, y2]]
	
	
-- [(x1, y1) | (x1, x2) <- [(1, 2), (3, 4)], (y1, y2) <- [(5, 6), (7, 8)]]
	



		
