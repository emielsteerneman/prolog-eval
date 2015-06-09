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


e = query (Atom "sister" [Var "X", Var "Y"]) royalfamily

-- ENTRY
query atom program = evalAtom atom program

evalAtom :: Atom -> Program -> Result
evalAtom atom@(Atom name args) program = return
	where
		atomFact = evalFact atom program		-- Is this atom a constant?
		ruleFact = evalRule atom program		-- Is this atom a rule?
		result
			| atomFact /= NoFact = atomFact		-- The atom is constant and a fact
			| ruleFact /= NoFact = ruleFact 	-- The atom is a rule and a fact
			| otherwise = NoFact				-- The atom is not a fact
		return = rinse result args				-- Removes all variables in all the returned subs that are not present in the arguments of the atom

rinse :: Result -> [Term] -> Result				-- Removes all variables from facts not present in args, e.g. rinse [X -> a, Y -> b, Z -> c] [X, Y] = [X -> a, Y -> B]
rinse (Fact facts) args = Fact $ nub $ map(\fact -> filter (\(t1, t2) -> elem t1 args) fact) facts
			
evalRule :: Atom -> Program -> Result	
evalRule atom@(Atom name args) program = return
	where
		rules = filter (\(Clause _atom@(Atom _name _args) children) -> name == _name && length args == length _args && children /= []) program	-- [Clause] contains all rules that have the same name and amount of arguments, which can prove this atom correct. Rules have children.
		proveRules = map (\clause -> evalSingleRule atom clause program) rules	-- [Result]			The result of the evaluation of 'rules'
		isFact = any (\rule -> rule /= NoFact) proveRules						-- Bool				If there is any rule that is a Fact, then this atom is proven correct.
		mergeSubs = foldl (++) [] $ map (\(Fact sub) -> sub) proveRules			-- [[(Term, Term)]]	Changes [Fact l1, Fact l2, Fact lx@[(Term, Term)]] into [L1, L2, L3, L4]
		return 
			| isFact = Fact mergeSubs											-- Fact 			Returns (Fact mergeSubs) if there is any rule that prove this atom correct
			| otherwise = NoFact												-- NoFact			Returns NoFact if there are no rules that prove this atom correect

{- ======= PROVING IF THE ATOM IS A CORRECT RULE ======= -}		
evalSingleRule :: Atom -> Clause -> Program -> Result		
evalSingleRule atom@(Atom name args) clause@(Clause atomC@(Atom nameC argsC) children) program = return
	where
		newChildren		= swapAtoms children $ createMapping args argsC			-- [Atom]				Swaps all default arguments in every child to the input arguments
		evalChildren	= map (\child -> evalAtom child program) newChildren	-- [Result] 			The result of evaluating every child
		isFact 			= notElem NoFact evalChildren							-- Bool					This rule is proven correct if every child of this rule is proven correct
		subsList		= map (\(Fact sub) -> sub) evalChildren					-- [[[(Term, Term)]]]	Changes [Fact l1, Fact l2, Fact lx@[[(Term, Term)]] ] into [l1, l2, lx]
		mergeSubsList 	= mergeTerms subsList									-- [[(Term, Term)]]		Compares all subs with each other and discards those that contradict eachother, e.g. Fact[(Var X, Const a)] and Fact[(Var X, Const b)]
		return 
			| isFact == False = NoFact											-- Bool					Returns NoFact if there is a child that is proven incorrect
			| otherwise = Fact mergeSubsList									-- Result				Returns (Fact mergeSubsList) if all children are proven correct. Contains all possible combination of variables in the arguments that prove this rule correct


{- ======= SWAPPING THE DEFAULT ARGUMENTS AND THE INPUT ARGUMENTS ======= -}			
createMapping :: [Term] -> [Term] -> [(Term, Term)]
createMapping [] [] = []
createMapping (x:xs) (y:ys) = (y, x) : createMapping xs ys

swapAtoms :: [Atom] -> [(Term, Term)] -> [Atom]
swapAtoms atoms mapping = map (\(atom@(Atom name vars)) -> Atom name (swapVars vars mapping)) atoms

swapVars :: [Term] -> [(Term, Term)] -> [Term]
swapVars listOfVars mapping = map (\var -> getSwap var mapping) listOfVars

getSwap :: Term -> [(Term, Term)] -> Term		
getSwap var mapping = result 
	where 
		_filter = (filter ((==var).fst) mapping)
		result
			| _filter == [] = var
			| otherwise = snd $ _filter !! 0
			
{- ======= PROVING IF THE ATOM IS A CORRECT CONSTANT ======= -}					
evalFact :: Atom -> Program -> Result
evalFact atom@(Atom name args) program = return
	where
		return
			| args 		 == []	= Fact []			-- The atom does exist, and has no arguments, thus proving the atom correct
			| allFacts 	 == [] 	= NoFact			-- The atom doesn't exist on the RHS, thus proving it incorrect
			| subsCorrect== [[]]= Fact [] 			-- There is no need to substitute any arguments, thus proving the atom correct
			| subsCorrect==[] 	= NoFact			-- There are no subs left after removing every Nothing, meaning that all facts were incorrect, thus proving the atom incorrect
			| otherwise  = Fact subsCorrect			-- There are facts that are correct, thus proving the atom correct
		allFacts 	= filter (\(Clause _atom@(Atom _name _args) ac) -> (length args) == (length _args) && name == _name && ac == []) program	-- [Clause]		This returns every fact that has the same name and amount of arguments as the atom that can prove it correct. Facts has no children.
		subsAll	 	= map (\(Clause _atom _) -> compareAtoms atom _atom) allFacts					-- [Just [(Term, Term)]]		All mappings from arguments of atom to arguments of rule
		subsCorrect	= map (\(Just mapping) -> mapping) $ filter (\sub -> sub /= Nothing) subsAll	-- [[(Term, Term)]]				Remove all Nothing and extracts mappings from (Just mapping), leaving only the correct mappings that make this atom True
		
{- ======= COMPARING COMPATIBILITY DEFAULT ARGUMENTS AND INPUT ARGUMENTS ======= -}
compareAtoms :: Atom -> Atom -> Maybe [(Term, Term)]
compareAtoms (Atom t1 args1) (Atom t2 args2) = compareArgs args1 args2

compareArgs :: [Term] -> [Term] -> Maybe [(Term, Term)]
compareArgs [] [] = Just []
compareArgs a1@(x:xs) a2@(y:ys) = return
	 where
		compare	= compareArg x y
		next	= compareArgs xs ys
		return
			| compare == Nothing = Nothing
			| next    == Nothing = Nothing
			| otherwise= Just (compareList ++ nextList)
		(Just compareList)	= compare
		(Just nextList)		= next
		
compareArg :: Term -> Term -> Maybe [(Term, Term)]
compareArg t1@(Var v1)   t2@(Var v2)	= Just [(t1, Anything)]	-- VAR TO VAR		= ANYTHING
compareArg t1@(Var v1)   t2@(Const v2)	= Just [(t1, t2)]		-- VAR TO CONST		= VAR -> CONST
compareArg t1@(Const v1) t2@(Var v2) 	= Just []				-- CONST TO VAR		= []
compareArg t1@(Const v1) t2@(Const v2)							-- CONST TO CONST	= [] || Nothing
	| t1 == t2 = Just []
	| otherwise = Nothing

mergeTerms :: [[[(Term, Term)]]] -> [[(Term, Term)]]
mergeTerms [l] = l
mergeTerms (l1:l2:ls) = mergeTerms(merge : ls)
	where
		merge = [(nub (x ++ y)) | x <- l1, y <- l2, not (conflictingSub x y)]

conflictingSub s1 s2 = [] /= [(t1, t2) | t1@(t11, t12) <- s1, t2@(t21, t22) <- s2, t11 == t21, t12 /= t22 && notElem Anything [t12, t22]]
	
