module Pretty
	(show)
	where

import Types	

instance Show Clause where
	show (Clause atom children) = "Clause " ++ show(atom) ++ " : " ++ show(children) ++ " "

instance Show Term where
	show (Const v) = "c_" ++ v ++ ""
	show (Var v) = "v_" ++ v ++ ""
	show Anything = "Anything"

instance Show Atom where
	show (Atom name args) = show(name) ++ " " ++ show(args)

-- instance Show Sub where
	-- show (Sub list) = " Sub[" ++ printSubs list ++ "] "
	-- show NoSub = "NoSub"
	
instance Show Result where
	show (Fact subs) = "Fact |> " ++ show(subs) ++ " <|"
	show NoFact = "NoFact"

	
--printSubs :: [(Term, Term)] -> String
printSubs [(t1, t2)] = "(" ++ (show t1) ++ " -> " ++ (show t2) ++ ")" 
printSubs (x:xs) = (printSubs [x]) ++ ", " ++ (printSubs xs)

	
	{-	
instance Show Predicate where
	show (Predicate n ts) = n ++ (showArgs ts)
	
showArgs :: [Term] -> String
showArgs args = "(" ++ (showArgs' args) ++ ")"

showArgs' :: [Term] -> String
showArgs' [] = ""
showArgs' [x] = (show x)
showArgs' (x:xs) = (show x) ++ "," ++ (showArgs' xs)	
	
instance Show Clause where
	show (Clause h []) = (show h) ++ "."
	show (Clause h b) = (show h) ++ ":-" ++ (showBody b) ++ "."

showBody :: [Predicate] -> String
showBody [] = ""
showBody [x] = (show x)
showBody (x:xs) = (show x) ++ "," ++ (showBody xs)
-}