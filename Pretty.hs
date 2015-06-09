module Pretty
	(show)
	where

import Types	

instance Show Clause where
	show (Clause atom children) = "Clause " ++ show(atom) ++ " : " ++ show(children) ++ " "

instance Show Term where
	show (Const v) = "" ++ v ++ ""
	show (Var v) = "" ++ v ++ ""
	show Anything = "Anything"

instance Show Atom where
	show (Atom name args) = show(name) ++ " " ++ show(args)

-- instance Show Sub where
	-- show (Sub list) = " Sub[" ++ printSubs list ++ "] "
	-- show NoSub = "NoSub"
	
instance Show Result where
	show (Fact subs) = "Fact " ++ show(subs)
	show NoFact = "NoFact"

printSubs :: [(Term, Term)] -> String
printSubs [(t1, t2)] = "(" ++ (show t1) ++ " -> " ++ (show t2) ++ ")" 
printSubs (x:xs) = (printSubs [x]) ++ ", " ++ (printSubs xs)

