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

instance Show Result where
	show (Fact subs) = "Fact " ++ (foldl (++) [] (map (\sub -> "\n\t" ++ (printSub sub)) subs))
	show NoFact = "NoFact"

printSub :: [(Term, Term)] -> String
printSub [(t1, t2)] = "(" ++ (show t1) ++ " -> " ++ (show t2) ++ ")" 
printSub (x:xs) = (printSub [x]) ++ ", " ++ (printSub xs)

