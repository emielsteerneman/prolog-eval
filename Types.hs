module Types where

type Program = [Clause]

data Clause = Clause Atom [Atom] deriving(Eq)

data Atom = Atom String [Term] deriving(Eq)

data Term = Const String 
		|   Var String 
		|   Anything deriving(Eq)
		
--data Sub = Sub [(Term, Term)] | NoSub deriving (Eq)

data Result = Fact [[(Term, Term)]] | NoFact deriving(Eq)