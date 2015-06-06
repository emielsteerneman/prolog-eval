module Types where

type Program = [Clause]

data Clause = Clause Atom [Atom] deriving(Show, Eq)

data Atom = Atom String [Term] deriving(Show, Eq)

data Term = Const String | Var String | Anything deriving(Show, Eq)
data Sub =  Sub [(Term, Term)] | NoSub deriving (Show, Eq)

data Result = Fact [Sub] | NoFact | Wer deriving(Show, Eq)