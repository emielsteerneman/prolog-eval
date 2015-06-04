module Types where

type Program = [Clause]

data Clause = Clause Atom [Atom] deriving(Show, Eq)

data Atom = Atom String [Term] deriving(Show, Eq)

data Term = Const String | Var String deriving(Show, Eq)