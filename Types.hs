module Types where

type Program = [Clause]

data Clause = Clause Atom [Atom] deriving(Show, Eq)

data Atom = Atom String [String] deriving(Show, Eq)
