module Types where

type Program = [Clause]

data Clause = Clause Predicate [Clause] deriving(Show)

data Predicate = Predicate String [String] deriving(Show)
