import Test
import Types
import Debug.Trace

t = Predicate "a0" []
m = evalProp (Predicate "mother" []) royalfamily

evalProp p@(Predicate name vars) program = trace ("evaluating: " ++ name) $ truth
	where
	-- all children True?
		truth
			| truthTable == [] = False					-- The predicate doesn't exist
			| otherwise = notElem False truthTable		-- If false not in truthTable, then true
	--	for each Clause is [Clause]
		truthTable = map (\clause-> evalClause clause) clauses
	-- [Clause]
		clauses = filter (\(Clause (Predicate cname vars) list) -> name == cname) program

evalClause c@(Clause p@(Predicate name vars) children) = truth
	where
		truth = notElem False truthTable
		truthTable = map (\clause -> evalClause clause) children
