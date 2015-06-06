import Test
import Types
import Project

t0 = evalAtom (Atom "woman" [Const "juliana"]) royalfamily
t1 = evalAtom (Atom "woman" [Var "X"]) royalfamily
t2 = evalAtom (Atom "mother" [Const "juliana",Const "beatrix"]) royalfamily
t3 = evalAtom (Atom "mother" [Const "juliana",Var "X"]) royalfamily
t4 = evalAtom (Atom "mother" [Var "X",Var "Y"]) royalfamily
t5 = evalAtom (Atom "child" [Const "beatrix", Var "X"]) royalfamily
t6 = evalAtom (Atom "child" [Var "X", Var "Y"]) royalfamily