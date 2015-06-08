import Pretty
import Test
import Types
import Project


t0 = query (Atom "woman" [Const "juliana"]) royalfamily
t1 = query (Atom "woman" [Var "X"]) royalfamily
t2 = query (Atom "mother" [Const "juliana",Const "beatrix"]) royalfamily
t3 = query (Atom "mother" [Const "juliana",Var "X"]) royalfamily
t4 = query (Atom "mother" [Var "X",Var "Y"]) royalfamily
t5 = query (Atom "child" [Const "beatrix", Var "X"]) royalfamily
t6 = query (Atom "child" [Var "X", Var "Y"]) royalfamily