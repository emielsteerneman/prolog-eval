Our program differs a little bit from the original assignment.
Instead of having multiple functions, we created one function that can evaluate all queries from all cases.
this function is called query and input is an atom: query (Atom String [Term]).
an example: query (Atom "a" [Var "X", Const "x"])
	

The testCase.hs file can be used for testing the queries for the royal family.
The following cases have been tested:
t0 -> This queries whether "juliana" is a woman. This is a fact.
t1 -> This queries variable X for every woman, this returns the list with
    every case this applies to. So juliana, beatrix, margriet, irene 
    and christina are all woman.
t2 -> This queries whether juliana is the mother of beatrix. This is a fact
t3 -> This queries variable X for every person that juliana is the mother of
    The result is beatrix, margriet and irene
t4 -> This queries variable X and Y for every person where X is the mother
    of Y. This results in (X,Y) -> (juliana, beatrix), (juliana, margriet)
    and (juliana,irene)
t5 -> this queries variable X where X is a person with beatrix as child
    this results in juliana and bernhard
t6 -> this queries variable X and Y where X is the child of Y this results
    in (X,Y) -> (beatrix, juliana), (margriet, juliana), (irene, juliana),
    (beatrix, bernhard), (margriet, bernhard)
    
The results of the program are in line the de royalfamily.pl prolog program
from blackboard. 

Emiel Steerneman    1499262
Olaf Haalstra       1482041