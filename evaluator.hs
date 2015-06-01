import Data.Char
import Debug.Trace
import Data.List
--------------------------
-- TOKENIZER
--------------------------
tstr = "a. b. c. d. e :- a, b, c. f :- a, b, z."
{-
Fact (CONST "a")
Fact (CONST "b")
Fact (CONST "c")
Fact (CONST "d")
Rule (CONST "e") [CONST "a",CONST "b",CONST "c"]	True
Rule (CONST "f") [CONST "a",CONST "b",CONST "z"]	False
-}

enter str = table
	where
		splitted = splitAtDot str
		tokenized = map (\rule -> tokenize rule) splitted
		parsed = map (\rule -> parse rule) tokenized
		table = buildTruthTable parsed
	
splitAtDot [] = []
splitAtDot str@(x:xs) 
	| x == '.' = splitAtDot xs
	| otherwise = ( (takeWhile (/= '.') str) : ( splitAtDot (dropWhile (/= '.') str) ) )

data Token =  
	  CONST String			-- constant
	| VAR 	String			-- variable
	| OP 	String			-- operator
	| DOT					-- end of line
	deriving(Show)
	
tokenize :: String -> [Token]
tokenize [] = []
tokenize str@(c : cs)
	| isLower c = (CONST word) : tokenize wordRest				-- if lower letter, then constant
	| isUpper c = (VAR word) : tokenize wordRest				-- if upper letter, then variable
	| isDot   c = tokenize cs									-- if . , then end of line
	| isColon c = (OP op) : tokenize opRest						-- if : , then operator
	| elem c " ,\n\t\r" = tokenize cs									-- skip ws and comma
	| otherwise = trace ("  --|" ++ [c] ++ "|--  ") $ error "Unrecognized character " 	-- unrecognized character
	where 
		(word, wordRest) = getWord str							-- e.g. turns "a0 b0 c0." into ("a0", " b0 c0.")
		(op  , opRest  ) = getOp str							-- e.g. turns ":= b0 c0." into (":=", " b0 c0.")

		
-- HELPER FUNCTIONS
getWord :: String -> (String, String)
getWord str = (takeWhile isAlphaNum str, dropWhile isAlphaNum str)	-- "a-zA-Z0-9" are accepted

getOp :: String -> (String, String)
getOp (':' : '-' : cs) = (":-", cs)

isDot :: Char -> Bool
isDot c = c == '.'

isColon :: Char -> Bool
isColon c = c == ':'

--------------------------
-- PARSER
--------------------------

data Clause = 
	  Rule Token [Token]
	| Fact Token
	deriving(Show)

parse :: [Token] -> Clause
-- FACT
parse [const@(CONST conStr)] = Fact const
-- RULE
parse (const@(CONST conStr) : op@(OP opStr) : xs) = 
	Rule const xs
	where
		args = map (\c@(CONST str) -> Fact c ) xs
		
x = tokenize "a."
y = tokenize "a :- b, c, d."

buildTruthTable :: [Clause] -> [String]
buildTruthTable parsed = buildTruthTable' [] parsed

buildTruthTable' :: [String] -> [Clause] -> [String]
buildTruthTable' table [] = table
buildTruthTable' table ((Fact (CONST str)) : xs) = buildTruthTable' (table ++ [str]) xs
buildTruthTable' table ((Rule (CONST str) args) : xs) = buildTruthTable' newTable xs
	where
		truth = map (\(CONST var) -> elem var table) args
		isTrue = all (==True) truth
		newTable
			| isTrue = table ++ [str]
			| otherwise = table








