import Data.Char
import Debug.Trace

str = "b0 :- a0, a1."


--------------------------
-- TOKENIZER
--------------------------

data Token =  
	  CONST String
	| VAR String
	| OP
	| DOT
	deriving(Show)
	
tokenize :: String -> [Token]
tokenize [] = []
tokenize str@(c : cs)
	| isLower c = (CONST word) : tokenize rest
	| isUpper c = (VAR word) : tokenize rest
	| isDot c = DOT : tokenize cs
	| c == '-' = OP : tokenize cs
	| elem c " :," = tokenize cs
	| otherwise = trace ("  --|" ++ [c] ++ "|--  ") $ error "Unrecognized character "
	where 
		(word, rest) = getWord str

getWord str = (takeWhile isAlphaNum str, dropWhile isAlphaNum str)

-- HELPER FUNCTIONS
isDot :: Char -> Bool
isDot c = c == '.'


--------------------------
-- PARSER
--------------------------
