import Data.Char
import Debug.Trace

--------------------------
-- TOKENIZER
--------------------------
-- Test Tokenizer
tt = tokenize "b0 :- A0, a1."

data Token =  
	  CONST String
	| VAR String
	| OP
	| DOT
	deriving(Show)
	
tokenize :: String -> [Token]
tokenize [] = []
tokenize str@(c : cs)
	| isLower c = (CONST word) : tokenize wordRest
	| isUpper c = (VAR word) : tokenize wordRest
	| isDot c = DOT : tokenize cs
	| c == '-' = OP : tokenize cs
	| elem c " :," = tokenize cs
	| otherwise = trace ("  --|" ++ [c] ++ "|--  ") $ error "Unrecognized character "
	where 
		(word, wordRest) = getWord str

getWord str = (takeWhile isAlphaNum str, dropWhile isAlphaNum str)

-- HELPER FUNCTIONS
isDot :: Char -> Bool
isDot c = c == '.'


--------------------------
-- PARSER
--------------------------
