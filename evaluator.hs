import Data.Char
import Debug.Trace
import Data.List
--------------------------
-- TOKENIZER
--------------------------
tt = tokenize "b0 :- A0, a1." -- Test Tokenizer

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
	| isDot   c = DOT : tokenize cs								-- if . , then end of line
	| isColon c = (OP op) : tokenize opRest						-- if : , then operator
	| elem c " ," = tokenize cs									-- skip ws and comma
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

data Program = Rule Token Token [Token]
	deriving(Show)
	

parse (const@(CONST conStr) : op@(OP opStr) : xs) 
	| isValid = :Rule const op arguments
	| otherwise = error "No arguments or invalid arguments"
	where
		(isValid, arguments, rest) = validArguments xs
	
validArguments ((DOT) : xs) = (True, [], xs)
validArguments (const@(CONST str) : xs) = (isValid, const : arguments, rest)
	where
		(isValid, arguments, rest) = validArguments xs

--validArguments ((VAR   str) : xs) = validArguments xs
validArguments xs = (False, [], xs)

-- let y = "b0 c0 d0. e0 f0 g0"
