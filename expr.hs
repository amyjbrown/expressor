-- Runtime types for a simple `expr` like simple calculator

data Expr = 
	| Number Double
	| Binary Operator Expr Expr
	| Unary Operator Expr
	deriving (Show)

data Operator = 
	| Plus 		-- +
	| Minus 	-- -
	| Times 	-- *
	| Div 		-- /
	| Rem 		-- //
	deriving (Show)

evaluate :: Expr -> Double

evaluate (Number n) = n 

evaluate (Binary op a b) = 
	case op of 
		Plus	-> evaluate a + evaluate b
		Minus 	-> evaluate a - evaluate b
		Times 	-> evaluate a * evaluate b
		Div		-> evaluate a / evaluate b
		Rem 	-> fromInteger $ (floor a) `rem` (floor b)


evaluate (Unary op a) = 
	case op of 
		Plus 	-> evaluate a -- Plusification, or "transformless transform"
		Minus 	-> - (evaluate a)