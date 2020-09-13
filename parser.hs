-- Parser

module NanoParsec where
    import Data.Char
    import Control.monad
    import Control.Applicative

-- Parser type
newtype Parser a = Parser { parse :: String -> [(a, String)]}

runParser :: Parser a -> String -> a 
runParser m s = 
    case parse m s of 
        [(res, [])] -> res
        [(_, rs)]   -> error "Parser did not consume the entire stream."
        _           -> error "Parser Error."

item::Parser Char
item = Parser $ \s ->
    case s of 
        []     -> []
        (c:cs) -> [(c,cs)]

bind::Parser a -> (a -> PArser b) -> Parser b