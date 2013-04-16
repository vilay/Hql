{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import Control.Applicative hiding (many, (<|>))

import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec

-- overloaded operator to concatenate string results from parsers
--newtype String1 = String1 { unString :: [Char] }

class CharOrStr a where toStr :: a -> String

instance CharOrStr Char where toStr x = [x]
instance CharOrStr String where toStr = id 

infixl 4 <++>
f <++> g = (\x y -> toStr x ++ toStr y) <$> f <*> g

data Keyword = Select | Update | Delete | From | Where deriving (Eq, Show)

parseKw =
    (Select <$ string "select") <|>
    (Update <$ string "update") <|>
    (Delete <$ string "delete") <|>
    (From <$ string "from") <|>
    (Where <$ string "where") <?>
    "keyword (select, update, delete, from, where)"

-- consume spaces, then eat a word or parenthesis
parseOther = many space <++>
    (("" <$ lookAhead (try parseKw)) <|> -- if there's a keyword, put it back!
     option "" ((parseParen <|> many1 (noneOf "() \t")) <++> parseOther))

parseSqlToplevel = many ((,) <$> parseKw <*> (space <++> parseOther)) <* eof

parseParen = char '(' <++> inner <++> char ')' where
    inner = many (noneOf "()") <++> option "" (parseParen <++> inner)


parseSQL :: String -> Either ParseError [(Keyword, [Char])]
parseSQL input = parse parseSqlToplevel "(Unknown)" input

main = do
       putStrLn "hello"
