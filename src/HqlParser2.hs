{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module HqlParser2 where
import Hql

import Control.Applicative hiding (many, (<|>))
import Text.Parsec.Combinator
import Data.Char

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import qualified Data.Text as T



data HqlQuery = SeqList [HqlQuery]
               | Create HqlCreateTable 
               | Select HqlSelectQuery   deriving (Show)



hqlDef =
 emptyDef {   Token.commentStart    = "/*"
            , Token.commentEnd      = "*/"
            , Token.commentLine     = "--"
            , Token.identStart      = alphaNum
            , Token.identLetter     = alphaNum
            , Token.reservedNames   = [ "create" , "select" , "insert" , "drop" , "delete" , "update" 
                                      , "table" , "into" , "from" , "values" , "where" , "any" , "all"
                                      , "some"
                                      ]
            , Token.reservedOpNames = [ "<", ">", "and", "or", ">=" , "<=" , "=" 
                                      ]
            , Token.caseSensitive   = False
            }


hqLexer = Token.makeTokenParser hqlDef

identifier = Token.identifier hqLexer -- parses an identifier
reserved   = Token.reserved   hqLexer -- parses a reserved name
reservedOp = Token.reservedOp hqLexer -- parses an operator
parens     = Token.parens     hqLexer -- parses surrounding parenthesis: parens p takes care of the parenthesis and uses p to parse what's inside them
integer    = Token.integer    hqLexer -- parses an integer
semi       = Token.semi       hqLexer -- parses a semicolon
whiteSpace = Token.whiteSpace hqLexer -- parses whitespace


main :: IO Integer
main = 
	do input <- getLine
	   case (parse hqlParser "(Unknown)" input) of
	    	Left  x -> do print x 
	    	              return (-2)
	    	Right z -> case z of 
                            Create y -> execHqlCreateTable y
                            Select y -> execHqlSelectTable y input
                            otherwise -> return (-3)
        
---------------------------------------------------------------------------------------
--    Top level Parser                                                              --
-- 																					--
---------------------------------------------------------------------------------------
hqlParser :: Parser HqlQuery
hqlParser = whiteSpace >> hqlQueryParser

hqlQueryParser :: Parser HqlQuery
hqlQueryParser = parens hqlQueryParser <|> 
                 sequenceOfQuery

sequenceOfQuery :: Parser HqlQuery
sequenceOfQuery = hqlStmt
      --do 
      --list <- (sepBy1 hqlStmt semi)
      --return $ if length list == 1 then (head list) else (SeqList list)
      --return (head list)

hqlStmt :: Parser HqlQuery
hqlStmt = createStmt 
          <|> selectStmt     
          <?> "Valid Statements"
 {-- -- 
     <|> insertStmt
          <|> deleteStmt
          <|> updateStmt
          <|> dropStmt
          <|> alterStmt--}
---------------------------------------------------------------------------------------
--    Individual Query Parser                                                       --
-- 																					--
---------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------
-- Create Query

createStmt :: Parser HqlQuery
createStmt = 
	do reserved "create"
	   whiteSpace
	   reserved "table" 
	   whiteSpace
	   tabName <- identifier 
	   whiteSpace
	   char '('
	   paraContent <- parsePara
	   whiteSpace
	   char ')'
	   whiteSpace
	   semi 
	   return ( Create ( HqlCreateTable tabName (fst (getParaContent (splitContent paraContent) )) (snd (getParaContent (splitContent paraContent)))) )

parsePara :: Parser String
parsePara = many (noneOf ")\n")



splitContent :: String -> [String]
splitContent input = map T.unpack (T.splitOn (T.pack ",") (T.pack input))

getParaContent :: [String] -> ([HqlColumn],[HqlType])
getParaContent input = (,) (map (head.words) input) (map (stringToHqlType.last.words) input)


---------------------------------------------------------------------------------------
--Select Query

selectStmt :: Parser HqlQuery
selectStmt =
	do reserved "select"
	   whiteSpace
	   colName <- parseTillFrom []
	   whiteSpace
	   --reserved "from"
	   whiteSpace
	   tabName <- identifier
	   whiteSpace
	   isSemi <- semi <|> string "where" <?> "Where keyword or semicolon"
	   case ((head isSemi) == ';') of 
	   	    True -> return ( Select (HqlSelectQuery tabName colName HqlEmpty) )
	   	    False -> do 
	   	                exp <- parseWhereClause
	   	                return (Select (HqlSelectQuery tabName colName  exp  ) )

	   
parseTillFrom :: [String] -> Parser [String]
parseTillFrom x =
	do whiteSpace
	   colName <- string "*" <|> identifier
	   case (head colName == '*') of
	   	True -> do whiteSpace; reserved "from"; return ((++) x [colName]) 
	   	False -> do whiteSpace; keywrd <- string "," <|> string "from" ;
	   	            case (head keywrd == ',') of
	   	             True -> parseTillFrom ((++) x [colName])
	   	             False -> return ((++) x [colName])
	  


parseWhereClause :: Parser HqlExp
parseWhereClause = 
	do  whiteSpace 
	    parseHqlRelExp
	    --(try parseHqlLogicExp) <|> (try parseHqlRelExp) <|> (try parseHqlSubExp) <?> "Valid where clause"
	   



{--
parseHqlRelExp :: Parser HqlExp
parseHqlRelExp =
	do  whiteSpace

	    return HqlEmpty
     
--}       

{--
parseWhereClasue' :: Parser HqlExp
parseWhereClasue' = (try parseHqlColumnExp)
                    <|> (try parseHqlConstExp)
	                <|> (try parseHqlLogicExp) 
                    <|> (try parseHqlRelExp) 
                    <|> (try parseHqlSubExp) 
                    <?> "Valid where clause"
	
--}

parseHqlRelExp :: Parser HqlExp
parseHqlRelExp =
	do whiteSpace;
	   colExp <- parseHqlColumnExp;
	   whiteSpace;
	   relOpType <- parseRelOp;
	   whiteSpace;
	   colConst <- parseHqlConstExp;
	   return (HqlRelExp Equals colExp colConst )
--return (HqlRelOpExp relOpType colExp colConst)

parseHqlColumnExp :: Parser HqlExp
parseHqlColumnExp = 
	do whiteSpace;
		  case (False) of
		  	False -> do colName <- identifier;return (HqlColumnExp colName)
		  	--True  -> do parseWhereClause'
     --return (HqlColumnExp "name")

parseHqlConstExp :: Parser HqlExp
parseHqlConstExp = 
	do --whiteSpace
       
       x <-  (try identifier) <|> (try parseFloat) <|> (try parseStringLiteral)  <?> "Some valid value for given column name"
       return (HqlConstExp x)
       --return (HqlConstExp "'prashant'") 

parseStringLiteral :: Parser String
parseStringLiteral = do char '\''; x <- identifier; char '\''; return ("\'" ++ x ++ "\'")

parseFloat :: Parser String
parseFloat = do x <- identifier; char '.' ; y <- identifier ; return (x ++ "." ++ y)

parseRelOp :: Parser HqlRelOp
parseRelOp =
    (GreaterThan    <$ try(string ">")) <|>
    (LessThan       <$ try(string "<")) <|>
    (LTEqual        <$ try(string "<=")) <|>
    (GTEqual        <$ try(string ">=")) <|>
    (Equals         <$ try(string "=")) <?> 
    "Valid relational operator"
    

{--
insertStmt :: Parser HqlQuery

deleteStmt :: Parser HqlQuery
 
updateStmt :: Parser HqlQuery

dropStmt :: Parser HqlQuery

alterStmt :: Parser HqlQuery
-}

--}
