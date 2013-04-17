{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module HqlParser where
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
               | Select HqlSelectQuery
               | Insert HqlInsertQuery
               | Update HqlUpdateQuery
               | Drop
               | Delete HqlDeleteQuery
               | Alter   deriving (Show)



hqlDef =
 emptyDef {   Token.commentStart    = "/*"
            , Token.commentEnd      = "*/"
            , Token.commentLine     = "--"
            , Token.identStart      = alphaNum
            , Token.identLetter     = alphaNum
            , Token.reservedNames   = [ "create" , "select" , "insert" , "drop" , "delete" , "update" , "alter"
                                      , "table" , "into" , "from" , "values" , "where" , "any" , "all" , "rename" , "add" , "to"
                                      , "some", "set" , "column"
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


main :: IO Integer                    -- Parses input query
main = 

       -- do input <- readFile "query.txt"
        do input <- getLine
           case (parse hqlParser "(Unknown)" input) of
                Left  x -> do print x 
                              return (-2)
                Right z -> case z of 
                            Create y -> execHqlCreateTable y
                            Select y -> execHqlSelectTable y input
                            Insert y -> execHqlInsert y input
                            Update y -> execHqlUpdate y input
                            Drop     -> execHql input
                            Delete y -> execHqlDelete y input
                            Alter    -> execHql input
                            otherwise -> return (-3)
        
---------------------------------------------------------------------------------------
--    Top level Parser                                                              --
--                                                                                                                                                                                                                              --
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
          <|> insertStmt
          <|> updateStmt   
          <|> selectStmt   
          <|> dropStmt  
          <|> deleteStmt
          <|> alterStmt
          <?> "Valid Statements"
 
 {--         <|> deleteStmt
          <|> updateStmt
          <|> dropStmt
          <|> alterStmt--}
---------------------------------------------------------------------------------------
--    Individual Query Parser                                                       --
--                                                                                                                                                                      --
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
splitContent input = map ((T.unpack).(T.strip)) (T.splitOn (T.pack ",") (T.pack input))
--splitContent input = map T.unpack (T.splitOn (T.pack ",") (T.pack input))


getParaContent :: [String] -> ([HqlColumn],[HqlType])
getParaContent input = (,) (map (head.words) input) (map (stringToHqlType.last.words) input)


---------------------------------------------------------------------------------------
-- Select Query

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
                                semi
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
          

--------------------------------------------------------------------------------------
-- Where Clause

parseWhereClause :: Parser HqlExp
parseWhereClause = 
        do  whiteSpace 
            (try parseHqlLogicExpNoBrace) <|> (try parseHqlSubExp) <|> (try parseHqlRelExp) <?> "Valid where clause"
            --parseHqlRelExp
            --(try parseHqlLogicExp) <|> (try parseHqlRelExp) <|> (try parseHqlSubExp) <?> "Valid where clause"
           

parseHqlLogicExpNoBrace :: Parser HqlExp
parseHqlLogicExpNoBrace =
           do whiteSpace;
              skipMany (oneOf "()")
              whiteSpace
              qry1 <- (try parseHqlSubExp) <|> (try parseHqlRelExp) <?> "Valid clause in logical expression"
              whiteSpace
              skipMany (oneOf "()")
              whiteSpace
              subLogOp <- parseLogicalOp
              whiteSpace
              qry2 <- parseWhereClause;
              whiteSpace
              skipMany (oneOf "()");
                                return (HqlLogicExp subLogOp qry1 qry2)
        
{--                     
parseHqlLogicExpBrace :: Parser HqlExp
parseHqlLogicExpBrace =
           do whiteSpace
              skipMany (char '(')
              qry1 <- parseWhereClause
              whiteSpace
              subLogOp <- parseLogicalOp
              whiteSpace
              qry2 <- parseWhereClause;
              skipMany (char ')');
                                return (HqlLogicExp subLogOp qry1 qry2) 
            --parseHqlRelExp
            --(try parseHqlLogicExp) <|> (try parseHqlRelExp) <|> (try parseHqlSubExp) <?> "Valid where clause"
--}        


parseHqlSubExp :: Parser HqlExp
parseHqlSubExp =
        do whiteSpace;
           colExp <- parseHqlColumnExp;
           whiteSpace
           --relOpType <- parseRelOp
           --whiteSpace
           subOpType <- parseSubOp
           whiteSpace
           char '('
           whiteSpace
           subQry <- subSelectStmt
           whiteSpace
           case subQry of
                 Select y -> return (HqlSubExp Equals subOpType colExp y)
                 otherwise -> return HqlEmpty


parseHqlRelExp :: Parser HqlExp
parseHqlRelExp =
        do whiteSpace;
           colExp <- parseHqlColumnExp;
           whiteSpace;
           relOpType <- parseRelOp;
           whiteSpace;
           colConst <- parseHqlConstExp;
           return (HqlRelExp relOpType colExp colConst )
--return (HqlRelOpExp relOpType colExp colConst)

parseHqlColumnExp :: Parser HqlExp
parseHqlColumnExp = 
        do whiteSpace;
                  colName <- identifier;
                  whiteSpace;
                  return (HqlColumnExp colName)


parseHqlConstExp :: Parser HqlExp
parseHqlConstExp = 
        do    whiteSpace;
              x <-  (try parseFloat) <|> (try identifier) <|>  (try parseStringLiteral)  <?> "Some valid value for given column name"
              return (HqlConstExp x)
       --return (HqlConstExp "'prashant'") 

parseStringLiteral :: Parser String
parseStringLiteral = do (char '"') ; x <- many (noneOf "\""); (char '"'); return ("\'" ++ x ++ "\'")
                          
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

parseSubOp :: Parser HqlSubOp
parseSubOp =
           (In    <$  try(string "in")) <|>
           (In    <$  try(string "In")) <?>
           "Valid SubQuery operator"

parseLogicalOp :: Parser HqlLogOp
parseLogicalOp =
           (AND    <$  try(string "and")) <|>
           (AND    <$  try(string "AND")) <|>
           (OR     <$  try(string "or")) <|>
           (OR     <$  try(string "OR")) <?>
           "Valid SubQuery operator"


subSelectStmt :: Parser HqlQuery
subSelectStmt =
        do reserved "select"
           whiteSpace
           colName <- parseTillFrom []
           whiteSpace
           --reserved "from"
           whiteSpace
           tabName <- identifier
           whiteSpace
           isSemi <- string ")" <|> string "where" <?> "Where keyword or semicolon"
           case ((head isSemi) == ')') of 
                    True -> return ( Select (HqlSelectQuery tabName colName HqlEmpty) )
                    False -> do 
                                exp <- parseWhereClause
                                return (Select (HqlSelectQuery tabName colName  exp  ) )
    
---------------------------------------------------------------------------------------
-- insert  Query

insertStmt :: Parser HqlQuery
insertStmt = (try insertStmt1) <|> insertStmt2

insertStmt1 :: Parser HqlQuery
insertStmt1 = 
        do reserved "insert"
           whiteSpace
           reserved "into" 
           whiteSpace
           tabName <- identifier 
           whiteSpace
           reserved "values" 
           whiteSpace
           char '('
           paraContent <- parsePara
           whiteSpace
           char ')'
           whiteSpace
           semi
           return (Insert (HqlInsertQuery tabName [] (splitContent paraContent)))

insertStmt2 :: Parser HqlQuery
insertStmt2 = 
        do reserved "insert"
           whiteSpace
           reserved "into" 
           whiteSpace
           tabName <- identifier 
           whiteSpace
           char '('
           colsContent <- parsePara
           whiteSpace
           char ')'
           whiteSpace
           reserved "values" 
           whiteSpace
           char '('
           valuesContent <- parsePara
           whiteSpace
           char ')'
           whiteSpace
           semi
           return (Insert (HqlInsertQuery tabName (splitContent colsContent) (splitContent valuesContent)))

---------------------------------------------------------------------------------------
--Update Query  

updateStmt :: Parser HqlQuery
updateStmt = (try updateStmtNoWhere) <|> (try updateStmtWithWhere) <?> "Valid update query"

updateStmtNoWhere :: Parser HqlQuery
updateStmtNoWhere = 
        do reserved "update"
           whiteSpace
           tabName <- identifier
           whiteSpace
           reserved "set"
           whiteSpace
           setcols <- parseTillWhere [] []
           whiteSpace
           semi
           return $ (Update (HqlUpdateQuery tabName (fst setcols) (snd setcols) HqlEmpty))
           
updateStmtWithWhere :: Parser HqlQuery
updateStmtWithWhere = 
        do reserved "update"
           whiteSpace
           tabName <- identifier
           whiteSpace
           reserved "set"
           whiteSpace
           setcols <- parseTillWhere [] []
           whiteSpace
           exp <- parseWhereClause
           whiteSpace
           semi
           return $ (Update (HqlUpdateQuery tabName (fst setcols) (snd setcols) exp))

parseTillWhere :: [String] -> [String] -> Parser ([String],[String])
parseTillWhere x y= do whiteSpace
                       colName <- identifier
                       whiteSpace
                       char '='
                       whiteSpace
                       z <-  (try parseFloat) <|> (try identifier) <|> (try parseStringLiteral)  <?> "Some valid value for given column name"
                       whiteSpace
                       keywrd <- (try (string ";")) <|> (try (string ",")) <|> (try (string "where"))
                       case (head keywrd == ',') of
                          True -> parseTillWhere ( x ++ [colName]) (y ++ [z])
                          False -> return (( x ++ [colName]),(y ++ [z]))


---------------------------------------------------------------------------------------
-- Drop Query

dropStmt :: Parser HqlQuery
dropStmt = 
            do  whiteSpace
                reserved "drop"
                whiteSpace
                reserved "table"
                whiteSpace
                identifier
                whiteSpace
                semi <?> "Semicolon"
                return Drop


---------------------------------------------------------------------------------------
-- Delete Query

deleteStmt :: Parser HqlQuery
deleteStmt = (try deleteStmtNoWhere) <|> (try deleteStmtWhere) <?> "Valid delete query"

deleteStmtWhere :: Parser HqlQuery
deleteStmtWhere =
      do whiteSpace
         reserved "delete"
         whiteSpace
         reserved "from"
         whiteSpace
         tabName <- identifier
         whiteSpace
         reserved "where"
         whiteSpace
         exp <- parseWhereClause
         whiteSpace
         semi <?> "Semicolon"
         return (Delete (HqlDeleteQuery tabName exp )) 

deleteStmtNoWhere =
             do whiteSpace
                reserved "delete"
                whiteSpace
                reserved "from"
                whiteSpace
                tabName <- identifier
                whiteSpace
                semi <?> "Semicolon"
                return (Delete (HqlDeleteQuery tabName HqlEmpty))

---------------------------------------------------------------------------------------
-- Create Query

alterStmt :: Parser HqlQuery
alterStmt = (try alterStmtRename) <|> (try alterStmtAdd) <?> "Valid Alter statement"

alterStmtRename :: Parser HqlQuery
alterStmtRename =
                do  whiteSpace
                    reserved "alter"
                    whiteSpace
                    reserved "table"
                    whiteSpace
                    tabName <- identifier
                    whiteSpace
                    reserved "rename"
                    whiteSpace
                    reserved "to"
                    whiteSpace
                    newTabName <- identifier
                    whiteSpace
                    semi <?> "Semicolon"
                    return Alter



alterStmtAdd :: Parser HqlQuery
alterStmtAdd =
               do  whiteSpace
                   reserved "alter"
                   whiteSpace
                   reserved "table"
                   whiteSpace
                   tabName <- identifier
                   reserved "add"
                   whiteSpace
                   reserved "column"
                   whiteSpace
                   tabDef <- identifier
                   whiteSpace
                   semi <?> "Semicolon"
                   return Alter

