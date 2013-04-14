{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Hql2
(
  HqlTable,   
  HqlColumn,
  HqlType(..),
  HqlCreateTable(..),
  HqlDB,
  HqlExp(..),
  HqlLogOp(..),
  HqlRelOp(..),
  execHqlCreateTable,       
  execHqlSelectTable,
  execHqlInsert,
  returnColumnType 
)
where


--importing libraries 
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Char
import Data.List
import Data.Functor
import Control.Applicative

--data types
type HqlTable  = String
type HqlDB     = String
type HqlColumn = String 
type HqlValue = String
 
data HqlType = Int|Varchar|Bool|Double deriving (Show,Ord,Eq)

data HqlCreateTable = HqlCreateTable { tableName  :: HqlTable,
                                       columnList :: [HqlColumn],
                                       typeList   :: [HqlType]
                                     } 
                               deriving Show
--database name
database = "test1.db"                             
        
        
data HqlExp = HqlColumnExp HqlColumn
               | HqlConstExp HqlValue 
               | HqlLogicExp HqlLogOp HqlExp HqlExp
               | HqlRelExp HqlRelOp HqlExp HqlExp       
               deriving (Show)
        
        
data HqlLogOp = AND | OR deriving (Show)

data HqlRelOp = LessThan | GreaterThan | Equals deriving (Show)


   
                                                 
--run query
execRunQuery :: String -> String -> [SqlValue] -> IO Integer
execRunQuery db query param = do conn <- connectSqlite3  db
                                 result <- run conn query param
                                 commit conn
                                 disconnect conn
                                 return result
                          
--quick Query
execQuickQuery :: String -> String -> [SqlValue] -> IO [[SqlValue]]
execQuickQuery db query param = do conn   <- connectSqlite3  db
                                   result <- quickQuery' conn query param
                                   commit conn
                                   disconnect conn
                                   return result
 
            
--creating table
createColumnList :: HqlCreateTable -> String
createColumnList table = let colList = columnList table
                             typList = typeList table
                             colPairList = zipWith (\ x y -> x ++ " " ++ show y ) colList typList 
                          in
                             foldl ( \ x y -> x ++ "," ++ y ) ( head colPairList ) (tail colPairList)

execHqlCreateTable :: HqlCreateTable -> IO Integer
execHqlCreateTable table = 
                            do  
                              let query="Create Table " ++ (tableName table) ++ " ( " ++ createColumnList table ++ " ); "
                              execRunQuery database query []

--return type of column 
returnColumnType :: HqlTable -> HqlColumn -> IO(String)
returnColumnType tabName colName  = do
                                       let query = "PRAGMA table_info("++tabName++");"
                                       r <- execQuickQuery database query []
                                       let
                                          stringRow = map ( \ y -> ((!!) y 1,(!!) y 2) ) r
                                          columnType = filter ( \ (a,_) -> if a == toSql colName then True else False) stringRow
                                       return $ map toUpper (fromSql ( snd $ head columnType )::String)
 
convertToHqlType :: IO(String) -> IO(HqlType)
convertToHqlType strType = do
                              strType' <- strType
                              case strType' of
                                 "INT"     -> return Int
                                 "VARCHAR" -> return Varchar
                                 "BOOL"    -> return Bool
                                 "DOUBLE"  -> return Double
                              
                              
ioAnd :: IO Bool -> IO (Bool -> Bool)
ioAnd ioBool = do
              bool <- ioBool
              return ((&&) bool) 
              
ioEq :: IO String -> IO (String -> Bool)
ioEq ioStr = do
              str <- ioStr
              return ((==) str)                           
                              
                              
                              
validateExp :: HqlTable -> HqlExp -> IO Bool                         
validateExp tabName expr = case expr of                     
                     HqlLogicExp logOp hqlExp1 hqlExp2 -> (<*>) (ioAnd (validateExp tabName hqlExp1)) (validateExp tabName hqlExp2)                      
                     HqlRelExp relOp hqlExp1 hqlExp2   -> (<*>) (ioEq (getType tabName hqlExp1)) (getType tabName hqlExp2)             
         
         
                       
getType :: HqlTable -> HqlExp -> IO String
getType tabName expr = case expr of 
                  HqlColumnExp hqlColumn -> do ans <- (returnColumnType tabName hqlColumn)
                                               print ans
                                               return ans
                  HqlConstExp hqlValue   -> do ans <- (return (getTypeFromValue hqlValue))
                                               print ans
                                               return  ans


getTypeFromValue :: HqlValue -> String 
getTypeFromValue hqlValue = if (isValidString hqlValue)
                              then "VARCHAR"
                              else if (elem hqlValue ["True","False"])
                                     then "BOOL"
                                     else if (isDouble hqlValue)
                                          then "DOUBLE"
                                          else if (and $ (map isDigit hqlValue))
                                               then "INT"
                                               else hqlValue ++ "Invalid"
isDouble :: String -> Bool
isDouble str = let check1 = and $ (map isDigit (filter (/= '.') str))
                   indices = elemIndices '.' str
                   check2 = (length indices) == 1
               in check1 && check2
        
isValidString :: String -> Bool
isValidString str = let indices = elemIndices '\'' str
                        check1 = if ((length indices) == 2)
                                 then ((head indices) == 0) && ((last indices) == ((length str) - 1))
                                 else False 
                        indice = elemIndices '"' str
                        check2 = if ((length indice) == 2)
                                 then ((head indice) == 0) && ((last indice) == ((length str) - 1))
                                 else False 
                    in check1 || check2
                        
                        

--select value from databases.
execHqlSelectTable :: HqlTable -> [HqlColumn] -> HqlExp -> IO ()
execHqlSelectTable tabName colName expr = do  
                                        print expr
                                        check <- validateExp tabName expr
                                        print check
                                        --convert List of column to String.
                                        {-let col colName 
                                                                | colName == ["*"] = " * " 
                                                                | otherwise = foldl ( \ x y -> x ++ "," ++ y ) ( head colName ) ( tail colName )
                                        --create a query
                                        let query = "select " ++ col colName ++ " from " ++ tabName ++ ";"
                                        r <- execQuickQuery database query [];
                                        let convRow :: [SqlValue] -> [String]
                                            x = (head r)
                                            len1 = length x -1
                                            convRow x = map (\ t -> fromSql $ ((!!) x t)::String ) [0..len1]
                                            stringRows = map convRow r        
                                        print stringRows -}
                                        

validateType :: HqlTable -> [HqlColumn] -> [HqlType] -> IO Bool
validateType tabName colName types = do 
                                       columnTypeList <- mapM (convertToHqlType.returnColumnType tabName ) colName 
                                       let compareList = zipWith compare columnTypeList types
                                           typeMismatchList = filter (\ a -> if a == EQ then False else True) compareList
                                       case typeMismatchList of  
                                         []        -> return True
                                         otherwise -> return False
                                       

execHqlInsert :: HqlTable -> [HqlColumn] -> [SqlValue] -> [HqlType] -> IO Integer
execHqlInsert tabName colName values types = do
                                               check <- validateType tabName colName types
                                               let 
                                                   colList = foldl ( \ x y -> x ++ "," ++ y ) ( head colName ) ( tail colName )
                                                   paramList = replicate (length colName) "?"
                                                   valueList  = foldl ( \ x y -> x ++ "," ++ y ) ( head paramList ) ( tail paramList )
                                                   query = "insert into " ++ tabName ++ " (" ++ colList ++ ") values (" ++ valueList  ++ ");"  
                                               case check of
                                                 True  -> execRunQuery database query  values 
                                                 False -> return (-1)
                                               
