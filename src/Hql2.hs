{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Hql2
(
  HqlTable,   
  HqlColumn,
  HqlType(..),
  HqlCreateTable(..),
  HqlDB,
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
               | HqlLogicExp LogOp HqlExp HqlExp
               | HqlBoolExp RelOp HqlExp HqlExp       
        
        
data LogOp = AND | OR

data RelOp = LessThan | GreaterThan | Equals


   
                                                 
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
                              

 


--select value from databases.
execHqlSelectTable :: HqlTable -> [HqlColumn] -> IO ()

execHqlSelectTable tabName colName  = do  
                                        --convert List of column to String.
                                        let col colName 
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
                                        print stringRows
                                        

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
                                               
