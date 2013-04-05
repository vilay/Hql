module Hql1
(
  HqlTable,   
  HqlColumn,
  HqlType(..),
  HqlCreateTable(..),
  HqlDB,
  execHqlCreateTable,       
  execHqlSelectTable,
  returnColumnType 
)
where


--importing libraries 
import Data.List
import Database.HDBC
import Database.HDBC.SqlValue
import Data.Convertible.Base
import Database.HDBC.Sqlite3
import Data.Char

--data types
type HqlTable  = String
type HqlDB     = String
type HqlColumn = String 
type HqlValues = String
 
data HqlType = Int|Varchar|Bool|Double deriving Show

data HqlCreateTable = HqlCreateTable { tableName  :: HqlTable,
                                       columnList :: [HqlColumn],
                                       typeList   :: [HqlType]
                                     } 
                               deriving Show
                                                                                     
            
--creating table
createColumnList :: HqlCreateTable -> String
createColumnList table = let colList = columnList table
                             typList = typeList table
                             colPairList = zipWith (\ x y -> x ++ " " ++ show y ) colList typList 
                          in
                             foldl ( \ x y -> x ++ "," ++ y ) ( head colPairList ) (tail colPairList)

execHqlCreateTable :: HqlCreateTable -> IO ()
execHqlCreateTable table = 
                            do  
                              let query="Create Table " ++ (tableName table) ++ " ( " ++ createColumnList table ++ " ); "
                              conn <- connectSqlite3  "test1.db";
                              --run conn "create table test (name Varchar(20),age Int)" [];
                              --quickQuery' conn query [];
                              run conn query [];
                              quickQuery' conn  ("select * from "++(tableName table)) [];
                              commit conn;
                              disconnect conn;
                              return () 

--return type of column 
returnColumnType tabName colName  = do
                                       let query = "PRAGMA table_info("++tabName++");"
                                       conn <- connectSqlite3 "test1.db";
                                       r <- quickQuery' conn query [];
                                       let
                                          stringRow = map ( \ y -> ((!!) y 1,(!!) y 2) ) r
                                          columnType = filter ( \ (a,b) -> if a == toSql colName then True else False) stringRow
                                       disconnect conn;
                                       return $ map toUpper (fromSql ( snd $ head columnType )::String)
 


--select value from databases.
execHqlSelectTable :: HqlTable -> [HqlColumn] -> IO ()

execHqlSelectTable tabName colName  = do  
                                                                                        --convert List of column to String.
                                                                                        let col colName 
                                                                                                                | colName == ["*"] = " * " 
                                                                                                                | otherwise = foldl ( \ x y -> x ++ "," ++ y ) ( head colName ) ( tail colName )
                                                                                        --create a query
                                                                                        let query = "select " ++ col colName ++ " from " ++ tabName ++ ";"
                                                                                        --database connection
                                                                                        conn <- connectSqlite3 "test1.db";
                                                                                        r <- quickQuery' conn query [];
                                                                                        
                                                                                        let   
                                                                                              convRow :: [SqlValue] -> [String]
                                                                                              x = (head r)
                                                                                              len1 = length x -1
                                                                                              convRow x = map (\ t -> fromSql $ ((!!) x t)::String ) [0..len1]
                                                                                              stringRows = map convRow r                                                                                              
                                                                                        print stringRows
                                                                                        
                                                                                        disconnect conn;
                                                                                        
                                        
