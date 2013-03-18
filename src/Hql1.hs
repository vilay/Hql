module Hql1
(
  HqlTable,   
  HqlColumn,
  HqlType(..),
  HqlCreateTable(..),
  HqlDB,
  execHqlCreateTable,       
  execHqlSelectTable 
)
where


--importing libraries 
import Data.List
import Database.HDBC
import Database.HDBC.SqlValue
import Data.Convertible.Base
import Database.HDBC.Sqlite3


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
											
			                
