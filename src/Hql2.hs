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

 
data HqlType = Int|Varchar|Bool|Double deriving (Show,Ord,Eq)

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
returnColumnType :: HqlTable -> HqlColumn -> IO(String)
returnColumnType tabName colName  = do
                                       let query = "PRAGMA table_info("++tabName++");"
                                       conn <- connectSqlite3 "test1.db";
                                       r <- quickQuery' conn query [];
                                       let
                                          stringRow = map ( \ y -> ((!!) y 1,(!!) y 2) ) r
                                          columnType = filter ( \ (a,b) -> if a == toSql colName then True else False) stringRow
                                       disconnect conn;
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

validateType :: HqlTable -> [HqlColumn] -> [HqlType] -> IO Bool
validateType tabName colName types = do 
                                       columnTypeList <- mapM (convertToHqlType.returnColumnType tabName ) colName 
                                       let compareList = zipWith compare columnTypeList types
                                           typeMismatchList = filter (\ a -> if a == EQ then False else True) compareList
                                       case typeMismatchList of  
                                         []        -> return True
                                         otherwise -> return False
                                       
                                             
                                            
                                          
                                        


{--


checkType typ val = case typ of 
                        "INT"     -> typeOf val == typeOf (1::Int)        
                        "VARCHAR" -> typeOf val == typeOf "a"           
                        "BOOL"    -> typeOf val == typeOf True
                        "DOUBLE"  -> typeOf val == typeOf 1.0
                        
  --}

execHqlInsert :: HqlTable -> [HqlColumn] -> [SqlValue] -> [HqlType] -> IO Bool
execHqlInsert tabName colName values types = do
                                               validateType tabName colName types
