{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}


module Hql
(
  HqlTable,   
  HqlColumn,
  HqlType(..),
  HqlCreateTable(..),
  HqlSelectQuery(..),
  HqlUpdateQuery(..),
  HqlInsertQuery(..),
  HqlDeleteQuery(..),
  HqlDB,
  HqlExp(..),
  HqlLogOp(..),
  HqlRelOp(..),
  HqlSubOp(..),
  execHqlCreateTable,       
  execHqlSelectTable,
  execHqlInsert,
  execHqlUpdate,
  execHqlDelete,
  execHql,
  returnColumnType,
  stringToHqlType 
)
where


--importing libraries 
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Char
import Data.List
import Data.Functor
import Control.Applicative
import Language.Haskell.TH
import Control.Exception


--data types
type HqlTable  = String
type HqlDB     = String
type HqlColumn = String 
type HqlValue = String
 
data HqlType = Int|Varchar|Bool|Double|Invalid deriving (Show,Ord,Eq)

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
               | HqlSubExp HqlRelOp HqlSubOp HqlExp HqlSelectQuery
               | HqlEmpty
               deriving (Show)
        
data HqlSelectQuery = HqlSelectQuery { tabName::HqlTable,
                           columnName::[HqlColumn],
                           expr:: HqlExp
                          } deriving (Show)
                          
                          
data HqlUpdateQuery = HqlUpdateQuery { updtTabName :: HqlTable,
                                       updtColName :: [HqlColumn],
                                       values :: [HqlValue], 
                                       updtExp :: HqlExp
                                     } deriving (Show) 
                                                              
data HqlInsertQuery = HqlInsertQuery { insTabName :: HqlTable,
                                       insColName :: [HqlColumn],
                                       insValues :: [HqlValue]
                                     } deriving (Show)                         
                                 
data HqlDeleteQuery = HqlDeleteQuery { delTabName :: HqlTable,
                                       delExp :: HqlExp
                                     } deriving (Show) 
                                                                      
data HqlSubOp = ANY | ALL | SOME   deriving (Show)     
data HqlLogOp = AND | OR deriving (Show)

data HqlRelOp = LessThan | GreaterThan | Equals | NotEqual | GTEqual | LTEqual deriving (Show)


   
                                                 
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
                              let query="Create Table " ++ tableName table ++ " ( " ++ createColumnList table ++ " ); "
                              execRunQuery database query []
                              

--return type of column 
returnColumnType :: HqlTable -> HqlColumn -> IO String
returnColumnType tabNam colName  = do
                                       let query = "PRAGMA table_info("++tabNam++");"
                                       result <- execQuickQuery database query []
                                       if length result == 0 
                                          --then return "NoTable"
                                          then fail $ "No Such Table " ++ tabNam ++ " present in database !!!"
                                          else Control.Exception.catch (searchColumn result colName) ( \ ex -> fail $ "Column Name " ++ colName ++ " not present in table " ++ show (ex :: SomeException))


returnColumnList :: HqlTable -> IO [HqlColumn]
returnColumnList tabNam = do
                            let query = "PRAGMA table_info("++tabNam++");"
                            result <- execQuickQuery database query []
                            let stringRow = map (\ y -> (!!) y 1 ) result
                                colNames = map (\x -> fromSql x :: String) stringRow                            
                            return colNames


{-                                      
writeExc :: SomeException -> IO String                                          
writeExc ex = do 
                let e = (ex :: SomeException) 
                print "writing exception"
                fail "Incorrect Column name !!"
                                   
-}                                    
                                            
searchColumn :: [[SqlValue]] -> HqlColumn -> IO String
searchColumn result colName = do
                    let stringRow = map ( \ y -> ((!!) y 1,(!!) y 2) ) result
                        columnType = filter ( \ (a,_) -> (a == toSql colName)) stringRow
                    return $ map toUpper (fromSql ( snd $ head columnType )::String)


 
convertToHqlType :: IO String -> IO HqlType
convertToHqlType strType = do
                              strType' <- strType
                              case map toUpper strType' of
                                 "INT"     -> return Int
                                 "VARCHAR" -> return Varchar
                                 "BOOL"    -> return Bool
                                 "DOUBLE"  -> return Double
                                 otherwise -> return Invalid
                              
stringToHqlType :: String -> HqlType
stringToHqlType strType = case map toUpper strType of
                                 "INT"     -> Int
                                 "VARCHAR" -> Varchar
                                 "BOOL"    -> Bool
                                 "DOUBLE"  -> Double
                                 otherwise -> Invalid

ioAnd :: IO Bool -> IO (Bool -> Bool)
ioAnd ioBool = do
              bool <- ioBool
              return (bool &&) 
              
ioEq :: IO String -> IO (String -> Bool)
ioEq ioStr = do
              str <- ioStr
              return (str ==)                         
                              
                              
                              
validateExp :: HqlSelectQuery -> IO Bool                         
validateExp hqlSelectQuery = case (expr hqlSelectQuery) of                     
                  HqlLogicExp logOp hqlExp1 hqlExp2 -> (<*>) (ioAnd (validateExp (HqlSelectQuery (tabName hqlSelectQuery) (columnName hqlSelectQuery) hqlExp1))) 
                                                                        (validateExp (HqlSelectQuery (tabName hqlSelectQuery) (columnName hqlSelectQuery) hqlExp2))                                                   
                  HqlRelExp relOp hqlExp1 hqlExp2   -> (<*>) (ioEq (getType (tabName hqlSelectQuery) hqlExp1)) (getType (tabName hqlSelectQuery) hqlExp2)
                  HqlSubExp hqlRelOp hqlSubOp hqlExp hqlSelectQuery -> (<*>) (ioEq (getType (tabName hqlSelectQuery) hqlExp)) (getSubQueryType hqlSelectQuery)
                  HqlEmpty -> return True
                  otherwise -> fail "Invalid Query"
                                                                                   
getSubQueryType :: HqlSelectQuery -> IO String
getSubQueryType hqlSelectQuery = do check <- validateExp hqlSelectQuery
                                    if check then getType (tabName hqlSelectQuery) (HqlColumnExp (head (columnName hqlSelectQuery)))
                                             --else return "Invalid Subquery"
                                             else fail "Invalid Subquery !!"                 
         
                       
getType :: HqlTable -> HqlExp -> IO String
getType hqlTable hqlExp = case hqlExp of 
                  HqlColumnExp hqlColumn -> do ans <- returnColumnType hqlTable hqlColumn
                                               print ans
                                               return ans
                  HqlConstExp hqlValue   -> do let ans = getTypeFromValue hqlValue
                                               print ans
                                               return  ans
                  otherwise              -> fail "Validation Failed. Incorrect parameter to getType"

getTypeFromValue :: HqlValue -> String 
getTypeFromValue hqlValue = if (isValidString hqlValue)
                              then "VARCHAR"
                              else if hqlValue `elem` ["True","False"]
                                     then "BOOL"
                                     else if isDouble hqlValue
                                          then "DOUBLE"
                                          else if all isDigit hqlValue
                                               then "INT"
                                               --else hqlValue ++ "Invalid"
                                               else fail "Datatype for value : " ++ hqlValue ++ " is not supported !!"


isDouble :: String -> Bool
isDouble str = let check1 = and $ (map isDigit (filter (/= '.') str))
                   indices = elemIndices '.' str
                   check2 = length indices == 1
               in check1 && check2

        
isValidString :: String -> Bool
isValidString str = let indices = elemIndices '\'' str
                        check1 = if (length indices) == 2
                                 then ((head indices) == 0) && ((last indices) == ((length str) - 1))
                                 else False 
                        indice = elemIndices '"' str
                        check2 = if ((length indice) == 2)
                                 then ((head indice) == 0) && ((last indice) == ((length str) - 1))
                                 else False 
                    in check1 || check2
                        
                        

--select value from databases.
execHqlSelectTable :: HqlSelectQuery -> String -> IO Integer
execHqlSelectTable hqlSelectQuery query = do  
                                        check <- validateExp hqlSelectQuery
                                        print check
                                        --r <- Control.Exception.catch (execQuickQuery database query []) dispException1
                                        r <- execQuickQuery database query []
                                        print r
                                        return $ toInteger $ length r
                                        
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
                                       
--insert into table
execHqlInsert :: HqlInsertQuery -> String -> IO Integer
execHqlInsert hqlInsertQuery query = do
                                       let typeList = map (stringToHqlType.getTypeFromValue) (insValues hqlInsertQuery)                                       
                                       cols <- if (insColName hqlInsertQuery == [])
                                                     then returnColumnList (insTabName hqlInsertQuery)
                                                     else return $ insColName hqlInsertQuery  
                                       print cols
                                       print hqlInsertQuery
                                       check <- validateType (insTabName hqlInsertQuery) cols typeList              
                                       --check <- validateType (insTabName hqlInsertQuery) (insColName hqlInsertQuery) typeList
                                       {--let colList = foldl ( \ x y -> x ++ "," ++ y ) ( head colName ) ( tail colName )
                                                   paramList = replicate (length colName) "?"
                                                   valueList  = foldl ( \ x y -> x ++ "," ++ y ) ( head paramList ) ( tail paramList )
                                                   query = "insert into " ++ tabName ++ " (" ++ colList ++ ") values (" ++ valueList  ++ ");" --}  
                                       case check of
                                            True  -> execRunQuery database query []
                                            False -> fail "insert query validation failed"                                           
                               
                                               
                                        


                     
dispException1 :: SomeException -> IO [[SqlValue]]
dispException1 ex = do
                     putStrLn $ "Caught exception: " ++ show ex
                     return [[]]
                     
                                               
--update the table
execHqlUpdate :: HqlUpdateQuery -> String -> IO Integer
execHqlUpdate hqlUpdateQuery query = do
                                       let typeList = map (stringToHqlType.getTypeFromValue) (values hqlUpdateQuery)
                                       check1 <- validateType (updtTabName hqlUpdateQuery) (updtColName hqlUpdateQuery) typeList
                                       check2 <- validateExp (HqlSelectQuery (updtTabName hqlUpdateQuery) (updtColName hqlUpdateQuery) (updtExp hqlUpdateQuery))
                                       case (check1 && check2) of
                                         True  -> execRunQuery database query []
                                         False -> fail "update query validation failed"
                                         
--delete the rows from table
execHqlDelete :: HqlDeleteQuery -> String -> IO Integer
execHqlDelete hqlDeleteQuery query = do
                                       check <- validateExp (HqlSelectQuery (delTabName hqlDeleteQuery) [] (delExp hqlDeleteQuery))
                                       case check of
                                         True  -> execRunQuery database query [] 
                                         False -> fail "delete query validation failed"
                                         
execHql :: String -> IO ()
execHql query = do
                  execRunQuery database query []
                  return ()
                  
