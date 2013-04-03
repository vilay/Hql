import Database.HDBC
import Database.HDBC.Sqlite3
import Hql1
import Data.Typeable

main = do
         x <- validateType "emp" "name" "pankaj"
         print x

--validateType :: String -> String -> a -> Bool
validateType tabName colName value = do
                                        x <- returnColumnType "emp" "name"
                                        return $ checkType x value
                                        
checkType typ val = case typ of 
                        --Resulting Affinity INTEGER 
                        "INT" -> typeOf val == typeOf (1::Int)
                        "INTEGER" -> typeOf val == typeOf (1::Int)
                        "TINYINT" -> typeOf val == typeOf (1::Int)
                        "SMALLINT" -> typeOf val == typeOf (1::Int)
                        "MEDIUMINT" -> typeOf val == typeOf (1::Int)
                        "BIGINT" -> typeOf val == typeOf (1::Int)
                        "UNSIGNED BIG INT" -> typeOf val == typeOf (1::Int)
                        "INT2" -> typeOf val == typeOf (1::Int)
                        "INT8" -> typeOf val == typeOf (1::Int)
                        
                        --Resulting Affinity TEXT
                        "VARCHAR" -> typeOf val == typeOf "a"           
                        
  
