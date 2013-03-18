import Database.HDBC
import Database.HDBC.Sqlite3
import Database.HDBC.SqlValue
import Data.Convertible.Base
import Data.List
{--
class ConvValue a where
   convertValue::a -> b  

instance ConvValue ( Either a b) where
   convertValue (Right b) = b
   convertValue (Left a ) = [a]
--}

{--
main = do  
			let col colName 
						| colName == ["*"] = " * " 
						| otherwise = foldl ( \ x y -> x ++ "," ++ y ) ( head colName ) ( tail colName )
			--let query = "select " ++ col ["*"] ++ " from " ++ "emp" ++ ";"
			let query = "select " ++ " name , age " ++ " from " ++ " emp " ++ " where 1" ++  ";"
			conn <- connectSqlite3 "test1.db";
			r <- quickQuery' conn query [];
			let   convRow :: [SqlValue] -> Either String [String]
			      x = (head r)
			      len1 = length x -1
			      convRow x = Right $ map (\ t -> fromSql $ ((!!) x t)::String ) [0..len1]
			      convRow y = Left $ fail $ "Unexpected result: " ++ show y
			      stringRows = map convRow r
			print stringRows
			disconnect conn;
--}			
			                
main = do  
			let col colName 
						| colName == ["*"] = " * " 
						| otherwise = foldl ( \ x y -> x ++ "," ++ y ) ( head colName ) ( tail colName )
			--let query = "select " ++ col ["*"] ++ " from " ++ "emp" ++ ";"
			let query = "select " ++ " name , age,salary " ++ " from " ++ " emp " ++ " where 1" ++  ";"
			conn <- connectSqlite3 "test1.db";
			r <- quickQuery' conn query [];
			let   --convRow :: [SqlValue] -> [ConvertResult String]
			      x = (head r)
			      len1 = length x -1
			      convRow x = map (\ t -> fromSql $ ((!!) x t)::(String) ) [0..len1]
			      --convRow y = ["Unexpected result: " ++ show y]
			      stringRows = map convRow r
			print stringRows
			disconnect conn;

