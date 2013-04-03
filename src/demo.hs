import Hql1
import Database.HDBC
import Database.HDBC.Sqlite3
         
{-|
table = HqlCreateTable "emp" ["name","age","salary"] [Varchar,Int,Double]
query = execHqlCreateTable table
main=query
--main = putStrLn $ query
--main = return $ query table
-}

main = do
         x <- returnColumnType "emp" "age"
         print x
         

--extracting type name of column
{--
main = do  
			let query = "PRAGMA table_info(emp)"
			conn <- connectSqlite3 "test1.db";
			r <- quickQuery' conn query [];
			let   
			      stringRow = map ( \ y -> ((!!) y 1,(!!) y 2) ) r
			      columnType = filter ( \ (a,b) -> if a == toSql "age" then True else False) stringRow
			
			return (fromSql ( snd $ head columnType )::String)
			disconnect conn;
--}           
--main = execHqlSelectTable "emp" ["*"]  
