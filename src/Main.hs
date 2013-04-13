module Main where



import Hql2
import Database.HDBC
import Database.HDBC.Sqlite3
         
{-|
table = HqlCreateTable "emp" ["name","age","salary"] [Varchar,Int,Double]
query = execHqlCreateTable table
main=query
--main = putStrLn $ query
--main = return $ query table
-}


main :: IO ()
main = do
         x <- execHqlInsert "emp" ["name","age","salary"] [toSql "prashant",toSql "22", toSql "0"] [Varchar,Int,Double]
         y <- execHqlSelectTable "emp" ["*"]
         print y
         
         
         

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
