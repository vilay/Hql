module Main where


import Hql2
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.List

         
{-|
table = HqlCreateTable "emp" ["name","age","salary"] [Varchar,Int,Double]
query = execHqlCreateTable table
main=query
--main = putStrLn $ query
--main = return $ query table
-}


main :: IO ()
main = do
         --c <- execHqlCreateTable (HqlCreateTable "newTable" ["rollNo", "address"] [Int, Varchar])
         --print c
         x <- execHqlInsert (HqlInsertQuery "empp" ["name","age","salary"] ["'prashant'","22", "0.0"]) "insert into emp values('manoj',20,5000.5)";
         --y <- execHqlSelectTable (HqlSelectQuery "emp" ["*"] (HqlLogicExp AND (HqlSubExp Equals ANY (HqlColumnExp "salary") (HqlSelectQuery "emp" ["salary"] HqlEmpty)) (HqlRelExp Equals (HqlColumnExp "name") (HqlConstExp "'Prashant'")) )) ("select * from emp where name = 'prashant';")
         --z <- execHqlUpdate (HqlUpdateQuery "emp" ["salary","age"] ["8000.0","80"] (HqlRelExp Equals (HqlColumnExp "name") (HqlConstExp "'prashant'"))) "update emp set salary=8000 where name = 'prashant';"
         --a <- execHqlDelete (HqlDeleteQuery "emp" (HqlRelExp Equals (HqlColumnExp "name") (HqlConstExp "'manoj'"))) "delete from emp where name = 'manoj'"
         --b <- execHql "drop table emp;"
         --b <- execHql "sqlite3 'test2.db';"
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
