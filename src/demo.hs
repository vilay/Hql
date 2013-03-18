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

main = execHqlSelectTable "emp" ["*"]  
