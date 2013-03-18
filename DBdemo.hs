import Database.HDBC
import Database.HDBC.Sqlite3

main = do  
           let query="select * from test"
           conn <- connectSqlite3  "test1.db"
           run conn "create table test (name Varchar(20),age Int)" [];
           quickQuery' conn query [];
           commit conn;
           disconnect conn;
	       return () 
