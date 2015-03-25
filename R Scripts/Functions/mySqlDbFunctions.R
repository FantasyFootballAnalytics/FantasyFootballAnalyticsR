require(RMySQL)
connectDb <- function(db){
  mySqlCred <- read.table(file = paste(getwd(), "/Config/mysqlaccess.txt", sep = ""), 
                          col.names = c("db_name", "host_name", "user_name", "pwd"), sep =",", 
                          stringsAsFactors = FALSE)
  mySqlCred <- mySqlCred[mySqlCred$db_name == db, ]
  conn <- dbConnect(RMySQL::MySQL(), dbname = db, host = mySqlCred$host_name, 
                    username = mySqlCred$user_name, password = mySqlCred$pwd)
  rm(mySqlCred)
  return(conn)
}


