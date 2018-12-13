writeconn <- function(x, ptsmall = NA){
  
# function just holds sql code snips with RPostgreSQL
  drv <- RPostgreSQL::dbDriver('PostgreSQL')  
  db <- 'postgres'  
  host_db <- 'localhost'  
  db_port <- '5432'  
  db_user <- 'postgres'  
  db_password <- 'bob1975'
  
  conn <- RPostgreSQL::dbConnect(drv, dbname=db, host=host_db, 
                    port=db_port, user=db_user, 
                    password=db_password)
  RPostgreSQL::dbListTables(conn)
  
  RPostgreSQL::dbWriteTable(conn, c("turtles", "test"), ptsmall)
  sql_command <- "ALTER TABLE turtles.test SET UNLOGGED;"
  RPostgreSQL::dbGetQuery(conn, sql_command)
  RPostgreSQL::dbExistsTable(conn, c("turtles", "test"))
  if(RPostgreSQL::dbExistsTable(conn, c("turtles", "test"))){
    RPostgreSQL::dbWriteTable(conn, c("turtles", "test"), ptsmall, 
                 row.names = FALSE, 
                 append = T)
  }else{
    RPostgreSQL::dbWriteTable(conn, c("turtles", "test"), ptsmall, 
                 row.names = FALSE)
    sql_command <- "ALTER TABLE turtles.test SET UNLOGGED;"
    RPostgreSQL::dbGetQuery(conn, sql_command)
  }
  RPostgreSQL::dbDisconnect(conn) 
  
  # column size, 23 columns 10 GB, 0.435 GB 435 MB per column
  # 100 / 23 , each column should be 4.347 %
  sql_command <- "select
  sum(pg_column_size('hunt')) as total_size,
  avg(pg_column_size('hunt')) as average_size,
  sum(pg_column_size('hunt')) * 100.0 / pg_relation_size('turtles.test') as percentage
  from turtles.test;"
  
  RPostgreSQL::dbGetQuery(conn, sql_command)
  
}