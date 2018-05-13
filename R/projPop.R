projPop <- function(x, write_csv = FALSE, write_db = FALSE){
library(popdemo)
  library(popbio)
  tracaja <- x$tracajam
  
  doproj <- function(x) {
    tracaja_n <-  x$adultF.n * c(11.1, 4, 2, 1) 
    
    # project PPM 
    pr_tracaja <- popdemo::project(tracaja, vector=tracaja_n, time=50)
    
    # data for plotting
    len <- length(pr_tracaja)
    Time.intervals <- 0:(len - 1)
    eggs <- as.integer(trunc(pr_tracaja * (popbio::stable.stage(tracaja)[1])))
    eju <- as.integer(trunc(pr_tracaja * (popbio::stable.stage(tracaja)[2])))
    lju <- as.integer(trunc(pr_tracaja * (popbio::stable.stage(tracaja)[3])))
    ad.fe <- as.integer(trunc(pr_tracaja * (popbio::stable.stage(tracaja)[4])))
    plambda = popbio::lambda(tracaja)
    
    # make dataframe 
    dfout <- data.frame(lambda = plambda,
                        ayear = Time.intervals, 
                        individuals = as.integer(trunc(pr_tracaja)),
                        ss_egghatchling = round(as.numeric(popbio::stable.stage(tracaja)[1]),3),
                        ss_earlyjuven = round(as.numeric(popbio::stable.stage(tracaja)[2]),3),
                        ss_latejuven = round(as.numeric(popbio::stable.stage(tracaja)[3]),3),
                        ss_adultfemale = round(as.numeric(popbio::stable.stage(tracaja)[4]),3),
                        egghatch = eggs,
                        early_juven = eju,
                        late_juven = lju,
                        adult_females = ad.fe
    )
    fem0 <- dfout[(dfout$ayear == 0), 'adult_females']
    dft <- data.frame(dfout, fem_t0 = fem0)
    dft$adult_female_diff <- round(((dft$adult_females - dft$fem_t0) / dft$fem_t0), 3)
    dft$change50_flag <- as.integer(ifelse(abs(dft$adult_female_diff) > 0.499, 1, 0))
    dft$double_flag <- as.integer(ifelse(dft$adult_female_diff > 0.999, 1, 0))
    dft
  }
  
  dfin <- x$rdata
  #dfin <- l.gpop$`Podocnemis unifilis.headstart.0`$rdata
  dout <- plyr::ddply(dfin, c("species", "atype", "increase","namekey", "accessible",
                              "variable", "prop_km", "dist_km"), doproj)
  
  fn <- paste(dfin[1,'species'], dfin[1,'atype'], dfin[1,'increase'], sep="_")
  fname <- paste(fn,"csv", sep = ".")
  csvout <- paste("inst/other/dataproj/",fname, sep = "")
  
  if(write_csv != FALSE){
    write.csv(dout, csvout, row.names = FALSE)
  }
  
  if(write_db != FALSE){
    library(RPostgreSQL)
    drv <- dbDriver('PostgreSQL')  
    db <- 'postgres'  
    host_db <- 'localhost'  
    db_port <- '5432'  
    db_user <- 'postgres'  
    db_password <- 'bob1975'
    
    conn <- dbConnect(drv, dbname=db, host=host_db, 
                      port=db_port, user=db_user, 
                      password=db_password)

    if(dbExistsTable(conn, c("turtles", "test"))){
      dbWriteTable(conn, c("turtles", "test"), dout, 
                   row.names = FALSE, 
                   append = T)
    }else{
      dbWriteTable(conn, c("turtles", "test"), dout, 
                   row.names = FALSE)
      sql_command <- "ALTER TABLE turtles.test SET UNLOGGED;"
      dbGetQuery(conn, sql_command)
    }
    dbDisconnect(conn) 
  }
  rm("dout")
  dflup <- data.frame(species = dfin[1,'species'], atype = dfin[1,'atype'], dfin[1,'increase'],
             fileout = csvout, csvname = fname)
  dflup
}