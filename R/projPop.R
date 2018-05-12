projPop <- function(x){
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
                        Years = Time.intervals, 
                        Individuals = as.integer(trunc(pr_tracaja)),
                        ss_egghatchling = round(as.numeric(popbio::stable.stage(tracaja)[1]),3),
                        ss_earlyjuven = round(as.numeric(popbio::stable.stage(tracaja)[2]),3),
                        ss_latejuven = round(as.numeric(popbio::stable.stage(tracaja)[3]),3),
                        ss_adultfemale = round(as.numeric(popbio::stable.stage(tracaja)[4]),3),
                        egghatch = eggs,
                        early_juven = eju,
                        late_juven = lju,
                        adult_females = ad.fe
    )
    fem0 <- dfout[(dfout$Years == 0), 'adult_females']
    dft <- data.frame(dfout, fem_t0 = fem0)
    dft$adult_female_diff <- round(((dft$adult_females - dft$fem_t0) / dft$fem_t0), 3)
    dft$change50_flag <- as.integer(ifelse(abs(dft$adult_female_diff) > 0.499, 1, 0))
    dft$double_flag <- as.integer(ifelse(dft$adult_female_diff > 0.999, 1, 0))
    dft
  }
  
  dfin <- x$rdata
  #dfin <- l.gpop$`Podocnemis unifilis.headstart.0`$rdata
  dout <- plyr::ddply(dfin, c("namekey", "accessible",
                              "variable", "propKM", "distKM"), doproj)
  
  dout
  # now seperate tables, split by species, type (5 levels), increase (10 levels),
  # and prop_km (20 levels)
  # write.csv, push to postgre
  # output is dataframe lookup table with 
  # .csv name, postgre table name, values of interest
  
}