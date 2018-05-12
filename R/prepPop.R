# function to prepare data for projection
prepPop <- function(x){
  vpop <- unlist(x[ ,4:19])
  tracaja <- matrix(vpop, byrow = TRUE, ncol=4)
  dimnames(tracaja) <- list(c("a", "b", "c", "d"),
                            c(1,2,3,4))
  
  #numeric vector of individuals at different age stage
  dft <- reshape2::melt(riverl, id.vars = c('namekey', 'accessible'), 
                        measure.vars = c('tot_km', 'tot_notPA', 'tot_PA',
                                         'tot_Ind', 'tot_SP', 'tot_use'), 
                        value.name = c('distKMa'))
  
  myq <- function(x){
    myquant <- seq(0.05,1,by=0.05)
    dfout <- data.frame(prop_km = myquant, dist_km = x$distKMa * myquant)
    dfout
  }
  
  dft2 <- plyr::ddply(dft, c("namekey", "accessible", "variable"), myq)
  adultF.d <- 10 # adult female density per river km
  #distkm <- 100 # length of river
  dft2$adultF.n <- trunc(adultF.d * dft2$dist_km)
  
  l1 <- list(rdata = dft2, tracajam = tracaja)
  
}