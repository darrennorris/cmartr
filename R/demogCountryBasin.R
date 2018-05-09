#' Title
#' @title Project population scenarios across countries and basins.
#' 
#' @description Generates dataframe with population projections.
#' 
#' @param popc File (RDS) with population parameters for differnt scenarios.
#' @param rlcb River lengths for each scenario, from "resTab.R".
#'
#' @return Creates dataframe with 50 year population projections.
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' lt <- resTab(listsf = lsf, input_rp = rp, make_html = FALSE)
#' popc <- "C:\\Users\\Darren\\Documents\\2018 Unifilis demography\\analysis\\dfpop.RDS"
#' demogC <- demogCountryBasin(popc = popc, rlcb = lbt$rlcb)
#' 
#' }
demogCountryBasin <- function(popc = NA, rlcb = NA) {
  #load data
  dfpop <- readRDS(popc)
  rlcb <- rlcb
  
  # add key and make sure all levels
  rlcb$namekey <- paste(rlcb$BASIN_N, rlcb$name, rlcb$subbasn, sep = "_")
  riverl <- expand.grid(namekey = unique(rlcb$namekey), 
                        accessible = unique(rlcb$accessible) )
  riverl <- merge(riverl, rlcb, all.x=TRUE)
  selNA <- which(is.na(riverl$tot_km))
  riverl[selNA, c("tot_km", "tot_notPA", "tot_PA", "tot_Ind", "tot_SP", "tot_use")] <- 0
  
  # function to prepare data for projection
  prepop <- function(x){
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
      dfout <- data.frame(propKM = myquant, distKM = x$distKMa * myquant)
      dfout
    }
    
    dft2 <- plyr::ddply(dft, c("namekey", "accessible", "variable"), myq)
    adultF.d <- 10 # adult female density per river km
    #distkm <- 100 # length of river
    dft2$adultF.n <- trunc(adultF.d * dft2$distKM)
    
    l1 <- list(rdata = dft2, tracajam = tracaja)
    
  }
  
  l.gpop <- plyr::dlply(dfpop, c("species", "type", "increase"), prepop)

  # function to project populations
  proj.rivl <- function(x){
    #tracaja <- l.gpop$`Podocnemis unifilis.headstart.0`$tracajam
    #x <- l.gpop$`Podocnemis unifilis.headstart.0`$rdata
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
                          Years = Time.intervals, Individuals = pr_tracaja,
                          ss_egghatchling = popbio::stable.stage(tracaja)[1],
                          ss_earlyjuven = popbio::stable.stage(tracaja)[2],
                          ss_latejuven = popbio::stable.stage(tracaja)[3],
                          ss_adultfemale = popbio::stable.stage(tracaja)[4],
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
  }

  #18:26 - 19:00
  dfres <- plyr::ldply(l.gpop, proj.rivl)
  return(dfres)
  
}
