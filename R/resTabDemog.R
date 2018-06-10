#' Title
#' @title Create summary for population scenarios in Table 1.

#' @param ldemog List with population projections from "PopScen.R"
#' @param make_html Logical (TRUE/FALSE). Should html tables be written.
#' 
#' @description Used in Table 1, Supplemental Tables.
#' Generates dataframe with summaries of population projections.
#'
#' @return Data.frame with summaries of population projections for three scenarios.
#' @export
#'
#' @examples
#' \dontrun{
#' #1) River length coverage
#' lsf <- prepTabcover(pBasin = B, pBasinSp = Bsp, 
#' pBasinC = BC, riv = rin, rastAc = ras1, make_shape = FALSE)
#' rp <- system.file("shape/shapes_rivers3395", package="cmartr")
#' lt <- resTab(listsf = lsf, input_rp = rp, make_html = FALSE)
#' atest <- lt$rlcb
#' # add key and make sure all levels are represented
#' atest$namekey <- paste(atest$BASIN_N, atest$name, atest$subbasn, sep = "_")
#' riverl <- expand.grid(namekey = unique(atest$namekey), 
#'                      accessible = unique(atest$accessible) )
#' riverl <- merge(riverl, atest, all.x=TRUE)
#' selNA <- which(is.na(riverl$tot_km))
#' riverl[selNA, c("tot_km", "tot_notPA", "tot_PA", "tot_Ind", "tot_SP", "tot_use")] <- 0
#' 
#' #2) Data frame with population parameters created from "PopParam.R" 
#' dfpop <- readRDS("inst/other/dfpop.RDS") 
#' 
#' #3) Create list with population parameters for different scenarios
#' # across river lengths per geographic/political coverage class: 
#' # basin, country, subbasin, accessible, protected area...
#' l.gpop <- plyr::dlply(dfpop, c("akey"), PopPrep, riverl=riverl)
#' 
#' #4) Project scenarios across species range
#' # takes 5 hours and writes 10 GB of results.
#' # Do not run unless you really want to.....
#' dflup <- plyr::ldply(l.gpop, PopProj, write_csv = TRUE, 
#' write_db = FALSE)
#' 
#' #5) Get results from 3 scenarios
#' lscen <- PopScen(dflup)
#' dfsum <- resTabDemog(ldemog = lscen, make_html = FALSE)
#' }
resTabDemog <- function(ldemog = NA, make_html = FALSE){
  
  x <- ldemog
  # Tab 1 summarise totals by country
  dfBAU <-  ddply(x$dft.BAU, .(country), summarize,
                  BAU_km_tot = sum(na.omit(dist_km.x)) + sum(na.omit(dist_km.y)), 
                  BAU_km_accessible = sum(na.omit(dist_km.x)),
                  BAU_km_inaccessible = sum(na.omit(dist_km.y)),
                  BAU_female_current = (sum(na.omit(dist_km.x)) + sum(na.omit(dist_km.y))) * 10,
                  BAU_female = round((sum(na.omit(adult_females.x)) + sum(na.omit(adult_females.y))), 3)
  ) 
  
  dfBAU$BAU_female_diff <- round((dfBAU$BAU_female - dfBAU$BAU_female_current),0)
  dfBAU$BAU_female_change <- (dfBAU$BAU_female_diff / dfBAU$BAU_female_current) * 100
  dfBAU$BAU_iucn_a3 <- cut(round(dfBAU$BAU_female_change, 0), 
                           breaks = c(-Inf, -80, -50, -30, Inf),
                           labels = c("CR", "EN", "VU", "not threatened"), 
                           right = TRUE)
  
  # Strict protection, 
  dfSP <-  ddply(x$dft.SP, .(country), summarize,
                 SP_km_tot = (sum(na.omit(dist_km.x)) + sum(na.omit(dist_km.y)) + 
                                sum(na.omit(dist_km))), 
                 SP_km_accessible = sum(na.omit(dist_km.x)) + sum(na.omit(dist_km.y)),
                 SP_km_inaccessible = sum(na.omit(dist_km)),
                 SP_female_current = (sum(na.omit(dist_km.x)) + sum(na.omit(dist_km.y)) + 
                                        sum(na.omit(dist_km))) * 10,
                 SP_female = round((sum(na.omit(adult_females.x)) + sum(na.omit(adult_females.y)) + 
                                      sum(na.omit(adult_females))), 3)
  ) 
  dfSP$SP_female_diff <- round((dfSP$SP_female - dfSP$SP_female_current),0)
  dfSP$SP_female_change <- (dfSP$SP_female_diff / dfSP$SP_female_current) * 100
  dfSP$SP_iucn_a3 <- cut(round(dfSP$SP_female_change, 0), 
                         breaks = c(-Inf, -80, -50, -30, Inf),
                         labels = c("CR", "EN", "VU", "not threatened"), 
                         right = TRUE)
  
  # Community management
  dfCM <-  ddply(x$dft.CM, .(country), summarize,
                 CM_km_tot = (sum(na.omit(dist_km.x)) + sum(na.omit(dist_km.y)) + 
                                sum(na.omit(dist_km))),
                 CM_km_accessible = sum(na.omit(dist_km.x)) + sum(na.omit(dist_km.y)),
                 CM_km_inaccessible = sum(na.omit(dist_km)),
                 CM_female_current = (sum(na.omit(dist_km.x)) + sum(na.omit(dist_km.y)) + 
                                        sum(na.omit(dist_km))) * 10,
                 CM_female = round((sum(na.omit(adult_females.x)) + sum(na.omit(adult_females.y)) + 
                                      sum(na.omit(adult_females))), 3)
  ) 
  dfCM$CM_female_diff <- round((dfCM$CM_female - dfCM$CM_female_current),0)
  dfCM$CM_female_change <- (dfCM$CM_female_diff / dfCM$CM_female_current) * 100
  dfCM$CM_iucn_a3 <- cut(round(dfCM$CM_female_change, 0), 
                         breaks = c(-Inf, -80, -50, -30, Inf),
                         labels = c("CR", "EN", "VU", "not threatened"), 
                         right = TRUE)
  
  dfsum <- merge(dfBAU, dfSP, by = c("country"))
  dfsum.c <- merge(dfsum, dfCM, by = c("country"))  
  
  dfsum.c$BAU_flag_50 <- ifelse(dfsum.c$BAU_female_change < -49.999, 1,0)
  dfsum.c$BAU_flag_30 <- ifelse(dfsum.c$BAU_female_change < -29.999, 1,0)
  dfsum.c$SP_flag_50 <- ifelse(dfsum.c$SP_female_change < -49.999, 1,0)
  dfsum.c$SP_flag_30 <- ifelse(dfsum.c$SP_female_change < -29.999, 1,0)
  dfsum.c$CM_flag_50 <- ifelse(dfsum.c$CM_female_change < -49.999, 1,0)
  dfsum.c$CM_flag_30 <- ifelse(dfsum.c$CM_female_change < -29.999, 1,0)
  
  if(make_html!=FALSE){
  library(htmlTable)
  outc <- c('country', 'BAU_female', 
            'BAU_female_change', 'SP_female', 'SP_female_change', 
            'CM_female', 'CM_female_change', 'BAU_iucn_a3','SP_iucn_a3','CM_iucn_a3')
  rv <- c(0,1,0,1,0,1)
  t1out <- htmlTable(txtRound(dfsum.c[, outc],  
                     digits = rv, excl.cols = c(1,8,9,10)), 
            rnames=FALSE)
    sink("inst/ms_res/tab1demog.html")
    print(t1out, type="html", useViewer = FALSE)
    sink()
    }
  
  return(dfsum.c)
  
}