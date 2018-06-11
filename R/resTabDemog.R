#' Title
#' @title Create summary for population scenarios in Table 1.

#' @param ldemog List with population projections from "PopScen.R"
#' @param make_html Logical (TRUE/FALSE). Should html tables be written.
#' 
#' @description Used in Table 1, Supplemental Tables.
#' Generates dataframe with summaries of population projections.
#'
#' @return Data.frame with summaries of population projections for three scenarios.
#' @importFrom utils write.csv
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
#' 
#' #6) Produce summary tables.
#' dfsum <- resTabDemog(ldemog = lscen, make_html = FALSE)
#' }
resTabDemog <- function(ldemog = NA, make_html = FALSE){
  
  x <- ldemog
  
  # Tab 1 summarise totals by basin
  dfBAU.b <-  plyr::ddply(x$dft.BAU, c("basin"), summarize,
                    BAU_km_tot = sum(na.omit(dist_km.x)) + sum(na.omit(dist_km.y)), 
                    BAU_km_accessible = sum(na.omit(dist_km.x)),
                    BAU_km_inaccessible = sum(na.omit(dist_km.y)),
                    BAU_female_current = (sum(na.omit(dist_km.x)) + sum(na.omit(dist_km.y))) * 10,
                    BAU_female = round((sum(na.omit(adult_females.x)) + sum(na.omit(adult_females.y))), 3)
  ) 
  
  dfBAU.b$BAU_female_diff <- round((dfBAU.b$BAU_female - dfBAU.b$BAU_female_current),0)
  dfBAU.b$BAU_female_change <- (dfBAU.b$BAU_female_diff / dfBAU.b$BAU_female_current) * 100
  dfBAU.b$BAU_iucn_a3 <- cut(round(dfBAU.b$BAU_female_change, 0), 
                             breaks = c(-Inf, -80, -50, -30, Inf),
                             labels = c("CR", "EN", "VU", "not threatened"), 
                             right = TRUE)
  
  # Strict protection, 
  dfSP.b <-  plyr::ddply(x$dft.SP, c("basin"), summarize,
                   SP_km_tot = (sum(na.omit(dist_km.x)) + sum(na.omit(dist_km.y)) + 
                                  sum(na.omit(dist_km))), 
                   SP_km_accessible = sum(na.omit(dist_km.x)) + sum(na.omit(dist_km.y)),
                   SP_km_inaccessible = sum(na.omit(dist_km)),
                   SP_female_current = (sum(na.omit(dist_km.x)) + sum(na.omit(dist_km.y)) + 
                                          sum(na.omit(dist_km))) * 10,
                   SP_female = round((sum(na.omit(adult_females.x)) + sum(na.omit(adult_females.y)) + 
                                        sum(na.omit(adult_females))), 3)
  ) 
  dfSP.b$SP_female_diff <- round((dfSP.b$SP_female - dfSP.b$SP_female_current),0)
  dfSP.b$SP_female_change <- (dfSP.b$SP_female_diff / dfSP.b$SP_female_current) * 100
  dfSP.b$SP_iucn_a3 <- cut(round(dfSP.b$SP_female_change, 0), 
                           breaks = c(-Inf, -80, -50, -30, Inf),
                           labels = c("CR", "EN", "VU", "not threatened"), 
                           right = TRUE)
  
  # Community management
  dfCM.b <-  plyr::ddply(x$dft.CM, c("basin"), summarize,
                   CM_km_tot = (sum(na.omit(dist_km.x)) + sum(na.omit(dist_km.y)) + 
                                  sum(na.omit(dist_km))),
                   CM_km_accessible = sum(na.omit(dist_km.x)) + sum(na.omit(dist_km.y)),
                   CM_km_inaccessible = sum(na.omit(dist_km)),
                   CM_female_current = (sum(na.omit(dist_km.x)) + sum(na.omit(dist_km.y)) + 
                                          sum(na.omit(dist_km))) * 10,
                   CM_female = round((sum(na.omit(adult_females.x)) + sum(na.omit(adult_females.y)) + 
                                        sum(na.omit(adult_females))), 3)
  ) 
  dfCM.b$CM_female_diff <- round((dfCM.b$CM_female - dfCM.b$CM_female_current),0)
  dfCM.b$CM_female_change <- (dfCM.b$CM_female_diff / dfCM.b$CM_female_current) * 100
  dfCM.b$CM_iucn_a3 <- cut(round(dfCM.b$CM_female_change, 0), 
                           breaks = c(-Inf, -80, -50, -30, Inf),
                           labels = c("CR", "EN", "VU", "not threatened"), 
                           right = TRUE)
  
  dfsum.b <- merge(dfBAU.b, dfSP.b, by = c("basin"))
  dfsum.b <- merge(dfsum.b, dfCM.b, by = c("basin"))  
  
  dfsum.b$BAU_flag_50 <- ifelse(dfsum.b$BAU_female_change < -49.999, 1,0)
  dfsum.b$BAU_flag_30 <- ifelse(dfsum.b$BAU_female_change < -29.999, 1,0)
  dfsum.b$SP_flag_50 <- ifelse(dfsum.b$SP_female_change < -49.999, 1,0)
  dfsum.b$SP_flag_30 <- ifelse(dfsum.b$SP_female_change < -29.999, 1,0)
  dfsum.b$CM_flag_50 <- ifelse(dfsum.b$CM_female_change < -49.999, 1,0)
  dfsum.b$CM_flag_30 <- ifelse(dfsum.b$CM_female_change < -29.999, 1,0)
  
  # Tab 1 summarise totals by country
  dfBAU <-  plyr::ddply(x$dft.BAU, c("country"), summarize,
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
  dfSP <-  plyr::ddply(x$dft.SP, c("country"), summarize,
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
  dfCM <-  plyr::ddply(x$dft.CM, c("country"), summarize,
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
  
  # Tab S2 summarise totals by basin and subbasin
  dfBAU.bs <-  plyr::ddply(x$dft.BAU, c("basin", "subbasin"), summarize,
                     BAU_km_tot = sum(na.omit(dist_km.x)) + sum(na.omit(dist_km.y)), 
                     BAU_km_accessible = sum(na.omit(dist_km.x)),
                     BAU_km_inaccessible = sum(na.omit(dist_km.y)),
                     BAU_female_current = (sum(na.omit(dist_km.x)) + sum(na.omit(dist_km.y))) * 10,
                     BAU_female = round((sum(na.omit(adult_females.x)) + sum(na.omit(adult_females.y))), 3)
  ) 
  
  dfBAU.bs$BAU_female_diff <- round((dfBAU.bs$BAU_female - dfBAU.bs$BAU_female_current),0)
  dfBAU.bs$BAU_female_change <- (dfBAU.bs$BAU_female_diff / dfBAU.bs$BAU_female_current) * 100
  dfBAU.bs$BAU_iucn_a3 <- cut(round(dfBAU.bs$BAU_female_change, 0), 
                              breaks = c(-Inf, -80, -50, -30, Inf),
                              labels = c("CR", "EN", "VU", "not threatened"), 
                              right = TRUE)
  
  # Strict protection, 
  dfSP.bs <-  plyr::ddply(x$dft.SP, c("basin", "subbasin"), summarize,
                    SP_km_tot = (sum(na.omit(dist_km.x)) + sum(na.omit(dist_km.y)) + 
                                   sum(na.omit(dist_km))), 
                    SP_km_accessible = sum(na.omit(dist_km.x)) + sum(na.omit(dist_km.y)),
                    SP_km_inaccessible = sum(na.omit(dist_km)),
                    SP_female_current = (sum(na.omit(dist_km.x)) + sum(na.omit(dist_km.y)) + 
                                           sum(na.omit(dist_km))) * 10,
                    SP_female = round((sum(na.omit(adult_females.x)) + sum(na.omit(adult_females.y)) + 
                                         sum(na.omit(adult_females))), 3)
  ) 
  dfSP.bs$SP_female_diff <- round((dfSP.bs$SP_female - dfSP.bs$SP_female_current),0)
  dfSP.bs$SP_female_change <- (dfSP.bs$SP_female_diff / dfSP.bs$SP_female_current) * 100
  dfSP.bs$SP_iucn_a3 <- cut(round(dfSP.bs$SP_female_change, 0), 
                            breaks = c(-Inf, -80, -50, -30, Inf),
                            labels = c("CR", "EN", "VU", "not threatened"), 
                            right = TRUE)
  
  # Community management
  dfCM.bc <-  plyr::ddply(x$dft.CM, c("basin", "subbasin"), summarize,
                    CM_km_tot = (sum(na.omit(dist_km.x)) + sum(na.omit(dist_km.y)) + 
                                   sum(na.omit(dist_km))),
                    CM_km_accessible = sum(na.omit(dist_km.x)) + sum(na.omit(dist_km.y)),
                    CM_km_inaccessible = sum(na.omit(dist_km)),
                    CM_female_current = (sum(na.omit(dist_km.x)) + sum(na.omit(dist_km.y)) + 
                                           sum(na.omit(dist_km))) * 10,
                    CM_female = round((sum(na.omit(adult_females.x)) + sum(na.omit(adult_females.y)) + 
                                         sum(na.omit(adult_females))), 3)
  ) 
  dfCM.bc$CM_female_diff <- round((dfCM.bc$CM_female - dfCM.bc$CM_female_current),0)
  dfCM.bc$CM_female_change <- (dfCM.bc$CM_female_diff / dfCM.bc$CM_female_current) * 100
  dfCM.bc$CM_iucn_a3 <- cut(round(dfCM.bc$CM_female_change, 0), 
                            breaks = c(-Inf, -80, -50, -30, Inf),
                            labels = c("CR", "EN", "VU", "not threatened"), 
                            right = TRUE)
  
  dfsum.bc <- merge(dfBAU.bs, dfSP.bs, by = c("basin", "subbasin"))
  dfsum.bc <- merge(dfsum.bc, dfCM.bc, by = c("basin", "subbasin"))  
  
  dfsum.bc$BAU_flag_50 <- ifelse(dfsum.bc$BAU_female_change < -49.999, 1,0)
  dfsum.bc$BAU_flag_30 <- ifelse(dfsum.bc$BAU_female_change < -29.999, 1,0)
  dfsum.bc$SP_flag_50 <- ifelse(dfsum.bc$SP_female_change < -49.999, 1,0)
  dfsum.bc$SP_flag_30 <- ifelse(dfsum.bc$SP_female_change < -29.999, 1,0)
  dfsum.bc$CM_flag_50 <- ifelse(dfsum.bc$CM_female_change < -49.999, 1,0)
  dfsum.bc$CM_flag_30 <- ifelse(dfsum.bc$CM_female_change < -29.999, 1,0)
  # add subbasin as per article table 
  amsub <- c("Abacaxis", "Amazon floodplain", "Putumayo", "Japurá - Caquetá", "Javari",
             "Juruá", "Madeira", "Marañón", "Curuá-una", "Guama", "Jari", "Jutai",
             "Madeirinha", "Manacapuru", "Nanay", "Pacajá", "Piorini", "Tefe", 
             "Uatumá", "Napo", "Negro", "Purus", "Tapajós", "Tocantins",
             "Trombetas", "Ucayali", "Xingu")
  dfsum.bc$subbasinT <- c(amsub, dfsum.bc$subbasin[28:52])
  
  if(make_html!=FALSE){
    outb <- c('basin', 'BAU_female_current', 'BAU_female',
              'BAU_female_change', 'SP_female', 'SP_female_change', 
              'CM_female', 'CM_female_change', 'BAU_iucn_a3','SP_iucn_a3','CM_iucn_a3')
    rv <- c(0,0,1,0,1,0,1)
    tbout <- htmlTable::htmlTable(htmlTable::txtRound(dfsum.b[, outb],  
                       digits = rv, excl.cols = c(1,9,10,11)), rnames=FALSE)
    sink("inst/ms_res/tabbasindemog.html")
    print(tbout, type="html", useViewer = FALSE)
    sink()
    
  outc <- c('country', 'BAU_female_current', 'BAU_female',
            'BAU_female_change', 'SP_female', 'SP_female_change', 
            'CM_female', 'CM_female_change', 'BAU_iucn_a3','SP_iucn_a3','CM_iucn_a3')
  rv <- c(0,0,1,0,1,0,1)
  t1out <- htmlTable::htmlTable(htmlTable::txtRound(dfsum.c[, outc],  
                     digits = rv, excl.cols = c(1,9,10,11)), rnames=FALSE)
    sink("inst/ms_res/tab1demog.html")
    print(t1out, type="html", useViewer = FALSE)
    sink() 
    
    outbs <- c('basin','subbasinT', 'subbasin','BAU_female_current', 'BAU_female',
              'BAU_female_change', 'SP_female', 'SP_female_change', 
              'CM_female', 'CM_female_change', 'BAU_iucn_a3','SP_iucn_a3','CM_iucn_a3')
    rv <- c(0,0,1,0,1,0,1)
    dt <- dfsum.bc[, outbs]
    dt2 <- dt[order(dt$basin, dt$subbasinT), ]
    tbcout <- htmlTable::htmlTable(htmlTable::txtRound(dt2,  
                                digits = rv, excl.cols = c(1,2,3,11,12,13)), rnames=FALSE)
    sink("inst/ms_res/tabS2demog.html")
    print(tbcout, type="html", useViewer = FALSE)
    sink()
    
    }
  
  lout <- list(dfsum.b = dfsum.b, dfsum.c = dfsum.c, dfsum.bc = dfsum.bc)
  return(lout)
  
}