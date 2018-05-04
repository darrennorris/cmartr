#' Title
#' @title Create summary for population scenarios in Table 1.

#' @param demogC Country level population projections from "demogCountry.R"
#' 
#' @description Used in Table 1.
#' Generates dataframe with country level summary of population projections.
#'
#' @return Data.frame with country level summary of population projections for three scenarios.
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' lt <- resTab(listsf = lsf, input_rp = rp, make_html = FALSE)
#' popc <- "C:\\Users\\Darren\\Documents\\2018 Unifilis demography\\analysis\\dfpop.RDS"
#' demogC <- demogCountry(popc = popc, rlc = lt$rlc)
#' dfsum <- resTabDemog(demogC = demogC)
#' }
resTabDemog <- function(demogC = NA){
  
  dfpop.res <- demogC
  # Business as usual 
  # final year totals
  # Accessible populations with nest collection (first year survival 0.1) and
  # adult harvest (10%).
  selBA <- which(dfpop.res$Years == max(dfpop.res$Years) & 
                   dfpop.res$accessible == "Yes" &
                   dfpop.res$variable == "tot_km" &
                   dfpop.res$type =="headstart, female-hunt 10%" &
                   dfpop.res$increase == "0.1" & 
                   dfpop.res$propKM == 1) # 9 rows (total for each country)
  # Inaccessible at base rates.
  selBNA <- which(dfpop.res$Years == max(dfpop.res$Years) & 
                    dfpop.res$accessible == "No" &
                    dfpop.res$variable == "tot_km" &
                    dfpop.res$type =="headstart" &
                    dfpop.res$increase == "0.2" &
                    dfpop.res$propKM == 1) # 53 rows (total for each subbasin)
  
  dft.BAU <-merge(dfpop.res[selBA, ], dfpop.res[selBNA, ], 
                  by = c("name"))
  # summarise totals
    dfBAU <-  plyr::ddply(dft.BAU, c("name"), summarize,
                  BAU_km_tot = max(na.omit(distKM.x)) + max(na.omit(distKM.y)), 
                  BAU_km_accessible = max(na.omit(distKM.x)),
                  BAU_km_inaccessible = max(na.omit(distKM.y)),
                  BAU_lambda_accessible = max(na.omit(lambda.x)), 
                  BAU_lambda_inaccessible = max(na.omit(lambda.y)),
                  BAU_female_current = (max(na.omit(distKM.x)) + max(na.omit(distKM.y))) * 10,
                  BAU_female = round(max(na.omit(adult_females.x)) + 
                                      max(na.omit(adult_females.y)), 3),
                  BAU_female_diff = (round(max(na.omit(adult_females.x)) + 
                                             max(na.omit(adult_females.y)), 3) - 
                                       ((max(na.omit(distKM.x)) + max(na.omit(distKM.y))) * 10)), 
                  BAU_female_change = ((round(max(na.omit(adult_females.x)) + max(na.omit(adult_females.y)), 3) - 
                                          ((max(na.omit(distKM.x)) + max(na.omit(distKM.y))) * 10)) / 
                                         ((max(na.omit(distKM.x)) + max(na.omit(distKM.y))) * 10)) * 100
                  
  ) 
  
  # Strict protection, 
  #Accessible with PAs set to base
  selSPA.pa <- which(dfpop.res$Years == max(dfpop.res$Years) & 
                       dfpop.res$accessible == "Yes" &
                       dfpop.res$variable == "tot_PA" &
                       dfpop.res$type =="headstart" &
                       dfpop.res$increase == "0.2"&
                       dfpop.res$propKM == 1) 
  #Accessible not PAs set to nest collection (first year survival 0.1) and
  # adult harvest (10%).
  selSPA.npa <- which(dfpop.res$Years == max(dfpop.res$Years) & 
                        dfpop.res$accessible == "Yes" &
                        dfpop.res$variable == "tot_notPA" &
                        dfpop.res$type =="headstart, female-hunt 10%" &
                        dfpop.res$increase == "0.1" &
                        dfpop.res$propKM == 1) 
  # Inaccessible at base rates.
  selSPNA <- which(dfpop.res$Years == max(dfpop.res$Years) & 
                     dfpop.res$accessible == "No" &
                     dfpop.res$variable == "tot_km" &
                     dfpop.res$type =="headstart" &
                     dfpop.res$increase == "0.2" &
                     dfpop.res$propKM == 1) 
  
  dft.SP <- merge(dfpop.res[selSPA.pa, ], dfpop.res[selSPA.npa, ],
                  by = c("name"))
  dft.SP <-  merge(dft.SP, dfpop.res[selSPNA, ], 
                   by = c("name"))
  
  dfSP <-  plyr::ddply(dft.SP, c("name"), summarize,
                       SP_km_tot = max(na.omit(distKM.x)) + max(na.omit(distKM.y)) + 
                         max(na.omit(distKM)), 
                       SP_km_accessible = max(na.omit(distKM.x)) + max(na.omit(distKM.y)),
                       SP_km_inaccessible = max(na.omit(distKM)),
                       SP_lambda_accessible = max(na.omit(lambda.y)), 
                       SP_lambda_inaccessible = max(na.omit(lambda)),
                       SP_female_current = (max(na.omit(distKM.x)) + max(na.omit(distKM.y)) + 
                                              max(na.omit(distKM))) * 10,
                       SP_female = round(max(na.omit(adult_females.x)) + 
                                           max(na.omit(adult_females.y)) + 
                                           max(na.omit(adult_females)), 3)
                       ) 
  
  dfSP$SP_female_diff <- dfSP$SP_female - dfSP$SP_female_current
  dfSP$SP_female_change <- ((dfSP$SP_female - dfSP$SP_female_current) / dfSP$SP_female_current) * 100
  
  # Community management
  #Accessible with PAs set set to nest collection (first year survival 0.1) and
  # adult harvest (10%).
  selCMA.pa <- which(dfpop.res$Years == max(dfpop.res$Years) & 
                       dfpop.res$accessible == "Yes" &
                       dfpop.res$variable == "tot_PA" &
                       dfpop.res$type =="headstart, female-hunt 10%" &
                       dfpop.res$increase == "0.1" &
                       dfpop.res$propKM == 1) 
  
  #Accessible not PAs to headstart 0.5 and harvest 10%
  selCMA.npa <- which(dfpop.res$Years == max(dfpop.res$Years) & 
                        dfpop.res$accessible == "Yes" &
                        dfpop.res$variable == "tot_notPA" &
                        dfpop.res$type =="headstart, female-hunt 10%" &
                        dfpop.res$increase == "0.5" &
                        dfpop.res$propKM == 1) 
  # Inaccessible at base rates.
  selCMNA <- which(dfpop.res$Years == max(dfpop.res$Years) & 
                     dfpop.res$accessible == "No" &
                     dfpop.res$variable == "tot_km" &
                     dfpop.res$type =="headstart" &
                     dfpop.res$increase == "0.2"&
                     dfpop.res$propKM == 1) 
  
  dft.CM <- merge(dfpop.res[selCMA.pa, ], dfpop.res[selCMA.npa, ],
                  by = c("name"))
  dft.CM <-  merge(dft.CM, dfpop.res[selCMNA, ], 
                   by = c("name"))
  
  dfCM <-  plyr::ddply(dft.CM, c("name"), summarize,
                       CM_km_tot = max(na.omit(distKM.x)) + max(na.omit(distKM.y)) + 
                         max(na.omit(distKM)), 
                       CM_km_accessible = max(na.omit(distKM.x)) + max(na.omit(distKM.y)),
                       CM_km_inaccessible = max(na.omit(distKM)),
                       CM_lambda_accessible = max(na.omit(lambda.y)), 
                       CM_lambda_inaccessible = max(na.omit(lambda)),
                       CM_female_current = (max(na.omit(distKM.x)) + max(na.omit(distKM.y)) + 
                                              max(na.omit(distKM))) * 10,
                       CM_female = round(max(na.omit(adult_females.x)) + 
                                           max(na.omit(adult_females.y)) + 
                                           max(na.omit(adult_females)), 3)
  ) 
  dfCM$CM_female_diff <- dfCM$CM_female - dfCM$CM_female_current
  dfCM$CM_female_change <- ((dfCM$CM_female - dfCM$CM_female_current) / dfCM$CM_female_current) * 100
  
  dfsum <- merge(dfBAU, dfSP, by = c("name"))
  dfsum <- merge(dfsum, dfCM, by = c("name"))  
  dfsum$BAU_flag_50 <- ifelse(dfsum$BAU_female_change < -49.999, 1,0)
  dfsum$BAU_flag_30 <- ifelse(dfsum$BAU_female_change < -29.999, 1,0)
  dfsum$SP_flag_50 <- ifelse(dfsum$SP_female_change < -49.999, 1,0)
  dfsum$SP_flag_30 <- ifelse(dfsum$SP_female_change < -29.999, 1,0)
  dfsum$CM_flag_50 <- ifelse(dfsum$CM_female_change < -49.999, 1,0)
  dfsum$CM_flag_30 <- ifelse(dfsum$CM_female_change < -29.999, 1,0) 
  
  return(dfsum)
  
}