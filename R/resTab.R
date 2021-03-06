#' @title Create Table 1 and Table 2
#' 
#' @description Generates html files with Table values. 
#'
#' @param listsf List of five sf objects created by prepTabcover.R
#' @param input_rp Location with points of river sections.
#' @param make_html Logical (TRUE/FALSE). Should html tables be written.
#'
#' @return Creates html tables with values used in article.
#' @import sf
#' @export
#'
#' @examples
#' \dontrun{
#' lsf <-  prepTabcover(pBasin = B, pBasinSp = Bsp, 
#' pBasinC = BC, riv = rin, make_shape = FALSE)
#' # hack to get folder path
#' rp <- system.file("shape/shapes_rivers3395", package="cmartr")
#' # run
#' lt <- resTab(listsf = lsf, input_rp = rp, make_html = TRUE)
#' }
resTab <- function(listsf = NA, input_rp = NA, make_html = FALSE){
  
  # need to correct error with making "key_area" in listsf$basinc,
  # is duplicating rows.....
  dc3bc <- plyr::ddply(listsf$basinc, c("name"), summarise, 
                       subbasin_n = length(unique(subbasinT)),
                       subbasin_area = sum(unique(areab_km))
  )
  
  # subasin PA per country
  dcbc <- plyr::ddply(listsf$basinp, c("name", "paclass"), summarise,
                      tota = round(sum(as.numeric(p_r_km2)),3)
  )
  dcb1c <- reshape2::dcast(dcbc, name ~ paclass, fill = 0, value.var = "tota")
  names(dcb1c) <- c("name", "akm_use", "akm_sp", "akm_it")
  dcb1c$akm_patot <- dcb1c$akm_use + dcb1c$akm_sp + dcb1c$akm_it
  
  # Country sum of all rivers 
  dc3 <- plyr::ddply(listsf$riverbc, c("name"), 
                     summarise,
                     rkm = round(sum(as.numeric(lenrb_km)),3)
  )

  # Country sum of all rivers in PAS
  dcb <- plyr::ddply(listsf$riverpbc, c("name", "paclass"), summarise,
                     totr = round(sum(as.numeric(lenrpa_km)),3)
  )
  
  dcb1 <- reshape2::dcast(dcb, name ~ paclass, fill = 0, value.var = "totr")
  names(dcb1) <- c("name", "rkm_use", "rkm_sp", "rkm_it")
  dcb1$rkm_patot <- dcb1$rkm_use + dcb1$rkm_sp + dcb1$rkm_it
  
  basinc <- merge(dc3, dcb1)
  basinc$rkm_nopa <- basinc$rkm - basinc$rkm_patot
  basinc[is.na(basinc)] <- 0
  basinc$pa_rprop <- (basinc$rkm_patot / basinc$rkm) * 100
  basinc$flag_aich17 <- ifelse(basinc$pa_rprop > 16.99999,1,0)
  basinc$flag_aich50 <- ifelse(basinc$pa_rprop > 49.99999,1,0)
  
  bt <- merge(dc3bc, dcb1c )
  bt$akm_nopa <- as.numeric(bt$subbasin_area) - bt$akm_patot
  bt$pa_aprop <- (bt$akm_patot / as.numeric(bt$subbasin_area)) * 100
  bt$flag_aich17a <- ifelse(bt$pa_aprop > 16.99999,1,0)
  bt$flag_aich50a <- ifelse(bt$pa_aprop > 49.99999,1,0)
  basinc <- merge(bt, basinc)
  
  # now acessibility
  myfiles <- file.info(list.files(input_rp, pattern = ".shp", full.names =TRUE))
  
  # 215866
  sf.rivp <- rbind(read_sf(rownames(myfiles)[1]), 
                   read_sf(rownames(myfiles)[2]),
                   read_sf(rownames(myfiles)[3]),
                   read_sf(rownames(myfiles)[4]),
                   read_sf(rownames(myfiles)[5]),
                   read_sf(rownames(myfiles)[6]),
                   read_sf(rownames(myfiles)[7]),
                   read_sf(rownames(myfiles)[8]),
                   read_sf(rownames(myfiles)[9]),
                   read_sf(rownames(myfiles)[10]),
                   read_sf(rownames(myfiles)[11]),
                   read_sf(rownames(myfiles)[12]),
                   read_sf(rownames(myfiles)[13]),
                   read_sf(rownames(myfiles)[14]),
                   read_sf(rownames(myfiles)[15]),
                   read_sf(rownames(myfiles)[16]),
                   read_sf(rownames(myfiles)[17]),
                   read_sf(rownames(myfiles)[18]),
                   read_sf(rownames(myfiles)[19]),
                   read_sf(rownames(myfiles)[20]),
                   read_sf(rownames(myfiles)[21]),
                   read_sf(rownames(myfiles)[22]),
                   read_sf(rownames(myfiles)[23]),
                   read_sf(rownames(myfiles)[24]),
                   read_sf(rownames(myfiles)[25]),
                   read_sf(rownames(myfiles)[26]),
                   read_sf(rownames(myfiles)[27]),
                   read_sf(rownames(myfiles)[28]),
                   read_sf(rownames(myfiles)[29]),
                   read_sf(rownames(myfiles)[30]),
                   read_sf(rownames(myfiles)[31]),
                   read_sf(rownames(myfiles)[32]),
                   read_sf(rownames(myfiles)[33]),
                   read_sf(rownames(myfiles)[34]),
                   read_sf(rownames(myfiles)[35]),
                   read_sf(rownames(myfiles)[36]),
                   read_sf(rownames(myfiles)[37]),
                   read_sf(rownames(myfiles)[38]),
                   read_sf(rownames(myfiles)[39]),
                   read_sf(rownames(myfiles)[40]),
                   read_sf(rownames(myfiles)[41]),
                   read_sf(rownames(myfiles)[42]),
                   read_sf(rownames(myfiles)[43]),
                   read_sf(rownames(myfiles)[44]),
                   read_sf(rownames(myfiles)[45]),
                   read_sf(rownames(myfiles)[46]),
                   read_sf(rownames(myfiles)[47]),
                   read_sf(rownames(myfiles)[48]),
                   read_sf(rownames(myfiles)[49]),
                   read_sf(rownames(myfiles)[50]),
                   read_sf(rownames(myfiles)[51]),
                   read_sf(rownames(myfiles)[52])
  )
  
  # add country borders to rivers and basin
  nec <- rnaturalearth::ne_countries(continent = "South America", 
                                     type = 'map_units', returnclass = "sf")
  nec3395 <- sf::st_transform(nec, crs=3395)
  st_crs(sf.rivp) <- st_crs(nec3395)
  sf.rivpc <- sf::st_intersection(sf.rivp, nec3395)
  
  # Access points with country
  dt1 <- plyr::ddply(sf.rivpc, c("name", "accessible", "All"), summarise,
               tot_km = length(na.omit(All))
  )
  dt1 <- na.omit(dt1)
  dt1$AllF <- as.factor(dt1$All)
  levels(dt1$AllF) <- c("non-pa", "pa")
  dt1$aflag <- paste(dt1$accessible, dt1$AllF, sep = "_")
  dt1w <- reshape2::dcast(dt1, name ~ aflag, fill = 0, value.var = "tot_km")
  dt1w$tot_kmac <- dt1w$`No_non-pa` + dt1w$No_pa + dt1w$`Yes_non-pa` + dt1w$Yes_pa
  myc <- c("name", "Yes_pa", "Yes_non-pa", "No_pa", "No_non-pa", "tot_kmac")
  basinc <- merge(basinc, dt1w[, myc])
  
  cout <- c("name", "subbasin_n", "subbasin_area", "akm_patot", "akm_nopa", 
            "pa_aprop", "flag_aich17a", "flag_aich50a", "rkm", "rkm_patot", 
            "rkm_nopa", "pa_rprop", "flag_aich17", "flag_aich50", 
            "Yes_pa", "Yes_non-pa", "No_pa", "No_non-pa", "tot_kmac")

  # River length per country, in format for demography model
  dtc1 <- plyr::ddply(sf.rivpc, c("name", "accessible", "All", 
                                     "Indigenous", "Strict", "Use"), summarise,
                      tot_km = length(na.omit(All))
  )
  dtc1 <- na.omit(dtc1)
  dtc1$tot_PA <- dtc1$All*dtc1$tot_km
  dtc1$tot_Ind <- dtc1$Indigenous*dtc1$tot_km
  dtc1$tot_SP <- dtc1$Strict*dtc1$tot_km
  dtc1$tot_use <- dtc1$Use*dtc1$tot_km
  dtc2 <- plyr::ddply(dtc1, c("name", "accessible"), summarise,
                      tot_km = sum(na.omit(tot_km)),
                      tot_notPA = sum(na.omit(tot_km)) - sum(na.omit(tot_PA)),
                      tot_PA = sum(na.omit(tot_PA)),
                      tot_Ind = sum(na.omit(tot_Ind)),
                      tot_SP = sum(na.omit(tot_SP)),
                      tot_use = sum(na.omit(tot_use))
  )
  
  # River length per basin and country, in format for demography model
  dtc1b <- plyr::ddply(sf.rivpc, c("BASIN_N",  "name", "subbasn","accessible", "All", 
                                  "Indigenous", "Strict", "Use"), summarise,
                      tot_km = length(na.omit(All))
  )
  dtc1b <- na.omit(dtc1b)
  dtc1b$tot_PA <- dtc1b$All*dtc1b$tot_km
  dtc1b$tot_Ind <- dtc1b$Indigenous*dtc1b$tot_km
  dtc1b$tot_SP <- dtc1b$Strict*dtc1b$tot_km
  dtc1b$tot_use <- dtc1b$Use*dtc1b$tot_km
  dtc2b <- plyr::ddply(dtc1b, c("BASIN_N",  "name", "subbasn", "accessible"), summarise,
                      tot_km = sum(na.omit(tot_km)),
                      tot_notPA = sum(na.omit(tot_km)) - sum(na.omit(tot_PA)),
                      tot_PA = sum(na.omit(tot_PA)),
                      tot_Ind = sum(na.omit(tot_Ind)),
                      tot_SP = sum(na.omit(tot_SP)),
                      tot_use = sum(na.omit(tot_use))
  )
  
  # Table 2 river summaries
  dc <- plyr::ddply(listsf$riverpbc, c("arearnk", "paclass"), summarise,
                    totr = round(sum(as.numeric(lenrpa_km)),3)
  )
  
  # sum of all rivers in PAS
  dc1 <- reshape2::dcast(dc, arearnk ~ paclass, fill = 0, value.var = "totr")
  names(dc1) <- c("arearank", "rkm_use", "rkm_sp", "rkm_it")
  dc1$rkm_patot <- dc1$rkm_use + dc1$rkm_sp + dc1$rkm_it
  
  # sum of all rivers in subbasins
  listsf$riverb$area_Mkm2 <- round(as.numeric(listsf$riverb$area_km2) /1000000, 3)
  dc3 <- plyr::ddply(listsf$riverb, c("arearank", "BASIN_NAME", "subbasin", 
                                   "subbasinT", "area_km2", "area_Mkm2"), 
                     summarise,
                     rkm = round(sum(as.numeric(lenrb_km)),3)
  )
  
  # check sum of all rivers in subbasin
  dc3c <- plyr::ddply(listsf$riverbc, c("arearank"), 
                      summarise,
                      rkm = round(sum(as.numeric(lenrb_km)),3)
  )
  # 9 subbsins with differences, max diff = 4.7
  #data.frame(old = dc3$rkm, new = dc3c$rkm, diffr = dc3$rkm - dc3c$rkm)
  
  # Table 2 subbasin areas
  t2ba <- plyr::ddply(listsf$basinc, c("arearank", "BASIN_NAME", "subbasinT"), summarise, 
                      country_n = length(unique(name)),
                      area_km2 = as.numeric(sum(areab_km)),
                      area_Mkm2 = round((as.numeric(sum(areab_km) / 1000000)), 3)
  )
  
  # subasin PA
  t2bp1 <- plyr::ddply(listsf$basinp, c("arearnk", "paclass"), summarise,
                       tota = round(sum(as.numeric(p_r_km2)),3)
  )
  t2bp <- reshape2::dcast(t2bp1, arearnk ~ paclass, fill = 0, value.var = "tota")
  names(t2bp) <- c("arearank", "akm_use", "akm_sp", "akm_it")
  t2bp$akm_patot <- t2bp$akm_use + t2bp$akm_sp + t2bp$akm_it
  t2bp$aMkm_patot <- t2bp$akm_patot / 1000000
  
  t2b <- merge(t2ba, t2bp, all.x = TRUE)
  t2b[is.na(t2b)] <- 0
  t2b$akm_nopa <- t2b$area_km2 - t2b$akm_patot
  t2b$aMkm_nopa <- t2b$akm_nopa / 1000000
  t2b$pa_aprop <- round(((t2b$akm_patot / t2b$area_km2) * 100), 1)
  t2b$flag_aich17a <- ifelse(t2b$pa_aprop > 16.9999,1,0)
  t2b$flag_aich50a <- ifelse(t2b$pa_aprop > 49.9999,1,0)
  
  # Table 2, river lengths
  basinrl <- merge(dc3c, dc1, all.x = TRUE)
  basinrl[is.na(basinrl)] <- 0
  basinrl$rkm_nopa <- basinrl$rkm - basinrl$rkm_patot
  basinrl$pa_rprop <- round(((basinrl$rkm_patot / basinrl$rkm)) * 100, 1)
  basinrl$flag_aich17 <- ifelse(basinrl$pa_rprop > 16.9999,1,0)
  basinrl$flag_aich50 <- ifelse(basinrl$pa_rprop > 49.9999,1,0)
  basinr <- merge(t2b, basinrl, all.x = TRUE)
  basinr[is.na(basinr)] <- 0
  
  # order for table, tidy rounding
  newdata <- basinr[order(basinr$BASIN_NAME, basinr$subbasinT),] 
  row.names(newdata) <- NULL
  newdata$npa <- paste("(", paste(round(newdata$aMkm_patot,3), 
                                  round(newdata$aMkm_nopa, 3), 
                                  sep="/"), ")", sep="")
  newdata$aMkmout <- paste(round(newdata$area_Mkm2, 3), newdata$npa, 
                           sep="\n")
  newdata$np <- paste("(", paste(round(newdata$rkm_patot,0), 
                                 round(newdata$rkm_nopa, 0), 
                                 sep="/"), ")", sep="")
  newdata$rkmout <- paste(round(newdata$rkm, 0), newdata$np, 
                          sep="\n")
  
  # cols for table
  mycnames <- c("BASIN_NAME", "subbasinT", "country_n", "pa_aprop", "aMkmout", 
                "area_Mkm2", "aMkm_patot", "aMkm_nopa", 
                "pa_rprop", "rkmout","rkm", "rkm_patot", "rkm_nopa")
  
  # Table 2 accessibility
  # Access points with country
  dt1b <- plyr::ddply(sf.rivpc, c("BASIN_N", "subbasn", "accessible", "All"), 
                      summarise,
                     tot_km = length(na.omit(All))
  )
  dt1b <- na.omit(dt1b)
  dt1b$AllF <- as.factor(dt1b$All)
  levels(dt1b$AllF) <- c("non-pa", "pa")
  dt1b$aflag <- paste(dt1b$accessible, dt1b$AllF, sep = "_")
  dt1bw <- reshape2::dcast(dt1b, BASIN_N + subbasn ~ aflag, fill = 0, value.var = "tot_km")
  dt1bw$tot_kmac <- dt1bw$`No_non-pa` + dt1bw$No_pa + dt1bw$`Yes_non-pa` + dt1bw$Yes_pa
  mycb <- c("BASIN_N", "subbasn", "Yes_pa","Yes_non-pa", "No_pa", "No_non-pa", "tot_kmac") 
  tab2acc <- dt1bw[, mycb]

  # Table 2 Basin totals
  # countries in each basin
  bs <- merge(
    plyr::ddply(listsf$basinc, ("BASIN_NAME"), summarise, 
                count_n = length(unique(name))),
    plyr::ddply(basinr, ("BASIN_NAME"), summarise, 
                pa_per = (sum(akm_patot) / sum(area_km2))*100,
                area_Mkm2 = sum(area_km2) / 1000000, 
                aMkm_patot = sum(akm_patot) / 1000000, 
                aMkm_nopa = (sum(area_km2) - sum(akm_patot)) / 1000000, 
                par_per = (sum(rkm_patot) / sum(rkm)) * 100, 
                rkm = sum(rkm), 
                rkm_patot = sum(rkm_patot), 
                rkm_nopa = sum(rkm_nopa)
    ) 
  )
   # write tables in html format 
  if(make_html!=FALSE){
    t1out <- htmlTable::htmlTable(htmlTable::txtRound(basinc[, cout], 1))
    sink("inst/ms_res/tab1riv.html")
    print(t1out, type="html", useViewer = FALSE)
    sink()
    
    t2out <- htmlTable::htmlTable(newdata[, mycnames], rnames=FALSE)
    sink("inst/ms_res/tab2riv.html")
    print(t2out, type="html", useViewer = FALSE)
    sink()
    
    t2outb <- htmlTable::htmlTable(bs, rnames=FALSE)
    sink("inst/ms_res/tab2rivBasin.html")
    print(t2outb, type="html", useViewer = FALSE)
    sink()
    
  }
    
    listout <- list(t1out = basinc[, cout], t2out = newdata[, mycnames], 
                    t2outac = tab2acc, t2outb = bs, rlc = dtc2, rlcb = dtc2b)
    return(listout)
 
}