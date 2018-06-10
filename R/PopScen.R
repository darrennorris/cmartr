PopScen <- function(x){
  # x = dflup from "projPop.R"
  # 2-3 mins, time takes to load large .csv files
  # selects relevant projection data from modelled scenarios
  f.base <- which(x$hunt==0 & x$increase==0.2)
  base <- read.csv(as.character(x[f.base,'fileout']))
  f.hunt <- which(x$hunt==10 & x$increase==0.1)
  hunt <- read.csv(as.character(x[f.hunt,'fileout']))
  f.manage <- which(x$hunt==10 & x$increase==0.5)
  manage <- read.csv(as.character(x[f.manage,'fileout']))
  
  # Business as usual, final year totals
  # Accessible populations with hunt
  selBA <- which(hunt$ayear == max(hunt$ayear) & 
                   hunt$accessible == "Yes" &
                   hunt$variable == "tot_km" &
                   hunt$prop_km == 1
  )
  # Inaccessible at base rates.
  selBNA <- which(base$ayear == max(base$ayear) & 
                    base$accessible == "No" &
                    base$variable == "tot_km" &
                    base$prop_km == 1
  )
  
  dft.BAU <-merge(hunt[selBA, ], base[selBNA, ], 
                  by = c("namekey"))
  library(plyr)
  library(dplyr)
  library(tidyr)
  dft.BAU$namekeys <- dft.BAU$namekey
  dft.BAU <- dft.BAU %>% separate(namekeys, 
                                  into = c("basin", "country", "subbasin"), sep="_")
  # limit population to 100 times original (i.e. 10*100 = 1000 adult females per km)
  if(dft.BAU$lambda.x[1] > 0.99999){
    dft.BAU$adult_females.x <- ifelse(dft.BAU$adult_females.x > (dft.BAU$fem_t0.x*100), 
                                      dft.BAU$fem_t0.x*100, 
                                      dft.BAU$adult_females.x)
  }
  
  if(dft.BAU$lambda.y[1] > 0.99999){
    dft.BAU$adult_females.y <- ifelse(dft.BAU$adult_females.y > (dft.BAU$fem_t0.y*100), 
                                      dft.BAU$fem_t0.y*100, 
                                      dft.BAU$adult_females.y)
  }
  
  # Strict protection, 
  #Accessible with PAs set to base
  selSPA.pa <- which(base$ayear == max(base$ayear) & 
                       base$accessible == "Yes" &
                       base$variable == "tot_PA" &
                       base$prop_km == 1
  ) # 80 rows 
  #Accessible not PAs set to nest collection (first year survival 0.1) and
  # adult harvest (10%).
  selSPA.npa <- which(hunt$ayear == max(hunt$ayear) & 
                        hunt$accessible == "Yes" &
                        hunt$variable == "tot_notPA" &
                        hunt$prop_km == 1)
  # Inaccessible at base rates.
  selSPNA <- which(base$ayear == max(base$ayear) & 
                     base$accessible == "No" &
                     base$variable == "tot_km" &
                     base$prop_km == 1)
  
  dft.SP <- merge(base[selSPA.pa, ], hunt[selSPA.npa, ],
                  by = c("namekey"))
  dft.SP <-  merge(dft.SP, base[selSPNA, ], 
                   by = c("namekey"))
  dft.SP$namekeys <- dft.SP$namekey
  dft.SP <- dft.SP %>% separate(namekeys, 
                                  into = c("basin", "country", "subbasin"), sep="_")
  # limit population to 100 times original (i.e. 10*100 = 1000 adult females per km)
  if(dft.SP$lambda.x[1] > 0.99999){
    dft.SP$adult_females.x <- ifelse(dft.SP$adult_females.x > (dft.SP$fem_t0.x*100), 
                                     dft.SP$fem_t0.x*100, 
                                     dft.SP$adult_females.x)
  }
  if(dft.SP$lambda.y[1] > 0.99999){
    dft.SP$adult_females.y <- ifelse(dft.SP$adult_females.y > (dft.SP$fem_t0.y*100), 
                                     dft.SP$fem_t0.y*100, 
                                     dft.SP$adult_females.y)
  }
  if(dft.SP$lambda[1] > 0.99999){
    dft.SP$adult_females <- ifelse(dft.SP$adult_females > (dft.SP$fem_t0*100), 
                                   dft.SP$fem_t0*100, 
                                   dft.SP$adult_females)
  }
  
  # Community management
  #Accessible with PAs set to hunt
  selCMA.pa <- which(hunt$ayear == max(hunt$ayear) & 
                       hunt$accessible == "Yes" &
                       hunt$variable == "tot_PA" &
                       hunt$prop_km == 1) 
  #Accessible not PAs to manage
  selCMA.npa <- which(manage$ayear == max(manage$ayear) & 
                        manage$accessible == "Yes" &
                        manage$variable == "tot_notPA" &
                        manage$prop_km == 1) 
  # Inaccessible at base rates.
  selCMNA <- which(base$ayear == max(base$ayear) & 
                     base$accessible == "No" &
                     base$variable == "tot_km" &
                     base$prop_km == 1) 
  
  dft.CM <- merge(hunt[selCMA.pa, ], manage[selCMA.npa, ],
                  by = c("namekey"))
  dft.CM <-  merge(dft.CM, base[selCMNA, ], 
                   by = c("namekey"))
  dft.CM$namekeys <- dft.CM$namekey
  dft.CM <- dft.CM %>% separate(namekeys, 
                                into = c("basin", "country", "subbasin"), sep="_")
  # limit population to 100 times original (i.e. 10*100 = 1000 adult females per km)
  if(dft.CM$lambda.x[1] > 0.99999){
    dft.CM$adult_females.x <- ifelse(dft.CM$adult_females.x > (dft.CM$fem_t0.x*100), 
                                     dft.CM$fem_t0.x*100, 
                                     dft.CM$adult_females.x)
  }
  if(dft.CM$lambda.y[1] > 0.99999){
    dft.CM$adult_females.y <- ifelse(dft.CM$adult_females.y > (dft.CM$fem_t0.y*100), 
                                     dft.CM$fem_t0.y*100, 
                                     dft.CM$adult_females.y)
  }
  if(dft.CM$lambda[1] > 0.99999){
    dft.CM$adult_females <- ifelse(dft.CM$adult_females > (dft.CM$fem_t0*100), 
                                   dft.CM$fem_t0*100, 
                                   dft.CM$adult_females)
  }
  
 l1 <-  list(dft.BAU = dft.BAU, dft.SP = dft.SP, dft.CM = dft.CM)
  return(l1)
}