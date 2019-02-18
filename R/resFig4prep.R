#' Title
#' @title Create data necessary for figure 4.
#' 
#' @description Creates data.frame necessary for figure 4. 
#' Calculates change in adult female number across 53 catchments.
#' 
#' @param x Data.frame. Lookup table created by "PopProj.R".
#'
#' @return Date.frame with change in female populations across 
#' 53 catchments for coverage of the range of hatchling graduation levels and 
#' female hunting levels.
#' 
#' @importFrom magrittr %>%
#' @import plyr
#' @export
#'
#' @examples
#' \dontrun{
#' #big data needs memory
#' memory.limit(12000)
#' dflup <- readRDS("inst/other/dflup.RDS")
#' #1 base rate
#' f.base <- which(dflup$hunt==0 & dflup$increase==0.2)
#' base <- read.csv(as.character(dflup[f.base,'fileout']))
#' selb <- which(base$variable=="tot_km" & base$prop_km < 1)
#' base <- base[selb, ]
#' library(magrittr)
#' library(tidyr)
#' base <- base %>% tidyr::separate(namekey, 
#' into = c("basin", "country", "subbasin"), sep="_")
#' 
#' # sum accessible and not accessible per catchment
#' library(plyr)
#' dfbt <- ddply(base, .(species, hunt, increase, subbasin, prop_km, ayear), 
#'              summarise, 
#'              lambda = min(na.omit(lambda)), 
#'              lam_n = length(unique(na.omit(lambda))),
#'              adult_fem = sum(na.omit(adult_females))
#' )
#' dfbt$rank_prop <- factor(dfbt$prop_km)
#' levels(dfbt$rank_prop) <- c(1:19)
#' dfbt$rank_prop <- as.numeric(dfbt$rank_prop)
#' 
#' #2 now with different hunt and graduation levels
#' dflupin <- dflup[-f.base, ]
#' # run function takes 40 minutes approx.
#' fig4prep <- ddply(dflupin, .(hunt, increase), .fun = resFig3prep)
#' # tidy for plotting
#' fig4prep$hunt <- factor(fig4prep$hunt) 
#' levels(fig4prep$hunt) <- c("No hunt", "Hunt 2.5%", "Hunt 10%",
#'                         "Hunt 25%", "Hunt 50%")
resFig4prep <- function(x){
  fd <- read.csv(as.character(x[ ,'fileout']))
  selfd <- which(fd$variable=="tot_km" & fd$prop_km < 1)
  fd <- fd[selfd, ]
  fd <- fd %>% tidyr::separate(namekey, 
                               into = c("basin", "country", "subbasin"), sep="_")
  
  dffd <- plyr::ddply(fd, .(species, hunt, increase, subbasin, prop_km, ayear), 
                summarise, 
                lambda_new = min(na.omit(lambda)), 
                lam_n_new = length(unique(na.omit(lambda))),
                adult_fem_new = sum(na.omit(adult_females))
  )
  rm("fd")
  #3 now add columns for merge
  dffd$rank_prop <- factor(1 - dffd$prop_km)
  levels(dffd$rank_prop) <- c(1:19)
  dffd$rank_prop <- as.numeric(dffd$rank_prop)
  #ddply(dffd, .(prop_km), summarise, r = mean(rank_prop))
  
  dft <- merge(dfbt, dffd,  
               by = c("species","subbasin", "ayear", "rank_prop"), 
               all.x=TRUE)
  dft$adult_fem_tot <- dft$adult_fem + dft$adult_fem_new
  rm("dffd")
  # 52 * 51 * 19 = 50388
  #length(unique(dft$subbasin)) * length(unique(dft$ayear)) * length(unique(dft$rank_prop))
  
  #function to  calculate percentage change
  fchange <- function(x){
    fem0 <- x[(x$ayear == 0), 'adult_fem_tot']
    dft1 <- data.frame(x, fem_t0 = fem0)
    dft1$adult_female_diff <- round(((dft1$adult_fem_tot - dft1$fem_t0) / dft1$fem_t0), 3)
    dft1$change50_flag <- as.integer(ifelse(abs(dft1$adult_female_diff) > 0.499, 1, 0))
    dft1$double_flag <- as.integer(ifelse(dft1$adult_female_diff > 0.999, 1, 0))
    dft1$prop_change_clean <- ifelse(dft1$adult_female_diff > 0.999, 1, dft1$adult_female_diff)
    dft1 <- dft1[which(dft1$ayear==50), ]
    dft1
  }
  # return percentage change after 50 years
  dft2 <- plyr::ddply(dft, .(subbasin, rank_prop), .fun = fchange) 
  dft2
}