
#' @title Prepare protected area polygons
#'
#' @param PAin File with protected area polygons
#'
#' @return Generates files for subsequent use.
#' @export
#'
#' @examples
#' \dontrun{
#' }
prepPA <- function(PAin = NA){
### protected areas
# PAs (wgs84)
# 3 classes ; Indigenous lands, Strict Protection and Use
# https://www.iucn.org/theme/protected-areas/about/protected-area-categories
 
sf.pa <- sf::read_sf(PAin)
sf.pa$paclass <- sf.pa$OBJECTID
sf.pa$paclass <- NA
getTI <- c("Indigenous Area", "Indigenous Reserve", "Indigenous Territory")
selTI <- which(sf.pa$DESIG_ENG %in% getTI)
sf.pa[selTI, 'paclass' ] <- 3

getSP <- c("Ia", "Ib")
selSP <- which(sf.pa$IUCN_CAT %in% getSP)
sf.pa[selSP, 'paclass' ] <- 2
selNA <- which(is.na(sf.pa$paclass) == TRUE)
sf.pa[selNA, 'paclass' ] <- 1
# sf1clean2 from demog_river.R
sfi <- sf::st_intersection(sf.pa, sf1clean2)
#library(raster)
#spPA <-  raster::crop(as(sf.pa, "SpatialPolygons"), 
#                      as(sf1clean2, "SpatialPolygons"))
}