
#' @title Prepare protected area polygons
#' @description Generates files for subsequent use in "PrepPAcover.R".
#'
#' @param PAin Input file with protected area polygons
#'
#' @return Files for subsequent use.
#' @details Takes georeferenced polygons of protected areas from the World Database on Protected Areas (WDPA, downloaded from https://protectedplanet.net/ on 6 December 2017).
#' Includes subset of terrestrial South American protected areas that are in the raster grid.
#' I.e. touching, intersecting, within bounding box created from 150 km  buffer around the catchments. 
#'  Cleans and identifies 3 classes: Indigenous lands, Strict Protection and Use.
#'  Definitions from https://www.iucn.org/theme/protected-areas/about/protected-area-categories .
#' @export
#'
#' @examples
#' \dontrun{
#' # Load shapefiles needed
#' # These large files are not available via github package. 
#' # Download "shapes.zip" from :
#' # https://drive.google.com/open?id=1QQArA7pPLemUVQTKx7PxigsQXKpOG0YQ
#' # For the code below to work, 
#' # extract files in "shapes.zip" to cmartr/inst/shape .
#' PA <- system.file("shape/wdpaselect.shp", package="cmartr")
#' sf.pa <- cmartr::prepPA(PAin = PA)
#' }
prepPA <- function(PAin = NA){
### protected areas
# PAs (wgs84)
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
return(sf.pa)

}