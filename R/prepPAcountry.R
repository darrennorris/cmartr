
#' @title Prepare polygons for PA calculations
#' 
#' @description Generates files for subsequent use. Optionally exports
#' polygon as shapefile "basinpacover.shp".
#'
#' @param PAcov Input file with PA cover polygons. Created from prepPAcover.
#' @param pBasin Input file with basin polygons.
#' @param make_shape Logical (TRUE/FALSE). Generate polygon shapefile of
#' protected areas in 3 classes in species range. With country and basin attributes.
#'
#' @return Files for future use. Optionally exports
#' polygon as shapefile "basinpacover.shp".
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
#' Pcov <- system.file("shape/pacover.shp", package="cmartr")
#' B <- system.file("shape/amazon_orinoco.shp", package="cmartr")
#' pacovc <- prepPAcountry(PAcov = Pcov, pBasin = B)
#' }
prepPAcountry <- function(PAcov = NA, pBasin = NA, make_shape=FALSE){
# big spatial processing needs memeory
memory.limit(84000)

sf.pacover <- sf::st_read(PAcov)
sfclean <- cmartr::prepBasin(Bain = pBasin)
#add id for dissolved basin polygon
sfclean$sbid <- "P.unifilis"
sf.pacover3395 <- sf::st_transform(sf.pacover[, c('paclass', 'geometry')], crs=3395)
sfclean3395 <- sf::st_transform(sfclean, crs=3395)

# get country borders
nec <- rnaturalearth::ne_countries(scale = 110, type = "countries", 
                                   continent = "South America", returnclass = "sf")
nec <- st_transform(nec, crs=3395)

# few minutes
pacovc <- st_intersection(sfclean3395, nec)
pacovc2 <- st_intersection(pacovc, sf.pacover3395) 
# need to make valid
pacovc2clean <- sf::st_union(sf::st_buffer(pacovc2,0), by_feature = TRUE) %>% 
  sf::st_cast("MULTIPOLYGON") 
pacovc2clean$pa_area_km2 <- units::set_units(sf::st_area(pacovc2clean), km^2)

if(make_shape!=FALSE){
st_write(st_transform(pacovc2clean, crs=4326), "basinpacover.shp")
}

return(pacovc2clean)
}