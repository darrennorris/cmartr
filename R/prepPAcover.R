#' @title Prepare polygons for PAcover calculations
#' @description Generates files for subsequent use.
#'
#' @param pPA Input file with protected area polygons.
#' @param pBasin Input file with basin polygons.
#' @param pBasinSp Input file with polygon covering basins in species range of occurance.
#'
#' @return Polygons of area covered by protected areas.
#' @details To remove overlapping areas with different PA classifications.
#' These are big areas and processing takes time: 4 - 5 hours on a laptop.
#' @import sf
#' @import dplyr
#' @importFrom utils memory.limit
#' @export
#'
#' @examples
#' \dontrun{
#' # Load shapefiles needed
#' # These large files are not available via github package. 
#' # Download "shapes.zip" from :
#' # https://drive.google.com/open?id=1QQArA7pPLemUVQTKx7PxigsQXKpOG0YQ
#' Bsp <- system.file("shape/speciesBasin.shp", package="cmartr")
#' PA <- system.file("shape/wdpaselect.shp", package="cmartr")
#' B <- system.file("shape/amazon_orinoco.shp", package="cmartr")
#' #add id for dissolved basin polygon
#' sfclean$sbid <- "P.unifilis"
#' pacover <- prepPAcover(pPA = PA, pBasin = B, pBasinSp = Bsp)
#' }
prepPAcover <- function(pPA = NA, pBasin = NA, pBasinSp = NA){
  # big spatial processing needs memory
  memory.limit(84000)
  # clean and prep shapefiles
  sf.speciesBasin <- pBasinSp
  sf.pa <- cmartr::prepPA(PAin = pPA)
  sfclean <- cmartr::prepBasin(Bain = pBasin)
  #add id for dissolved basin polygon
  sfclean$sbid <- "P.unifilis"
  
  # crop PA polygon
  # covers a big area so transform to planar
  sf.pa3395 <- sf::st_transform(sf.pa, crs = 3395)
  sf.speciesBasin3395 <- sf::st_transform(sf.speciesBasin, crs = 3395)
  # 1.5 hours approx
  sf.pa.crop <- sf::st_intersection(sf.pa3395, sf.speciesBasin3395)
  
  # now remove overlapping areas with different classifications
  # 2 - 3 hours with laptop, remove area covered by lower levels
  # Indigenous areas, remove areas covered by SP and Use
  selTI <- which(sf.pa.crop$paclass==3)
  sf.pa.cropTI <- sf.pa.crop[selTI,]
  sf.pa.cropTIn <- sf.pa.crop[-selTI,]
  sf.pa.cropTIclean <- sf::st_difference(sf::st_union(sf.pa.cropTI), 
                                     sf::st_union(sf.pa.cropTIn))
  sf.pa.cropTIclean <- sf::st_sf(paclass=3,
                                 geometry = sf::st_geometry(sf.pa.cropTIclean))
  
  # Use
  selUSE <- which(sf.pa.crop$paclass==1)
  sf.pa.cropUSE <- sf.pa.crop[selUSE, ]
  sf.pa.cropUSEclean <- sf::st_union(sf.pa.cropUSE)
  sf.pa.cropUSEclean <- sf::st_sf(paclass=1, 
                              geometry = sf::st_geometry(sf.pa.cropUSEclean))
  
  # Strict protection, remove areas covered by Use
  selSP <- which(sf.pa.crop$paclass==2)
  sf.pa.cropSP <- sf.pa.crop[selSP,]
  sf.pa.cropSPclean <- sf::st_difference(sf::st_union(sf.pa.cropSP), 
                                     sf::st_union(sf.pa.cropUSEclean))
  sf.pa.cropSPclean <- sf::st_sf(paclass=2, 
                             geometry = sf::st_geometry(sf.pa.cropSPclean))
  
  # Put the cleaned and unioned class polygons back together
  sf.pa.cropclean <- rbind(sf.pa.cropTIclean[,'paclass'], 
                           sf.pa.cropSPclean[,'paclass'], 
                           sf.pa.cropUSEclean[,'paclass'])
  
  # calculate area for each protected area class
  sf.pa.cropclean$area_km2 <- units::set_units(sf::st_area(sf.pa.cropclean), km^2)
  # as proportion of total area covered by basins
  totarea <- sum(units::set_units( sf::st_area(sf::st_transform(sfclean, crs=3395)), km^2))
  sf.pa.cropclean$area_prop <- sf.pa.cropclean$area_km2 / totarea
  
  return(sf.pa.cropclean)
  
}