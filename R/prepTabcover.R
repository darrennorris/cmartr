#' @title Prepare polygons for Table calculations
#'
#' @description Generates sf objects with attributes for Table values. Optionally exports
#' polygon as shapefile.
#' 
#' @param pBasin Subbasin cover polygons.
#' @param pBasinSp Extent polygon covering 
#' subbasins in species range of occurance.
#' @param pBasinC PA cover in subbasins with country.
#' @param riv River lines.
#' @param rastAc Raster brick with acessibility values.
#' @param make_shape Logical (TRUE/FALSE). Generate polygon shapefiles of
#' ......
#'
#' @return List with objects of class sf with attribute data 
#' used to generate result tables (epsg = 3395). 
#' Optionally writes shapefiles.
#' riverb (sf.rivb) = 4629 river lines with basin attributes.
#' riverbc (sf.rivb2) = 4791 river lines with basin and country attributes.
#' riverpbc (sf.riv) = 2519 river lines with PA class, basin and country.
#' basinc (sfclean3395c) = 71 polygons with basin and country.
#' basinp (pac3395) = 169 polygons with PA class basin and country.
#' 
#' @import sp
#' @importFrom methods as
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
#' # This function needs 4 shapefiles as listed below.
#' 
#' # Subbasin cover polygons
#' B <- system.file("shape/amazon_orinoco.shp", package="cmartr")
#' # Extent polygon covering subbasins in species range of occurance.
#' # From prepBasin.R .
#' Bsp <- system.file("shape/speciesBasin.shp", package="cmartr")
#' # PA cover in subbasins with country. From prepPAcountry.R 
#' BC <- system.file("shape/basinpacover.shp", package="cmartr")
#' # River lines.
#' rin <- system.file("shape/sa_strwgs84.shp", package="cmartr")
#' # Brick with acessibility raster
#' ras1 <- system.file("raster/uas.grd", package="cmartr")
#' 
#' # run
#' lsf <- prepTabcover(pBasin = B, pBasinSp = Bsp, 
#'   pBasinC = BC, riv = rin, rastAc = ras1, make_shape = FALSE)
#' }
prepTabcover <- function(pBasin = NA, pBasinSp = NA, 
                         pBasinC = NA, riv = NA, rastAc = NA, make_shape = FALSE){
  # 6.5 hours approx overall, big spatial processing needs memeory
  memory.limit(84000)

  # 15 mins Load necessary files
  # Load and tidy 53 subbasins
  sfclean3395 <- sf::st_transform(cmartr::prepBasin(Bain = pBasin), 
                              crs = 3395)
  # Species basin coverage (extent polygon)
  sb3395 <- sf::st_transform(sf::read_sf(pBasinSp), crs=3395)
  # PA cover in basins
  pac3395 <- sf::st_transform(sf::read_sf(pBasinC), crs=3395)
  pac3395 <- lwgeom::st_make_valid(pac3395)
  # Load river lines
  sf.r3395 <- sf::st_transform(sf::read_sf(riv), crs=3395)
  
  # Acessibility raster
  sall <- raster::brick(rastAc)
  
  # 2.5 - 3 hours crop by general extent to make subsequent processess faster
  sf.r.crop3395 <- sf::st_intersection(sf.r3395, sb3395)
  gc()
  # 1.5 hr intersect rivers in 53 basins, 4629 features
  sf.rivb <- sf::st_intersection(sf.r.crop3395, sfclean3395)
  sf.rivb$lenrb_km <- units::set_units(sf::st_length(sf.rivb), km)
  
  # intersect rivers in PA coverage, 2483 features
  sf.riv <- sf::st_intersection(sf.r.crop3395, pac3395)
  sf.riv$lenrpa_km <- units::set_units(sf::st_length(sf.riv), km)

  # add country borders to rivers and basin
  nec <- rnaturalearth::ne_countries(continent = "South America", 
                                     type = 'map_units', returnclass = "sf")
  nec3395 <- sf::st_transform(nec, crs=3395)
  
  # rivers with country
  sf.rivb2 <- sf::st_intersection(sf.rivb, nec3395)
  sf.rivb2$lenrb_km <- units::set_units(sf::st_length(sf.rivb2), km)

  # 2 hours basins with country and access
  sfclean3395c <- sf::st_intersection(sfclean3395, nec3395)
  sfclean3395c$areab_km <- units::set_units(sf::st_area(sfclean3395c), km^2)
  # index key for merge
  if("arearank" %in% names(sfclean3395c)){
  sfclean3395c$key_area <- paste(sfclean3395c$arearank, sfclean3395c$basinc$name, sep="_")
  }else{
    sfclean3395c$key_area <- paste(sfclean3395c$arearnk, sfclean3395c$basinc$name, sep="_")
  }
  
  # Identify inacessible cells
  ru <- raster::reclassify(sall[["Dist..km."]],  c(-Inf,48.99999,NA, 
                                           49.0000000, Inf, 1))
  sf1 <- sf::st_transform(sfclean3395c, crs = 3395) %>% sf::st_cast("MULTIPOLYGON")
  sf1 <- sf1[, c('key_area', 'geometry')]
  sp1 <- as(sf1, "Spatial")
  # 15:54 - 17:16
  lout <- raster::extract(ru, sp1) # extract values for each polygon
  # how many cells inacessible
  sp1$inac <- unlist(lapply(lout, function(x) if (!is.null(x)) sum(x, na.rm=TRUE) else NA))
  # total cells
  sp1$totc <- unlist(lapply(lout, function(x) if (!is.null(x)) length(x) else NA))
  # how many cells accessible
  sp1$acc <- sp1$totc - sp1$inac
  # merge back with original data
  sfclean3395c <- merge(sfclean3395c, data.frame(sp1))
  
  gc()
  # 16:18 - 16:24
  # intersect rivers in PA coverage, 2483 features
  sf.riv <- sf::st_intersection(sf.r.crop3395, pac3395)
  sf.riv$lenrpa_km <- units::set_units(sf::st_length(sf.riv), km)
  
  # write shapes
  if(make_shape!=FALSE){
    sf::st_write(sf::st_transform(sf.rivb2, crs=4326),"rivercountries.shp")
  }
  
  # list of sf objects to return
  lout <- list(riverb = sf.rivb, riverbc = sf.rivb2, 
               riverpbc = sf.riv, basinc = sfclean3395c, 
               basinp = pac3395)
  return(lout)
}