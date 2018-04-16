#' @title Prepare basin polygons
#' @description Generates files for subsequent use. 
#' Optionally creates "SpeciesBasin.shp" .
#'
#' @param Bain Input file with basin polygons
#' @param make_shape Logical (TRUE/FALSE). Generate polygon shapefile 
#' covering basins with species locations ("SpeciesBasin.shp").
#' @details Takes georeferenced polygons of river basins
#'
#' @return Files for subsequent use.
#' @import sf
#' @import dplyr
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' \dontrun{
#' }
prepBasin <- function(Bain = NA, make_shape = FALSE){
  sf1 <- sf::read_sf(Bain)
  sf1clean <- sf::st_union(sf::st_buffer(sf1,0), by_feature = TRUE) %>% 
    dplyr::group_by(BASIN_NAME, subbasin) %>% 
    dplyr::summarise() %>% sf::st_cast("MULTIPOLYGON")
  
  # add subbasin as per article table , cause of warning non-ASCII characters
 amsub <- c("Abacaxis", "Amazon floodplain", "Putumayo", "Japurá - Caquetá", "Javari",
             "Juruá", "Madeira", "Marañón", "Curuá-una", "Guama", "Jari", "Jutai",
             "Madeirinha", "Manacapuru", "Nanay", "Pacajá", "Piorini", "Tefe", 
             "Uatumá", "Napo", "Negro", "Purus", "Tapajós", "Tocantins",
            "Trombetas", "Ucayali", "Xingu")
  sf1clean$subbasinT <- c(amsub, sf1clean$subbasin[28:53])
  
  sf1clean$BASIN_FLAG <- as.numeric(as.factor(sf1clean$BASIN_NAME))
  sf1clean$SUBBASIN_FLAG <- as.numeric(as.factor(sf1clean$subbasin))
  sf13395 <- sf::st_transform(sf1clean, crs=3395)
  sf13395$area_km2 <- units::set_units(sf::st_area(sf13395), km^2)
  sf1clean$area_km2 <- sf13395$area_km2
  sf1clean$arearank <- rank(sf1clean$area_km2) 
  
  # make a single polygon covering all basins where species is
  # buffer needed to dissolve borders, but still has slivers
  if(make_shape!=FALSE){
  sf.speciesBasin <- sf::st_union(sf::st_buffer(sf1clean,0.00005))
  sf.speciesBasin <- st_sf(sf.speciesBasin)
  sf.speciesBasin$sbid <- "P.unifilis"
  st_write(sf.speciesBasin, "speciesBasin.shp")
  
  #sin <- file.choose()
  # this file has second feature covering holes and slivers
  #[1] "C:\\Users\\Darren\\Documents\\ms\\unpublished\\2018 Unifilis demography\\analises\\shapes\\amazon_orinoco\\spciesBasin.shp"
  #td <- st_read(sin)
  #sf.td <- sf::st_union(sf::st_buffer(td,0.0))
  #sf.td <- st_sf(sf.td)
  #sf.td$sbid <- "P.unifilis"
  #st_write(sf.td, "speciesBasin.shp")
  }
  
  return(sf1clean)
  
}