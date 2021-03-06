#' @title Create Figure 2
#'
#' @description Generates Figure 2. Returns list with ggplot2 figures. 
#' Optionally exports as png.
#' 
#' @param listsf List of five sf objects created by prepTabcover.R
#' @param listTab List of data.frames created by resTab.R
#' @param pBasin Subbasin cover polygons.
#' @param pBasinSp Extent polygon covering 
#' subbasins in species range of occurance.
#' @param  pBasinAcc Subbasin cover polygons with human access proportion,
#' from demog_river.R (SAGA (grid tools, grids stats for poly)).
#' @param rastAc Acessibility raster.
#' @param make_png Logical (TRUE/FALSE). Generate png version of Fig 1.
#'
#' @return List of plots created by ggplot2. Optionally exports as png.
#' @import ggplot2
#' @importFrom grDevices dev.off png pdf
#' @importFrom gridExtra grid.arrange
#' @importFrom stats na.omit
#' @importFrom viridis scale_fill_viridis
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
#' # This function needs 3 shapefiles as listed below.
#' 
#' # Subbasin cover polygons
#' B <- system.file("shape/amazon_orinoco.shp", package="cmartr")
#' # Extent polygon covering subbasins in species range of occurance.
#' # From prepBasin.R .
#' Bsp <- system.file("shape/speciesBasin.shp", package="cmartr")
#' # Human access from demog_river.R . Need to add earlier in prep stages.
#' BAc <- system.file("shape/basinsum4326.shp", package="cmartr")
#' # Human aceesibility raster
#' ras1 <- system.file("raster/uas.grd", package="cmartr")
#' # run
#' lsf <- prepTabcover(pBasin = B, pBasinSp = Bsp, 
#'   pBasinC = BC, riv = rin, make_shape = FALSE)
#
#' lt <- resTab(listsf = lsf)
#' lfig2 <- resFig2(listsf = lsf, listTab = lt, pBasin = B, 
#'   pBasinSp = Bsp, pBasinAcc = BAc, rastAc = ras1, make_png = FALSE)
#' 
#' }
resFig2 <- function(listsf = NA, listTab = NA, pBasin = NA, 
                    pBasinSp = NA, pBasinAcc = NA, rastAc = NA, make_png = FALSE){

# load data
  #big spatial processing needs memeory
  memory.limit(84000)
  
  #Acessibility raster
  # raster to points SP, then sf transform to dataframe for ggplot2
  sall <- raster::brick(rastAc)
  ru <- sall[["Dist..km."]]
  # proj.4 projection description
  newproj <- "+proj=longlat +datum=WGS84 +no_defs"
  #18:35 - 18:38
  pr1 <- raster::projectRaster(ru, crs=newproj, res = 0.00930)
  # convert to points hack
  myPoints <- raster::rasterToPoints(pr1)
  myDataFrame <- data.frame(myPoints)
  names(myDataFrame)
  colnames(myDataFrame) <- c("X", "Y", "Values")
  rm("sall")
  rm("ru")
  rm("pr1")
sfcoun<- rnaturalearth::ne_countries(continent = "South America", 
                                     type = 'map_units', returnclass = "sf")
sfcoun <- st_sf(a= rep(1,14), geom=st_geometry(sfcoun))
sfcounD <- sf::st_union(rnaturalearth::ne_countries(continent = "South America", 
                                                    type = 'map_units', returnclass = "sf"))
sfcounD <- st_sf(a=1, geom=st_geometry(sfcounD))
# by country
pac <- merge(st_transform(lsf$basinc, crs = 4326), lt$t1out)

# basin
sfclean <- cmartr::prepBasin(Bain = pBasin)
sfclean <- merge(sfclean, lt$t2out)
sfclean$F_prop <- (sfclean$rkm / sum(sfclean$rkm))
# Basin accessibility proportions
sfclean <- merge(sfclean,
                 plyr::ddply(pac, ("arearank"), summarise,
                             totaccB = sum(na.omit(acc)), 
                             totcellB = sum(na.omit(totc)),
                             prop_accB = sum(na.omit(acc)) / sum(na.omit(totc))
                 )
)

# Country accessibility proportions
pac <- merge(pac,
             plyr::ddply(pac, ("name"), summarise,
                         totaccC = sum(na.omit(acc)), 
                         totcellC = sum(na.omit(totc)),
                         prop_accC = sum(na.omit(acc)) / sum(na.omit(totc))
             )
)

pac$F_prop <- (pac$rkm / sum(sfclean$rkm))

sfcatch2 <- read_sf(pBasinSp)
sfpa <- st_transform(lsf$basinp, crs=4326)


# now make ggplot figures
# PA class 
f1a <- ggplot(sfpa) +
  geom_sf(data = sfcounD) +
  geom_sf(data = sfcoun, size= 1.7, color="black", fill=NA) +
  geom_sf(data = sfcatch2, size = 1.1, color="black", fill=NA) +
  geom_sf(data = sfcatch2, size=1,color="yellow", fill=NA, lty=2) +
  geom_sf(data = sfpa, aes(fill = factor(paclass))) +
  scale_fill_discrete(name = "Type", 
                      labels=c("Use", "Strict\nprotection", 
                               "Indigenous\nlands")) +
  coord_sf(xlim = c(-80, -43), ylim = c(-20, 10)) +
  theme_bw() +
  theme(legend.margin=margin(t=0, r=0, b=0, l= -0.2, unit="cm")) +
  ggtitle("A) Protected area type")

f1b <- ggplot(sfcounD) + 
  geom_sf(data = sfcounD) +
  geom_raster(data=myDataFrame, aes(y = Y, x = X, fill = Values)) +
  scale_fill_viridis(name = "Dist (km)") +
  geom_sf(data = sfcoun, size= 1.7, color="white", fill=NA) +
  geom_sf(data = sfcatch2, size = 1.1, color="black", fill=NA) +
  geom_sf(data = sfcatch2, size=1,color="yellow", fill=NA, lty=2) +
  coord_sf(xlim = c(-80, -43), ylim = c(-20, 10))+
  theme_bw() + ylab("") + xlab("") +
  theme(legend.margin=margin(t=0, r=0, b=0, l= -0.2, unit="cm")) +
  ggtitle("B) Proximity to people")

# PA coverage by basin
# predefined colours
# http://sape.inf.usi.ch/quick-reference/ggplot2/colour
f1c <- ggplot(sfclean) +
  geom_sf(data = sfcounD, size=1.7) +
  geom_sf(data = sfcatch2, size = 1.1, color="black", fill=NA) +
  geom_sf(data = sfcatch2, size=1,color="yellow", fill=NA, lty=2) +
  geom_sf(aes(fill = pa_rprop)) +
  scale_fill_gradientn("%\nrivers", 
                       colours = c("darkred","tomato1", 
                                   "orange","yellow", 
                                   "lightblue","darkblue"), 
                       values = c(0, 0.17, 0.1701, 0.55,0.551, 1)) +
  coord_sf(xlim = c(-80, -43), ylim = c(-20, 10)) +
  theme_bw() +
  theme(legend.margin=margin(t=0, r=0, b=0, l= -0.2, unit="cm")) +
  ggtitle("C) Protected area cover")

# PA coverage by country
f1d <- ggplot(pac) +
  geom_sf(data = sfcounD, size=1.7) +
  geom_sf(data = sfcatch2, size = 1.1, color="black", fill=NA) +
  geom_sf(data = sfcatch2, size=1,color="yellow", fill=NA, lty=2) +
  geom_sf(aes(fill = pa_rprop), size = 0.3) +
  scale_fill_gradientn("%\nrivers", 
                       colours = c("darkred","tomato1", 
                                   "orange","yellow", 
                                   "lightblue","skyblue2"), 
                       values = c(0, .16, .201, .90, .901, 1.0)) +
  geom_sf(data = sfcoun, size = 1, color="black", fill=NA) +
  coord_sf(xlim = c(-80, -43), ylim = c(-20, 10)) +
  theme_bw() +
  theme(legend.margin=margin(t=0, r=0, b=0, l= -0.2, unit="cm")) +
  ggtitle("D) Protected area cover")

# access
f1e <- ggplot(sfclean) +
  geom_sf(data = sfcounD, size=1.7) +
  geom_sf(data = sfcatch2, size = 1.1, color="black", fill=NA) +
  geom_sf(data = sfcatch2, size=1,color="yellow", fill=NA, lty=2) +
  geom_sf(aes(fill = prop_accB * 100)) +
  scale_fill_gradientn("% \n< 49 km", 
                       colours = c("darkblue", "lightblue", "tomato1", "darkred"), 
                       values = c(0,0.45,0.4501,1)) +
  coord_sf(xlim = c(-80, -43), ylim = c(-20, 10)) +
  theme_bw() +
  theme(legend.margin=margin(t=0, r=0, b=0, l= -0.2, unit="cm")) +
  ggtitle("E) Human accessibility cover")

# Acess by country
f1f <- ggplot(pac) +
  geom_sf(data = sfcounD, size=1.7) +
  geom_sf(data = sfcatch2, size = 1.1, color="black", fill=NA) +
  geom_sf(data = sfcatch2, size=1,color="yellow", fill=NA, lty=2) +
  geom_sf(aes(fill = prop_accC * 100), size = 0.3) +
  scale_fill_gradientn("% \n< 49 km", 
                       colours = c("skyblue3", "lightblue", "tomato1", "darkred"), 
                       values = c(0,0.005,0.0511,1)) +
  geom_sf(data = sfcoun, size = 1, color="black", fill=NA) +
  coord_sf(xlim = c(-80, -43), ylim = c(-20, 10)) +
  theme_bw() +
  theme(legend.margin=margin(t=0, r=0, b=0, l= -0.2, unit="cm")) +
  ggtitle("F) Human accessibility cover")

# Population by basin
f1g <- ggplot(sfclean) +
  geom_sf(data = sfcounD, size=1.7) +
  geom_sf(data = sfcatch2, size = 1.1, color="black", fill=NA) +
  geom_sf(data = sfcatch2, size=1,color="yellow", fill=NA, lty=2) +
  geom_sf(aes(fill = F_prop * 100), size = 0.3) +
  scale_fill_gradient2("%\npopulation") +
  coord_sf(xlim = c(-80, -43), ylim = c(-20, 10)) +
  theme_bw() +
  theme(legend.margin=margin(t=0, r=0, b=0, l= -0.2, unit="cm")) +
  ggtitle("G) Podocnemis unifilis")

# Population by country
f1h <- ggplot(pac) +
  geom_sf(data = sfcounD, size=1.7) +
  geom_sf(data = sfcatch2, size = 1.1, color="black", fill=NA) +
  geom_sf(data = sfcatch2, size=1,color="yellow", fill=NA, lty=2) +
  geom_sf(aes(fill = F_prop * 100)) +
  geom_sf(data = sfcoun, size = 1, color="black", fill=NA) +
  scale_fill_gradient2("%\npopulation") +
  coord_sf(xlim = c(-80, -43), ylim = c(-20, 10)) +
  theme_bw() +
  theme(legend.margin=margin(t=0, r=0, b=0, l= -0.2, unit="cm")) +
  ggtitle("H) Podocnemis unifilis")

if(make_png!=FALSE){
  lay <- rbind(c(1,2),
               c(3,4),
               c(5,6),
               c(7,8))
  png("inst/ms_res/Fig2.png", width = 7, height = 10, 
      units = 'in', res = 600, type="cairo-png")
  gridExtra::grid.arrange(f1a, f1b, f1c, f1d, f1e, f1f, f1g, f1h, layout_matrix = lay)
  dev.off()
}

listgg <- list(f1a = f1a, f1b = f1b, f1c = f1c, f1d = f1d, 
               f1e = f1e, f1f = f1f, f1g = f1g, f1h = f1h)
return(listgg)

}