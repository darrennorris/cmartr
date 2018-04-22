#' @title Create Figure 1
#'
#' @description Generates Figgure 1. Returns list with ggplot2 figures. 
#' Optionally exports Figure 1 as png.
#' 
#' @param listsf List of five sf objects created by prepTabcover.R
#' @param listTab List of data.frames created by resTab.R
#' @param pBasin Subbasin cover polygons.
#' @param pBasinSp Extent polygon covering 
#' subbasins in species range of occurance.
#' @param make_png Logical (TRUE/FALSE). Generate png version of Fig 1.
#'
#' @return List of plots created by ggplot2. Optionally exports as png.
#' @import ggplot2
#' @importFrom grDevices dev.off png
#' @importFrom gridExtra grid.arrange
#' @importFrom stats na.omit
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
#' # This function needs 2 shapefiles as listed below.
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
#
#' lt <- resTab(listsf = lsf)
#' 
#' lfig1 <- resFig1(listsf = lsf, listTab = lt, pBasin = B, 
#'   pBasinSp = Bsp, make_png = FALSE)
#' 
#' }
resFig1 <- function(listsf = NA, listTab = NA, pBasin = NA, 
                    pBasinSp = NA, make_png = FALSE){

# load data
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

f1b <- NA

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
                        values = c(0,0.5,0.501,1)) +
  coord_sf(xlim = c(-80, -43), ylim = c(-20, 10)) +
  theme_bw() +
  theme(legend.margin=margin(t=0, r=0, b=0, l= -0.2, unit="cm")) +
  ggtitle("C) Human accessibility cover")

# Acess by country
f1f <- ggplot(pac) +
  geom_sf(data = sfcounD, size=1.7) +
  geom_sf(data = sfcatch2, size = 1.1, color="black", fill=NA) +
  geom_sf(data = sfcatch2, size=1,color="yellow", fill=NA, lty=2) +
  geom_sf(aes(fill = prop_accC * 100), size = 0.3) +
  scale_fill_gradientn("% \n< 49 km", 
                       colours = c("darkblue", "lightblue", "tomato1", "darkred"), 
                       values = c(0,0.5,0.501,1)) +
  geom_sf(data = sfcoun, size = 1, color="black", fill=NA) +
  coord_sf(xlim = c(-80, -43), ylim = c(-20, 10)) +
  theme_bw() +
  theme(legend.margin=margin(t=0, r=0, b=0, l= -0.2, unit="cm")) +
  ggtitle("D) Protected area cover")

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
lay <- rbind(c(1,NA),
             c(3,4),
             c(5,6),
             c(7,8))
png("f1.png", width = 7, height = 10, 
    units = 'in', res = 600, type="cairo-png")
gridExtra::grid.arrange(f1a, f1c, f1d, f1e, f1f, f1g, f1h, layout_matrix = lay)
dev.off()
}

listgg <- list(f1a = f1a, f1b = f1b, f1c = f1c, f1d = f1d, 
               f1e = f1e, f1f = f1f, f1g = f1g, f1h = f1h)
return(listgg)

}