#' @title Create and export Figure 6.
#' 
#' @description Generates Figure 6.
#'
#' @param x Data.frame with results from "resTabDemog.R".
#' @param pBasin Input file with basin polygons.
#'
#' @return Exports Figure 6 as .png file.
#' 
#' @import ggplot2
#' @importFrom grDevices dev.off png pdf
#' @importFrom gridExtra grid.arrange
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get results from 3 scenarios
#' load("~/ms/unpublished/2018 Unifilis demography/analises/testl.RData")
#' library(magrittr)
#' source("R/PopScen.R")
#' lscen <- PopScen(dflup)
#' library(plyr)
#' source("R/resTabDemog.R")
#' lsum <- resTabDemog(ldemog = lscen, make_html = FALSE)
#' dfsum <- lsum$dfsum.bc
#' B <- system.file("shape/amazon_orinoco.shp", package="cmartr")
#' resFig5(dfsum, pBasin = B)
#' }
#' 
resFig6 <- function(x, pBasin = NA){

  dfsum <- x
dfsum$BAU_flag_50 <- ifelse(dfsum$BAU_female_change < -49.999, 1,0)
dfsum$BAU_flag_30 <- ifelse(dfsum$BAU_female_change < -29.999, 1,0)
dfsum$SP_flag_50 <- ifelse(dfsum$SP_female_change < -49.999, 1,0)
dfsum$SP_flag_30 <- ifelse(dfsum$SP_female_change < -29.999, 1,0)
dfsum$CM_flag_50 <- ifelse(dfsum$CM_female_change < -49.999, 1,0)
dfsum$CM_flag_30 <- ifelse(dfsum$CM_female_change < -29.999, 1,0) 

amsub <- c("Abacaxis", "Amazon floodplain", "Putumayo", "Japurá - Caquetá", "Javari",
           "Juruá", "Madeira", "Marañón", "Curuá-una", "Guama", "Jari", "Jutai",
           "Madeirinha", "Manacapuru", "Nanay", "Pacajá", "Piorini", "Tefe", 
           "Uatumá", "Napo", "Negro", "Purus", "Tapajós", "Tocantins",
           "Trombetas", "Ucayali", "Xingu")
dfsum$subbasinT <- c(amsub, dfsum$subbasin[28:52])

# Subbasin cover polygons
sfclean <- cmartr::prepBasin(Bain = pBasin)
sfclean2 <- merge(sfclean, dfsum, by = c("subbasinT"), all.x = TRUE)

# use scale_fill_gradientn for finer control of fill
xbr = c(-80, -70, -60, -50)
ybr = c(-20, -10, -0, 10)
fbau <- ggplot2::ggplot(sfclean2) +
  geom_sf(aes(fill = BAU_female_change)) +
  scale_fill_gradient2("%\nchange") +
  scale_x_continuous(breaks = xbr) +
  scale_y_continuous(breaks = ybr) +
  theme(legend.margin=margin(t=0, r=0, b=0, l= -0.2, unit="cm")) +
  ggtitle("A) Business as usual")

fsp <- ggplot2::ggplot(sfclean2) +
  geom_sf(aes(fill = SP_female_change)) +
  scale_fill_gradient2("%\nchange") +
  scale_x_continuous(breaks = xbr) +
  scale_y_continuous(breaks = ybr) +
  theme(legend.margin=margin(t=0, r=0, b=0, l= -0.2, unit="cm")) +
  ggtitle("B) Protection")

fcm <- ggplot2::ggplot(sfclean2) +
  geom_sf(aes(fill = CM_female_change)) +
  scale_fill_gradient2("%\nchange") +
  scale_x_continuous(breaks = xbr) +
  scale_y_continuous(breaks = ybr) +
  theme(legend.margin=margin(t=0, r=0, b=0, l= -0.2, unit="cm")) +
  ggtitle("C) Community management")

# map of BAU loss for IUCN
# It's recommended to use a named vector
cols <- c("0" = "darkgrey", "1" = "red", "NA" = "darkgrey")
f50BAU <- ggplot2::ggplot(sfclean2) +
  geom_sf(aes(fill = factor(BAU_flag_50))) +
  scale_fill_manual("",
                    values = cols,
                    breaks = c("0", "1", "NA"),
                    labels = c("No", "Loss\n(50%)", "NA")
  ) +
  scale_x_continuous(breaks = xbr) +
  scale_y_continuous(breaks = ybr) +
  theme(legend.margin=margin(t=0, r=0, b=0, l= -0.2, unit="cm")) +
  ggtitle("D) Catchments with 50% loss")

f50SP <- ggplot2::ggplot(sfclean2) +
  geom_sf(aes(fill = factor(SP_flag_50))) +
  scale_fill_manual("",
                    values = cols,
                    breaks = c("0", "1", "NA"),
                    labels = c("No", "Loss\n(50%)", "NA")
  ) +
  scale_x_continuous(breaks = xbr) +
  scale_y_continuous(breaks = ybr) +
  theme(legend.margin=margin(t=0, r=0, b=0, l= -0.2, unit="cm")) +
  ggtitle("E) Catchments with 50% loss")

f50CM <- ggplot2::ggplot(sfclean2) +
  geom_sf(aes(fill = factor(CM_flag_50))) +
  scale_fill_manual("",
                    values = cols,
                    breaks = c("0", "1", "NA"),
                    labels = c("No", "Loss\n(50%)", "NA")
  ) +
  scale_x_continuous(breaks = xbr) +
  scale_y_continuous(breaks = ybr) +
  theme(legend.margin=margin(t=0, r=0, b=0, l= -0.2, unit="cm")) +
  ggtitle("F) Catchments with 50% loss")

f30BAU <- ggplot2::ggplot(sfclean2) +
  geom_sf(aes(fill = factor(BAU_flag_30))) +
  scale_fill_manual("",
                    values = cols,
                    breaks = c("0", "1", "NA"),
                    labels = c("No", "Loss\n(30%)", "NA")
  ) +
  scale_x_continuous(breaks = xbr) +
  scale_y_continuous(breaks = ybr) +
  theme(legend.margin=margin(t=0, r=0, b=0, l= -0.2, unit="cm")) +
  ggtitle("G) Catchments with 30% loss")

f30SP <- ggplot2::ggplot(sfclean2) +
  geom_sf(aes(fill = factor(SP_flag_30))) +
  scale_fill_manual("",
                    values = cols,
                    breaks = c("0", "1", "NA"),
                    labels = c("No", "Loss\n(30%)", "NA")
  ) +
  scale_x_continuous(breaks = xbr) +
  scale_y_continuous(breaks = ybr) +
  theme(legend.margin=margin(t=0, r=0, b=0, l= -0.2, unit="cm")) +
  ggtitle("H) Catchments with 30% loss")

f30CM <- ggplot2::ggplot(sfclean2) +
  geom_sf(aes(fill = factor(CM_flag_30))) +
  scale_fill_manual("",
                    values = cols,
                    breaks = c("0", "1", "NA"),
                    labels = c("No", "Loss\n(30%)", "NA")
  ) +
  scale_x_continuous(breaks = xbr) +
  scale_y_continuous(breaks = ybr) +
  theme(legend.margin=margin(t=0, r=0, b=0, l= -0.2, unit="cm")) +
  ggtitle("I) Catchments with 30% loss")

lay <- rbind(c(1,2,3),
             c(4,5,6),
             c(7,8,9))
#windows(width=14, height=10)
#gridExtra::grid.arrange(fbau, fsp, fcm, 
#                        f50BAU, f50SP, f50CM, 
#                        f30BAU, f30SP, f30CM, layout_matrix = lay)

png("inst/ms_res/f6.png", width = 25/2.5, height = 18/2.5, 
    units = 'in', res = 600, type="cairo-png")
gridExtra::grid.arrange(fbau, fsp, fcm, 
                        f50BAU, f50SP, f50CM, 
                        f30BAU, f30SP, f30CM, layout_matrix = lay)
dev.off()
}