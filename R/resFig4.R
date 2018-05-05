#' Title
#' @title Create Figure 4
#' 
#' @description Generates Figure 4 country figures. Returns list with ggplot2 figures. 
#' Optionally exports Figure 4 as png.
#' 
#' @param listsf List of five sf objects created by prepTabcover.R
#' @param dfsumC Dataframe with country population summary from resTabDemog.R
#' @param dfsumB Dataframe with basin population summary.
#' @param make_png Logical (TRUE/FALSE). Generate png version of Fig 4.
#'
#' @return ggplot2 figures and optionally writes png file.
#' @import ggplot2
#' @importFrom grDevices dev.off png
#' @importFrom gridExtra grid.arrange
#' @export
#'
#' @examples
#' \dontrun{
#' lsf <-  prepTabcover(pBasin = B, pBasinSp = Bsp, 
#' pBasinC = BC, riv = rin, make_shape = FALSE)
#' popc <- "~/ms/unpublished/2018 Unifilis demography/analises/dfpop.RDS"
#' demogC <- cmartr::demogCountry(popc = popc, rlc = lt$rlc)
#' save(demogC, file = "inst/ms_res/dfdemogC.RData")
#' dfsum <- cmartr::resTabDemog(demogC = demogC, make_html = FALSE)
#' save(dfsum, file = "inst/ms_res/dfsumC.RData")
#' fig4C <- resFig4(listsf = lsf, dfsumC = dfsum, dfsumB = NA)
#' }
resFig4 <- function(listsf = NA, dfsumC = NA, dfsumB = NA, make_png = FALSE){
  
  #join with country intersected with basin
  pac <- merge(st_transform(listsf$basinc, crs = 4326), dfsumC)

  sfcoun<- rnaturalearth::ne_countries(continent = "South America", 
                                       type = 'map_units', returnclass = "sf")
  sfcoun <- st_sf(a= rep(1,14), geom=st_geometry(sfcoun))
  sfcounD <- sf::st_union(rnaturalearth::ne_countries(continent = "South America", 
                                                      type = 'map_units', returnclass = "sf"))
  sfcounD <- st_sf(a=1, geom=st_geometry(sfcounD))
 
  # female change 
  f4cbau <- ggplot(pac) +
    geom_sf(data = sfcounD, size=1.7, fill="grey95") +
    geom_sf(aes(fill = BAU_female_change))  +
    scale_fill_gradient2("%\nchange") +
    geom_sf(data = sfcoun, size = 1, color="black", fill=NA) +
    coord_sf(xlim = c(-80, -43), ylim = c(-20, 10)) +
    theme_bw() +
    theme(legend.margin=margin(t=0, r=0, b=0, l= -0.2, unit="cm")) +
    ggtitle("J) Business as usual")
  
  f4csp <- ggplot(pac) +
    geom_sf(data = sfcounD, size=1.7, fill="grey95") +
    geom_sf(aes(fill = SP_female_change))  +
    scale_fill_gradient2("%\nchange") +
    geom_sf(data = sfcoun, size = 1, color="black", fill=NA) +
    coord_sf(xlim = c(-80, -43), ylim = c(-20, 10)) +
    theme_bw() +
    theme(legend.margin=margin(t=0, r=0, b=0, l= -0.2, unit="cm")) +
    ggtitle("K) Strict protection")
  
  f4ccm <- ggplot(pac) +
    geom_sf(data = sfcounD, size=1.7, fill="grey95") +
    geom_sf(aes(fill = CM_female_change))  +
    scale_fill_gradient2("%\nchange") +
    geom_sf(data = sfcoun, size = 1, color="black", fill=NA) +
    coord_sf(xlim = c(-80, -43), ylim = c(-20, 10)) +
    theme_bw() +
    theme(legend.margin=margin(t=0, r=0, b=0, l= -0.2, unit="cm")) +
    ggtitle("L) Community management")
  
  # IUCN, 50% loss
  # It's recommended to use a named vector
  cols <- c("0" = "darkgrey", "1" = "red", "NA" = "darkgrey")
  f4c50BAU <- ggplot(pac) +
    geom_sf(data = sfcounD, size=1.7, fill="grey95") +
    geom_sf(aes(fill = factor(BAU_flag_50))) +
    scale_fill_manual("",
                      values = cols,
                      breaks = c("0", "1", "NA"),
                      labels = c("No", "Loss\n(50%)", "NA")
    ) +
    geom_sf(data = sfcoun, size = 1, color="black", fill=NA) +
    coord_sf(xlim = c(-80, -43), ylim = c(-20, 10)) +
    theme_bw() +
    theme(legend.margin=margin(t=0, r=0, b=0, l= -0.2, unit="cm")) +
    ggtitle("M) Countries with 50% loss")
  
  f4c50SP <- ggplot(pac) +
    geom_sf(data = sfcounD, size=1.7, fill="grey95") +
    geom_sf(aes(fill = factor(SP_flag_50))) +
    scale_fill_manual("",
                      values = cols,
                      breaks = c("0", "1", "NA"),
                      labels = c("No", "Loss\n(50%)", "NA")
    ) +
    geom_sf(data = sfcoun, size = 1, color="black", fill=NA) +
    coord_sf(xlim = c(-80, -43), ylim = c(-20, 10)) +
    theme_bw() +
    theme(legend.margin=margin(t=0, r=0, b=0, l= -0.2, unit="cm")) +
    ggtitle("N) Countries with 50% loss")
  
  f4c50CM <- ggplot(pac) +
    geom_sf(data = sfcounD, size=1.7, fill="grey95") +
    geom_sf(aes(fill = factor(CM_flag_50))) +
    scale_fill_manual("",
                      values = cols,
                      breaks = c("0", "1", "NA"),
                      labels = c("No", "Loss\n(50%)", "NA")
    ) +
    geom_sf(data = sfcoun, size = 1, color="black", fill=NA) +
    coord_sf(xlim = c(-80, -43), ylim = c(-20, 10)) +
    theme_bw() +
    theme(legend.margin=margin(t=0, r=0, b=0, l= -0.2, unit="cm")) +
    ggtitle("O) Countries with 50% loss")
  
  # IUCN, 30% loss
  f4c30BAU <- ggplot(pac) +
    geom_sf(data = sfcounD, size=1.7, fill="grey95") +
    geom_sf(aes(fill = factor(BAU_flag_30))) +
    scale_fill_manual("",
                      values = cols,
                      breaks = c("0", "1", "NA"),
                      labels = c("No", "Loss\n(30%)", "NA")
    ) +
    geom_sf(data = sfcoun, size = 1, color="black", fill=NA) +
    coord_sf(xlim = c(-80, -43), ylim = c(-20, 10)) +
    theme_bw() +
    theme(legend.margin=margin(t=0, r=0, b=0, l= -0.2, unit="cm")) +
    ggtitle("M) Countries with 30% loss")
  
  f4c30SP <- ggplot(pac) +
    geom_sf(data = sfcounD, size=1.7, fill="grey95") +
    geom_sf(aes(fill = factor(SP_flag_30))) +
    scale_fill_manual("",
                      values = cols,
                      breaks = c("0", "1", "NA"),
                      labels = c("No", "Loss\n(30%)", "NA")
    ) +
    geom_sf(data = sfcoun, size = 1, color="black", fill=NA) +
    coord_sf(xlim = c(-80, -43), ylim = c(-20, 10)) +
    theme_bw() +
    theme(legend.margin=margin(t=0, r=0, b=0, l= -0.2, unit="cm")) +
    ggtitle("N) Countries with 30% loss")
  
  f4c30CM <- ggplot(pac) +
    geom_sf(data = sfcounD, size=1.7, fill="grey95") +
    geom_sf(aes(fill = factor(CM_flag_30))) +
    scale_fill_manual("",
                      values = cols,
                      breaks = c("0", "1", "NA"),
                      labels = c("No", "Loss\n(30%)", "NA")
    ) +
    geom_sf(data = sfcoun, size = 1, color="black", fill=NA) +
    coord_sf(xlim = c(-80, -43), ylim = c(-20, 10)) +
    theme_bw() +
    theme(legend.margin=margin(t=0, r=0, b=0, l= -0.2, unit="cm")) +
    ggtitle("O) Countries with 30% loss")
  
  if(make_png!=FALSE){
    lay <- rbind(c(1,2,3),
                 c(4,5,6),
                 c(7,8,9))
    png("inst/ms_res/f4c.png", width = 8.5, height = 6, 
        units = 'in', res = 600, type="cairo-png")
    gridExtra::grid.arrange(f4cbau, f4csp, f4ccm, 
                            f4c50BAU, f4c50SP, f4c50CM, 
                            f4c30BAU, f4c30SP, f4c30CM, layout_matrix = lay)
    dev.off()
  }
  
  listgg <- list(f4cbau = f4cbau, f4csp = f4csp, f4ccm = f4ccm, 
                 f4c50BAU = f4c50BAU, f4c50SP = f4c50SP, f4c50CM = f4c50CM, 
                 f4c30BAU = f4c30BAU, f4c30SP = f4c30SP, f4c30CM = f4c30CM)
  return(listgg)
  
  }