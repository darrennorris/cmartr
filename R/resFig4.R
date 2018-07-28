#' @title Create and export Figure 4.
#' 
#' @description Generates Figure 4. Needs updating to take results from 
#' "PopProj.R" and "PopScen.R"
#'
#' @param x Data.frame created by "proj.rivl.R".
#'
#' @return Exports Figure 4 as .png file.
#' @import ggplot2
#' @importFrom grDevices dev.off png pdf
#' @importFrom gridExtra grid.arrange
#' @export
#'
#' @examples
#' \dontrun{
#' dfpop.res <- 
#' readRDS(
#' "C:\\Users\\Darren\\Documents\\ms\\unpublished\\2018 Unifilis demography\\analises\\dfpopres.RDS"
#' )
#' resFig4(dfres)
#' }
resFig4 <- function(x){
  ## Now how long ?
sel50All <- which(dfpop.res$change50_flag==1 & 
                    dfpop.res$accessible=="Yes",
                  dfpop.res$variable=="tot_km")
mycoln <- c("type" , "increase" , "BASIN_N" ,"subbasn", 
            "accessible", "propKM",  "distKM", "lambda" )           

dfpop.resY <- plyr::ddply(dfpop.res[sel50All, ], (mycoln), 
                          summarise, 
                          Years_50 = min(Years))
dfpop.resY$Years_50c <- ifelse(as.numeric(dfpop.resY$lambda) < 1, 
                               dfpop.resY$Years_50 * -1, 
                               dfpop.resY$Years_50)

levels(dfpop.resY$type) <- c("No hunt", "Hunt 2.5%", "Hunt 10%",
                             "Hunt 25%", "Hunt 50%")
selP1 <- which(dfpop.resY$propKM==1)

f4 <- ggplot2::ggplot(dfpop.resY[selP1, ], aes(x = increase, y = Years_50c, 
                                               color=lambda)) +
  geom_jitter(alpha=0.3) +
  scale_color_gradientn("lambda", 
                        colours = c("darkred","tomato1", 
                                    "lightblue","darkblue"), 
                        values = c(0, 0.76,0.77, 1)) +
  scale_y_continuous("Years to change", limits = c(-50, 50), 
                     labels = c("50", "25", "0", "25", "50")) +
  scale_x_discrete("Hatchling graduation", breaks = c(0, 0.3, 0.6, 0.9)) +
  facet_wrap(~type, nrow=1, labeller = label_wrap_gen(width=19)) 

png("inst/ms_res/Fig4.png", width = 7, height = 3.5, 
    units = 'in', res = 600, type="cairo-png")
f4
dev.off()
}