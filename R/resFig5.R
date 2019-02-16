#' @title Create and export Figure 5.
#' 
#' @description Generates Figure 5. Needs updating to take results from 
#' "PopProj.R" and "PopScen.R"
#'
#' @param x Data.frame created by "proj.rivl.R".
#' @param make_col Logical. If TRUE then a colour .png figure will be produced. 
#' Default is grey scale.
#'
#' @return Exports Figure 5 as .png file.
#' @import ggplot2
#' @importFrom grDevices dev.off png pdf
#' @importFrom gridExtra grid.arrange
#' @export
#'
#' @examples
#' \dontrun{
#' # big file needs memory
#' memory.limit(12000)
#' dfpop.res <- 
#' readRDS(
#' "C:\\Users\\Darren\\Documents\\ms\\2019 Unifilis demography\\analises\\dfpopres.RDS"
#' )
#' resFig5(dfpop.res)
#' }
resFig5 <- function(x, make_col = FALSE){
  ## Now how long ?
  # select data
sel50All <- which(x$change50_flag==1 & 
                    x$accessible=="Yes" &
                  x$variable=="tot_km" & 
                    x$propKM==1)
mycoln <- c("type" , "increase" , "BASIN_N" ,"subbasn", 
            "distKM", "lambda" )           
# summarise
dfpop.resY <- plyr::ddply(x[sel50All, ], (mycoln), 
                          summarise, 
                          Years_50 = min(Years))
# tidy for plotting
dfpop.resY$Years_50c <- ifelse(as.numeric(dfpop.resY$lambda) < 1, 
                               dfpop.resY$Years_50 * -1, 
                               dfpop.resY$Years_50)

levels(dfpop.resY$type) <- c("No hunt", "Hunt 2.5%", "Hunt 10%",
                             "Hunt 25%", "Hunt 50%")

if(make_col!=FALSE){
  f5col <- ggplot2::ggplot(dfpop.resY, aes(x = increase, y = Years_50c, 
                                           fill=lambda)) +
    geom_jitter(alpha=0.3, color = "black", shape = 21, size = 2.5) +
    scale_fill_gradientn("lambda", 
                         colours = c("darkred","tomato1", 
                                     "lightblue","darkblue"), 
                         values = c(0, 0.76,0.77, 1)) +
    scale_y_continuous("Years to change", limits = c(-50, 50), 
                       labels = c("50", "25", "0", "25", "50")) +
    scale_x_discrete("Hatchling graduation", breaks = c(0, 0.3, 0.6, 0.9)) +
    facet_wrap(~type, nrow=1, labeller = label_wrap_gen(width=19)) 
  png("inst/ms_res/Fig5.png", width = 7, height = 3.5, 
      units = 'in', res = 600, type="cairo-png")
  f5col
  dev.off()
}else{
  # grouped subset for visual clarity in greyscale
  dt2 <- plyr::ddply(dfpop.resY, 
                     c("type" , "increase" , 
                       "BASIN_N" , "lambda" ) , 
                     summarise, 
                     Years_50 = mean(Years_50c))
  
  f5bw <- ggplot2::ggplot(dt2, aes(x = increase, y = Years_50, 
                                   fill=lambda)) +
    geom_jitter(color = "black", shape=21, size=2.5) +
    scale_fill_gradientn("lambda", 
                         colours = c("black","grey90", 
                                     "grey95","white"), 
                         values = c(0, 0.76,0.77, 1)) +
    scale_y_continuous("Years to change", limits = c(-50, 50), 
                       labels = c("50", "25", "0", "25", "50")) +
    scale_x_discrete("Hatchling graduation", breaks = c(0, 0.3, 0.6, 0.9)) +
    facet_wrap(~type, nrow=1, labeller = label_wrap_gen(width=19)) +
    theme_bw()
  png("inst/ms_res/Fig5bw.png", width = 7, height = 3.5, 
      units = 'in', res = 600, type="cairo-png")
  f5bw
  dev.off()
}
}