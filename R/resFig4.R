#' @title Create and export Figure 4.
#' 
#' @description Generates Figure 4.  
#' 
#' @param x Data.frame created by "resFig4prep.R".
#'
#' @return Exports Figure 4 as .png and .pdf files.
#' @import ggplot2
#' @importFrom grDevices dev.off png pdf
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # From "resFig4prep.R".
#' f.base <- which(dflup$hunt==0 & dflup$increase==0.2)
#' dflupin <- dflup[-f.base, ]
#' 
#' # run function takes 40 minutes approx.
#' fig4prep <- ddply(dflupin, .(hunt, increase), .fun = resFig3prep)
#' # tidy for plotting
#' fig4prep$hunt <- factor(fig4prep$hunt) 
#' levels(fig4prep$hunt) <- c("No hunt", "Hunt 2.5%", "Hunt 10%",
#'                         "Hunt 25%", "Hunt 50%")
#' resFig4(fig4prep)
#' }
resFig4 <- function(x){
  # custom colours
  mycol <- c("#FF00FF", "#CC33CC", "#FF00CC", 
             "#FFFF33", "#FF9933", "#CC6600", "#993300", 
             "#00FF00", "#339900", "#336600")
  #plot
  pdf("inst/ms_res/Fig4.pdf", width= 7, height = 3.5, useDingbats = FALSE)
  ggplot2::ggplot(x, aes(prop_km.y, prop_change_clean, 
                              color = factor(increase))) +
    geom_hline(yintercept = 0) +
    geom_jitter(width = 0.1, height = 0.1, alpha=0.1, size = 0.6) +
    stat_smooth(se=FALSE) +
    scale_x_continuous(breaks = c(0,0.5,1)) +
    facet_wrap(~ hunt, nrow=1, labeller = label_wrap_gen(width=19))   +
    ylab("Relative population change\n(50 year projection)") + 
    xlab("Scenario cover (Proportion of catchment river length)") + 
    scale_color_manual(name="Hatchling\nGraduation", values = mycol) +
    coord_cartesian(xlim = c(-0.15, 1.15), ylim = c(-1.08, 1.02), expand = FALSE)
dev.off()

png("inst/ms_res/Fig4.png", width = 7, height = 3.5, 
    units = 'in', res = 600, type="cairo-png")
ggplot2::ggplot(x, aes(prop_km.y, prop_change_clean, 
                       color = factor(increase))) +
  geom_hline(yintercept = 0) +
  geom_jitter(width = 0.1, height = 0.1, alpha=0.1, size = 0.6) +
  stat_smooth(se=FALSE) +
  scale_x_continuous(breaks = c(0,0.5,1)) +
  facet_wrap(~ hunt, nrow=1, labeller = label_wrap_gen(width=19))   +
  ylab("Relative population change\n(50 year projection)") + 
  xlab("Scenario cover (Proportion of catchment river length)") + 
  scale_color_manual(name="Hatchling\nGraduation", values = mycol) +
  coord_cartesian(xlim = c(-0.15, 1.15), ylim = c(-1.08, 1.02), expand = FALSE)
dev.off()
}