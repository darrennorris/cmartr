
#' Title 
#' @title Create and export Figure 3.
#' 
#' @description Generates Figure 3. 
#'
#' @param x Data.frame from cmartr::PopParam
#'
#' @return pdf of Figure 3.
#' @import ggplot2
#' @importFrom grDevices dev.off png pdf
#' @importFrom gridExtra grid.arrange
#' @export
#'
#' @examples
#' \dontrun{
#' dfpop <- cmartr::PopParam(species = "Podocnemis unifilis", 
#' make_rds = FALSE)
#' resFig3(dfpop)
#' }
resFig3 <- function(x){
  dfpop <- x
  #1) apply projection function to each row, 
  # result is data frame with projections for each row of "dfpop"
  mypop <- function(x){
    library(popdemo)
    library(popbio)
    vpop <- unlist(x[ ,4:19])
    class(vpop)
    tracaja <- matrix(vpop, byrow = TRUE, ncol=4)
    dimnames(tracaja) <- list(c("a", "b", "c", "d"),
                              c(1,2,3,4))
    #numeric vector of individuals at different time stage
    tracaja_n <- c(11100, 4000, 2000, 1000) 
    # project PPM 
    pr_tracaja <- project(tracaja, vector=tracaja_n, time=50)
    
    # data for plotting
    len <- length(pr_tracaja)
    Time.intervals <- 0:(len - 1)
    eggs <- pr_tracaja * (popbio::stable.stage(tracaja)[1])
    eju <- pr_tracaja * (popbio::stable.stage(tracaja)[2])
    lju <- pr_tracaja * (popbio::stable.stage(tracaja)[3])
    ad.fe <- pr_tracaja * (popbio::stable.stage(tracaja)[4])
    plambda = popbio::lambda(tracaja)
    
    # make dataframe 
    dfout <- data.frame(lambda = plambda,
                        Years = Time.intervals, Individuals = pr_tracaja,
                        ss_egghatchling = popbio::stable.stage(tracaja)[1],
                        ss_earlyjuven = popbio::stable.stage(tracaja)[2],
                        ss_latejuven = popbio::stable.stage(tracaja)[3],
                        ss_adultfemale = popbio::stable.stage(tracaja)[4],
                        egghatch = eggs,
                        early_juven = eju,
                        late_juven = lju,
                        adult_females = ad.fe
    )
    
  }
  library(plyr)
  dfgpop <- ddply(dfpop, .(species, type, increase), mypop)
  
  #2) tidy
  dfgpop$Individuals <- as.numeric(dfgpop$Individuals)
  dfgpop$adult_females <- as.numeric(dfgpop$adult_females)
  dfgpop$type <- factor(dfgpop$type)
  levels(dfgpop$type) <- c("No hunt", "Hunt 2.5%", "Hunt 10%", "Hunt 25%", "Hunt 50%")
  
  #3)plot
  mycol <- c("#FF00FF", "#CC33CC", "#FF00CC", 
             "#FFFF33", "#FF9933", "#CC6600", "#993300", 
             "#00FF00", "#339900", "#336600")
  f2.1 <- ggplot(dfgpop, aes(Years, Individuals, 
                             color = factor(increase))) +
    stat_smooth(se=FALSE) +
    scale_y_continuous(limits = c(0, 50000))+
    coord_cartesian(ylim = c(0,40000))+
    facet_wrap(~type, nrow = 1, labeller = label_wrap_gen(width=19)) +
    scale_color_manual(name="Hatchling\nGraduation", values = mycol)
  
  f2.2 <- ggplot(dfgpop, aes(Years, adult_females, 
                             color = factor(increase))) +
    stat_smooth(se=FALSE) +
    scale_y_continuous(limits = c(0, 50000))+
    coord_cartesian(ylim = c(0,10000))+
    ylab("Individuals (adult females)") +
    facet_wrap(~type, nrow = 1, labeller = label_wrap_gen(width=19))+
    scale_color_manual(name="Hatchling\nGraduation", values = mycol)
  
  pdf(file="fig3new.pdf",width=7, height=6, useDingbats = FALSE)
  gridExtra::grid.arrange(f2.1, f2.2, ncol = 1)
  dev.off()  
 
}