#' @title Create and export Figure 3.
#' 
#' @description Generates Figure 3. Needs updating to take results from 
#' "PopProj.R" and "PopScen.R"
#' 
#' @param x Data.frame created by "proj.rivl.R".
#'
#' @return Exports Figure 3 as .pdf file.
#' @import ggplot2
#' @importFrom grDevices dev.off png pdf
#' @importFrom gridExtra grid.arrange
#' @export
#'
#' @examples
#' \dontrun{
#' dfres <- 
#' readRDS(
#' "C:\\Users\\Darren\\Documents\\ms\\unpublished\\2018 Unifilis demography\\analises\\dfpopres.RDS"
#' )
#' resFig3(dfres)
#' }
resFig3 <- function(x){
# make tables
dfpop.res <- x #
selH50 <- which(dfpop.res$Years == max(dfpop.res$Years))
dfpop.res50 <- dfpop.res[selH50, ]

dataF5 <- function(x) {
  # acessible  propKM between 0.05 and .95
  selAc <- which(x$accessible == "Yes" & x$propKM < 1)
  dft <- x[selAc, ]
  # sort by basin, subbasin, increase, propKM, variable
  df5as <- dplyr::arrange(dft, 
                          increase, propKM, BASIN_N, subbasn, variable)
  
  
  #Add missing levels
  df5as$subbasn_In <- rep(df5In$subbasn, (60420/318))
  df5as$variable_In <- rep(df5In$variable, (60420/318))
  df5as$In_prop <- rep(df5In$propKM, (60420/318))
  df5as$distKM_In <- rep(df5In$distKM, (60420/318))
  df5as$adult_females_baseIn <- rep(df5In$adult_females, (60420/318))
  
  df5as$subbasn_baseAC <- rep(df5asBase$subbasn, 10)
  df5as$variable_baseAC <- rep(df5asBase$variable, 10)
  df5as$propKM_baseAC <- rep(df5asBase$propKM, 10)
  df5as$distKM_baseAC <- rep(df5asBase$distKM, 10)
  df5as$adult_females_baseAC <- rep(df5asBase$adult_females, 10)
  df5as$acc_propbaseAC <- df5as$propKM_baseAC + df5as$propKM
  
  df5as$subbasn_baseACBAU <- rep(df5asBaseBAU$subbasn, 10)
  df5as$variable_baseACBAU <- rep(df5asBaseBAU$variable, 10)
  df5as$propKM_baseACBAU <- rep(df5asBaseBAU$propKM, 10)
  df5as$distKM_baseACBAU <- rep(df5asBaseBAU$distKM, 10)
  df5as$adult_females_baseACBAU <- rep(df5asBaseBAU$adult_females, 10)
  df5as$acc_propBAU <- df5as$propKM_baseACBAU + df5as$propKM
  
  # integers for comparison
  df5as$adult_females_total <- trunc(df5as$adult_females_baseIn + 
                                       df5as$adult_females_baseAC + df5as$adult_females)
  df5as$adult_females_current <- trunc((df5as$distKM + 
                                          df5as$distKM_In + df5as$distKM_baseAC) * 10)
  df5as$prop_change <- ((df5as$adult_females_total - df5as$adult_females_current) / 
                          df5as$adult_females_current)
  df5as$prop_change_clean <- ifelse(df5as$prop_change > 1, 1, df5as$prop_change)
  
  # only accessible
  df5as$adult_females_total_acc <- trunc(df5as$adult_females_baseAC + df5as$adult_females)
  df5as$adult_females_current_acc <- trunc((df5as$distKM + df5as$distKM_baseAC) * 10)
  df5as$prop_change_acc <- (df5as$adult_females_total_acc - df5as$adult_females_current_acc) / df5as$adult_females_current_acc
  df5as$prop_change_acc_clean <- ifelse(df5as$prop_change_acc > 1, 1, df5as$prop_change_acc)
  
  # only accessible BAU
  df5as$adult_females_total_accBAU <- trunc(df5as$adult_females_baseACBAU + df5as$adult_females)
  df5as$adult_females_current_accBAU <- trunc((df5as$distKM + df5as$distKM_baseACBAU) * 10)
  df5as$prop_change_accBAU <- (df5as$adult_females_total_accBAU - df5as$adult_females_current_acc) / df5as$adult_females_current_acc
  df5as$prop_change_accBAU_clean <- ifelse(df5as$prop_change_accBAU > 1, 1, df5as$prop_change_accBAU)
  
  df5as
}

df5hs <- plyr::ddply(dfpop.res50, .(species, type), .fun = dataF5)
levels(df5hs$variable) <- c("All", "Not protected", "Protected", 
                            "Indigenous", "Strict", "Use")
levels(df5hs$type) <- c("No hunt", "Hunt 2.5%", "Hunt 10%",
                        "Hunt 25%", "Hunt 50%")

# Fig 3 plot = How much?
mycol <- c("#FF00FF", "#CC33CC", "#FF00CC", 
           "#FFFF33", "#FF9933", "#CC6600", "#993300", 
           "#00FF00", "#339900", "#336600")

selKM1 <- which(df5hs$variable %in% c("All"))
# 173 mm (7inch) width, http://onlinelibrary.wiley.com/journal/10.1111/(ISSN)1755-263X/homepage/ForAuthors.html
#saveRDS(df5hs[selKM1, ], "Fig2dat.RDS")
#dff3 <- readRDS("C:\\Users\\Darren\\Documents\\ms\\unpublished\\2018 Unifilis demography\\analises\\Fig2dat.RDS")

dff3 <- df5hs[selKM1, ]

pdf("Fig3.pdf", width= 7, height = 3.5, useDingbats = FALSE)
ggplot2::ggplot(dff3, aes(propKM, prop_change_clean, color = increase)) +
  geom_hline(yintercept = 0) +
  geom_jitter(width = 0.1, height = 0.1, alpha=0.1) +
  stat_smooth(se=FALSE) +
  scale_x_continuous(breaks = c(0,0.5,1)) +
  #facet_grid(variable~type, labeller = label_wrap_gen(width=19)) +
  facet_wrap(~type, nrow=1, labeller = label_wrap_gen(width=19)) +
  ylab("Relative population change\n(50 year projection)") + 
  xlab("Scenario cover (Proportion of catchment river length)") + 
  scale_color_manual(name="Hatchling\nGraduation", values = mycol)
dev.off()
}