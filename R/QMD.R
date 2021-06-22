#' QMD
#'
#' This function calculates the quadratic mean diameter value of each plot for every tree within your data set.
#'
#'@param DBH Diameter at breast height in cm.
#'@param EXP.F Expansion factor for each tree.
#'
#'@return Returns a vector of the length N with the quadratic mean diameter of each plot for each tree in your data set.
#'
#'@details
#' This function requires that you calculate the Basal Area Per Hectare (BAPH) and Trees Per Hectare (TPH) for every tree
#' in your dataset. Values must be metric and in hectares. These values can be calculated using the BAPH and TPH functions included in this
#' package.
#'
#'@seealso [inventoryfunctions::EXP.F]
#'@author Ryan Smith
#'@family Plot Level Functions
#'
#'@examples
#'
#' TPH <- c(1200, 1300, 2000, 700)
#' BAPH <- c(14, 20, 60, 12)
#' QMD(TPH, BAPH)
#'
#'@export

QMD <- function(DBH, EXPF){

  Temp <- data.frame(DBH, EXPF)
  Temp$QMD <- sqrt(Temp$BAPH/(0.00007854*Temp$EXPF))
  Temp$QMD <- round(Temp$QMD, 2)

  return(Temp$QMD)
}




