#' Expansion Factor Formula for Fixed Area and Variable Radius Plots
#'
#' This function calculates the expansion factor of a tree for
#' fixed area and variable radius plots.
#'
#' For Fixed area plots, enter the portion of an acre which was used
#' as the fixed area. If you used 1/5 of an acre, enter .2.
#'
#' For variable radius plots enter the BAF that was used.
#'
#' This function will distinguish between any fixed radius plot
#' smaller than 1 acre and variable radius plots with any BAF.
#'
#' Ex: Fixed Area - EXP.F(16, .2)
#' Ex: Variable Radius - EXP.F(16, 20)
#'
#'@param DBH Diameter at breast height in inches.
#'@param BAF.Area Enter either the percent of an acre for fixed or BAF for variable.
#'
#'@return Returns the number of trees per acre each measured tree represents.
#'
#'
#'@details
#' This function incorporates both fixed area and variable radius
#' expansion factors in a single function so that multiple data sets using
#' different samplign methods can be combined without confusing expansion factors
#' of measured trees. Make sure all fixed area plots are represented as a percentage
#' in a value <= 1 (.2, .25, .05, etc.) as any value over 1 will be assumed to be
#' Variable Radius.
#'
#'@export

EXP.F <- function(DBH, BAF.Area) {
  if (BAF.Area <= 1){
    EXP.F <- 1/BAF.Area
  } else {
    EXP.F <- (BAF.Area) / (0.005454 * (DBH^2))
  }
  return(EXP.F)
}
