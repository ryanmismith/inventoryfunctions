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
#' smaller than 1 hectare and variable radius plots with any BAF.
#'
#'@examples
#' # Fixed Area - EXP.F(16, .2)
#' # Variable Radius - EXP.F(16, 20)
#'
#'@param DBH Diameter at breast height in cm.
#'@param BAF.Area Enter either the percent of an acre for fixed or BAF for variable.
#'
#'@return Returns the number of trees per hectare each measured tree represents.
#'
#'@details
#' Make sure all fixed area plots are represented as a percentage
#' in a value <= 1 (ex: .2, .25, .05, etc.) as any value over 1 will be assumed to be
#' Variable Radius.
#'
#' ###
#' This function allows for subplots, fixed area plots, and variable radius plots to be included
#' in the same column or vector. You may want to create a separate factor variable that identifies fixed area,
#' variable radius, regeneration plots or subplots for your analysis, but this function will allow you to have
#' the expansion factor for each tree regardless of sampling method used. This has been designed with the recognition
#' that cruising may often include small fixed area subplots for sampling regeneration while using variable
#' radius plots for trees that are > 10cm.
#'
#'@export

EXP.F <- function(DBH, BAF.Area) {
  if (BAF.Area <= 1){
    X <- 1/BAF.Area
  } else {
    X <- (BAF.Area) / (0.00007854 * (DBH^2))
  }
   X <- round(X, 2)
  return(X)
}
