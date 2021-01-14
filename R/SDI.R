#' Single Tree SDI Function
#'
#' This function computes the SDI value of a single tree with
#' no regard for its expansion factor or the influence of the tree
#' on a plot or stand level. See Details.
#'
#' This function is meant to be applied on a plot or stand level using
#' the included EXP.F function and dplyr. See the included full inventory script
#' for its application on a stand and plot level.
#'
#' For example: an effective use of this function would be:
#'
#' Tree %>% mutate(SDI = SDI(DBH)*EXP.F)
#'
#' @param DBH Diameter at Breast Height
#'
#' @export

SDI <- function(DBH){
  SDI <- (DBH/25.4)^1.605
  return(SDI)
}
