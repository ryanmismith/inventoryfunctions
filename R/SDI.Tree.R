#' Single Tree SDI Function
#'
#' This function computes the SDI value of a single tree with
#' no regard for its expansion factor or the influence of the tree
#' on a plot or stand level.
#'
#' This internal function is used by the SDI.Plot function to calculate Stand Density Index values
#' using the summation method.
#'
#'@keywords internal
#'
#'@param DBH Diameter at Breast Height
#'
#'@examples
#'
#' SDI(25)
#'
#'@references
#'Woodall, C. W., Miles, P. D., & Vissage, J. S. (2005). Determining maximum stand density index
#'in mixed species stands for strategic-scale stocking assessments.
#'Forest Ecology and Management, 216(1–3), 367–377. https://doi.org/10.1016/j.foreco.2005.05.050
#'@export

SDI.Tree <- function(DBH){
  tree <- (DBH/25.4)^1.605
  return(tree)
}
