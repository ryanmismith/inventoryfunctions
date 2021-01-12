#' Basal Area Formula
#'
#' This function calculates the basal area of a tree.
#' The basal area is the cross sectional area of the tree.
#'
#'
#'@param DBH Diameter at breast height in inches
#'
#'@return BA - Basal Area in sq ft
#'
#'@export


BA <- function(DBH) {
  BA <- .005454 * DBH^2
  return(BA)
}
