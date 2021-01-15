#' Basal Area Formula
#'
#' This function calculates the basal area of a tree.
#' The basal area is the cross sectional area of the tree.
#' ##
#' Inputs and return values are metric.
#'
#'@param DBH Diameter at breast height in cm.
#'
#'@return BA - Basal Area in sq meters
#'
#'@examples
#'
#' BA(34)
#'
#' # Tibble %>% mutate(BA = BA("DBH Vector"))
#'
#'@export


BA <- function(DBH) {
  BA <- round((0.00007854 * DBH^2),2)
  return(BA)
}
