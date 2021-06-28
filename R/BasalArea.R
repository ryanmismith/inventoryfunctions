#' Basal Area Formula
#'
#' This function calculates the basal area of a tree in square meters.
#' The basal area is the cross sectional area of the tree.
#' ##
#' Inputs and return values are metric.
#'
#'@param DBH Diameter at breast height in cm.
#'
#'@return A numeric vector with the basal area of n trees.
#'
#'@family Basal Area Functions
#'
#'@references
#'Kershaw, J. A., Ducey, M. J., Beers, T. W., & Husch, B. (2017).
#'Forest mensuration (Fifth edition). Wiley/Blackwell.
#'
#'@examples
#'
#' BA(34)
#'
#' \dontrun{
#' # Tibble %>% mutate(BA = BA("DBH Vector"))
#' }
#'@export


BA <- function(DBH) {
  Diam <- DBH/2.54
  Imperial <- (0.005454 * Diam^2)
  Conversion <- Imperial*0.09290304

  return(Conversion)
}
