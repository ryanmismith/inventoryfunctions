#' Basal Area Per Hectare
#'
#' This function calculates the Basal Area Per Hectare for each plot within your data set.
#'
#'@param Stand The unique Stand ID for each Plot.
#'@param Plot The unique Plot ID for each Plot.
#'@param BA Diameter at breast height in cm.
#'@param EXPF Expansion factor for each tree.
#'
#'@return Returns a vector of the length N with the basal area in sq meters per hectare for each plot.
#'
#'@details
#' This function requires that you calculate the Basal Area and the Expansion Factor for each tree in
#' your inventory. See the EXP.F and BA functions if you need to do this.
#'
#' ###
#' Basal Area Per Hectare is a dependency for a number of other functions that may be useful in your analysis, so
#' using a standard naming convention, while always good practice, will help when applying to additional functions.
#' (ex: BAPH, BA.PH, BasalAreaPerHectare, BAPerHectare).
#'
#'@seealso [inventoryfunctions::BA]
#'@seealso [inventoryfunctions::EXP.F]
#'
#'@family Basal Area Functions
#'@family Plot Level Functions
#'
#'@examples
#'
#' Stand <- c(1,1,1,1)
#' Plot  <- c(1,1,1,1)
#' BA    <- c(.01, .12, .06, .04)
#' EXPF  <- c(16, 16, 16, 16)
#' BAPH(Stand, Plot, BA, EXPF)
#'
#'@export

BAPH <- function(Stand, Plot, BA, EXPF){
   treebasal <- x <- NULL
  temp <- tidyr::tibble(Stand, Plot, BA, EXPF)
  temp <- temp %>%
    dplyr::mutate(
      treebasal = BA * EXPF
      ) %>%
    dplyr::select(Stand, Plot, treebasal)

  temp <- temp %>%
    dplyr::group_by(Stand, Plot) %>%
    dplyr::mutate(
      x = sum(treebasal)
    ) %>%
    dplyr::select(Stand, Plot, x)

  temp$x <- round(temp$x, 2)

  return(temp$x)
}







