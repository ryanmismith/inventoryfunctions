#' Single Tree SDI Function
#'
#' This function computes the SDI value of a single tree with
#' no regard for its expansion factor or the influence of the tree
#' on a plot or stand level. See Details for utilizing this function alone
#' without other functions in the Stand Density Index family.
#'
#' This function is meant to be applied on a plot or stand level using
#' the included EXP.F function and dplyr. See the included full inventory script
#' for its application on a stand and plot level, and reference examples below. This is
#' a dependency for the more useful SDI.Plot, SDI.Max, and RD functions which can be found
#' below.
#'
#'
#'@family Stand Density Index Functions
#'@param DBH Diameter at Breast Height
#'
#'@seealso [inventoryfunctions::SDI.Plot]
#'@seealso [inventoryfunctions::SDI.Max]
#'@seealso [inventoryfunctions::RD]
#'
#'@examples
#'
#' SDI(25)
#'
#' ## Effective uses of this function would be:
#' #
#' # Tree %>% mutate(SDI = SDI(DBH)*EXP.F)
#' # This will provide you of the SDI value of each tree for summation on the plot or stand level.
#' #
#' # For SDI on the plot level you can use the code:
#' # test <- test %>%
#' # mutate(EXP.F = EXP.F(DBH, .05)) %>%
#' # group_by(Plot) %>% mutate(SDI = sum(SDI(DBH)*EXP.F))
#' #
#' # However, this is more easily achieved by using the [inventoryfunctions::SDI.Plot] function
#'
#'@export

SDI.Tree <- function(DBH){
  tree <- (DBH/25.4)^1.605
  return(tree)
}
