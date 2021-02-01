#' Basal Area in Larger Trees
#'
#' ## This function requires that your data.frame be sorted as detailed in examples before running.
#' This function calculates the cumulative basal areas of larger trees within a plot.
#' This function must be run with the same sorting function as the CCFL function.
#' Please refer to details for explanation of outputs and required inputs, as this function
#' will not run without properly sorting your inventory as demonstrated in the details and/or examples.
#' The basal area for measured trees must be computed first using the BA function.
#'
#'@param Unique.ID Unique Plot ID defined by the Unique.ID function
#'@param DBH Diameter at breast height in cm.
#'@param BA Basal area of each tree in sq meters. Can be calculated with the BA function.
#'
#'@seealso [inventoryfunctions::BA]
#'@seealso [inventoryfunctions::Unique.ID]
#'@seealso [inventoryfunctions::CCF.Larger]
#'
#'@family Values in Larger Trees
#'@family Basal Area Functions
#'@family Plot Level Functions
#'
#'@details This function uses the dplyr package to select, arrange, and mutate the data.
#'
#'## This function requires that your data frame be sorted as such prior to running:
#'
#' df <- df %>%
#' group_by(ID) %>%
#'   arrange(desc(DBH), .by_group = TRUE)
#'
#' Your data should include unique IDs specific to each plot. If you have not done this, it can be done using the
#' Unique.ID function included in this package.
#' ##
#' This function is a dependency for a variety of additional functions in the inventoryfunctions package,
#' and the example inventory script included in this package will demonstrate how to execute it prior to
#' running those additional dependencies. Thus, it is recommended that you run the function into an object
#' named BAL in your data.frame or tibble.
#' ##
#'
#'
#'@return The function returns BAL values for every tree in sq. meters as a numeric vector of length n.
#'@author Ryan Smith
#'@examples
#'
#' \dontrun{
#' # trees_2010 <- trees_2010 %>%
#'    dplyr::group_by(ID) %>%
#'    dplyr::arrange(desc(DBH), .by_group = TRUE)
#'
#'  trees_2010 <- trees_2010 %>%
#'  dplyr::mutate(
#'     BAL = BA.Larger.Trees(ID, DBH, BA)
#'   )
#' }
#'@export

BA.Larger.Trees <- function(ID, DBH, BA){

  Temp <- data.frame(ID, DBH, BA)
  Temp <- Temp[order(-DBH),]
  Temp$csum <- ave(Temp$BA, Temp$ID, FUN=cumsum)
  Temp$result <- (Temp$csum - BA)
  return(Temp$result)

}

