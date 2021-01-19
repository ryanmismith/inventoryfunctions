#' Crown Competition in Larger Trees
#'
#' ## This function requires that your data.frame be sorted as detailed in examples before running.
#' This function calculates the cumulative Crown Competition Factor of larger trees within a plot.
#' This function must be run with the same sorting function as the BAL function.
#' Please refer to details for explanation of outputs and required inputs, as this function
#' will not run without properly sorting your inventory as demonstrated in the details and/or examples.
#' The expansion factor for measured trees must be computed first using the EXP.F function.
#'
#'@param Unique.ID Unique Plot ID defined by the Unique.ID function
#'@param SPP Species for each tree
#'@param DBH Diameter at breast height in cm.
#'@param EXPF Expansion Factor for each tree. Can be calculated with the EXP.F function.
#'
#'@seealso [inventoryfunctions::Unique.ID]
#'@seealso [inventoryfunctions::EXP.F]
#'@seealso [inventoryfunctions::BA.Larger.Trees]
#'
#'@family Values in Larger Trees
#'@family Plot Level Functions
#'@family Crown Functions
#'
#'
#'@details This function uses the dplyr package to select, arrange, and mutate the data.
#'
#'## This function requires that your data.frame be sorted as such prior to running:
#'
#' Tibble <- Tibble %>%
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
#'@return The function returns CCF values of larger trees for each tree in a plot as a numeric vector of length n.
#'
#'@examples
#'
#' # trees_2010 <- trees_2010 %>%
#' # group_by(ID) %>%
#' #  arrange(desc(DBH), .by_group = TRUE)
#'
#' # trees_2010 <- trees_2010 %>%
#' # mutate(
#' #   CCFL = CCF.Larger(ID, SPP, CBH, EXPF)
#' # )
#'
#'@export

CCF.Larger <- function(ID, SPP, DBH, EXPF){
  Diam <- DBH
  maxcrown <- ifelse(Diam < 10, 0, MCW(SPP, Diam))
  MCA <- ifelse(maxcrown == 0, 0, 100*((pi*(maxcrown/2)^2)/10000)*EXPF)

  Temp <- data.frame(ID, Diam, MCA)
  Temp <- Temp[order(-DBH),]
  Temp$csum <- ave(Temp$MCA, Temp$ID, FUN=cumsum)
  Temp$CCFL <- (Temp$csum - MCA)
  Temp$CCFL <- round(Temp$CCFL, 2)
  return(Temp$CCFL)

}
