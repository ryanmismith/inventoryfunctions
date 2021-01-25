#' Crown Competition in Larger Trees
#'
#' ## This function requires that your data.frame be sorted as detailed in examples before running.
#' This function calculates the cumulative Crown Competition Factor of larger trees within a plot.
#' This function must be run with the same sorting function as the BA.Larger function.
#' Please refer to details for explanation of required inputs, as this function
#' will not run without properly sorting your inventory as demonstrated in the details and/or examples.
#'
#'@param Unique.ID Unique Plot ID defined by the Unique.ID function
#'@param SPP Species for each tree
#'@param DBH Diameter at breast height in cm.
#'@param EXPF Expansion Factor for each tree. Can be calculated with the EXP.F function.
#'@param CUTOFF The minimum diameter in cm of a tree to be included in the analysis. If left blank the default CUTOFF is 10cm.
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
#'@details
#'This function calculates the crown competition factor in larger trees. By default, this function will only
#'include trees with a DBH > 10 in the analysis. This value can be increased or decreased by adding a cutoff value.
#'
#'## This function requires that your data.frame be sorted as such prior to running:
#'
#' df <- df %>%
#' group_by(ID) %>%
#'   arrange(desc(DBH), .by_group = TRUE)
#'
#' Your data should include unique IDs specific to each plot. If you have not done this, it can be done using the
#' Unique.ID function included in this package. Each tree must have an expansion factor value. This can be done using
#' the EXP.F function.
#'
#'
#'@return The function returns CCF values of larger trees for each tree in a plot as a numeric vector of length n.
#'
#'@examples
#'
#'ID <- c(1,1,1,1,1)
#'SPP <- c("BF", "RO", "RM", "BF". "RO")
#'DBH <- c(25, 30, 8, 19, 32)
#'EXPF <- c(8, 8, 8, 8, 8)
#'CCF.Larger(ID, SPP, DBH, EXPF, CUTOFF = TRUE)
#'CCF.Larger(ID, SPP, DBH, EXPF, CUTOFF = 5)
#'
#' \donttest{
#' df <- df %>%
#' group_by(ID) %>%
#' #  arrange(desc(DBH), .by_group = TRUE)
#'
#' # df <- df %>%
#' # mutate(
#' #   CCFL = CCF.Larger(ID, SPP, DBH, EXPF)
#' # )
#' }
#'
#'@export

CCF.Larger <- function(ID, SPP, DBH, EXPF, CUTOFF = TRUE){
  if(CUTOFF == TRUE){
  maxcrown <- ifelse(DBH < 10, 0, MCW(SPP, DBH))
  } else {
  maxcrown <- ifelse(DBH < CUTOFF, 0, MCW(SPP, DBH))
  }
  MCA <- ifelse(maxcrown == 0, 0, 100*((pi*(maxcrown/2)^2)/10000)*EXPF)

  Temp <- data.frame(ID, DBH, MCA)
  Temp <- Temp[order(-DBH),]
  Temp$csum <- ave(Temp$MCA, Temp$ID, FUN=cumsum)
  Temp$CCFL <- (Temp$csum - MCA)
  Temp$CCFL <- round(Temp$CCFL, 2)
  return(Temp$CCFL)

}
