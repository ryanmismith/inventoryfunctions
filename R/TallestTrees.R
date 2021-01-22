#' Average Height of the 100 Tallest Trees
#'
#'
#' ## This function requires that your data.frame be sorted as detailed in examples before running.
#' This function calculates the average height of the 100 tallest trees in your data set. Using the EXPF and heights provided
#' the 100 tallest trees will be identified and averaged. This mean value will then be applied to a variable listing the average
#' of the tallest trees within the plot as an observation for each tree in your data set.
#'
#'
#'@param ID Unique Plot ID for each plot in your data set.
#'@param HT Height of each tree in meters.
#'@param EXPF Expansion factor of each tree.
#'
#'@seealso [inventoryfunctions::Unique.ID]
#'@seealso [inventoryfunctions::EXPF]
#'
#'@family Plot Level Functions
#'
#'@details This function uses the dplyr package to select, arrange, and mutate the data.
#'
#'## This function requires that your data frame be sorted as such prior to running:
#'
#' df <- df %>%
#' group_by(ID) %>%
#'   arrange(desc(HT), .by_group = TRUE)
#'
#' Your data should include unique IDs specific to each plot. If you have not done this, it can be done using the
#' Unique.ID function included in this package. This function requires either a measured or predicted height for each
#' tree in your inventory.
#'
#'@return The function returns a vector of length n with the 100 tallest trees in each plot.
#'
#'@examples
#'
#' # df <- df %>%
#' # group_by(ID) %>%
#' #   arrange(desc(HT), .by_group = TRUE)
#'
#' # df <- df %>%
#' # mutate(
#' #    TallestTrees = TallestTrees(ID, HT, EXPF)
#' #  )
#'
#'@export


TallestTrees <- function(ID, HT, EXPF){

  Temp <- data.frame(ID, HT, EXPF)
  Temp <- Temp[order(-HT),]
  Temp$csum <- ave(Temp$EXPF, Temp$ID, FUN=cumsum)

  for (i in 1:length(Temp$csum)){
    if(Temp$csum[i] <= 100){
      Temp$X[i] = Temp$HT[i] * Temp$EXPF[i]
      Temp$Counts[i] = Temp$csum[i]
    } else {
      Temp$X[i] = NA
      Temp$Counts[i] = NA
    }
  }


  Temp$remainder <- ave(Temp$Counts, Temp$ID, FUN = function(x) 100-max(x, na.rm = TRUE))  # Number of trees not included in X
  Temp$minheight <- ave(Temp$Counts, Temp$ID, FUN = function(x) Temp$HT[1 + which.max(x)]) # Height of tree not included in X

  for (i in 1:length(Temp$remainder)){    # Create column with total combined heights of trees that were missing from column x
    Temp$leftover[i] <- Temp$remainder[i] * Temp$minheight[i]
  }

  Temp$Y <- ave(Temp$X, Temp$ID, FUN = function(x) sum(x, na.rm = TRUE)) #Combined heights of trees in X (cumsum <= 100)

  for (i in 1:length(Temp$Y)){
    Temp$Total[i] <- Temp$leftover[i] + Temp$Y[i]                        # Create column with combined heights of 100 tallest trees
  }

  Temp$Result <- ave(Temp$Total, Temp$ID, FUN = function(x) x/100)       # Divide the combined height of 100 tallest trees by 100.

  round(Temp$Result, 2)


  return(Temp$Result)

}
