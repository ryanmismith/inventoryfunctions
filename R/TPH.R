#' Trees Per Hectare
#'
#' This function calculates the Trees Per Hectare for each plot within your data set. The minimum diameter of trees included
#' in the analysis is 10cm. This diameter can be changed using the CUTOFF parameter in this function.
#'
#'
#'@param Stand The unique Stand ID for each Plot.
#'@param Plot The unique Plot ID for each Plot.
#'@param DBH Diameter at breast height in cm.
#'@param EXPF Expansion factor for each tree.
#'@param CUTOFF The minimum diameter in cm of a tree to be included in the analysis. If left blank the default CUTOFF is 10cm.
#'
#'@return Returns a vector of the length N with the trees per hectare for each plot.
#'
#'@details
#' This function requires that you calculate the Expansion Factor for each tree in
#' your inventory. See the EXP.F function if you need to do this.
#' ###
#' # TPH(Stand, Plot, DBH, EXPF, 20) <- this is an example of a CUTOFF that only includes trees >= 20cm.
#' ###
#' Trees Per Hectare is a dependency for a number of other functions that may be useful in your analysis, so
#' using a standard naming convention, while always good practice, will help when applying to additional functions.
#' (ex: TPH, TreePH).
#'
#'@seealso [inventoryfunctions::EXP.F]
#'
#'@family Plot Level Functions
#'
#'@examples
#' Stand <- c(1,1,1,1,1,1,1,1,1)
#' Plot  <- c(1,1,1,1,1,2,2,2,2)
#' DBH   <- c(5, 6, 33, 23, 11, 22, 4, 17, 40)
#' EXPF  <- c(15, 15, 15, 15, 15, 15, 15, 15, 15)
#' TPH(Stand, Plot, DBH, EXPF)
#' TPH(Stand, Plot, DBH, EXPF, 7)
#' TPH(Stand, Plot, DBH, EXPF, 20)
#'
#'@export

TPH <- function(Stand, Plot, DBH, EXPF, CUTOFF = TRUE){

    temp <- tibble(Stand, Plot, DBH, EXPF)

  if(CUTOFF != TRUE) {

    temp$EXPF <- ifelse(temp$DBH < CUTOFF, 0, EXPF)

    temp <- temp %>%
      group_by(Stand, Plot) %>%
      mutate(
        x = sum(EXPF)
      ) %>%
      dplyr::select(Stand, Plot, x)
  } else {

    temp$EXPF <- ifelse(temp$DBH < 10, 0, EXPF)

    temp <- temp %>%
      group_by(Stand, Plot) %>%
      mutate(
        x = sum(EXPF)
      )
  }

    x <- round(temp$x, 1)
    return(x)
}

