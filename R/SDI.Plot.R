#' Plot Level Stand Density Index Function
#'
#' This function computes the Stand Density Index for each plot in your inventory.
#'
#' @details This function utilizes the SDI.tree function and each trees expansion factor to
#' provide you with the SDI for each plot in your inventory.
#'
#' This index of SDI is obtained through summation (see references). This is a way of determining stand density
#' in uneven-aged stands with non-normal diameter distributions.
#'
#' ###
#' This is a function simplifies obtaining plot level
#' SDI measurements. This SDI value is
#' useful when paired with the SDI.Max function to
#' obtain a Relative Density value using the RD function.
#' @return The return value is a numerical vector of length n providing the
#' stand density index for each plot in your inventory.
#'
#'@family Plot Level Functions
#'@family Stand Density Index Functions
#'@seealso [inventoryfunctions::SDI.Tree]
#'@seealso [inventoryfunctions::SDI.Max]
#'
#'@references
#'Woodall, C. W., Miles, P. D., & Vissage, J. S. (2005). Determining maximum stand density index
#'in mixed species stands for strategic-scale stocking assessments.
#'Forest Ecology and Management, 216(1–3), 367–377. https://doi.org/10.1016/j.foreco.2005.05.050
#'@examples
#'
#'Stand <- c(1,1,1,1,1,1)
#'Plot  <- c(1,1,1,2,2,2)
#'Tree  <- c(1,2,3,1,2,3)
#'DBH   <- c(24, 34, 18, 41, 28, 20)
#'EXPF <- c(5, 5, 5, 5, 5, 5)
#'SDI.Plot(Stand, Plot, Tree, DBH, EXPF)
#'
#'
#' @param Stand Unique Stand ID
#' @param Plot Unique Plot ID
#' @param Tree Unique Tree ID
#' @param DBH Diameter at breast height in cm.
#' @param EXPF Expansion factor for each tree.
#'
#' @export

SDI.Plot <- function(Stand, Plot, Tree, DBH, EXPF) {
  if(length(EXPF) != length(DBH) | length(Plot) != length(DBH)) {
    stop("Error: Please provide each tree with a unique Tree and Plot ID")
  } else {
    SDI.Per.HA <- SDIPlot <- NULL
    sditree <- SDI.Tree(DBH)
    trees <- tidyr::tibble(Stand, Plot, Tree, DBH, sditree, EXPF)
    trees <- trees %>% dplyr::mutate(                        # Multiply Tree SDI by Expansion Factor
      SDI.Per.HA = (sditree * EXPF)
    )
    trees <- trees %>%# Sum SDI values by plot
      dplyr::group_by(Plot, Stand) %>%
      dplyr::mutate(
        SDIPlot = sum(SDI.Per.HA)
      )


    trees$SDIPlot <- round(trees$SDIPlot, 2)
    return(trees$SDIPlot)
  }
}
