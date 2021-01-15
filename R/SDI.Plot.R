#' Plot Level Stand Density Index Function
#'
#' This function computes the Stand Density Index for each plot in your inventory.
#'
#' This function utilizes the SDI.tree function and each trees expansion factor to
#' provide you with the SDI for each plot in your inventory.
#'
#' SDI is obtained through summation.
#'
#' This is a function simplifies obtaining plot level
#' SDI measurements as opposed to utilizing the example
#' code shown in SDI.Tree.
#'
#'@examples
#'
#'Plot  <- c(1,1,1,2,2,2)
#'Tree  <- c(1,2,3,1,2,3)
#'DBH   <- c(24, 34, 18, 41, 28, 20)
#'EXP.F <- c(5, 5, 5, 5, 5, 5)
#'SDI.Plot(Plot, Tree, DBH, EXP.F)
#'
#' @return The return value will be a stand density index for each plot in your
#' inventory. This SDI value is valuable when paired with the SDI.Max function to
#' obtain a Relative Density value using the RD function.
#'
#' @family Stand Density Index Functions
#' @seealso [inventoryfunctions::SDI.Tree]
#' @seealso [inventoryfunctions::SDI.Max]
#' @seealso [inventoryfunctions::RD]
#'
#' @param Plot Unique Plot ID
#' @param Tree Unique Tree ID
#' @param DBH Diameter at breast height in cm.
#' @param EXP.F Expansion factor for each tree.
#'
#' @export

SDI.Plot <- function(Plot, Tree, DBH, EXP.F) {
  if(length(EXP.F) != length(DBH) | length(Plot) != length(DBH)) {
    stop("Error: Please provide each tree with a unique Tree and Plot ID")
  } else {
    SDI.Tree <- SDI.Tree(DBH)
    trees <- tibble(Plot, Tree, DBH, SDI.Tree, EXP.F)
    trees <- trees %>% mutate(                        # Multiply Tree SDI by Expansion Factor
      SDI.Per.HA = (SDI.Tree * EXP.F)
    )
    trees <- trees %>%                                # Sum SDI values by plot
      group_by(Plot) %>%
      mutate(
        SDI.Plot = sum(SDI.Per.HA)
      ) %>%
      arrange(Plot, Tree) %>%
      select(Plot, Tree, SDI.Plot)

    trees$SDI.Plot <- round(trees$SDI.Plot, 2)
    return(trees$SDI.Plot)
  }
}
