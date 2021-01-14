#' Basal Area in Larger Trees
#'
#' This function calculates the cumulative basal areas of larger trees within a plot.
#' Please refer to details for explanation of outputs and required inputs.
#' The basal area for measured trees must be computed first using the BA function.
#'
#'@param Plot Unique Plot ID
#'@param Tree Unique Tree ID within each plot
#'@param BA basal area of each tree. Can be calculated with the BA function.
#'
#'@seealso [inventoryfunctions::BA]
#'
#'@details This function uses the dplyr package to select, arrange, and mutate the data. It is important that
#'you enter vectors of equal length otherwise the package will give you an error message.
#' ##
#' Your data should include individual plot IDs for each plot otherwise the BAL value will be inaccurate.
#' ##
#' This function is a dependency for a variety of additional functions in the inventoryfunctions package,
#' and the example inventory script included in this package will demonstrate how to execute it prior to
#' running those additional dependencies. Thus, it is recommended that you run the function into an object
#' named BAL in your dataframe or tibble. A recommended way to run the function is this:
#' ##
#' Tibble <- Tibble %>% mutate(BAL = BAL(Plot, Tree, BA))
#' ##
#' If you prefer base R, just make sure it is saved as the variable BAL in your df so that it can be easily integrated
#' into additional functions for which it is a dependency.
#'
#'@return The function returns BAL values as a vector of length n
#'
#'@export

BAL <- function(Plot, Tree, BA) {
  if(length(Tree) != length(Plot) | length(Plot) != length(BA)) {
    stop("Error: Please provide each tree with a unique Tree and Plot ID")
  } else {
    trees <- tibble(Plot, Tree, BA)
    trees <- trees %>%
      arrange(desc(BA)) %>%
      group_by(Plot) %>%
      mutate(
        BAL = cumsum(BA) - BA
        ) %>%
      arrange(Plot, Tree) %>%
      select(Plot, Tree, BAL)
    trees$BAL <- round(trees$BAL, 2)
    return(trees$BAL)
  }
}



