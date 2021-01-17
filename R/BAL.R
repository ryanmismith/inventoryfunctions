#' Basal Area in Larger Trees
#'
#'## THIS FUNCTION BREAKS. NO GOOD.
#'
#' This function calculates the cumulative basal areas of larger trees within a plot.
#' Please refer to details for explanation of outputs and required inputs.
#' The basal area for measured trees must be computed first using the BA function.
#'
#'@param Stand Unique Stand ID
#'@param Plot Unique Plot ID
#'@param DBH Diameter at breast height in cm.
#'@param BA Basal area of each tree in sq meters. Can be calculated with the BA function.
#'
#'@seealso [inventoryfunctions::BA]
#'
#'@details This function uses the dplyr package to select, arrange, and mutate the data. It is important that
#' you enter vectors of equal length otherwise the package will give you an error message.
#' ##
#' Your data should include individual plot IDs for each plot otherwise the BAL value will be inaccurate.
#' ##
#' Metric values as inputs and returns.
#' ##
#' This function is a dependency for a variety of additional functions in the inventoryfunctions package,
#' and the example inventory script included in this package will demonstrate how to execute it prior to
#' running those additional dependencies. Thus, it is recommended that you run the function into an object
#' named BAL in your dataframe or tibble. A recommended way to run the function is this:
#' ##
#' Tibble <- Tibble %>% mutate(BAL = BAL(Stand, Plot, Tree, BA))
#' ##
#' If you prefer base R, just make sure it is saved as the variable BAL in your df so that it can be easily integrated
#' into additional functions for which it is a dependency.
#'
#'@return The function returns BAL values in sq. meters as a numeric vector of length n.
#'
#'@examples
#'
#'Stand <- c(1,1,1,1,1,1)
#'Plot <- c(1,1,1,2,2,2)
#'BA   <- c(.04, .12, .06, .11, .12, .06)
#'BAL(Plot, Tree, BA)
#'
#'@export

BA.Larger.Trees <- function(Stand, Plot, DBH, BA) {
  Temp <- tibble(Stand, Plot,DBH, BA)
  Temp <- Temp %>%
    group_by(Stand, Plot) %>%
    arrange(desc(DBH), .by_group = TRUE)

  Temp <- Temp %>%
    group_by(Stand, Plot) %>%
    mutate(
      X = cur_group_id()
    ) %>% ungroup()

  Temp <-  Temp %>%
    group_by(X) %>%
    arrange(desc(DBH), .by_group = TRUE)

  Temp <- Temp %>%
    group_by(X) %>%
    mutate(Y = cumsum(BA)) %>%
    ungroup()

  Temp <- Temp %>%
    mutate(Z = (Y - BA))

  return(Temp$Z)
}




