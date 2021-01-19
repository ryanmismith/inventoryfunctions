#' Unique Plot ID Code
#'
#' When working with a large data set of multiple stands and plots,
#' it can be helpful to provide each individual plot a unique ID code.
#' As an example, his ensures that when using some functions,
#' like basal area of larger trees, you don't confused plot 2 in
#' stands 1, 2, 3.
#'
#'@param Stand The unique stand ID for each stand.
#'@param Plot The unique plot ID for each plot within a stand.
#'@family Plot Level Functions
#'
#' @return This function returns a vector of length n with unique id
#' numbers for every individual plot.
#'
#' @examples
#' Stand <- c(1,1,1,1,1,1,2,2,2,2,2,2)
#' Plot  <- c(1,2,3,4,5,6,1,2,3,4,5,6)
#' Unique.ID(Stand, Plot)
#'
#' @export

Unique.ID <- function(Stand, Plot) {
  Temp <- tibble(Stand, Plot)
  Temp <- Temp %>%
    group_by(Stand, Plot) %>%
    mutate(
      X = cur_group_id()
    ) %>% ungroup()

  return(Temp$X)
}
