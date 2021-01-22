#' Crown Competition Factor
#'
#' This function returns the plot level Crown Competition Factor (CCF). The CCF is based on
#' the MCW of trees per Hectare. By default, this function only includes trees with a DBH > 10
#' in the analysis. This DBH can be changed to any value by adding a CUTOFF value.
#'
#'
#' ## Metric
#' This function uses metric units.
#'
#' @details
#' ##
#' @details Species should be entered as a character vector. This function uses the dplyr package to select,
#' arrange, and mutate the data. It is important that
#' you enter vectors of equal length otherwise the package will give you an error message.
#' ##
#' @details MCW value needed to calculate CCF, please refer to MCW function in see also.
#' ##
#' CCF estimates the area available to the average tree in a stand relative
#' to the maximum area it could use if it were open-growth. Although not a measure
#' of crown closure (Curtis, 1970), complete crown closure of unthinned stands is
#' generally assumed to occur from a CCF of 100 to the maximum for the species (Krajicek et al., 1961),
#' which can be over 500 for some species like Douglas-fir and western hemlock (e.g. Hann et al., 2003).
#'
#' @seealso [inventoryfunctions::MCW]
#' @seealso [inventoryfunctions::EXP.F]
#'
#' @family Crown Functions
#' @family Plot Level Functions
#' @param Plot Unique Plot ID
#' @param Tree Unique Tree ID
#' @param SPP Tree Species: use the FVS code
#' @param DBH Diameter at breast height in cm.
#' @param EXPF Expansion factor for each tree.
#' @param CUTOFF The minimum diameter in cm of a tree to be included in the analysis. If left blank the default CUTOFF is 10cm.
#'
#' @return This function will return a numeric vector of the Crown Competition Factor for each plot in your inventory.
#'
#' @examples
#'
#'Stand <- c(1,1,1,1,1,1)
#'Plot  <- c(1,1,1,2,2,2)
#'Tree  <- c(1,2,3,1,2,3)
#'SPP   <- c("BF", "RO", "RS", "YB", "RO", "YB")
#'DBH   <- c(24, 34, 18, 41, 6, 20)
#'EXPF <- c(5, 5, 5, 5, 5, 5)
#'CrownCompF(Stand, Plot, Tree, SPP, DBH, EXPF)
#'CrownCompF(Stand, Plot, Tree, SPP, DBH, EXPF, CUTOFF = 2)
#'CrownCompF(Stand, Plot, Tree, SPP, DBH, EXPF, CUTOFF = 21)
#'
#' # Tibble %>% mutate(
#' #    CCF = CrownCompF("Stand ID", Plot ID", "Tree ID", "SPP Variable",
#' #              "DBH Variable", "EXPF Variable")
#' #  )
#'
#' @export

CrownCompF <- function(Stand, Plot, Tree, SPP, DBH, EXPF, CUTOFF = TRUE) {
    Diam <- DBH
    if(CUTOFF == TRUE){
    maxcrown <- ifelse(Diam < 10, 0, MCW(SPP, Diam))
    } else {
    maxcrown <- ifelse(Diam < CUTOFF, 0, MCW(SPP, Diam))
    }
    MCA <- ifelse(maxcrown == 0, 0, 100*((pi*(maxcrown/2)^2)/10000)*EXPF)
    temp <- tibble(Stand, Plot, MCA)
    temp <- temp %>%
      group_by(Stand, Plot) %>%
      mutate(
        X = sum(MCA)
      )
    temp$X <- round(temp$X, 2)
    return(temp$X)
  }





