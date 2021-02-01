#' Relative Density
#'
#' This function computes the Relative Density Index for each plot in your inventory.
#'
#' @details
#' Stand density is a quantitative measure of the degree of crowding
#' and resulting level of competition existing within the stand.
#'
#' Combined with the SDI.Plot (SDI using summation method) and SDI.Max functions
#' (using either method in the package - Woodall or Weiskittel), this is a measure for examining
#' and comparing competition within mixed and uneven aged stands.
#'
#' ## Interpreting Relative Density
#'
#' Based on the interpretation of RD proposed by Drew and Flewelling (1979), relative densities
#' of 0.15, 0.40, and 0.55 correspond to the onset of competition, lower limit of full site occupancy, and the zone of
#' imminent competition mortality, respectively. Curtis (2010) suggests that trees less than 4 cm DBH should be excluded
#' from the computation of any relative density measure.
#'
#' ## Inputs - Plot, Stand, and Tree Tables
#'
#' This function can be used in three different ways. First, you can run a tibble or dataframe of tree level
#' measurements where each tree has a corresponding SDI and SDImax based on its plot or stand location. Second,
#' you can run a tibble or dataframe of plot level data that will provide you with the relative density ratios at each plot.
#' Third, you can utilize this at the stand level, by running mean stand density and mean stand density max data for an entire
#' stand.
#'
#' The ratio is unitless so it can be used for both imperial and metric data sets.
#'
#'@references
#'Curtis, R.O. 2010. Effect of diameter limits and stand structure on relative density indices:
#'A case study. Wes. J. Appl. For. 25(4): 169–175.
#'
#'Drew, T.J. and J.W. Flewelling. 1979. Stand density management:
#'An alternative approach and its application to Douglas-fir plantations.
#'For. Sci. 25(3): 518–532.
#'
#'Weiskittel, A.R., D.W. Hann, J.A. Kershaw Jr and J.K. Vanclay. 2011a.
#'Forest growth and yield modeling. Wiley. Chichester, UK.
#'
#'@examples
#'
#'SDIPlot  <- c(1200, 987, 1823)
#'SDIMax   <- c(2100, 2050, 2150)
#'RD(SDIPlot, SDIMax)
#'
#' @return The return value will be a relative density which is a ratio of Stand Density and Maximum Stand Density.
#' @author Ryan Smith
#' @family Stand Density Index Functions
#' @seealso [inventoryfunctions::SDI.Plot]
#' @seealso [inventoryfunctions::SDI.Max]
#'
#' @param SDIPlot The calculated SDI at each plot.
#' @param SDIMax The calculated SDImax for each plot.
#'
#' @export

RD <- function(SDIPlot, SDIMax) {
  if(length(SDIPlot) != length(SDIMax)) {
    stop("Error: Please enter vectors of equal length.")
  } else {
    temp <- (SDIPlot / SDIMax)

    temp <- round(temp, 2)

    return(round(temp, 2))
  }
}

