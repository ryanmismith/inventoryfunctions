#' Kozak Volume Equation
#'
#' This function allows you to calculate the volume at any point on tree
#' for common tree species found in the northeastern and acadian forests.
#' All measurements must be entered as metric. Calculates volume by breaking
#' tree into 100 sections using the kozak taper function and calculating volume
#' of each section using the Smalians volume formula.
#'
#' @param Bark specify if you want measurement inside or outside bark: 'ob' = outside, 'ib' = inside.
#' @param SPP Species: use FVS species codes: example 'RO' - Red Oak, 'WS' = White Spruce
#' @param DBH Diameter Breast Height in cm
#' @param HT Tree Height in meters
#' @param Planted specify if the tree is planted, specify as TRUE or FALSE, defaults to FALSE
#' @param stump the height of the stumpin meters you want excluded from the volume measurement, defaults to NA.
#' @param topHT Top height of crown in meters, defaults to NA
#' @param topD The diameter in cm at which you want the volume measurement to stop, defaults to NA.
#'
#' @seealso [inventoryfunctions::KozakTaper]
#' @seealso [inventoryfunctions::EXP.F]
#' @family Volume Functions
#' @references
#'Kozak, A. (2004). My last words on taper equations. The Forestry Chronicle,
#'80(4), 507–515. https://doi.org/10.5558/tfc80507-4
#'
#'Li, R., Weiskittel, A., Dick, A. R., Kershaw, J. A., & Seymour, R. S. (2012).
#'Regional Stem Taper Equations for Eleven Conifer Species in the Acadian Region of
#'North America: Development and Assessment. Northern Journal of Applied Forestry,
#'29(1), 5–14. https://doi.org/10.5849/njaf.10-037
#'
#'MacFarlane, D. W., & Weiskittel, A. R. (2016). A new method for capturing stem taper
#'variation for trees of diverse morphological types. Canadian Journal of Forest Research,
#'46(6), 804–815. https://doi.org/10.1139/cjfr-2016-0018
#'
#'Weiskittel, A., & Li, R. (n.d.). DEVELOPMENT OF REGIONAL TAPER AND VOLUME EQUATIONS: HARDWOOD SPECIES. 8.
#'
#' @examples
#'
#' KozakTreeVol(Bark = 'ib', SPP = 'RO', DBH = 40, HT = 18, Planted = FALSE, stump = .5, topHT = NA, topD = 25.3)
#' KozakTreeVol('ob', 'WP', 32.5, 21)
#'
#' @export

KozakTreeVol <- function(Bark, SPP, DBH, HT, Planted = FALSE,stump=NA,topHT=NA,topD=NA){
  sgmts <- 100
  stump <- ifelse(is.na(stump),as.numeric(0.0),stump)
  topHT <- ifelse(is.na(topHT),as.numeric(HT),topHT)
  topHT <- ifelse(topHT>HT,as.numeric(HT),topHT)
  topD <- (ifelse(is.na(topD),as.numeric(0.001),topD))
  L <- (topHT - stump)/sgmts
  i <- 0
  treeVolume <- 0
  while (i < sgmts)
  {
    H1 <- L * i + stump
    H2 <- L * (i + 1) + stump
    if (HT - H1 < 1e-04){
      dob1 <- 0
      dib1 <- 0
    }
    else {
      if (H1 == 0)
        H1 <- 0.001
      Esty1 <- KozakTaper(Bark='ob',SPP=SPP,DHT=H1,DBH=DBH,HT=HT,Planted=Planted)
      dob1 <- as.numeric(Esty1)
      dob1 <- ifelse(dob1<topD,0,dob1)
      dib1 <- DOBtoDIB(SPP=SPP,dob=dob1)
      dib1 <- ifelse(dob1<topD,0,dib1)
    }
    if (HT - H2 < 1e-04){
      dob2 <- 0
      dib2 <- 0
    }
    else {
      if (H2 == 0)
        H2 <- 0.001
      Esty2 <- KozakTaper(Bark='ob',SPP=SPP,DHT=H2,DBH=DBH,HT=HT,Planted=Planted)
      dob2 <- as.numeric(Esty2)
      dob2 <- ifelse(dob2<topD,0,dob2)
      dib2 <- DOBtoDIB(SPP=SPP,dob=dob2)
      dib2 <- ifelse(dob1<topD,0,dib2)
    }
    treeVolume <- ifelse(Bark=='ob',treeVolume + Smalians(dob1, dob2, L * 100),
                         treeVolume + Smalians(dib1, dib2, L * 100))
    i <- i + 1
  }
  treeVolume <- round(treeVolume/1e+06, 6)
  return(treeVolume <- treeVolume)
}
