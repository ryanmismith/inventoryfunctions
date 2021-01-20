#' AGS/UGS Merchandising
#'
#' This function calculates the total tree volume, merchantable volume,
#' sawlog volume, pulp volume, cull volume, and saw board feet for trees
#' using AGS/UGS measurements. Inputs should be in metric.
#'
#' AGS is acceptable growing stock. UGS is unacceptable growing stock.
#'
#' AGS/UGS can have multiple definitions, but this function determines all
#' portions of the tree to be sawlog to minimum inside bark merchantable diameter, pulp
#' to minimum inside bark merchantable diameter, and cull for the remainder for all 'AGS'
#' trees. All UGS trees will be calculated as CULL.
#'
#' ## All trees default to UGS when no value is input.
#'
#' Volumes determined using Kozak Taper Equations and Smalians Volume Formula.
#' Merch diameters establish by the MerchDiam function.
#'
#' Sawlog board feet is estimated using the international 1/4 inch rule. The sawlog portion of the stem is broken
#' into 4 sections of equal length and the international 1/4 inch rule is applied to each section.
#'
#' df <- df %>% rownames_to_column()
#' %>% gather(variable, value, -rowname) %>% spread(rowname, value)
#' is a useful pipe for unnesting the lists into dataframe when used with mapply.
#'
#'
#'@param Stand The Unique Stand Identification Number
#'@param Plot The Unique Plot Identification Number
#'@param Tree The Unique Tree Identification Number
#'@param SPP The species identification using FVS codes: ex 'RO' = Red Oak
#'@param DBH Diameter at breast height in cm
#'@param HT Height of tree in meters
#'@param Stump Stump height in meters. Recommended value if not measured is .5
#'@param GS Record the growing stock of the tree as AGS or UGS, defaults to UGS
#'
#'@seealso [inventoryfunctions::KozakTreeVol]
#'@seealso [inventoryfunctions::KozakTaper]
#'@seealso [inventoryfunctions::MerchDiam]
#'@family Merchandising Functions
#'@return
#' Metric, with the exception of Board Feet which is returned with imperial values.
#' ###
#' data.frame(Stand, Plot, Tree, Method, SPP, Saw.BF.AGS, Saw.Vol.AGS,
#' Pulp.Vol.AGS, Cull.Vol.AGS, Total.Vol, Merch.Vol, Percent.Sawlog.AGS)
#'
#'@examples
#' AGS.UGS(1, 1, 1, 'RS', 30, 14, .5, 'AGS')
#' AGS.UGS(1, 1, 2, 'RO', 25, 12, .5, 'UGS')
#' AGS.UGS(1, 1, 3, 'SM', 40, 18, .5)
#'
#'@export


AGS.UGS <- function(Stand, Plot, Tree, SPP, DBH, HT, Stump, GS = "UGS") {

  # Merchantable Diameters By Species ---------------------------------------
  aa <- sapply(SPP, MerchDiam)
  sd <- as.numeric(t(aa)[, 1]) # Saw Diameter
  pald <- as.numeric(t(aa)[, 2]) # Pallet Diameter
  pd <- as.numeric(t(aa)[, 3]) # Pulp Diameter

  # Diameters --------------------------------------------------------
  Top.Diam <- sd
  Low.Diam <- KozakTaper(Bark = "ib", SPP, Stump, DBH, HT, Planted = 0)

  # Total Tree Vol --------------------------------------------------------
  Total.Vol <- KozakTreeVol(Bark = "ib", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .5, topHT = NA, topD = NA) * 35.3147
  Merch.Vol <- KozakTreeVol(Bark = "ib", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .5, topHT = NA, topD = pd) * 35.3147

  # Height at Diameter Function --------------------------------------------------------

  if (Top.Diam > 0) {
    f <- function(x) abs(Top.Diam - KozakTaper("ib", SPP = SPP, x, DBH = DBH, HT = HT, Planted = 0))
    o <- optimize(f,
                  lower = (HT * .25), upper = (HT + 1),
                  maximum = FALSE, tol = .Machine$double.eps^0.25
    )
    Log.Length <- (o$minimum)[[1]] - Stump
  } else {
    Log.Length <- 0
  }

  # Log in 4 Sections
  Log <- Log.Length / 4
  Log.1 <- Log
  Diam.1 <- KozakTaper("ib", SPP = SPP, Log, DBH = DBH, HT = HT, Planted = 0)
  Log.2 <- Log * 2
  Diam.2 <- KozakTaper("ib", SPP = SPP, Log.2, DBH = DBH, HT = HT, Planted = 0)
  Log.3 <- Log * 3
  Diam.3 <- KozakTaper("ib", SPP = SPP, Log.3, DBH = DBH, HT = HT, Planted = 0)
  Log.4 <- Log.Length - Log.3
  Diam.4 <- Top.Diam

  # International 1/4in Rule for Board Feet ---------------------------------
  board.feet <- function(TopDiam, Log) {
    a <- 0.049621
    b <- 0.00622
    c <- 0.185476
    d <- 0.000259
    e <- 0.011592
    f <- 0.04222
    len <- (Log * 3.28084)
    inches <- 0.3937008

    (a * (len * (inches * TopDiam)^2)) + (b * (len^2 * (inches * TopDiam))) - (c * (len * TopDiam * inches)) +
      (d * len^3) - (e * len^2) + (f * len)
  }

  # Merchandize Saw Vol --------------------------------------------------------
  merchandize.saw.vol <- function(GS) {
    if (GS == "AGS") {
      saw.vol <- ((KozakTreeVol(Bark = "ib", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = Stump, topHT = NA, topD = Top.Diam)) -
                    (KozakTreeVol(Bark = "ib", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = Stump, topHT = NA, topD = Low.Diam))) * 35.3147
    } else {
      saw.vol <- 0
    }
  }
  Saw.Vol <- round(merchandize.saw.vol(GS), 4)

  # Merchandise Sawlogs BF ---------------------------------------------------
  if (Saw.Vol > 0) {
    saw.bf <- board.feet(Diam.1, Log) + board.feet(Diam.2, Log) +
      board.feet(Diam.3, Log) + board.feet(Diam.4, Log)
  } else {
    saw.bf <- 0
  }

  Saw.BF <- saw.bf

  # Merchandize Pulp Vol--------------------------------------------------------
  pulp.vol <- function(GS) {
    if (GS == "AGS") {
      pulp <- (Merch.Vol - Saw.Vol)
    } else if (GS == "UGS") {
      pulp <- 0
    }
  }
  Pulp.Vol <- round(pulp.vol(GS), 4)

  # Cull Vol ----------------------------------------------------------------
  if (GS == "AGS") {
    Cull <- Total.Vol - Merch.Vol
  } else {
    Cull <- Total.Vol
  }
  Cull.Vol <- round(Cull, 4)

  # Return Values -----------------------------------------------------------
  Saw.BF.AGS <- round(Saw.BF, 4)
  Saw.Vol.AGS <- Saw.Vol
  Pulp.Vol.AGS <- Pulp.Vol
  Cull.Vol.AGS <- Cull.Vol
  Total.Vol <- round(Total.Vol, 4)
  Merch.Vol <- round(Merch.Vol, 4)
  Percent.Sawlog.AGS <- round((Saw.Vol.AGS / Merch.Vol) * 100, 2)
  Method <- "AGS.UGS"

  values <- data.frame(Stand, Plot, Tree, Method, SPP, Saw.BF.AGS, Saw.Vol.AGS, Pulp.Vol.AGS, Cull.Vol.AGS, Total.Vol, Merch.Vol, Percent.Sawlog.AGS)
  return(values)
}
