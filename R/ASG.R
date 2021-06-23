#' ASG Merchandising
#'
#' This function calculates the total tree volume, merchantable volume,
#' sawlog volume, pulp volume, cull volume, and saw board feet for trees
#' using Acceptable or Unacceptable Sawlog Grade Observations Inputs should be in metric.
#'
#' ASG is any tree tree with at least one 8 foot sawlog grade section.
#' USG trees have no sawlogs and are marked as either Pulp or Cull.
#'
#' ASG/USG can have multiple definitions, but this function assumes all
#' portions of the tree to be sawlog to minimum outside bark merchantable diameter, pulp
#' to minimum outside bark merchantable diameter, and cull for the remainder for all 'ASG'
#' trees. All USG trees will be calculated as Pulp or CULL.
#'
#' ## All trees default to USG when no value is input.
#'
#' Volumes determined using Kozak Taper Equations and Smalians Volume Formula.
#' Merch diameters establish by the MerchDiam function.
#'
#' Sawlog board feet is estimated using the international 1/4 inch rule. The sawlog portion of the
#' stem is broken into 2.4384m sections and the international 1/4 inch rule is applied
#' to each section. If the final section is longer than 2.4384m but smaller than 4.8768m
#' then that entire length will be used as the final log for calculating board feet.
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
#'@param GS Record the growing stock of the tree as ASG or USG, defaults to USG
#'@author Ryan Smith
#'@seealso [inventoryfunctions::KozakTreeVol]
#'@seealso [inventoryfunctions::KozakTaper]
#'@seealso [inventoryfunctions::MerchDiam]
#'@family Merchandising Functions
#'@return
#' Metric, with the exception of Board Feet which is returned with imperial values.
#' ###
#' data.frame(Stand, Plot, Tree, Method, SPP, LogLength, LogCount, Saw.BF.ASG, Saw.Vol.ASG,
#' Pulp.Vol.ASG, Cull.Vol.ASG, Total.Vol, Merch.Vol, Percent.Sawlog.ASG)
#'
#'@examples
#' ASG(1, 1, 1, 'RS', 30, 14, 'ASG')
#' ASG(1, 1, 2, 'RO', 25, 12, 'USG')
#' ASG(1, 1, 3, 'SM', 40, 18)
#'
#'@export


ASG <- function(Stand, Plot, Tree, SPP, DBH, HT, GS = "USG") {

  # Merchantable Diameters By Species ---------------------------------------
  aa <- sapply(SPP, MerchDiam)
  sd <- as.numeric(t(aa)[, 1]) # Saw Diameter
  pald <- as.numeric(t(aa)[, 2]) # Pallet Diameter
  pd <- as.numeric(t(aa)[, 3]) # Pulp Diameter

  # Diameters --------------------------------------------------------
  Top.Diam <- sd
  Low.Diam <- KozakTaper(Bark = "ob", SPP, .3, DBH, HT, Planted = 0)

  # Total Tree Vol --------------------------------------------------------
  Total.Vol <- KozakTreeVol(Bark = "ob", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .1, topHT = NA, topD = NA)
  Merch.Vol <- KozakTreeVol(Bark = "ob", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .3, topHT = NA, topD = pd)

  # Height at Diameter Function --------------------------------------------------------

  if (Top.Diam > 0) {
    f <- function(x) abs(Top.Diam - KozakTaper("ob", SPP = SPP, x, DBH = DBH, HT = HT, Planted = 0))
    o <- optimize(f,
                  lower = (HT * .25), upper = (HT + 1),
                  maximum = FALSE, tol = .Machine$double.eps^0.25
    )
    Log.Length <- (o$minimum)[[1]] - .3
  } else {
    Log.Length <- 0
  }

  if(Log.Length < 2.4384){
    Log.Length <- 0
  } else if(Log.Length == 2.4384) {
    Log.Length <- 2.43840001
  } else {
    Log.Length <- Log.Length
  }

  # Create 8ft Sections and find top diameters; last log includes remainder ---------
  if(Log.Length > 0){
    Sections <- seq(from = 0, to = Log.Length, by = 2.4384)
    LastLog <- (Log.Length - Sections[length(Sections)]) + 2.4384 # Remainder + 8ft
    Sections[length(Sections)] <- Sections[length(Sections)-1] + LastLog #LastLog replaces last sequence value
    LogHeights <- Sections[2:length(Sections)] # Remove 0 value from vector

    LogHeights <- purrr::map_dbl(LogHeights, function(x) x + .3)
    Logs <- Sections[2:length(Sections)]

    #LogText <- "Height of Top Of Log"
    #print(paste(LogText, LogHeights, sep = " - "))

    for(i in 1:length(Logs)){
      Logs[i] <- Logs[i] - Sections[i]
    }
    TopDiam <- Logs
    for(i in 1:length(Logs)){
      TopDiam[i] <- KozakTaper("ob", SPP = SPP, LogHeights[i], DBH = DBH, HT = HT, Planted = 0)
    }
  } else {
    emptyvalue <- 0
  }

  if(Log.Length == 0){
    LogCount <- 0
  } else {
    LogCount <- length(Logs)
  }

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
    if (GS == "ASG") {
      saw.vol <- KozakTreeVol(Bark = "ob", SPP = SPP, DBH = DBH, HT = HT, Planted = 0,
                                         stump = .3, topHT = NA, topD = Top.Diam)
    } else {
      saw.vol <- 0
    }
  }
  Saw.Vol <- round(merchandize.saw.vol(GS), 4)

  # Merchandise Sawlogs BF ---------------------------------------------------
  if (Log.Length > 0) {
    LogBF <- mapply(board.feet, TopDiam, Logs)

    #BFtext <- "BF in Log"
    #print(paste(BFtext, LogBF, sep = " - "))

    saw.bf <- sum(LogBF)
  } else {
    saw.bf <- 0
  }
  Saw.BF <- saw.bf

  # Merchandize Pulp Vol--------------------------------------------------------
  pulp.vol <- function(GS) {
    if (GS == "ASG") {
      pulp <- (Merch.Vol - Saw.Vol)
    } else if (GS == "USG") {
      pulp <- 0
    }
  }
  Pulp.Vol <- round(pulp.vol(GS), 4)

  # Cull Vol ----------------------------------------------------------------
  if (GS == "ASG") {
    Cull <- Total.Vol - Merch.Vol
  } else {
    Cull <- Total.Vol
  }
  Cull.Vol <- round(Cull, 4)

  # Return Values -----------------------------------------------------------
  Saw.BF.ASG <- round(Saw.BF, 4)
  Saw.Vol.ASG <- Saw.Vol
  Pulp.Vol.ASG <- Pulp.Vol
  Cull.Vol.ASG <- Cull.Vol
  Total.Vol <- round(Total.Vol, 4)
  Merch.Vol <- round(Merch.Vol, 4)
  Percent.Sawlog.ASG <- round((Saw.Vol.ASG / Merch.Vol) * 100, 2)
  Method <- "ASG"

  values <- data.frame(Stand, Plot, Tree, Method, SPP, Log.Length, LogCount, Saw.BF.ASG, Saw.Vol.ASG, Pulp.Vol.ASG, Cull.Vol.ASG, Total.Vol, Merch.Vol, Percent.Sawlog.ASG)
  return(values)
}
