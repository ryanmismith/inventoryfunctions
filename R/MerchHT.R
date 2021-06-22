#' Merchantable Stopper Height Merchandising
#'
#' This function calculates the total tree volume, merchantable volume,
#' sawlog volume, pulp volume, cull volume, and saw board feet for trees
#' using merchantable stopper height measurements.
#' In this method a beginning 'Stump' height and a top 'Saw.Height'
#' are measured along the portion of the stem that is determined to be
#' sawlog quality. All volumes calculated using Kozak Volume Function.
#' Merch diameters establish by the MerchDiam function.
#'
#' @details This function will automatically downgrade products to pulp for portions of the
#' measured sawlog stem where minimum sawlog dimensions are not met. Only the sawlog portion of the
#' stem meeting minimum diameter requirements is used to estimate board feet.
#'
#' Sawlog board feet is estimated using the international 1/4 inch rule. The sawlog portion of the
#' stem is broken into 2.4384m sections and the international 1/4 inch rule is applied
#' to each section. If the final section is longer than 2.4384m but smaller than 4.8768m
#' then that entire length will be used as the final log for calculating board feet.
#'
#' @details df <- df %>% rownames_to_column()
#' %>% gather(variable, value, -rowname) %>% spread(rowname, value)
#' is a useful pipe for unnesting the lists into dataframe when used with mapply.
#'
#'
#'@param Stand The Unique Stand Identification Number
#'@param Plot The Unique Plot Identification Number
#'@param Tree The Unique Tree Identification Number
#'@param SPP The species identification using FVS codes: ex 'RO' = Red Oak
#'@param DBH Diameter at breast height in cm
#'@param HT Tree height in meters
#'@param Stump Stump height in meters, can be value where sawlog quality stem begins
#'@param Saw.Height Height at which sawlog quality stem ends. Value is 0 for pulp of cull trees.
#'@param Pulp defaults to TRUE. If the tree is pulp quality, enter TRUE.
#'@param Cull defaults to FALSE. If the tree is cull quality, enter TRUE.
#'
#'@return
#'This function will return the cubic volumes of product potential in cubic meters. All inputs are metric
#'and all outputs are metric with the exception of Board Feet which is returned with imperial values.
#'###
#'Returns the following dataframe:
#'###
#'data.frame(Stand, Plot, Tree, Method, SPP, Saw.BF.MH, Saw.Vol.MH,
#'Pulp.Vol.MH, Cull.Vol.MH, Total.Vol, Merch.Vol, Percent.Sawlog.MH)
#'@author Ryan Smith
#'@examples
#'MerchHT(1, 1, 1, 'RS', 30, 14, .5, 6)
#'MerchHT(1, 1, 1, 'SM', 52, 14, 2, 7)
#'MerchHT(1, 1, 2, 'RS', 41, 14, .5, 0, 'Pulp')
#'MerchHT(1, 1, 3, 'RS', 30, 14, .5, 0, 'Cull')
#'
#'@seealso [inventoryfunctions::KozakTreeVol]
#'@seealso [inventoryfunctions::KozakTaper]
#'@seealso [inventoryfunctions::MerchDiam]
#'@family Merchandising Functions
#'@export

MerchHT <- function(Stand, Plot, Tree, SPP, DBH, HT, Stump, Saw.Height, Pulp = TRUE, Cull = FALSE) {

  SawHt <- (Saw.Height - .1)

  # If Saw Height Measurement Doesn't Make an 8ft Log, Dismiss It (This can be removed for F/R Analysis)

  if((Saw.Height - Stump) < 2.4384){
    SawHt <- 0
  }


  # Merchantable Diameters By Species ---------------------------------------
  aa <- sapply(SPP, MerchDiam)
  min_saw_diameter <- as.numeric(t(aa)[, 1]) # Saw Diameter
  min_pulp_diameter <- as.numeric(t(aa)[, 3]) # Pulp Diameter


  ### Correct SawHeight Taper Issue -------------------------------------------


  # Diameters ---------------------------------------------------------------
  if(SawHt > 0){
    SawDiam <- KozakTaper(Bark = "ob", SPP, SawHt, DBH, HT, Planted = 0)
  } else {
    SawDiam <- 0
  }

  if (SawDiam >= min_saw_diameter) {
    Top.Diam <- SawDiam
  } else if(SawDiam != 0 && SawDiam <= min_saw_diameter && DBH > min_saw_diameter){
    Top.Diam <- min_saw_diameter
  } else {
    Top.Diam <- 0
  }

  # Height at Diameter Functions - Find Length of Sawlog
  if (Top.Diam > 0) {
    f <- function(x) abs(Top.Diam - KozakTaper("ob", SPP = SPP, x, DBH = DBH, HT = HT, Planted = 0))
    o <- optimize(f,
                  lower = (HT * .1), upper = (HT + 1),
                  maximum = FALSE, tol = .Machine$double.eps^0.25
    )
    Log.Length <- (o$minimum)[[1]] - Stump
  } else {
    Log.Length <- 0
  }

  Uncorrected <- "Uncorrected Log Length"
  print(paste(Uncorrected, Log.Length, sep = " - "))

  if(Log.Length < 2.4384){
    Log.Length <- 0
  } else if(Log.Length == 2.4384) {
    Log.Length <- 2.43840001
  } else if(Log.Length > (Saw.Height - Stump)) {
    Log.Length <- Saw.Height - Stump
  } else {
    Log.Length <- Log.Length
  }

  # Create 8ft Sections and find top diameters; last log includes remainder ---------
  if(Log.Length > 0){
    Sections <- seq(from = 0, to = Log.Length, by = 2.4384)
    LastLog <- (Log.Length - Sections[length(Sections)]) + 2.4384 # Remainder + 8ft
    Sections[length(Sections)] <- Sections[length(Sections)-1] + LastLog #LastLog replaces last sequence value
    LogHeights <- Sections[2:length(Sections)] # Remove 0 value from vector

    LogHeights <- purrr::map_dbl(LogHeights, function(x) x + Stump)
    Logs <- Sections[2:length(Sections)]

    LogText <- "Height of Top Of Log"
    print(paste(LogText, LogHeights, sep = " - "))

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

    (a * (len * (inches * TopDiam)^2)) + (b * (len^2 * (inches * TopDiam))) -
      (c * (len * TopDiam * inches)) +
      (d * len^3) - (e * len^2) + (f * len)
  }

  # Total Tree Vol ---------------------------------------------------

  Total.Vol <- KozakTreeVol(Bark = "ob", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .1, topHT = NA, topD = NA)
  Merch.Vol <- KozakTreeVol(Bark = "ob", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .3, topHT = NA, topD = min_pulp_diameter)

  # Merchandize Saw Vol --------------------------------------------------------
  if (Top.Diam != 0) {
    Saw.Volob <- KozakTreeVol(Bark = "ob", SPP = SPP, DBH = DBH, HT = HT, Planted = 0,
                            stump = Stump, topHT = NA, topD = Top.Diam)
    Saw.Volib <- KozakTreeVol(Bark = "ib", SPP = SPP, DBH = DBH, HT = HT, Planted = 0,
                              stump = Stump, topHT = NA, topD = Top.Diam)
  } else {
    Saw.Vol <- 0
  }

  # Merchandise Sawlogs BF ---------------------------------------------------

  if (Log.Length > 0) {
    LogBF <- mapply(board.feet, TopDiam, Logs)

    BFtext <- "BF in Log"
    print(paste(BFtext, LogBF, sep = " - "))

    saw.bf <- sum(LogBF)
  } else {
    saw.bf <- 0
  }
  Saw.BF <- saw.bf

  # Merchandize Pulp Vol--------------------------------------------------------
  pulp.vol <- function(Saw.Vol, Pulp, Cull){
    if (Saw.Vol > 0){
      pulp <- (Merch.Vol - Saw.Vol)
    } else if (Pulp == TRUE){
      pulp <- Merch.Vol
    } else {
      pulp <- 0
    }
  }
  Pulp.Vol <- pulp.vol(Saw.Vol, Pulp, Cull)

  # Cull Vol ----------------------------------------------------------------
  if (Cull == TRUE){
    Cull <- Total.Vol
    Pulp.Vol <- 0
    Saw.Vol <- 0
    Saw.BF <- 0
  } else {
    Cull <- Total.Vol - Merch.Vol
  }

  # Fix NaN problems in Values ----------------------------------------------
  fix_nan <- function(x) {
    x[is.nan(x)] <- 0
    x
  }

  Cull <- fix_nan(Cull)
  Saw.Vol <- fix_nan(Saw.Vol)
  Pulp.Vol <- fix_nan(Pulp.Vol)
  Saw.BF <- fix_nan(Saw.BF)

  # Return Values -----------------------------------------------------------
  Saw.BF.MH <- round(Saw.BF, 4)
  Saw.Vol.MH <- round(Saw.Vol, 4)
  Pulp.Vol.MH <- round(Pulp.Vol, 4)
  Cull.Vol.MH <- round(Cull, 4)
  Merch.Vol <- round(Merch.Vol, 4)
  Total.Vol <- round(Total.Vol, 4)
  Percent.Sawlog.MH <- round((Saw.Vol.MH / Merch.Vol) * 100, 2)
  Percent.Sawlog.MH <- fix_nan(Percent.Sawlog.MH)
  Method <- "Merch.Height"

  values <- data.frame(LogCount, Log.Length, Top.Diam, Stand, Plot, Tree, Method, SPP, Saw.BF.MH, Saw.Vol.MH, Pulp.Vol.MH, Cull.Vol.MH, Total.Vol, Merch.Vol, Percent.Sawlog.MH)
  return(values)
}

