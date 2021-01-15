#' Merchantable Stopper Height Merchandising
#'
#' This function calculates the total tree volume, merchantable volume,
#' sawlog volume, pulp volume, cull volume, and saw board feet for trees
#' using merchantable stopper height measurements.
#' In this method a beginning 'Stump' height and a top 'Saw.Height'
#' are measured along the portion of the stem that is determined to be
#' sawlog quality. All volumes calculated using Kozak Volume Function.
#' Each section that is not entered defaults to Cull.
#' Merch diameters establish by the MerchDiam function.
#'
#' @details This function will automatically downgrade products to pulp for portions of the
#' measured sawlog stem where minimum sawlog dimensions are not met. Only the sawlog portion of the
#' stem meeting minimum diameter requirements is used to estimate board feet.
#'
#' Sawlog board feet is estimated using the international 1/4 inch rule. The sawlog portion of the stem is broken
#' into 4 sections of equal length and the international 1/4 inch rule is applied to each section.
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
#'@param Pulp defaults to 'Other'. For pulp and cull trees value is
#''Pulp' or 'Cull' and Saw.Height value is 0.
#'
#'@return
#'This function will return the cubic volumes of product potential in cubic meters. All inputs are metric
#'and all outputs are metric with the exception of Board Feet which is returned with imperial values.
#'###
#'Returns the following dataframe:
#'###
#'data.frame(Stand, Plot, Tree, Method, SPP, Saw.BF.MH, Saw.Vol.MH,
#'Pulp.Vol.MH, Cull.Vol.MH, Total.Vol, Merch.Vol, Percent.Sawlog.MH)
#'
#'@examples
#'Merchantable.Height(1, 1, 1, 'RS', 30, 14, .5, 6)
#'Merchantable.Height(1, 1, 2, 'RS', 30, 14, .5, 0, 'Pulp')
#'Merchantable.Height(1, 1, 3, 'RS', 30, 14, .5, 0, 'Cull')
#'
#'@seealso [inventoryfunctions::KozakTreeVol]
#'@seealso [inventoryfunctions::KozakTaper]
#'@seealso [inventoryfunctions::MerchDiam]
#'@family Merchandising Functions
#'@export

Merchantable.Height <- function(Stand, Plot, Tree, SPP, DBH, HT, Stump, Saw.Height, Pulp = "OTHER") {


  # Merchantable Diameters By Species ---------------------------------------
  aa <- sapply(SPP, MerchDiam)
  sd <- as.numeric(t(aa)[, 1]) # Saw Diameter
  pald <- as.numeric(t(aa)[, 2]) # Pallet Diameter
  pd <- as.numeric(t(aa)[, 3]) # Pulp Diameter


  # Diameters ---------------------------------------------------------------
  PotDiam <- KozakTaper(Bark = "ib", SPP, Saw.Height, DBH, HT, Planted = 0)

  if (Saw.Height > 0 && PotDiam > sd) {
    Top.Diam <- PotDiam
  } else if (Saw.Height > 0 && PotDiam <= sd) {
    Top.Diam <- sd
  } else {
    Top.Diam <- 0
  }

  Low.Diam <- KozakTaper(Bark = "ib", SPP, Stump, DBH, HT, Planted = 0)

  # Adjusted Saw Heights (if height exceed minimum saw upper diameter height)

  # Height at Diameter Functions (Divide Log Into 4 Sections For BF)
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

  # Total Tree Vol ---------------------------------------------------

  Total.Vol <- KozakTreeVol(Bark = "ib", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = Stump, topHT = NA, topD = NA) * 35.3147
  Merch.Vol <- KozakTreeVol(Bark = "ib", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = Stump, topHT = NA, topD = pd) * 35.3147

  # Merchandize Saw Vol --------------------------------------------------------
  merchandize.saw.vol <- function(Top.Diam, Low.Diam, Saw.Height) {
    if (Saw.Height > 0) {
      saw.vol <- ((KozakTreeVol(Bark = "ib", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = Stump, topHT = NA, topD = Top.Diam)) -
                    (KozakTreeVol(Bark = "ib", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = Stump, topHT = NA, topD = Low.Diam))) * 35.3147
    } else {
      saw.vol <- 0
    }
  }
  Saw.Vol <- merchandize.saw.vol(Top.Diam, Low.Diam, Saw.Height)

  # Merchandise Sawlogs BF ---------------------------------------------------

  if (Saw.Vol > 0) {
    saw.bf <- board.feet(Diam.1, Log) + board.feet(Diam.2, Log) +
      board.feet(Diam.3, Log) + board.feet(Diam.4, Log)
  } else {
    saw.bf <- 0
  }

  Saw.BF <- saw.bf

  # Merchandize Pulp Vol--------------------------------------------------------
  pulp.vol <- function(Saw.Vol) {
    if (Saw.Vol > 0) {
      pulp <- (Total.Vol - Saw.Vol)
    } else if (Pulp == "Pulp") {
      pulp <- Merch.Vol
    } else {
      pulp <- 0
    }
  }
  Pulp.Vol <- pulp.vol(Saw.Vol)

  # Cull Vol ----------------------------------------------------------------
  if (Pulp == "Cull") {
    Cull <- Total.Vol
  } else {
    Cull <- Total.Vol - Merch.Vol
  }

  # Return Values -----------------------------------------------------------
  Saw.BF.MH <- round(Saw.BF, 4)
  Saw.Vol.MH <- round(Saw.Vol, 4)
  Pulp.Vol.MH <- round(Pulp.Vol, 4)
  Cull.Vol.MH <- round(Cull, 4)
  Merch.Vol <- round(Merch.Vol, 4)
  Total.Vol <- round(Total.Vol, 4)
  Percent.Sawlog.MH <- round((Saw.Vol.MH / Merch.Vol) * 100, 2)
  Method <- "Merch.Height"

  values <- data.frame(Stand, Plot, Tree, Method, SPP, Saw.BF.MH, Saw.Vol.MH, Pulp.Vol.MH, Cull.Vol.MH, Total.Vol, Merch.Vol, Percent.Sawlog.MH)
  return(values)
}
