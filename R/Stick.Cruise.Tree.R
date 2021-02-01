#' Stick Cruise Merchandising
#'
#' This function calculates the total tree volume, merchantable volume,
#' sawlog volume, pulp volume, cull volume, and saw board feet for trees
#' using stick cruise measurements. In a stick cruise each 2.4384 meter section of the tree
#' is called out as either potential saw, pulp, or cull. Up to 20 sections may be entered.
#' First section begins at .5 meters.
#' Each section that is not entered defaults to Pulp.
#' Merch diameters establish by the MerchDiam function.
#'
#' @details This function will automatically downgrade products to pulp for sections where a
#' sawlog product call is made but where the small end inside bark diameter of the section does
#' not meet minimum sawlog dimensions. Inside bark diameters are measured using the Kozak Taper Equation.
#'
#' Sawlog board feet is estimated using the international 1/4 inch rule. The international 1/4 inch rule is applied
#' to each eight foot section that is called as "Saw" and which meets diameter requirements
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
#'@param HT Tree height in meters
#'@param Cull if it is a cull tree than enter TRUE, else enter FALSE
#'@param S1 Each section called out as 'Saw', 'Pulp', or 'Cull'.
#'@param S2... You can call as many as 20 sections.
#'
#'@return
#'Metric, with the exception of Board Feet which is returned with imperial values.
#'###
#'data.frame(Stand, Plot, Tree, Method, SPP, Saw.BF.SC, Saw.Vol.SC,
#'Pulp.Vol.SC, Cull.Vol.SC, Total.Vol, Merch.Vol, Percent.Sawlog.SC)
#'
#'@seealso [inventoryfunctions::KozakTreeVol]
#'@seealso [inventoryfunctions::KozakTaper]
#'@seealso [inventoryfunctions::MerchDiam]
#'
#'@family Merchandising Functions
#'@author Ryan Smith
#'@examples
#'Stick.Cruise.Tree(1, 1, 1, 'RO', 30, 16, FALSE, 'Saw', 'Pulp', 'Saw')
#'Stick.Cruise.Tree(1, 1, 1, 'RO', 30, 16, TRUE, 'Saw', 'Pulp', 'Saw')
#'Stick.Cruise.Tree(1, 1, 1, 'RO', 30, 16, FALSE)
#'Stick.Cruise.Tree(1, 1, 1, 'RO', 30, 16, TRUE)
#'
#'@export

Stick.Cruise.Tree <- function(Stand, Plot, Tree, SPP, DBH, HT, Cull = FALSE, S1 = "Pulp", S2 = "Pulp", S3 = "Pulp", S4 = "Pulp",
                              S5 = "Pulp", S6 = "Pulp", S7 = "Pulp", S8 = "Pulp", S9 = "Pulp",
                              S10 = "Pulp", S11 = "Pulp", S12 = "Pulp", S13 = "Pulp", S14 = "Pulp",
                              S15 = "Pulp", S16 = "Pulp", S17 = "Pulp", S18 = "Pulp", S19 = "Pulp", S20 = "Pulp") {

  # Merchantable Diameter For Species ---------------------------------------
  aa <- sapply(SPP, MerchDiam)
  sd <- as.numeric(t(aa)[, 1]) # Saw Diameter
  pald <- as.numeric(t(aa)[, 2]) # Pallet Diameter
  pd <- as.numeric(t(aa)[, 3]) # Pulp Diameter

  # Stump Height of .5 Meters (1.64 ft) - 8 Foot Sections
  eight.ft <- seq(.5, 100, 2.4384)

  if (HT > eight.ft[2]) {
    d1 <- KozakTaper(Bark = "ib", SPP, DHT = eight.ft[2], DBH, HT, Planted = 0)
  } else {
    d1 <- 0
  }
  if (HT > eight.ft[3]) {
    d2 <- KozakTaper(Bark = "ib", SPP, DHT = eight.ft[3], DBH, HT, Planted = 0)
  } else {
    d2 <- 0
  }
  if (HT > eight.ft[4]) {
    d3 <- KozakTaper(Bark = "ib", SPP, DHT = eight.ft[4], DBH, HT, Planted = 0)
  } else {
    d3 <- 0
  }
  if (HT > eight.ft[5]) {
    d4 <- KozakTaper(Bark = "ib", SPP, DHT = eight.ft[5], DBH, HT, Planted = 0)
  } else {
    d4 <- 0
  }
  if (HT > eight.ft[6]) {
    d5 <- KozakTaper(Bark = "ib", SPP, DHT = eight.ft[6], DBH, HT, Planted = 0)
  } else {
    d5 <- 0
  }
  if (HT > eight.ft[7]) {
    d6 <- KozakTaper(Bark = "ib", SPP, DHT = eight.ft[7], DBH, HT, Planted = 0)
  } else {
    d6 <- 0
  }
  if (HT > eight.ft[8]) {
    d7 <- KozakTaper(Bark = "ib", SPP, DHT = eight.ft[8], DBH, HT, Planted = 0)
  } else {
    d7 <- 0
  }
  if (HT > eight.ft[9]) {
    d8 <- KozakTaper(Bark = "ib", SPP, DHT = eight.ft[9], DBH, HT, Planted = 0)
  } else {
    d8 <- 0
  }
  if (HT > eight.ft[10]) {
    d9 <- KozakTaper(Bark = "ib", SPP, DHT = eight.ft[10], DBH, HT, Planted = 0)
  } else {
    d9 <- 0
  }
  if (HT > eight.ft[11]) {
    d10 <- KozakTaper(Bark = "ib", SPP, DHT = eight.ft[11], DBH, HT, Planted = 0)
  } else {
    d10 <- 0
  }
  if (HT > eight.ft[12]) {
    d11 <- KozakTaper(Bark = "ib", SPP, DHT = eight.ft[12], DBH, HT, Planted = 0)
  } else {
    d11 <- 0
  }
  if (HT > eight.ft[13]) {
    d12 <- KozakTaper(Bark = "ib", SPP, DHT = eight.ft[13], DBH, HT, Planted = 0)
  } else {
    d12 <- 0
  }
  if (HT > eight.ft[14]) {
    d13 <- KozakTaper(Bark = "ib", SPP, DHT = eight.ft[14], DBH, HT, Planted = 0)
  } else {
    d13 <- 0
  }
  if (HT > eight.ft[15]) {
    d14 <- KozakTaper(Bark = "ib", SPP, DHT = eight.ft[15], DBH, HT, Planted = 0)
  } else {
    d14 <- 0
  }
  if (HT > eight.ft[16]) {
    d15 <- KozakTaper(Bark = "ib", SPP, DHT = eight.ft[16], DBH, HT, Planted = 0)
  } else {
    d15 <- 0
  }
  if (HT > eight.ft[17]) {
    d16 <- KozakTaper(Bark = "ib", SPP, DHT = eight.ft[17], DBH, HT, Planted = 0)
  } else {
    d16 <- 0
  }
  if (HT > eight.ft[18]) {
    d17 <- KozakTaper(Bark = "ib", SPP, DHT = eight.ft[18], DBH, HT, Planted = 0)
  } else {
    d17 <- 0
  }
  if (HT > eight.ft[19]) {
    d18 <- KozakTaper(Bark = "ib", SPP, DHT = eight.ft[19], DBH, HT, Planted = 0)
  } else {
    d18 <- 0
  }
  if (HT > eight.ft[20]) {
    d19 <- KozakTaper(Bark = "ib", SPP, DHT = eight.ft[20], DBH, HT, Planted = 0)
  } else {
    d19 <- 0
  }
  if (HT > eight.ft[21]) {
    d20 <- KozakTaper(Bark = "ib", SPP, DHT = eight.ft[21], DBH, HT, Planted = 0)
  } else {
    d20 <- 0
  }

  # Create List of Top Diameter of Each Section
  TopDiam.List <- c(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15, d16, d17, d18, d19, d20)

  # Create List of Lower Diameter of Each Section
  stump.diam <- KozakTaper(Bark = "ib", SPP, DHT = .5, DBH, HT, Planted = 0)
  LowDiam.List <- lag(TopDiam.List, default = stump.diam)

  # Create list of Section Calls (Saw, Pulp, Cull)
  Sections <- c(S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15, S16, S17, S18, S19, S20)


  # International 1/4in Rule for Board Feet ---------------------------------
  board.feet <- function(TopDiam) {
    a <- 0.049621
    b <- 0.00622
    c <- 0.185476
    d <- 0.000259
    e <- 0.011592
    f <- 0.04222
    len <- 8
    inches <- 0.3937008

    (a * (len * (inches * TopDiam)^2)) + (b * (len^2 * (inches * TopDiam))) - (c * (len * TopDiam * inches)) +
      (d * len^3) - (e * len^2) + (f * len)
  }

  # Mechanize Sawlogs BF ---------------------------------------------------

  merchandize.saw.bf <- function(TopDiam.List, Sections) {
    if (Sections == "Saw" && TopDiam.List >= sd) {
      saw.bf <- board.feet(TopDiam.List)
      saw.bf <- round(saw.bf, 4)
    } else {
      saw.bf <- 0
    }
  }

  # Merchandize Saw Vol --------------------------------------------------------
  merchandize.saw.vol <- function(TopDiam.List, LowDiam.List, Sections) {
    if (Sections == "Saw" && TopDiam.List >= sd) {
      saw.vol <- ((KozakTreeVol(Bark = "ib", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = NA, topHT = NA, topD = TopDiam.List)) -
                    (KozakTreeVol(Bark = "ib", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = NA, topHT = NA, topD = LowDiam.List))) * 35.3147
    } else {
      saw.vol <- 0
    }
  }

  # Merchandize Pulp Vol--------------------------------------------------------
  merchandize.pulp <- function(TopDiam.List, LowDiam.List, Sections) {
    if (Sections == "Pulp" && TopDiam.List >= pd) {
      pulp <- ((KozakTreeVol(Bark = "ib", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = NA, topHT = NA, topD = TopDiam.List)) -
                 (KozakTreeVol(Bark = "ib", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = NA, topHT = NA, topD = LowDiam.List))) * 35.3147
    } else if (Sections == "Saw" && TopDiam.List < sd && TopDiam.List >= pd) {
      pulp <- ((KozakTreeVol(Bark = "ib", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = NA, topHT = NA, topD = TopDiam.List)) -
                 (KozakTreeVol(Bark = "ib", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = NA, topHT = NA, topD = LowDiam.List))) * 35.3147
    } else {
      pulp <- 0
    }
  }

  # Cull Vol ----------------------------------------------------------------
  merchandize.cull <- function(TopDiam.List, LowDiam.List, Sections) {
    if (Sections == "Cull") {
      cull <- ((KozakTreeVol(Bark = "ib", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = NA, topHT = NA, topD = TopDiam.List)) -
                 (KozakTreeVol(Bark = "ib", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = NA, topHT = NA, topD = LowDiam.List))) * 35.3147
    } else if (Sections == "Saw" && TopDiam.List < pd) {
      cull <- ((KozakTreeVol(Bark = "ib", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = NA, topHT = NA, topD = TopDiam.List)) -
                 (KozakTreeVol(Bark = "ib", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = NA, topHT = NA, topD = LowDiam.List))) * 35.3147
    } else if (Sections == "Pulp" && TopDiam.List < pd) {
      cull <- ((KozakTreeVol(Bark = "ib", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = NA, topHT = NA, topD = TopDiam.List)) -
                 (KozakTreeVol(Bark = "ib", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = NA, topHT = NA, topD = LowDiam.List))) * 35.3147
    } else {
      cull <- 0
    }
  }

  # Create Merch Value Vectors ----------------------------------------------
  saw.bf <- mapply(merchandize.saw.bf, TopDiam.List, Sections)
  saw.vol <- mapply(merchandize.saw.vol, TopDiam.List, LowDiam.List, Sections)
  pulp.vol <- mapply(merchandize.pulp, TopDiam.List, LowDiam.List, Sections)
  cull.vol <- mapply(merchandize.cull, TopDiam.List, LowDiam.List, Sections)
  Section.Length <- rep(8, len = length(cull.vol))
  Low.Diam.Inch <- round(LowDiam.List * 0.393701, 2)
  Top.Diam.Inch <- round(TopDiam.List * 0.393701, 2)
  saw.bf <- round(saw.bf, 4)
  pulp.vol <- round(pulp.vol, 4)
  cull.vol <- round(cull.vol, 4)
  saw.vol <- round(saw.vol, 4)

  # Fix NaN problems in Values ----------------------------------------------
  fix_nan <- function(x) {
    x[is.nan(x)] <- 0
    x
  }

  cull.vol <- fix_nan(cull.vol)
  saw.vol <- fix_nan(saw.vol)
  pulp.vol <- fix_nan(pulp.vol)
  saw.bf <- fix_nan(saw.bf)

  # Total Merch Data For Tree By Section ------------------------------------
  merch.data <- data.frame(Low.Diam.Inch, Top.Diam.Inch, Section.Length, Sections, saw.bf, saw.vol, pulp.vol, cull.vol) # Full Tree Data Frame

  # Create Sum of Total Values For Tree -------------------------------------
  if (Cull == TRUE){
    Saw.BF.SC <- 0
    Saw.Vol.SC <- 0
    Pulp.Vol.SC <- 0
    Cull.Vol.SC <- round(sum(saw.vol, pulp.vol, cull.vol), 4)
   } else {
     Saw.BF.SC <- round(sum(saw.bf), 4)
     Saw.Vol.SC <- round(sum(saw.vol), 4)
     Pulp.Vol.SC <- round(sum(pulp.vol), 4)
     Cull.Vol.SC <- round(sum(cull.vol), 4)
   }

  Total.Vol <- KozakTreeVol(Bark = "ib", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .5, topHT = NA, topD = NA) * 35.3147
  Total.Vol <- round(Total.Vol, 4)
  Merch.Vol <- KozakTreeVol(Bark = "ib", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .5, topHT = NA, topD = pd) * 35.3147
  Merch.Vol <- round(Merch.Vol, 4)
  Percent.Sawlog.SC <- round((Saw.Vol.SC / Merch.Vol) * 100, 2)
  Method <- "Stick.Cruise"

  # Return Values -----------------------------------------------------------
  values <- data.frame(Stand, Plot, Tree, Method, SPP, Saw.BF.SC, Saw.Vol.SC, Pulp.Vol.SC, Cull.Vol.SC, Total.Vol, Merch.Vol, Percent.Sawlog.SC)
  return(values)
}
