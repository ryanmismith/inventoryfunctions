#' Stick Cruise Merchandising
#'
#' This function calculates the total tree volume, merchantable volume,
#' sawlog volume, pulp volume, cull volume, and saw board feet for trees
#' using stick cruise measurements. In a stick cruise each 2.4384 meter section of the tree
#' is called out as either potential saw, pulp, or cull. Up to 19 sections may be entered.
#' See details for input options.
#'
#' @details
#' Sawlogs may be entered in two different fashions.
#' ###
#' The first option is to simply enter the total number of sawlogs called.
#' When the argument Sawlogs = # is entered the function will call that
#' number of 2.4384 meter sections of the stem as Sawlog grade beginning at a default
#' atump height of .5 meters. The maximum number of Sawlogs that can be called is 19.
#' This option assumes Sawlogs will be in the lower sections of the stem.
#' ###
#' The second option is to call each 2.4384 meter individual section of the tree. The maximum number
#' of section calls is 19. Beginning with S1, each section may be entered as "Saw", "Pulp",
#' or "Cull". You may call as few sections as you would like, and each uncalled merchantable section of the stem
#' will default to "Pulp". The default stump height is .5 meters.
#' ###
#' Using the second input option will give you a more accurate volume and sawlog board foot
#' estimate if there is a "Pulp" or "Cull" call before or between Sawlog calls.
#' If this option is used no argument should be entered for the Sawlog parameter.
#' ###
#' This function will automatically downgrade products to pulp for sections where a
#' sawlog product call is made but where the small end inside bark diameter of the section does
#' not meet minimum sawlog dimensions for the species. Merch dimensions can be altered and species can be added
#' by altering the internal MerchDiam function. Inside bark diameters are measured using the Kozak Taper Equation.
#' ###
#' Sawlog board feet is estimated using the international 1/4 inch rule. The international 1/4 inch rule is applied
#' to each eight foot section that is called as "Saw" and which meets diameter requirements
#' ###
#' df <- df %>% rownames_to_column()
#' %>% gather(variable, value, -rowname) %>% spread(rowname, value)
#' is a useful pipe for unnesting the lists into a dataframe when this function
#' is used for a large number of trees using the mapply function.
#'
#'
#'@param Stand The Unique Stand Identification Number
#'@param Plot The Unique Plot Identification Number
#'@param Tree The Unique Tree Identification Number
#'@param SPP The species identification using FVS codes: ex 'RO' = Red Oak
#'@param DBH Diameter at breast height in cm
#'@param HT Tree height in meters
#'@param S1 Each section called out as 'Saw', 'Pulp', or 'Cull'.
#'@param S2... You can call as many as 20 sections.
#'@param Cull if it is a cull tree than enter TRUE, else enter FALSE
#'@param Sawlogs If each individual section is not called, you may use this
#' input to call the total number of Sawlogs in the tree. If individual sections are called
#' no Sawlog argument should be entered.
#'
#'@return
#'Metric, with the exception of Board Feet which is returned with imperial values.
#'###
#'data.frame(Stand, Plot, Tree, Method, SPP, Saw.BF, Saw.Vol,
#'Pulp.Vol, Cull.Vol, Total.Vol, Merch.Vol, Percent.Sawlog)
#'
#'@seealso [inventoryfunctions::KozakTreeVol]
#'@seealso [inventoryfunctions::KozakTaper]
#'@seealso [inventoryfunctions::MerchDiam]
#'
#'@family Merchandising Functions
#'@author Ryan Smith
#'@examples
#'Stick.Cruise.Tree(1, 1, 1, 'RO', 30, 16, 'Saw', 'Pulp', 'Saw')
#'Stick.Cruise.Tree(1, 1, 1, 'RO', 30, 16, 'Pulp', 'Saw', 'Saw')
#'Stick.Cruise.Tree(1, 1, 1, 'RO', 30, 16, Sawlogs = 2)
#'Stick.Cruise.Tree(1, 1, 1, 'RO', 30, 16, Cull = True)
#'Stick.Cruise.Tree(1, 1, 1, 'RO', 30, 16)
#'Stick.Cruise.Tree(1, 1, 1, 'RO', 30, 16, Sawlogs = 5)
#'Stick.Cruise.Tree(1, 1, 1, 'AB', 30, 16, 'Saw', 'Pulp', 'Saw')
#'Stick.Cruise.Tree(1, 1, 1, 'RS', 30, 16, 'Pulp', 'Saw', 'Saw')
#'Stick.Cruise.Tree(1, 1, 1, 'RS', 30, 16, Sawlogs = 2)
#'Stick.Cruise.Tree(1, 1, 1, 'RO', 30, 16, Cull = True)
#'Stick.Cruise.Tree(1, 1, 1, 'QA', 30, 16)
#'Stick.Cruise.Tree(1, 1, 1, 'AB', 30, 16, Sawlogs = 5)
#'
#'@export

Stick.Cruise.Tree <- function(Stand, Plot, Tree, SPP, DBH, HT, S1 = "Pulp", S2 = "Pulp",
                              S3 = "Pulp", S4 = "Pulp", S5 = "Pulp", S6 = "Pulp",
                              S7 = "Pulp", S8 = "Pulp", S9 = "Pulp",
                              S10 = "Pulp", S11 = "Pulp", S12 = "Pulp",
                              S13 = "Pulp", S14 = "Pulp", S15 = "Pulp", S16 = "Pulp", S17 = "Pulp",
                              S18 = "Pulp", S19 = "Pulp", S20 = "Pulp", Stump = .5, Cull = FALSE, Sawlogs = FALSE, Veneer = FALSE) {

  # Merchantable Diameter For Species ---------------------------------------
  aa <- sapply(SPP, MerchDiam)
  sd <- as.numeric(t(aa)[, 1]) # Saw Diameter
  pald <- as.numeric(t(aa)[, 2]) # Pallet Diameter
  pd <- as.numeric(t(aa)[, 3]) # Pulp Diameter

  # Stump Height of .5 Meters (1.64 ft) - 8 Foot Sections
  eight.ft <- seq(.5, 48.768, 2.4384)
  a <- seq(.5, 48.768, 2.4384)
  temp <- data.frame(eight.ft, a)

  for (i in 1:length(temp$eight.ft)){
    if (HT > temp$eight.ft[i]) {
      temp$HeightList[i] <- temp$eight.ft[i+1]
    } else {
      temp$HeightList[i] <- (.0001)
    }
  }
  for (i in 1:length(temp$eight.ft)){
    if (HT > temp$eight.ft[i]) {
      temp$LowHeight[i] <- temp$eight.ft[i]
    } else {
      temp$LowHeight[i] <- (.0001)
    }
  }
  for (i in 1:length(temp$eight.ft)){
    if (HT > temp$eight.ft[i]) {
      temp$TopDiam.List[i] <- KozakTaper(Bark = "ib", SPP, DHT = temp$eight.ft[i+1], DBH, HT, Planted = 0)
    } else {
      temp$TopDiam.List[i] <- 0
    }
  }
  # Fix NaN problems in Values ----------------------------------------------
  fix_nan <- function(x) {
    x[is.nan(x)] <- 0
    x
  }

  HeightList <- temp$HeightList
  LowHeight <- temp$LowHeight
  TopDiam.List <- temp$TopDiam.List
  TopDiam.List <- fix_nan(TopDiam.List)

  # Create List of Lower Diameter of Each Section
  stump.diam <- KozakTaper(Bark = "ib", SPP, DHT = .5, DBH, HT, Planted = 0)
  LowDiam.List <- dplyr::lag(TopDiam.List, default = stump.diam)

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
  #If Sawlogs are caleld by count
  if (Sawlogs == FALSE) {
    Sections <- Sections
  } else {
    Sections[1:Sawlogs] <- "Saw"
    Sections[(Sawlogs + 1):20] <- "Pulp"
  }

  merchandize.saw.bf <- function(TopDiam.List, Sections) {
    if (Sections == "Saw" && TopDiam.List >= sd) {
      saw.bf <- board.feet(TopDiam.List)
      saw.bf <- round(saw.bf, 4)
    } else {
      saw.bf <- 0
    }
  }

  # Merchandize Saw Vol --------------------------------------------------------
  merchandize.saw.vol <- function(TopDiam.List, LowDiam.List, Sections, HeightList, LowHeight) {
    if (Sections == "Saw" && TopDiam.List >= sd) {
      saw.vol <- ((KozakTreeVol(Bark = "ib", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .5, topHT = HeightList, topD = NA)) -
                    (KozakTreeVol(Bark = "ib", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .5, topHT = LowHeight, topD = NA)))
    } else {
      saw.vol <- 0
    }
  }

  # Merchandize Pulp Vol--------------------------------------------------------
  merchandize.pulp <- function(TopDiam.List, LowDiam.List, Sections, HeightList, LowHeight) {
    if (Sections == "Pulp" && TopDiam.List >= pd) {
      pulp <- ((KozakTreeVol(Bark = "ib", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .5, topHT = HeightList, topD = NA)) -
                 (KozakTreeVol(Bark = "ib", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .5, topHT = LowHeight, topD = NA)))
    } else if (Sections == "Saw" && TopDiam.List < sd && TopDiam.List >= pd) {
      pulp <- ((KozakTreeVol(Bark = "ib", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .5, topHT = HeightList, topD = NA)) -
                 (KozakTreeVol(Bark = "ib", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .5, topHT = LowHeight, topD = NA)))
    } else {
      pulp <- 0
    }
  }

  # Cull Vol ----------------------------------------------------------------
  merchandize.cull <- function(TopDiam.List, LowDiam.List, Sections, HeightList, LowHeight) {
    if (Sections == "Cull") {
      cull <- ((KozakTreeVol(Bark = "ib", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .5, topHT = HeightList, topD = NA)) -
                 (KozakTreeVol(Bark = "ib", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .5, topHT = LowHeight, topD = NA)))
    } else if (Sections == "Saw" && TopDiam.List < pd) {
      cull <- ((KozakTreeVol(Bark = "ib", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .5, topHT = HeightList, topD = NA)) -
                 (KozakTreeVol(Bark = "ib", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .5, topHT = LowHeight, topD = NA)))
    } else if (Sections == "Pulp" && TopDiam.List < pd) {
      cull <- ((KozakTreeVol(Bark = "ib", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .5, topHT = HeightList, topD = NA)) -
                 (KozakTreeVol(Bark = "ib", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .5, topHT = LowHeight, topD = NA)))
    } else {
      cull <- 0
    }
  }

  # Create Merch Value Vectors ----------------------------------------------
  saw.bf <- mapply(merchandize.saw.bf, TopDiam.List, Sections)
  saw.vol <- mapply(merchandize.saw.vol, TopDiam.List, LowDiam.List, Sections, HeightList, LowHeight)
  pulp.vol <- mapply(merchandize.pulp, TopDiam.List, LowDiam.List, Sections, HeightList, LowHeight)
  cull.vol <- mapply(merchandize.cull, TopDiam.List, LowDiam.List, Sections, HeightList, LowHeight)
  Section.Length <- rep(8, len = length(cull.vol))
  Low.Diam.Inch <- round(LowDiam.List * 0.393701, 2)
  Top.Diam.Inch <- round(TopDiam.List * 0.393701, 2)
  saw.bf <- round(saw.bf, 4)
  pulp.vol <- round(pulp.vol, 4)
  cull.vol <- round(cull.vol, 4)
  saw.vol <- round(saw.vol, 4)

  cull.vol <- fix_nan(cull.vol)
  saw.vol <- fix_nan(saw.vol)
  pulp.vol <- fix_nan(pulp.vol)
  saw.bf <- fix_nan(saw.bf)

  # Total Merch Data For Tree By Section ------------------------------------
  merch.data <- data.frame(Low.Diam.Inch, Top.Diam.Inch, Section.Length, Sections, saw.bf, saw.vol, pulp.vol, cull.vol) # Full Tree Data Frame

  # Create Sum of Total Values For Tree -------------------------------------
  if (Cull == TRUE){
    Saw.BF <- 0
    Saw.Vol <- 0
    Pulp.Vol <- 0
    Cull.Vol <- round(sum(saw.vol, pulp.vol, cull.vol), 4)
  } else {
    Saw.BF <- round(sum(saw.bf), 4)
    Saw.Vol <- round(sum(saw.vol), 4)
    Pulp.Vol <- round(sum(pulp.vol), 4)
    Cull.Vol <- round(sum(cull.vol), 4)
  }

  Total.Vol <- KozakTreeVol(Bark = "ib", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .5, topHT = NA, topD = NA)
  Total.Vol <- round(Total.Vol, 4)
  Merch.Vol <- KozakTreeVol(Bark = "ib", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .5, topHT = NA, topD = pd)
  Merch.Vol <- round(Merch.Vol, 4)
  Percent.Sawlog <- round((Saw.Vol / Merch.Vol) * 100, 2)
  Method <- "Stick Cruise"

  # Return Values -----------------------------------------------------------
  values <- data.frame(Stand, Plot, Tree, Method, SPP, Saw.BF, Saw.Vol, Pulp.Vol, Cull.Vol, Total.Vol, Merch.Vol, Percent.Sawlog)
  return(values)
}

