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
#' stump height of .5 meters. The maximum number of Sawlogs that can be called is 10. You
#' can set a section to begin your sawlog count at using the StartingLog param. This is useful
#' if the first 2.4384 meter section of the stem has disqualifying defect. If no value is
#' entered for a StartingLog then the measurements will begin at the .5m stump.
#' ###
#' The second option is to call each 2.4384 meter individual section of the tree. The maximum number
#' of section calls is 10. Beginning with S1, each section may be entered as "Saw", "Pulp",
#' or "Cull". You may call as few sections as you would like, and each uncalled merchantable section of the stem
#' will default to "Pulp". The default stump height is .5 meters.
#' ###
#' Using the second input option will give you a more accurate volume and sawlog board foot
#' estimate if there is a "Pulp" or "Cull" call before or between Sawlog calls.
#' If this option is used no argument should be entered for the Sawlog or StartingLog params.
#' ###
#' This function will automatically downgrade products to pulp for sections where a
#' sawlog product call is made but where the small end inside bark diameter of the section does
#' not meet minimum sawlog dimensions for the species. Merch dimensions can be altered and species can be added
#' by altering the internal MerchDiam function. Outside bark diameters are measured using the Kozak Taper Equation.
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
#'@param S2... You can call as many as 10 'S#' sections.
#'@param Cull if it is a cull tree than enter TRUE, else enter FALSE. Default is FALSE.
#'@param Sawlogs If each individual section is not called, you may use this
#' input to call the total number of Sawlogs in the tree. If individual sections are called
#' no Sawlog argument should be entered.
#'@param StartingLog If the 8 foot section where sawlogs begin is recorded you can use
#' StartingLog to name which section to begin your Sawlogs input at. This can only be used
#' with the Sawlogs parameter and should not be used if individual sections are called.
#'@param UnlimLog If all sections that meet diameter requirements
#'are sawlogs enter UnlimLog as TRUE.
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
#'Stick.Cruise.Tree(1, 1, 1, 'RO', 30, 16, S1 = 'Saw', S2 = 'Pulp', S3 = 'Saw')
#'Stick.Cruise.Tree(1, 2, 2, 'RO', 68, 26, 'Pulp', 'Saw', 'Saw', Pulp,
#'                  S5 = "Saw", S6 = "Saw", S7 = "Pulp" S8 = "Cull")
#'Stick.Cruise.Tree(857, 9, 16, 'SM', 62, 24, S3 = "Saw", S5 = "Saw")
#'Stick.Cruise.Tree(1, 1, 1, 'RO', 30, 16, Sawlogs = 2)
#'Stick.Cruise.Tree(1, 1, 1, 'RO', 30, 16, Sawlogs = 2, StartingLog = 2)
#'Stick.Cruise.Tree(1, 1, 1, 'RO', 30, 16, Cull = TRUE)
#'Stick.Cruise.Tree(1, 1, 1, 'RO', 30, 16)
#'Stick.Cruise.Tree(1, 1, 1, 'AB', 30, 16, 'Saw', 'Pulp', 'Saw')
#'Stick.Cruise.Tree(1, 1, 1, 'RS', 30, 16, 'Pulp', 'Saw', 'Saw')
#'Stick.Cruise.Tree(1, 1, 1, 'RS', 30, 16, Sawlogs = 2)
#'Stick.Cruise.Tree(1, 1, 1, 'AB', 30, 16, Sawlogs = 5, StartingLog = 3)
#'
#'@export

Stick.Cruise <- function(Stand, Plot, Tree, SPP, DBH, HT, S1 = "Pulp", S2 = "Pulp",
                              S3 = "Pulp", S4 = "Pulp", S5 = "Pulp", S6 = "Pulp", S7 = "Pulp",
                              S8 = "Pulp", S9 = "Pulp", S10 = "Pulp", Cull = FALSE,
                              Sawlogs = FALSE, StartingLog = FALSE, UnlimLog = FALSE, Veneer = FALSE){

  # Merchantable Diameter For Species ---------------------------------------
  aa <- sapply(SPP, MerchDiam)
  sd <- as.numeric(t(aa)[, 1]) # Saw Diameter
  pald <- as.numeric(t(aa)[, 2]) # Pallet Diameter
  pd <- as.numeric(t(aa)[, 3]) # Pulp Diameter

  # Stump Height of .3 Meters (1 ft) - 8 Foot Sections

  eight.ft <- seq(.3, 48.568, 2.4384)
  a <- seq(.3, 48.568, 2.4384)
  temp <- data.frame(eight.ft, a)


  for (i in 1:length(temp$eight.ft)){
    if (HT > temp$eight.ft[i]) {
      temp$TopDiam.List[i] <- KozakTaper(Bark = "ob", SPP, DHT = temp$eight.ft[i+1], DBH, HT, Planted = 0)
    } else {
      temp$TopDiam.List[i] <- 0
    }
  }

  # Fix NaN problems in Values ----------------------------------------------
  fix_nan <- function(x) {
    x[is.nan(x)] <- 0
    x
  }

  TopDiam.List <- temp$TopDiam.List
  TopDiam.List <- fix_nan(TopDiam.List)

  # Create List of Lower Diameter of Each Section
  stump.diam <- KozakTaper(Bark = "ob", SPP, DHT = .3, DBH, HT, Planted = 0)
  LowDiam.List <- dplyr::lag(TopDiam.List, default = stump.diam)
  # Create list of Section Calls (Saw, Pulp, Cull)

  Sections <- c(S1, S2, S3, S4, S5, S6, S7, S8, S9, S10,
                "Pulp", "Pulp", "Pulp", "Pulp", "Pulp", "Pulp", "Pulp", "Pulp", "Pulp", "Pulp")


  for(i in 1:length(Sections)){
    if(Sections[i] != "Saw" && Sections[i] != "Cull"){
      Sections[i] == "Pulp"
    }
  }


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

  # Merchandize Sawlogs BF ---------------------------------------------------
  #If Sawlogs are called by count
  if(Sawlogs > 0 && StartingLog > 0) {
    Sections[StartingLog:(Sawlogs + StartingLog)] <- "Saw"
    Sections[(Sawlogs + StartingLog):20] <- "Pulp"
  } else if(Sawlogs > 0 && StartingLog == FALSE) {
    Sections[1:Sawlogs] <- "Saw"
    Sections[(Sawlogs + 1):20] <- "Pulp"
  } else {
    Sections <- Sections
  }

  if(UnlimLog == TRUE){
    for(i in 1:length(Sections)){
      if(TopDiam.List[i] >= sd){
        Sections[i] <- "Saw"
      } else {
        Sections[i] <- Sections[i]
      }
    }
  }

  merchandize.saw.bf <- function(TopDiam, Sections) {
    if (Sections == "Saw" && TopDiam >= sd) {
      saw.bf <- board.feet(TopDiam)
      saw.bf <- round(saw.bf, 4)
    } else {
      saw.bf <- 0
    }
  }

  # Merchandize Saw Vol --------------------------------------------------------
  merchandize.saw.vol <- function(TopDiam.List, LowDiam.List, Sections) {
    if (Sections == "Saw" && TopDiam.List >= sd) {
      saw.vol <- ((KozakTreeVol(Bark = "ob", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .3, topD = TopDiam.List)) -
                    (KozakTreeVol(Bark = "ob", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .3, topD = LowDiam.List)))
    } else {
      saw.vol <- 0
    }
  }

  # Merchandize Pulp Vol--------------------------------------------------------
  merchandize.pulp <- function(TopDiam.List, LowDiam.List, Sections) {
    if (Sections == "Pulp" && TopDiam.List >= pd) {
      pulp <- ((KozakTreeVol(Bark = "ob", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .3, topHT = NA, topD = TopDiam.List)) -
                 (KozakTreeVol(Bark = "ob", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .3, topHT = NA, topD = LowDiam.List)))
    } else if (Sections == "Saw" && TopDiam.List < sd && TopDiam.List >= pd) {
      pulp <- ((KozakTreeVol(Bark = "ob", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .3, topHT = NA, topD = TopDiam.List)) -
                 (KozakTreeVol(Bark = "ob", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .3, topHT = NA, topD = LowDiam.List)))
    } else {
      pulp <- 0
    }
  }

  # Cull Vol ----------------------------------------------------------------
  merchandize.cull <- function(TopDiam.List, LowDiam.List, Sections) {
    if (Sections == "Cull") {
      cull <- KozakTreeVol(Bark = "ob", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .1, topD = NA)
    } else if (Sections == "Saw" && TopDiam.List < pd && TopDiam.List > 1) {
      cull <- ((KozakTreeVol(Bark = "ob", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .1, topD = TopDiam.List)) -
                 (KozakTreeVol(Bark = "ob", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .1, topD = LowDiam.List)))
    } else if (Sections == "Pulp" && TopDiam.List < pd && TopDiam.List > 1) {
      cull <- ((KozakTreeVol(Bark = "ob", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .1, topD = TopDiam.List)) -
                 (KozakTreeVol(Bark = "ob", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .1, topD = LowDiam.List)))
    } else {
      cull <- 0
    }
  }

  # Create Merch Value Vectors ----------------------------------------------
  saw.bf <- mapply(merchandize.saw.bf, TopDiam.List, Sections)
  # print(saw.bf)
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

  cull.vol <- fix_nan(cull.vol)
  saw.vol <- fix_nan(saw.vol)
  pulp.vol <- fix_nan(pulp.vol)
  saw.bf <- fix_nan(saw.bf)

  # Total Merch Data For Tree By Section ------------------------------------
   merch.data <- data.frame(Low.Diam.Inch, Top.Diam.Inch, Section.Length, Sections, saw.bf, saw.vol, pulp.vol, cull.vol) # Full Tree Data Frame
   print(merch.data)
  # Create Sum of Total Values For Tree -------------------------------------
  if (Cull == TRUE){
    Saw.BF <- 0
    Saw.Vol <- 0
    Pulp.Vol <- 0
    Cull.Vol <- KozakTreeVol(Bark = "ob", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .1, topD = NA)
  } else {
    Saw.BF <- round(sum(saw.bf), 4)
    Saw.Vol <- round(sum(saw.vol), 4)
    Pulp.Vol <- round(sum(pulp.vol), 4)
    Cull.Vol <- round(sum(cull.vol), 4)
  }

  Total.Vol <- KozakTreeVol(Bark = "ob", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .1, topD = NA)
  Total.Vol <- round(Total.Vol, 4)
  Merch.Vol <- KozakTreeVol(Bark = "ob", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .3, topD = pd)
  Merch.Vol <- round(Merch.Vol, 4)
  Cull.Vol <- Cull.Vol + (Total.Vol - (Cull.Vol + Saw.Vol + Pulp.Vol))
  Percent.Sawlog <- round((Saw.Vol / Merch.Vol) * 100, 2)
  Percent.Sawlog <- fix_nan(Percent.Sawlog)
  Method <- "Stick Cruise"

  # Return Values -----------------------------------------------------------
  values <- data.frame(Stand, Plot, Tree, Method, SPP, Saw.BF, Saw.Vol, Pulp.Vol, Cull.Vol, Total.Vol, Merch.Vol, Percent.Sawlog)
  return(values)
}

