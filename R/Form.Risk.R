#' Form.Risk Merchandizing
#'
#' This function calculates the total tree volume, merchantable volume,
#' sawlog volume, pulp volume, cull volume, and saw board feet for trees
#' using Form/Risk analysis.
#'
#' For explanation of form/risk analysis see castle. et al 2017 and
#' the NHRI Silvicultural guide for New Brunswick.
#'
#' Volumes determined using Kozak Taper Equations and Smalians Volume Formula.
#' Merch diameters establish by the MerchDiam function.
#'
#' Sawlog board feet is estimated using the international 1/4 inch rule. The length of the stem that is sawlog
#' quality is calculated based on the predicted sawlog volume using the Kozak Taper Equation.
#' The The sawlog portion of the stem is then broken into 2.4384m sections of equal length and the
#' international 1/4 inch rule is applied to each section. If the final section is longer than
#' 2.4384m but smaller than 4.8768m then that entire length will be used as the final log for
#' calculating board feet. \cr
#' object <- as.data.frame(object)\cr
#' df <- df %>% rownames_to_column()
#' %>% gather(variable, value, -rowname) %>% spread(rowname, value)\cr
#' is a useful pipe for unnesting the lists into dataframe when used with mapply.
#'
#'@details The only species this will currently execute for are Sugar Maple ("SM"),
#' Yellow Birch ("YB"), Red Maple ("RM"), and Red Oak ("RO").
#'
#'@param Stand The Unique Stand Identification Number
#'@param Plot The Unique Plot Identification Number
#'@param Tree The Unique Tree Identification Number
#'@param SPP The species idientification using FVS codes: ex 'RO' = Red Oak
#'@param DBH Diameter at breast height in cm
#'@param HT Height of tree in meters
#'@param Form Form classes 1-8 or 'GF', 'AF', 'PF' values are accepted. Defaults to 'AF'
#'@param Risk Risk class may be entered using 1-4 values or 'HR' or 'LR'. Defaults to 'LR'
#'@param Cull if tree is cull enter TRUE, defaults to FALSE
#'@family Merchandising Functions
#'@return
#' Metric, with the exception of Board Feet which is returned with imperial values.
#' ###
#' data.frame(Stand, Plot, Tree, Method, SPP, Saw.BF.FR, Saw.Vol.FR,
#' Pulp.Vol.FR, Cull.Vol.FR, Total.Vol, Merch.Vol, Percent.Sawlog.FR)
#'
#'@author Ryan Smith
#'@seealso [inventoryfunctions::KozakTreeVol]
#'@seealso [inventoryfunctions::KozakTaper]
#'@seealso [inventoryfunctions::MerchDiam]
#'
#'@examples
#' Form.Risk(1, 1, 1, 'YB', 30, 14, 1, 2)
#' Form.Risk(1, 1, 2, 'RO', 25, 12, 4, 4, TRUE)
#' Form.Risk(1, 1, 2, 'RO', 25, 12, 7, 2)
#' Form.Risk(1, 1, 3, 'SM', 40, 18, 'GF', 'LR')
#' Form.Risk(1, 1, 3, 'SM', 40, 18, 'PF', 'LR')
#' Form.Risk(1, 1, 3, 'SM', 40, 18, 'AF', 'HR', TRUE)
#'@export

Form.Risk <- function(Stand, Plot, Tree, SPP, DBH, HT, Form = "AF", Risk = "LR", Cull = FALSE) {
  as.factor(SPP)

  # Merchantable Diameters By Species ---------------------------------------

  aa <- sapply(SPP, MerchDiam)
  sd <- as.numeric(t(aa)[, 1]) # Saw Diameter
  pald <- as.numeric(t(aa)[, 2]) # Pallet Diameter
  pd <- as.numeric(t(aa)[, 3]) # Pulp Diameter

  # Intercept and DBH Coefficients
  b0 <- -32.248
  b1 <- -0.2663
  b2 <- 11.3164

  # Species Coefficients
  if (SPP == "RM") { # Red Maple
    b3 <- 0
    b6 <- 0
  }
  if (SPP == "RO") { # Red Oak
    b3 <- -1.2726
    b6 <- 0.0451
  }
  if (SPP == "SM") { # Sugar Maple
    b3 <- 0.1479
    b6 <- 0.0023
  }
  if (SPP == "YB") { # Yellow Birch
    b3 <- 0.4955
    b6 <- -0.0112
  }

  # Convert Form and Risk
  if (Form == 1) {
    b4 <- 0.3325
  }
  if (Form == 2 | Form == 7) {
    b4 <- 0
  }
  if (Form == 3 | Form == 5 | Form == 6 | Form == 8) {
    b4 <- -0.6982
  }
  if (Form == 4) {
    Cull == TRUE
  }

  if (Risk == 1 | Risk == 2) {
    b5 <- 0.9236
  }
  if (Risk == 3 | Risk == 4) {
    b5 <- 0
  }

  # Form Coefficients
  if (Form == "GF") { # Good Form, F1
    b4 <- 0.3325
  }
  if (Form == "AF") { # Acceptable Form, F2, F7
    b4 <- 0
  }
  if (Form == "PF") { # Poor Form, F3, F5,  F6, F8
    b4 <- -0.6982
  }

  # Unacceptable Forms Are Automatically CULL
  if (Form == "UF") { # F4
    Cull == TRUE
  }

  # Risk Coefficients
  if (Risk == "LR") { # Risk Classes 1 and 2
    b5 <- 0.9236
  }
  if (Risk == "HR") { # Risk Classes 3 and 4
    b5 <- 0
  }

  if (SPP %in% c("RM", "RO", "SM", "YB") && Form != "UF" && Form != 4) {
    Percent.Sawlog <- (exp(b0 + b1 * DBH + b2 * log(DBH) + b3 + b4 + b5 + b6 * DBH)) /
      (1 + exp(b0 + b1 * DBH + b2 * log(DBH) + b3 + b4 + b5 + b6 * DBH))
  }
  else {
    Percent.Sawlog <- 0
  }

  Total.Vol <- KozakTreeVol(Bark = "ob", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .1, topHT = NA, topD = NA)
  Merch.Vol <- KozakTreeVol(Bark = "ob", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .3, topHT = NA, topD = pd)

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

  # Sawlog Recovery
  if (Cull == TRUE) {
    Saw.Vol.FR <- 0
  } else if (SPP %in% c("RM", "RO", "SM", "YB")) {
    Saw.Vol.FR <- (Merch.Vol * Percent.Sawlog)
  } else {
    Saw.Vol.FR <- KozakTreeVol(Bark = "ob", SPP = SPP, DBH = DBH, HT = HT, Planted = 0,
                               stump = .3, topHT = NA, topD = sd)
  }

  # Merchandise Sawlogs BF ---------------------------------------------------
  # Find Diameter at Saw Vol
  if (Saw.Vol.FR > 0) {
    TDiam <- function(x) abs(Saw.Vol.FR - KozakTreeVol(Bark = "ob", SPP = SPP, DBH = DBH, HT = HT,
                                                    Planted = 0, stump = .3, topHT = NA, topD = x)
    )
    TD <- optimize(TDiam,
                  lower = (DBH * .1), upper = (DBH * 3),
                  maximum = FALSE, tol = .Machine$double.eps^0.25
    )
    topD <- (TD$minimum)[[1]]
  } else {
    topD <- 0
  }

  # Find Length of Log and Height from ground using Diameter at Vol
  if (topD > 0) {
      f <- function(x) abs(topD - KozakTaper("ob", SPP = SPP, x, DBH = DBH, HT = HT, Planted = 0))
      o <- optimize(f,
                    lower = (HT * .1), upper = (HT + 1),
                    maximum = FALSE, tol = .Machine$double.eps^0.25
      )
      Log.Length <- (o$minimum)[[1]] - .3
    } else {
      Log.Length <- 0
    }

    if(Log.Length > 0){
     LogHeight <- Log.Length + .3
    } else {
     LogHeight <- 0
    }

# Reduce Log Length if Log Diameter is < min saw diameter
 if(topD <= sd){
  f <- function(x) abs(sd - KozakTaper("ob", SPP = SPP, x, DBH = DBH, HT = HT, Planted = 0))
  o <- optimize(f,
                lower = (HT * .1), upper = (HT + 1),
                maximum = FALSE, tol = .Machine$double.eps^0.25
  )
    Log.Length <- (o$minimum)[[1]] - .3
    } else {
    Log.Length <- Log.Length
   }

  # Create Logs
  if(Log.Length < 2.4384){
    Log.Length <- 0
   } else if(Log.Length == 2.4384) {
    Log.Length <- 2.43840001
   } else {
    Log.Length <- Log.Length
   }

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
  if (Log.Length > 0) {

    LogBF <- mapply(board.feet, TopDiam, Logs)

    #BFtext <- "BF in Log"
    #print(paste(BFtext, LogBF, sep = " - "))

    saw.bf <- sum(LogBF)
  } else {
    saw.bf <- 0
  }
  Saw.BF <- saw.bf

# Pulp Recovery
  if (Cull == TRUE) {
    Pulp.Vol.FR <- 0
  } else {
    Pulp.Vol.FR <- (Merch.Vol - Saw.Vol.FR)
  }

  # Cull Volume
  if (Cull == FALSE) {
    Cull.Vol.FR <- (Total.Vol - Merch.Vol)
  } else {
    Cull.Vol.FR <- Total.Vol
  }

  # Fix NaN problems in Values ----------------------------------------------
  fix_nan <- function(x) {
    x[is.nan(x)] <- 0
    x
  }

  Cull.Vol.FR <- fix_nan(Cull.Vol.FR)
  Saw.Vol.FR <- fix_nan(Saw.Vol.FR)
  Pulp.Vol.FR <- fix_nan(Pulp.Vol.FR)
  Saw.BF <- fix_nan(Saw.BF)

  # Return Values
  Saw.BF.FR <- round(Saw.BF, 4)
  Saw.Vol.FR <- round(Saw.Vol.FR, 4)
  Pulp.Vol.FR <- round(Pulp.Vol.FR, 4)
  Cull.Vol.FR <- round(Cull.Vol.FR, 4)
  Total.Vol <- round(Total.Vol, 4)
  Merch.Vol <- round(Merch.Vol, 4)
  Percent.Sawlog.FR <- round(Percent.Sawlog * 100, 2)
  Method <- "Form.Risk"

  values <- data.frame(Stand, Plot, Tree, SPP, LogHeight, LogCount, Method, Saw.BF.FR, Saw.Vol.FR, Pulp.Vol.FR, Cull.Vol.FR, Total.Vol, Merch.Vol, Percent.Sawlog.FR)
  return(values)
}
