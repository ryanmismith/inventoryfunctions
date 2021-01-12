#' Form.Risk Merchandizing
#'
#' This function calculates the total tree volume, merchantable volume,
#' sawlog volume, pulp volume, cull volume, and saw board feet for trees
#' using Form/Risk analysis.
#'
#' For explaination of form/risk analsis see castle. et al 2017 and
#' the NHRI Silvicultural guide for New Brunswick.
#'
#' Volumes determined using Kozak Taper Equations and Smalians Volume Formula.
#' Merch diameters establish by the merc function.
#'
#' df <- df %>% rownames_to_column()
#' %>% gather(variable, value, -rowname) %>% spread(rowname, value)
#' is a useful pipe for unnesting the lists into dataframe when used with mapply.
#'
#'
#'@param Stand The Unique Stand Identification Number
#'@param Plot The Unique Plot Identification Number
#'@param Tree The Unique Tree Identification Number
#'@param SPP The species idientification using FVS codes: ex 'RO' = Red Oak
#'@param DBH Diameter at breast height in cm
#'@param Stump Stump height in meters. Recommended value if not measured is .5
#'@param Form Form classes 1-8 or 'GF', 'AF', 'PF' values are accepted. Defaults to 'AF'
#'@param Risk Risk class may be entered using 1-4 values or 'HR' or 'LR'. Defaults to 'LR'
#'@param CULL if tree is cull enter TRUE, defaults to FALSE
#'
#'@return
#' data.frame(Stand, Plot, Tree, Method, SPP, Saw.BF.FR, Saw.Vol.FR,
#' Pulp.Vol.FR, Cull.Vol.FR, Total.Vol, Merch.Vol, Percent.Sawlog.FR)
#'
#'@examples
#' Form.Risk(1, 1, 1, 'RS', 30, 14, .5, 1, 2)
#' Form.Risk(1, 1, 2, 'RO', 25, 12, .5, 4, 4, TRUE)
#' Form.Risk(1, 1, 2, 'RO', 25, 12, .5, 7, 2)
#' Form.Risk(1, 1, 3, 'SM', 40, 18, .5, 'GF', 'LR')
#' Form.Risk(1, 1, 3, 'SM', 40, 18, .5, 'PF', 'LR')
#' Form.Risk(1, 1, 3, 'SM', 40, 18, .5, 'AF', 'HR', TRUE)
#'@export

Form.Risk <- function(Stand, Plot, Tree, SPP, DBH, HT, Stump, Form = "AF", Risk = "LR", Cull = FALSE) {
  as.factor(SPP)

  # Merchantable Diameters By Species ---------------------------------------

  aa <- sapply(SPP, merc)
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

  Total.Vol <- KozakTreeVol(Bark = "ib", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .5, topHT = NA, topD = NA) * 35.3147
  Merch.Vol <- KozakTreeVol(Bark = "ib", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .5, topHT = NA, topD = pd) * 35.3147

  # Height at Diameter Function --------------------------------------------------------

  if (sd > 0) {
    f <- function(x) abs(sd - KozakTaper("ib", SPP = SPP, x, DBH = DBH, HT = HT, Planted = 0))
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
  Diam.4 <- sd

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
    Saw.Vol.FR <- KozakTreeVol(Bark = "ib", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .5, topHT = NA, topD = sd) * 35.3147
  }

  # Merchandise Sawlogs BF ---------------------------------------------------
  if (Saw.Vol.FR > 0) {
    saw.bf <- board.feet(Diam.1, Log) + board.feet(Diam.2, Log) +
      board.feet(Diam.3, Log) + board.feet(Diam.4, Log)
  } else {
    saw.bf <- 0
  }

  Saw.BF <- saw.bf * Percent.Sawlog

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

  # Return Values
  Saw.BF.FR <- round(Saw.BF, 4)
  Saw.Vol.FR <- round(Saw.Vol.FR, 4)
  Pulp.Vol.FR <- round(Pulp.Vol.FR, 4)
  Cull.Vol.FR <- round(Cull.Vol.FR, 4)
  Total.Vol <- round(Total.Vol, 4)
  Merch.Vol <- round(Merch.Vol, 4)
  Percent.Sawlog.FR <- round(Percent.Sawlog * 100, 2)
  Method <- "Form.Risk"

  values <- data.frame(Stand, Plot, Tree, Method, SPP, Saw.BF.FR, Saw.Vol.FR, Pulp.Vol.FR, Cull.Vol.FR, Total.Vol, Merch.Vol, Percent.Sawlog.FR)
  return(values)
}
