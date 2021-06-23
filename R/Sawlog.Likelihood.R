#' Likelihood of Having A Sawlog
#'
#' For explanation of form/risk analysis see Castle. et al 2017 . This function
#' is meant to supplement the Form.Risk merchandising function.
#'
#'
#'@details The only species this will currently execute for are Sugar Maple ("SM"),
#' Yellow Birch ("YB"), Red Maple ("RM"), and Red Oak ("RO").
#'###
#'These values can be used in a multitude of ways, but if you are looking for binary TRUE/FALSE results
#'for if a tree is likely to produce a sawlog, the optimal cutoff point for determining whether or not a tree
#'has a sawlog can be found using the coords function
#'in the pRoc package. Cutpoints that have been used with exceptionally high AUC values (greater than .85)
#'in Northern Maine are:\cr
#'Red Maple: Cutpoint 0.737935\cr
#'Sugar Maple: Cutpoint 0.8500554\cr
#'Yellow Birch: Cutpoint 0.8586569\cr
#'
#'
#'@param SPP The species identification using FVS codes: ex 'RO' = Red Oak
#'@param DBH Diameter at breast height in cm
#'@param Form Form classes 1-8 or 'GF', 'AF', 'PF' values are accepted.
#'@param Risk Risk class may be entered using 1-4 values or 'HR' or 'LR'.
#'@family Merchandising Functions
#'@return
#'Returns a predictive value to be used in AUC analysis to determine the likelihood that a tree contains a sawlog.
#'
#'@author Ryan Smith
#'@seealso [inventoryfunctions::Form.Risk]
#'
#'@examples
#'Sawlog.Likelihood("RO", 42, 1, 1)
#'@export


Sawlog.Likelihood <- function(SPP, DBH, Form, Risk){
  if(!(SPP %in% c("RO", "RM", "YB", "SM"))){
    stop("This function only runs for RO, RM, YB, and SM Species. All other species should be excluded from
    the analysis.")
  }
  # Intercept and DBH Coefficients
  b0 <- -3.4273
  b1 <- .0837

  # Species Coefficients
  if (SPP == "RM") { # Red Maple
    b2 <- 0
    b5 <- 0
  }
  if (SPP == "RO") { # Red Oak
    b2 <- -8.1868
    b5 <- 0.2989
  }
  if (SPP == "SM") { # Sugar Maple
    b2 <- 0.9316
    b5 <- -.0102
  }
  if (SPP == "YB") { # Yellow Birch
    b2 <- 1.8234
    b5 <- -0.0411
  }

  # Convert Form and Risk
  if (Form == 1) {
    b3 <- .6043
  }
  if (Form == 2 | Form == 7) {
    b3 <- 0
  }
  if (Form == 3 | Form == 5 | Form == 6 | Form == 8 | Form == 4) {
    b3 <- -1.7642
  }

  if (Risk == 1 | Risk == 2) {
    b4 <- 1.7642
  }
  if (Risk == 3 | Risk == 4) {
    b4 <- 0
  }

  # Form Coefficients
  if (Form == "GF") { # Good Form, F1
    b3 <- .6043
  }
  if (Form == "AF") { # Acceptable Form, F2, F7
    b3 <- 0
  }
  if (Form == "PF") { # Poor Form, F3, F5,  F6, F8
    b3 <- -1.7642
  }

  # Risk Coefficients
  if (Risk == "LR") { # Risk Classes 1 and 2
    b4 <- 1.7642
  }
  if (Risk == "HR") { # Risk Classes 3 and 4
    b4 <- 0
  }

 Liklihood <- (exp(b0 + (b1*DBH) + b2 + b3 + b4 + b5*DBH) /
              (1 + exp(b0 + (b1*DBH) + b2 + b3 + b4 + b5*DBH)))

 return(Liklihood)
}

