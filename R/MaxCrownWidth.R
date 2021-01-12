#' Maximum Crown Width
#'
#' This function returns the maximum crown width for an open grown tree
#' for the most common species found in the acadian forest.
#'
#' SPP included: BF, BS, EH, WP, NC, RS, WS, AB, GB, RB, RO, PB, QA, RM, SM, YB.
#' SPP that are not included can be enter as 'OH' for hardwoods and 'OS' for softwoods.
#' If no SPP is entered a standard coefficent will be used but may not reflect the
#' crown conditions accurately - please enter 'OS' or 'OH' if species is not included.
#'
#'@param SPP Tree Species: use the FVS code
#'@param DBH Diameter at breast height in inches
#'
#'@return The maximum crown width for the tree.
#'
#'@export


MCW <- function(SPP, DBH) {
  if (sp == "BF") {
    a1 <- 1.37
    a2 <- 0.572
  }
  else if (sp == "BS") {
    a1 <- 0.535
    a2 <- 0.742
  }
  else if (sp == "EH") {
    a1 <- 2.44
    a2 <- 0.408
  }
  else if (sp == "WP") {
    a1 <- 1.24
    a2 <- 0.585
  }
  else if (sp == "NC") {
    a1 <- 1.63
    a2 <- 0.436
  }
  else if (sp == "RS") {
    a1 <- 1.80
    a2 <- 0.461
  }
  else if (sp == "WS") {
    a1 <- 1.50
    a2 <- 0.496
  }
  else if (sp == "AB") {
    a1 <- 2.93
    a2 <- 0.434
  }
  else if (sp == "GB" | sp == "RB") {
    a1 <- 2.24
    a2 <- 0.382
  }
  else if (sp == "RO") {
    a1 <- 4.08
    a2 <- 0.310
  }
  else if (sp == "PB") {
    a1 <- 1.48
    a2 <- 0.623
  }
  else if (sp == "QA") {
    a1 <- 1.31
    a2 <- 0.586
  }
  else if (sp == "RM") {
    a1 <- 2.17
    a2 <- 0.491
  }
  else if (sp == "SM") {
    a1 <- 3.31
    a2 <- 0.356
  }
  else if (sp == "YB") {
    a1 <- 4.04
    a2 <- 0.308
  }
  else if (sp == "OH") {
    a1 <- 4.04
    a2 <- 0.308
  }
  else if (sp == "OS") {
    a1 <- 1.597128571
    a2 <- 0.513957143
  }
  else {
    a1 <- 2.24262
    a2 <- 0.462653333
  }
  mcw <- a1 * DBH**a2
  return(MCW)
}
