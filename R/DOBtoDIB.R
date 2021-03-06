#' Diameter Outside Bark to Diameter Inside Bark
#'
#' This function converts the outside bark diameter measurement (cm) of a tree to the diameter inside bark (cm).
#' Useful for inside bark volumes or for making conversions to set minimal merchantable diamters in
#' FVS which only allows for IB diameters.
#'
#' @details All diameters must be metric (cm). Includes species found in the acadian forest.
#'
#' @param SPP Species: use FVS species codes: example 'RO' - Red Oak, 'WS' = White Spruce
#' @param dob Outside bark diameter of a location on the tree.
#'
#' @seealso [inventoryfunctions::EXP.F]
#' @family Volume Functions
#'
#' @examples
#'DOBtoDIB("SM", 25.4)
#'DOBtoDIB("RS", 15)
#'DOBtoDIB("PB", 12)
#'
#' @export

DOBtoDIB <- function(SPP, dob) {
  if (SPP == "AB") {
    pcntbark <- 7
    b0_bark <- 1
    b1_bark <- 1
  }
  else if (SPP == "BC") {
    pcntbark <- 10
    b0_bark <- 1
    b1_bark <- 1
  }
  else if (SPP == "BF") {
    pcntbark <- 0
    b0_bark <- 0.878
    b1_bark <- 1.025
  }
  else if (SPP == "BP") {
    pcntbark <- 18
    b0_bark <- 1
    b1_bark <- 1
  }
  else if (SPP == "BS") {
    pcntbark <- 0
    b0_bark <- 0.871
    b1_bark <- 1.026
  }
  else if (SPP == "BT") {
    pcntbark <- 15
    b0_bark <- 1
    b1_bark <- 1
  }
  else if (SPP == "EH") {
    pcntbark <- 0
    b0_bark <- 0.8916
    b1_bark <- 1.0121
  }
  else if (SPP == "GA") {
    pcntbark <- 13
    b0_bark <- 1
    b1_bark <- 1
  }
  else if (SPP == "GB") {
    pcntbark <- 12
    b0_bark <- 1
    b1_bark <- 1
  }
  else if (SPP == "JP") {
    pcntbark <- 0
    b0_bark <- 0.916
    b1_bark <- 1.01
  }
  else if (SPP == "NS") {
    pcntbark <- 0
    b0_bark <- 0.8558
    b1_bark <- 1.0363
  }
  else if (SPP == "PB") {
    pcntbark <- 0
    b0_bark <- 0.8969
    b1_bark <- 1.0179
  }
  else if (SPP == "QA") {
    pcntbark <- 0
    b0_bark <- 0.8449
    b1_bark <- 1.0332
  }
  else if (SPP == "RM") {
    pcntbark <- 0
    b0_bark <- 0.9214
    b1_bark <- 1.0117
  }
  else if (SPP == "RO") {
    pcntbark <- 11
    b0_bark <- 1
    b1_bark <- 1
  }
  else if (SPP == "RP" | SPP == "RN") {
    pcntbark <- 0
    b0_bark <- 0.928
    b1_bark <- 0.999
  }
  else if (SPP == "RS") {
    pcntbark <- 0
    b0_bark <- 0.864
    b1_bark <- 1.029
  }
  else if (SPP == "SB") {
    pcntbark <- 12
    b0_bark <- 1
    b1_bark <- 1
  }
  else if (SPP == "SM") {
    pcntbark <- 0
    b0_bark <- 0.9383
    b1_bark <- 1.0064
  }
  else if (SPP == "TL") {
    pcntbark <- 0
    b0_bark <- 1.5106
    b1_bark <- 0.8134
  }
  else if (SPP == "WA") {
    pcntbark <- 0
    b0_bark <- 0.8834
    b1_bark <- 1.0188
  }
  else if (SPP == "WC") {
    pcntbark <- 0
    b0_bark <- 0.7797
    b1_bark <- 1.0569
  }
  else if (SPP == "WP") {
    pcntbark <- 0
    b0_bark <- 0.926
    b1_bark <- 1
  }
  else if (SPP == "WS") {
    pcntbark <- 0
    b0_bark <- 0.8619239
    b1_bark <- 1.0324892
  }
  else if (SPP == "YB") {
    pcntbark <- 0
    b0_bark <- 0.8688
    b1_bark <- 1.0275
  }
  else if (SPP == "OH" | SPP == "OHW") {
    pcntbark <- 0
    b0_bark <- 0.8688
    b1_bark <- 1.0275
  }
  else {
    pcntbark <- 0
    b0_bark <- 0.8688
    b1_bark <- 1.0275
  }
  dib <- ifelse(pcntbark == 0, b0_bark * dob^b1_bark, dob * (1.0 - (pcntbark / 100)))
  return(dib = round(dib, 4))
}
