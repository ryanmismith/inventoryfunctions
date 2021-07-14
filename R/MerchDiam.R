#' Minimum Merchantable Diameter For Product by Species
#'
#' This function establishes the minimum merchantable diameter for sawlog,
#' pulp, and pallet products for each species. As these dimensions are constantly
#' changing and location dependent, these values only provide a baseline.
#' If the species is not capable of producing a value the minimum
#' diameter is set at 1000. Diameters are all stated in cm.
#'
#' ## If merch diameters need to be changed for a specific use this function should be changed locally
#' ## but should not be included in any update to this package.
#'
#'@param SPP Species: use FVS species codes
#'
#'@return This function returns a vector with the minimum dimensions for sawlogs (saw),
#'pallet wood (pallet), and pulp (pulp) for the desired species.
#'
#'@examples
#'
#'MerchDiam("RO")
#'
#'@export

MerchDiam <- function(SPP) {
  if (SPP == "AB" | SPP == "BE") { # American beech
    saw <- 25.4
    pallet <- 20.32
    pulp <- 10.16
  }
  else if (SPP == "AS") { # ash
    saw <- 25.4
    pallet <- 20.32
    pulp <- 10.16
  }
  else if (SPP == "BA") { # black ash
    saw <- 25.4
    pallet <- 20.32
    pulp <- 10.16
  }
  else if (SPP == "BC") { # black cherry
    saw <- 25.4
    pallet <- 20.32
    pulp <- 10.16
  }
  else if (SPP == "BF") { # balsam fir
    saw <- 12.7
    pallet <- 1000
    pulp <- 10.16
  }
  else if (SPP == "BP") { # balsam poplar
    saw <- 25.4
    pallet <- 20.32
    pulp <- 10.16
  }
  else if (SPP == "BS") { # black spruce
    saw <- 12.7
    pallet <- 1000
    pulp <- 10.16
  }
  else if (SPP == "BT" | SPP == "PO") { # bigtooth aspen
    saw <- 25.4
    pallet <- 1000
    pulp <- 10.16
  }
  else if (SPP == "EC") { # eastern cottonwood
    saw <- 1000
    pallet <- 1000
    pulp <- 10.16
  }
  else if (SPP == "EH" | SPP == "HE") { # eastern hemlock
    saw <- 20.32
    pallet <- 1000
    pulp <- 10.16
  }
  else if (SPP == "GA") { # green ash
    saw <- 25.4
    pallet <- 20.32
    pulp <- 10.16
  }
  else if (SPP == "GB") { # gray birch
    saw <- 25.4
    pallet <- 20.32
    pulp <- 10.16
  }
  else if (SPP == "HH") { # eastern hophornbeam
    saw <- 1000
    pallet <- 1000
    pulp <- 10.16
  }
  else if (SPP == "JP") { # jack pine
    saw <- 1000
    pallet <- 1000
    pulp <- 10.16
  }
  else if (SPP == "NS") { # Norway spruce
    saw <- 12.7
    pallet <- 1000
    pulp <- 10.16
  }
  else if (SPP == "OH") { # other hardwoods
    saw <- 1000
    pallet <- 1000
    pulp <- 10.16
  }
  else if (SPP == "OS") { # other softwoods
    saw <- 1000
    pallet <- 1000
    pulp <- 10.16
  }
  else if (SPP == "PB" | SPP == "WB") { # paper birch
    saw <- 25.4
    pallet <- 20.32
    pulp <- 10.16
  }
  else if (SPP == "PC" | SPP == "PR") { # pin cherry
    saw <- 25.4
    pallet <- 20.32
    pulp <- 10.16
  }
  else if (SPP == "QA") { # quaking aspen
    saw <- 25.4
    pallet <- 20.32
    pulp <- 10.16
  }
  else if (SPP == "RB") { # river birch
    saw <- 1000
    pallet <- 1000
    pulp <- 10.16
  }
  else if (SPP == "RM") { # red maple
    saw <- 25.4
    pallet <- 20.32
    pulp <- 10.16
  }
  else if (SPP == "RP" | SPP == "RN") { # red pine
    saw <- 20.32
    pallet <- 1000
    pulp <- 10.16
  }
  else if (SPP == "RO") { # red oak
    saw <- 25.4
    pallet <- 20.32
    pulp <- 10.16
  }
  else if (SPP == "RS") { # red spruce
    saw <- 12.7
    pallet <- 1000
    pulp <- 10.16
  }
  else if (SPP == "SB") { # Sweet birch
    saw <- 1000
    pallet <- 1000
    pulp <- 10.16
  }
  else if (SPP == "SM" | SPP == "HM") { # sugar maple
    saw <- 25.4
    pallet <- 20.32
    pulp <- 10.16
  }
  else if (SPP == "ST") { # striped maple
    saw <- 25.4
    pallet <- 20.32
    pulp <- 10.16
  }
  else if (SPP == "TA") { # larch/tamarack
    saw <- 20.32
    pallet <- 1000
    pulp <- 10.16
  }
  else if (SPP == "WA") { # white ash
    saw <- 25.4
    pallet <- 20.32
    pulp <- 10.16
  }
  else if (SPP == "EC" | SPP == "OC" | SPP == "WC") { # northern white cedar
    saw <- 25.4
    pallet <- 20.32
    pulp <- 10.16
  }
  else if (SPP == "WP") { # white pine
    saw <- 20.32
    pallet <- 1000
    pulp <- 10.16
  }
  else if (SPP == "WS") { # white spruce
    saw <- 12.7
    pallet <- 1000
    pulp <- 10.16
  }
  else if (SPP == "WO") { # White Oak
    saw <- 25.4
    pallet <- 20.32
    pulp <- 10.16
  }
  else if (SPP == "YB") { # yellow birch
    saw <- 25.4
    pallet <- 20.32
    pulp <- 10.16
  }
  else {
    saw <- 1000
    pallet <- 1000
    pulp <- 10.16
  }
  return(c(saw = saw, pallet = pallet, pulp = pulp))
}
