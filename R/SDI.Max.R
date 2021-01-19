#' Plot Level Maximum Stand Density Function
#'
#' This function computes the Maximum Stand Density of each plot in your inventory based on the
#' standard gravity of the tree weighted by its expansion factor. This is a method of obtaining
#' Maximum Stand Density for uneven aged mixed stands.
#'
#' Woodalls SDIMax
#'
#'@param Stand Unique Stand ID
#'@param Plot Unique Plot ID
#'@param Tree Unique Tree ID
#'@param SPP Diameter at breast height in cm.
#'@param EXPF Expansion factor for each tree.
#'
#'@family Stand Density Index Functions
#'@family Plot Level Functions
#'
#'@seealso [inventoryfunctions::SDI.Tree]
#'@seealso [inventoryfunctions::SDI.Plot]
#'@seealso [inventoryfunctions::RD]
#'
#'@return The return value will be a maximum stand density per hectare.
#'This function, along with [inventoryfunctions::SDI.Plot], is required to
#'compute the relative density of a stand with [inventoryfunctions::RD].
#'
#'@examples
#'
#'Stand <- c(1,1,1,1,1,1)
#'Plot  <- c(1,1,1,2,2,2)
#'Tree  <- c(1,2,3,1,2,3)
#'SPP   <- c("RO", "WP", "EH", "YB", "YB", "SM")
#'EXPF <- c(746.037, 282.52, 86.45, 94.31, 165.21, 361.03)
#'SDI.Max(Stand, Plot, Tree, SPP, EXPF)
#'
#'@export

SDI.Max <- function(Stand, Plot, Tree, SPP, EXPF){
  if(length(EXPF) != length(Plot) | length(Plot) != length(SPP)) {
    stop("Error: Please provide each tree with a unique Tree and Plot ID")
  } else {
    TPH <- Plot.SG <- Tree.Factor.SG <- NULL
  SPP.Func <- sapply(SPP, SPP.func)
  SPP.SG <- as.vector(SPP.Func[3,])
  SPP.SG <- as.numeric(SPP.SG)
  trees <- tibble(Stand, Plot, Tree, SPP, EXPF, SPP.SG)

  trees <- trees %>%        # Trees Per Hectare
    group_by(Plot, Stand) %>%
    mutate(
      TPH = sum(EXPF)
    )

  trees <- trees %>%       # Weighting the Standard Gravity of Each Tree with the Expansion Factor
    mutate(
      Tree.Factor.SG = (SPP.SG * EXPF)
    )

  trees <- trees %>%      # Plot Mean Standard Gravity
    group_by(Plot, Stand) %>%
    mutate(
      Plot.SG = (sum(Tree.Factor.SG) / mean(TPH))
    )

  trees <- trees %>%       # Calculating SDI.Max Per Plot
    mutate(
      Max = ((-6017.3 * Plot.SG) + 4156.3)
    )
  }

  trees$Max <- round(trees$Max, 2)
  return(trees$Max)
}


