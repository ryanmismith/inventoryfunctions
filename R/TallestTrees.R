#' Average Height of the 100 Tallest Trees
#'
#'
#' ## This function requires that your data.frame be sorted as detailed in examples before running.
#' This function calculates the average height of the 100 tallest trees in your data set. Using the EXPF and heights provided
#' the 100 tallest trees will be identified and averaged. This mean value will then be applied to a variable listing the average
#' of the tallest trees within the plot as an observation for each tree in your data set.
#'
#'
#'@param ID Unique Plot ID for each plot in your data set.
#'@param HT Height of each tree in meters.
#'@param EXPF Expansion factor of each tree.
#'
#'@seealso [inventoryfunctions::Unique.ID]
#'@seealso [inventoryfunctions::HeightPredict]
#'@seealso [inventoryfunctions::EXPF]
#'
#'@family Plot Level Functions
#'
#'@details This function uses the dplyr package to select, arrange, and mutate the data.
#'
#' Your data should include unique IDs specific to each plot. If you have not done this, it can be done using the
#' Unique.ID function included in this package. This function requires either a measured or predicted height for each
#' tree in your inventory. If you have not added predicted values where this is no tree HT measurement, use the Height Predict
#' function in this package.
#'
#'@return The function returns a vector of length n with the 100 tallest trees in each plot.
#'@author Ryan Smith
#'@examples
#'
#'\dontrun{
#'  ###### RUNNING THE SCRIPT ######
#'  ###### REQUIRES FULL VECTORS #####
#'  ###### AS SEEN HERE ######
#'
#'  trees$Tallest <-  TallestTrees(trees$ID, trees$HT, trees#EXPF)
#'
#'  ##### RUN WITH FULL VECTORS AND NOT WITH MAPPLY #####
#'
#' # If you don't have predicted heights or IDs
#'
#'data(tree_data)
#'
#'trees <- tree_data
#'
#'cord <- data.frame(trees$X, trees$Y)
#'sp::coordinates(cord) <- cord
#'sp::proj4string(cord) <- sp::CRS("+proj=longlat +datum=WGS84")
#'CSI <- raster::raster("EastSI_ENSEMBLE_rcp60_2030.tif")
#'CSIdata   <- raster::extract(CSI, cord, method = 'simple', df = TRUE)
#'trees$CSI <- CSIdata$EastSI_ENSEMBLE_rcp60_2030
#'
#'trees <- trees %>%
#' dplyr::mutate(
#'  EXPF = EXP.F(DBH, BAF),
#'  BA = BA(DBH),
#'  CCF = CrownCompF(Stand, Plot, Tree, SPP, DBH, EXPF),
#'  ID = Unique.ID(Stand, Plot),
#' )
#'trees <- trees %>%
#'  dplyr::group_by(ID) %>%
#'  dplyr::arrange(desc(DBH), .by_group = TRUE)
#'trees <- trees %>%
#'  dplyr::mutate(
#'    BAL = BA.Larger.Trees(ID, DBH, BA)
#'  )
#'
#'  trees$HT <-  HeightPredict(trees$SPP, trees$DBH, trees$CSI,
#'      trees$CCF, trees$BAL, trees$Plot, trees$HT)
#'
#'  trees$Tallest <-  TallestTrees(trees$ID, trees$HT, trees#EXPF)
#'}
#'
#'
#'@export


TallestTrees <- function(ID, HT, EXPF){

  Temp <- data.frame(ID, HT, EXPF)                    # Make Dataframe
  Temp <- Temp[order(-HT),]                           # Order it for Cumsum function
  Temp$csum <- ave(Temp$EXPF, Temp$ID, FUN=cumsum)    # Cumsum each ID (Plot or Stand)

  for (i in 1:length(Temp$csum)){
    if(Temp$csum[i] <= 100){
      Temp$X[i] <- Temp$HT[i] * Temp$EXPF[i]          # Get a total sum of heights for trees where cumsum <= 100
      Temp$Counts[i] <- Temp$csum[i]                  # Identify which trees are included in that sum
    } else {
      Temp$X[i] <- 0                                  # Identify which trees are not included in that sum
      Temp$Counts[i] <- 0
    }
  }

  Temp$MaxCum <- ave(Temp$csum, Temp$ID, FUN = function(x) 100 - max(x))                    # 100-MaxCum == the number of trees in plots where cumsum < 100
  Temp$remainder <- ave(Temp$Counts, Temp$ID, FUN = function(x) 100-max(x, na.rm = TRUE))   # Identify number of trees needed for X to get to 100
  Temp$max <- ave(Temp$Counts, Temp$ID, FUN = function(x) which.max(x))                     # Index for max tree if first tree has EXPF > 100
  Temp$index <- ave(Temp$Counts, Temp$ID, FUN = function(x) which.max(x) + 1)               # Index of the first tree not included in X
  Temp <- Temp %>%                                                                          # Identify the height of the remainder trees
      dplyr::group_by(ID) %>%
      dplyr::arrange(desc(HT), .by_group = TRUE) %>%
      dplyr::mutate(
        minheight = HT[index]
        )
  Temp <- Temp %>%                                                                          # Identify the top tree when expf of first tree > 100
    dplyr::group_by(ID) %>%
    dplyr::arrange(desc(HT), .by_group = TRUE) %>%
    dplyr::mutate(
      maxht = HT[max]
    )

  for(i in 1:length(Temp$minheight)){                                     # Get rid of NAs for plots with less than 100 stems
    if(is.na(Temp$minheight[i]) == TRUE){
      Temp$minheight[i] <- 0
      } else {
      Temp$minheight[i] <- Temp$minheight[i]
      }
  }

  for (i in 1:length(Temp$remainder)){                                    # Create column with total combined heights of remainder trees
    if(Temp$remainder[i] < 100){
    Temp$leftover[i] <- Temp$remainder[i] * Temp$minheight[i]
    } else {
    Temp$leftover[i] <- Temp$remainder[i] * Temp$maxht[i]                 # Sum of 100*MaxTree if Tallest tree has EXPF > 100
    }
  }

  Temp$Y <- ave(Temp$X, Temp$ID, FUN = function(x) sum(x, na.rm = TRUE))  #Combined heights of trees in X (cumsum <= 100)

  for (i in 1:length(Temp$Y)){
    Temp$Total[i] <- Temp$leftover[i] + Temp$Y[i]                         # Create column with combined heights of 100 tallest trees
  }

  for (i in 1:length(Temp$Total)){
    if(Temp$MaxCum[i] > 0){
      Temp$Result[i] <- Temp$Total[i]/(100-Temp$MaxCum[i])                # If there are less than 100 trees divide by the # of trees.
    } else {
      Temp$Result[i] <- Temp$Total[i]/100                                 # If there are > 100 trees, combined height of 100 tallest trees / 100.
    }
  }


  as.numeric(round(Temp$Result, 2))                                                 # Round return.

  return(Temp$Result)                                                     # Result returned

}




