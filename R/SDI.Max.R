#' Plot Level Maximum Stand Density Function
#'
#' @description This function computes the Maximum Stand Density of each plot using one of two methods. If you include the necessary
#' Climate Site Index data and plot coordinates (lat, long or a SpatialPointsDF) SDImax will be calculated using the Weiskittel and Kuehne method.
#' If you fail to input this information SDImax will be calculated using Woodall's method based solely on
#' mean standard gravity.
#'
#' @details Both methods in the description are proposed methods for obtaining Maximum Stand Density for uneven aged
#' mixed stands. Please see references for further information. It is the recommendation of the author of this
#' function that you utilize the Weiskittel method for calculating SDI.Max.
#'
#' You can enter either a set of X and Y coordinates or a SpatialPointsDF into this function.
#'
#'@param Stand Unique Stand ID
#'@param Plot Unique Plot ID
#'@param Tree Unique Tree ID
#'@param SPP Species observation for every tree (FVS species code)
#'@param EXPF Expansion factor for each tree.
#'@param CSI Climate site index for each tree.
#'@param X_Coord Plot X coordinate for each tree
#'@param Y_Coord Plot Y coordinate for each tree
#'@param project The projection used for your provided coordinates will be EPSG:4326 unless another value is provided.
#'@param SpatialPointsDF A SpatialPointsDF may be provided for the data instead of X and Y coords.
#'
#'@family Stand Density Index Functions
#'@family Plot Level Functions
#'
#'@seealso [inventoryfunctions::SDI.Plot]
#'@seealso [inventoryfunctions::RD]
#'
#'@return The return a numeric vector of length n with the value of the maximum stand density per hectare.
#'This function, along with SDI.Plot, is required to
#'compute the relative density of a stand with RD.
#'
#'@author Ryan Smith
#'
#'@references
#'Weiskittel, A. R., & Kuehne, C. (2019). Evaluating and modeling variation in site-level maximum carrying
#'capacity of mixed-species forest stands in the Acadian Region of northeastern North America.
#'The Forestry Chronicle, 95(03), 171–182. https://doi.org/10.5558/tfc2019-026
#'
#'Woodall, C.W., P.D. Miles and J.S. Vissage. 2005. Determining maximum stand density index
#'in mixed species stands for strategic- scale stocking assessments. For. Ecol. Manage. 216: 367–377.
#'@examples
#'
#'Stand <- c(1,1,1,1,1,1)
#'Plot  <- c(1,1,1,2,2,2)
#'Tree  <- c(1,2,3,1,2,3)
#'SPP   <- c("RO", "WP", "EH", "YB", "YB", "SM")
#'DBH <- c(42, 23, 61, 25, 55, 15)
#'EXPF <- c(866.1473, 2888.249, 410.6111, 2444.614, 505.0856, 6790.595)
#'SDI.Max(Stand, Plot, Tree, SPP, DBH, EXPF)
#'@import elevatr
#'@export

SDI.Max <- function(Stand, Plot, Tree, SPP, DBH, EXPF,
                    CSI = NULL, X_Coord = NULL, Y_Coord = NULL, project = NULL, SpatialPointsDF = NULL){
    x <- NULL
    if(is.null(CSI) == TRUE | (is.null(X_Coord) == TRUE && is.null(SpatialPointsDF) == TRUE)){
    ### Simple SDI Max Equation ###

    SPP.Attributes <- sapply(SPP, SPP.func)
    SPP.SG <- as.vector(SPP.Attributes[3,])
    SPP.SG <- as.numeric(SPP.SG)
    trees <- data.frame(Stand, Plot, Tree, SPP, EXPF, SPP.SG)

    ### Trees Per Hectare ###
    trees <- trees %>%
      dplyr::group_by(Plot, Stand) %>%
      dplyr::mutate(
        TPH = sum(EXPF)
      )

    ### Mean Standard Gravity ###
    trees <- trees %>%
      dplyr::mutate(
        Tree.Factor.SG = (SPP.SG * EXPF)
      )
    trees <- trees %>%
      dplyr::group_by(Plot, Stand) %>%
      dplyr::mutate(
        sgm = (sum(Tree.Factor.SG) / mean(TPH))
      )

    ### SDI.Max Per Plot ###
    trees <- trees %>%
      dplyr:: mutate(
        Max = ((-6017.3 * sgm) + 4156.3)
      )

    x <- round(trees$Max, 2)
    print("Without Climate Site Index Variables and either Plot X and Y values or a SpatialPointsDF this function is using Woodall's SDImax equation")
    return(x)

  } else {

    ### Species Attributes ###
    SPP.Attributes <- sapply(SPP, SPP.func)
    SPP.SG  <- as.vector(SPP.Attributes[3,])
    SPP.SG  <- as.numeric(SPP.SG)
    SPPtype <- as.vector(SPP.Attributes[1,])
    SPPtype <- as.character(SPPtype)

    trees   <- data.frame(Stand, Plot, Tree, SPP, EXPF, DBH, CSI, SPPtype, SPP.SG)
    x       <- X_Coord
    y       <- Y_Coord
    coords  <- data.frame(x, y)


    ### Trees Per Hectare ###
    trees <- trees %>%
      dplyr::group_by(Plot, Stand) %>%
      dplyr::mutate(
        TPH = sum(EXPF)
      )

    ### Mean Standard Gravity ###
    trees <- trees %>%
      dplyr::mutate(
        Tree.Factor.SG = (SPP.SG * EXPF)
      )
    trees <- trees %>%
      dplyr::group_by(Plot, Stand) %>%
      dplyr::mutate(
        meanSG = (sum(Tree.Factor.SG) / mean(TPH))
      )

    ### Number of Unique Species ###
    trees$SPP.DIV <- (length(unique(trees$SPP)))

    ### DBH Range ###
    trees <- trees %>%
      dplyr::group_by(Plot, Stand) %>%
      dplyr::mutate(
        DBH.RANGE = max(DBH) - min(DBH)
      )

    ### Tree Basal Area ###
    trees<- trees %>%
      dplyr::mutate(
        treeBA = BA(DBH)*EXPF
      )

    ### Basal Area Per Hectare ###
    trees <- trees %>%
      dplyr::group_by(Stand, Plot) %>%
      dplyr::mutate(
        BAhectare = sum(treeBA)
      )

    ### Basal Percent Hardwood ###
    trees <- trees %>%
      dplyr::mutate(
        ba.SW = ifelse(SPPtype == "SW", treeBA, 0)
      )
    trees <- trees %>%
      dplyr::group_by(Plot, Stand) %>%
      dplyr::mutate(
        pHW.ba = (1 - sum(ba.SW)/BAhectare)
      )

    ### Elevation ###
    if(is.null(SpatialPointsDF) == FALSE) {
      getpoints <- elevatr::get_elev_point(SpatialPointsDF)
      trees$ELEV <- unlist(getpoints[[3]])
    } else if (is.null(project) == FALSE) {
      project <- project
      getpoints <- elevatr::get_elev_point(coords, units = "meters", prj = project, src = "aws")
      trees$ELEV <- unlist(getpoints[[1]])
    } else {
      project <- "EPSG:4326"
      getpoints <- elevatr::get_elev_point(coords, units = "meters", prj = project, src = "aws")
      trees$ELEV <- unlist(getpoints[[1]])
    }

    ### Calculate SDIMax ###
    for (i in 1:length(trees$Tree)){
      trees$x[i] <- (483.2448 - (1.456*trees$pHW.ba[i]) - (212.705*log(trees$meanSG[i])) +
                       (45.351*sqrt(trees$DBH.RANGE[i])) + (14.811*trees$SPP.DIV[i]) - (0.0848*trees$ELEV[i]) +
                       (0.0001*trees$ELEV[i]^2) + (331.3714*(1/trees$CSI[i])))
    }
    x <- round(trees$x, 2)
    return(x)
  }
}


