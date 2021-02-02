#' Predicted Height Values
#'
#' @description This function predicts the heights of any trees that have missing height values. If no height values are provided,
#' heights will be predicted using the FVS acadian growth model (formula citation???). If height values are provided, this function
#' will leverage the provided height information by running the predicted heights and provided heights through the following
#' equation (HT ~ HTPred + (1|SPP/Stand/Plot)) - Species, Stand, and Plot are integrated into the equation as random effects. You must enter
#' both Stand and Plot inputs, the random effects will adjust accordingly.
#'
#'@details This function requires that all data be entered as a vector of length n. See example.
#'
#'@param SPP Species observation for every tree (FVS species code)
#'@param DBH Diameter at breast height in cm.
#'@param CSI Climate site index for each tree.
#'@param CCF Plot based Crown Competition Factor for each tree.
#'@param BAL Basal area of larger trees within the plot
#'@param Plot Either a Unique Stand or a Unique Plot ID for each tree.
#'@param HT Measured HT values for all trees with measured heights (trees with no heights should be entered as 0)
#'
#'@family Plot Level Functions
#'
#'@seealso [inventoryfunctions::BAL]
#'@seealso [inventoryfunctions::CCF]
#'
#'@return This function returns a numeric vector of length n with values for all trees with missing heights.
#'
#'@author Ryan Smith
#'
#'@import lme4
#'
#'@references
#' NEED TO WRITE REFERENCES
#'
#'@examples
#'\dontrun{
#'
#'  ###### RUNNING THE SCRIPT ######
#'  ###### REQUIRES FULL VECTORS #####
#'  ###### AS SEEN HERE ######
#'
#'  trees$HT <-  HeightPredict(trees$Stand, trees$Plot, trees$SPP, trees$DBH,
#'                             trees$CSI, trees$CCF, trees$BAL, trees$HT)
#'
#'  ##### RUN WITH FULL VECTORS AND NOT WITH MAPPLY #####
#'
#'data(tree_data)
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
#'  SDIPlot = SDI.Plot(Stand, Plot, Tree, DBH, EXPF),
#'  SDIMax = SDI.Max(Stand, Plot, Tree, SPP, DBH = DBH, EXPF = EXPF, CSI = CSI, X_Coord = X, Y_Coord = Y),
#'  BA = BA(DBH),
#'  CCF = CrownCompF(Stand, Plot, Tree, SPP, DBH, EXPF),
#'  ID = Unique.ID(Stand, Plot),
#'  RD = RD(SDIPlot, SDIMax),
#'  BAPH = BAPH(Stand, Plot, BA, EXPF),
#'  TPH = TPH(Stand, Plot, DBH, EXPF)
#' )
#'trees <- trees %>%
#'  dplyr::group_by(ID) %>%
#'  dplyr::arrange(desc(DBH), .by_group = TRUE)
#'trees <- trees %>%
#'  dplyr::mutate(
#'    BAL = BA.Larger.Trees(ID, DBH, BA)
#'  )
#'
#'  trees$HT <-  HeightPredict(trees$Stand, trees$Plot, trees$SPP, trees$DBH,
#'                             trees$CSI, trees$CCF, trees$BAL, trees$HT)
#'
#'}
#'@export


HeightPredict <- function(Stand, Plot, SPP, DBH, CSI, CCF, BAL, HT = NULL){

  ### PREDICTED HEIGHT FUNCTION FROM THE ACADIAN GROWTH MODEL ###
    pred <- function(SPP, DBH, CSI, CCF, BAL){
    c0=  12.44847305
    c1=  0.801705832
    c2=  0.043617034
    c3=  1.048674338
    c4=  0.011483716
    c5=  -0.007550999
    switch (SPP,
            'AB' = {c0.spp=-1.63260226433876; c3.spp=-0.123848276720533}  ,
            'AE' = {c0.spp=-0.692010776894357; c3.spp=0.0346080772461358} ,
            'AH' = {c0.spp=-5.98009416964362; c3.spp=-0.032783189788012}  ,
            'AI' = {c0.spp=-6.44978562263189; c3.spp=-0.0984022226851643} ,
            'AP' = {c0.spp=-12.0735361325049; c3.spp=-0.475976304087567}  ,
            'AS' = {c0.spp=6.69760483331092; c3.spp=-0.125318550191217}   ,
            'BA' = {c0.spp=-1.61716890163543; c3.spp=-0.141587177559468}  ,
            'BC' = {c0.spp=-4.52724204655813; c3.spp=-0.172605143041673}  ,
            'BE' = {c0.spp=-1.60563943164767; c3.spp=-0.424565045666305}  ,
            'BF' = {c0.spp=1.77471065080046; c3.spp=0.1571978021787}      ,
            'BL' = {c0.spp=-9.82751389751524; c3.spp=-0.292624067773788}  ,
            'BN' = {c0.spp=0.861243905640667; c3.spp=-0.103226577993538}  ,
            'BO' = {c0.spp=1.17024253111731; c3.spp=-0.0431150821737857}  ,
            'BP' = {c0.spp=2.52163661595498; c3.spp=-0.0633568443480465}  ,
            'BR' = {c0.spp=5.44303876725562; c3.spp=0.354363882079203}    ,
            'BS' = {c0.spp=3.88571664605334; c3.spp=0.1886808269048}      ,
            'BT' = {c0.spp=5.2832906396451; c3.spp=-0.0670620453873463}   ,
            'BW' = {c0.spp=2.52499880080404; c3.spp=0.153925181183304}    ,
            'EC' = {c0.spp=7.0881673102164; c3.spp=0.182126907461261}     ,
            'EH' = {c0.spp=0.0643545746867161; c3.spp=0.260671290553969}  ,
            'EL' = {c0.spp=-0.460173779709119; c3.spp=0.194209222023289}  ,
            'GA' = {c0.spp=-1.47263156202317; c3.spp=-0.0743884734979349} ,
            'GB' = {c0.spp=-1.03938349314791; c3.spp=-0.238717166776341}  ,
            'HH' = {c0.spp=-2.45397316779551; c3.spp=-0.17636502944365}   ,
            'HK' = {c0.spp=-0.139225685811506; c3.spp=-0.107092112450714} ,
            'HT' = {c0.spp=-0.498373011862981; c3.spp=0.000508524335021695},
            'JP' = {c0.spp=12.2041796567474; c3.spp=0.507127061137884}    ,
            'NC' = {c0.spp=-0.154777524407996; c3.spp=0.0506677142901254} ,
            'NS' = {c0.spp=10.7572546403292; c3.spp=0.761510842024224}    ,
            'OH' = {c0.spp=-0.152274609199728; c3.spp=-0.0773943323672784},
            'OP' = {c0.spp=-1.25201364651662; c3.spp=0.19704750471505}    ,
            'OS' = {c0.spp=6.64418468070433; c3.spp=0.154974733190601}    ,
            'PB' = {c0.spp=2.85568786741337; c3.spp=-0.053133050063968}   ,
            'PI' = {c0.spp=6.11926846598162; c3.spp=0.396180643433203}    ,
            'PL' = {c0.spp=-12.5774578312843; c3.spp=-0.354402924932074}  ,
            'PP' = {c0.spp=-2.03524755338192; c3.spp=0.0284495830511636}  ,
            'PR' = {c0.spp=-5.27943940700261; c3.spp=-0.276675997378359}  ,
            'PY' = {c0.spp=2.39961434182412; c3.spp=-0.0302798406740612}  ,
            'QA' = {c0.spp=5.28547878831447; c3.spp=-0.0166932060459991}  ,
            'RC' = {c0.spp=-13.3554875880232; c3.spp=-0.364956123989416}  ,
            'RL' = {c0.spp=-13.464860796814; c3.spp=-0.373319415146871}   ,
            'RM' = {c0.spp=1.13361861116141; c3.spp=-0.124923006598654}   ,
            'RN' = {c0.spp=2.35424925615196; c3.spp=0.4332474439509}      ,
            'RO' = {c0.spp=0.567158528857343; c3.spp=5.49241830858304E-05},
            'RS' = {c0.spp=3.28913339723299; c3.spp=0.197832299388656}    ,
            'SB' = {c0.spp=5.0591881231944; c3.spp=0.263270570106115}     ,
            'SC' = {c0.spp=-1.77707771556552; c3.spp=0.272984904670842}   ,
            'SE' = {c0.spp=-1.20436304154548; c3.spp=-0.217453987421684}  ,
            'SH' = {c0.spp=3.42398432816088; c3.spp=0.00852401379505827}  ,
            'SM' = {c0.spp=1.83135273162116; c3.spp=-0.1509017085778}     ,
            'SO' = {c0.spp=-0.337608194904877; c3.spp=0.00266584203067429},
            'ST' = {c0.spp=-4.21455499968947; c3.spp=-0.158534452384565}  ,
            'SV' = {c0.spp=-1.66795214963112; c3.spp=-0.180378852473763}  ,
            'SW' = {c0.spp=1.3081369384269; c3.spp=0.031660020193251}     ,
            'TA' = {c0.spp=3.03898203229266; c3.spp=-0.070139469703331}   ,
            'WA' = {c0.spp=1.58571626982993; c3.spp=-0.152599799179656}   ,
            'WC' = {c0.spp=-2.63796730677436; c3.spp=0.157097825004126}   ,
            'WO' = {c0.spp=-0.179572014160004; c3.spp=0.050014945223132}  ,
            'WP' = {c0.spp=1.89177965275704; c3.spp=0.217933074706457}    ,
            'WS' = {c0.spp=2.83053645408208; c3.spp=0.284913386127066}    ,
            'YB' = {c0.spp=-1.13450171811265; c3.spp=-0.179629568670318}  ,
            {c0.spp=0; c3.spp=0}
    )
    HTPred <- (1.37+((c0+c0.spp)+CSI^c1)*(1-exp(-c2*DBH))^(c3+c3.spp+c4*log(CCF+1)+c5*BAL))
    #ht=(1.37+((c0)+CSI^c1)*(1-exp(-c2*DBH))^(c3+c4*log(CCF+1)+c5*BAL))
    return(HTPred)
    }

    HTPred <- mapply(pred, SPP, DBH, CSI, CCF, BAL)

    if(is.null(HT) == TRUE){

     return(HTPred)

    } else {

      ### Regression HT ~ HTPred with random effects (1|SPP/Stand/Plot) ###

      trees <- tidyr::tibble(SPP, DBH, CSI, CCF, BAL, Plot, Stand, HT, HTPred)

      ### Create columns for matching coefficients in the tree tibble ###
      trees$PLOTSPP <- paste(trees$Plot, trees$Stand, trees$SPP, sep = ":")
      trees$STANDSPP <- paste(trees$Stand, trees$SPP, sep = ":")

      temp <- trees %>% dplyr::filter(HT > 0)

      ### Predicted Height Model ###
      model <- lme4::lmer(HT ~ HTPred + (1|SPP/Stand/Plot), data = temp, na.action = na.omit)

      fixed  <- lme4::fixef(model)
      random <- lme4::ranef(model)

      SPP <- rownames(random$SPP)
      SPPValues <- unlist(random$SPP)
      SPPValues <- as.numeric(as.vector(SPPValues))

      Stand <- rownames(random$`Stand:SPP`)
      StandValues <- unlist(random$`Stand:SPP`)
      StandValues <- as.numeric(as.vector(StandValues))

      Plot <- rownames(random$`Plot:(Stand:SPP)`)
      PlotValues <- unlist(random$`Plot:(Stand:SPP)`)
      PlotValues <- as.numeric(as.vector(PlotValues))

      SPPTable   <- tidyr::tibble(SPP, SPPValues)
      StandTable <-tidyr::tibble(Stand, StandValues)
      PlotTable  <- tidyr::tibble(Plot, PlotValues)

      trees$SPPcoef <- ifelse(trees$SPP %in% SPPTable$SPP,
                            SPPTable$SPPValues[match(trees$SPP, SPPTable$SPP)], 0)

      trees$STANDcoef <- ifelse(trees$STANDSPP %in% StandTable$Stand,
                            StandTable$StandValues[match(trees$STANDSPP, StandTable$Stand)], 0)

      trees$PLOTcoef <- ifelse(trees$PLOTSPP %in% PlotTable$Plot,
                            PlotTable$PlotValues[match(trees$PLOTSPP, PlotTable$Plot)], 0)

      ### Final Model ###
      trees$HT1 <- (fixed[1] + trees$SPPcoef + trees$STANDcoef + trees$PLOTcoef) + (fixed[2]*trees$HTPred)

      ### Return either measured HT value or the adjusted HTPred Value ###
      trees$HT2 <- ifelse(trees$HT > 0, trees$HT, trees$HT1)


      return(trees$HT2)
    }
}
