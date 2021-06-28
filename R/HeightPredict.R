#' Predicted Height Values
#'
#'@description This function predicts the heights of any trees that have missing height values. If no height values are provided,
#'heights will be predicted using the FVS acadian growth model (formula citation???). If height values are provided, this function
#'will leverage the provided height, SPP, Stand, and Plot data by running the predicted heights and provided heights through the following
#'equation (HT ~ HTPred + (1|SPP) + (1|Stand/Plot)).
#'
#'@details Stand, Plot, and SPP will only be included as random variables when either
#'at least 5 Unique Stands, Plots and Spp (HT ~ HTPred + (1|SPP) + (1|Stand/Plot)),
#'5 Unique Plots and Spp (HT ~ HTPred + (1|SPP) + (1|Plot)), or 5 Unique SPP are entered
#'(HT ~ HTPred + (1|SPP)). HTPred is the predicted heights using the acadian growth model formulas.
#'If there are not enough categorical variables
#'for a LMM a simple lm will be run as (HT ~ HTpred) from the acadian growth model.
#'
#'@details All measured height values will override predicted values in the output.
#'
#'@details Each Plot and Stand needs to have its own unique ID! If plot IDs
#'are recycled they will be lumped together as a single plot in this function.
#'
#'A simple solution if plot IDs are recycled in each stand
#'(ex Stand 1 Plots 1,2,3,4 - Stand 2 Plots 1,2,3,4) is to create
#'a new variable PlotID. df$PlotID <- paste(Stand, Plot, sep = "-").
#'This will give you unique plot IDs for every plot.
#'ex: Stand 1 Plots 1_1, 1_2, 1_3, 1_4 - Stand 2 Plots 2_1, 2_2, 2_3, 2_4, etc.
#'
#'@details This function requires that all data be entered as a vector of length n. See example.
#'
#'@param Stand Stand ID for Plot where the nth tree is located.
#'@param Plot Plot ID for Plot where the nth tree is located.
#'@param SPP Species observation for every tree (FVS species code)
#'@param DBH Diameter at breast height in cm.
#'@param CSI Climate site index for each tree.
#'@param CCF Plot based Crown Competition Factor for each tree.
#'@param BAL Basal area of larger trees within the plot
#'@param HT Measured HT values for all trees with measured heights (trees with no heights should be entered as 0)
#'
#'@family Plot Level Functions
#'
#'@seealso [inventoryfunctions::BAL]
#'@seealso [inventoryfunctions::CCF]
#'
#'@return This function returns a numeric vector of length n with values for all trees with missing heights.
#'@return This function returns the random variables used (if any) and a plot of the model's residuals.
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

  Stand <- as.factor(Stand)
  Plot <- as.factor(Plot)
  SPP <- as.integer(SPP)
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

    print("Used Acadian Model Formula with No Adjustments")
    return(HTPred)

  } else if(length(unique(Stand)) < 5 && length(unique(Plot)) >= 5 && length(unique(SPP)) >= 5) {

    ### Regression HT ~ HTPred with SPP and Plot random effects (1|SPP) + (1|Plot) ###
    trees <- tidyr::tibble(SPP, DBH, CSI, CCF, BAL, Plot, Stand, HT, HTPred)

    ### Predicted Height Model ###
    model <- lme4::lmer(HT ~ HTPred + (1|SPP) + (1|Plot), data = trees, na.action = na.omit)

    ### Draw Out Coef ###
    fixed  <- lme4::fixef(model)
    random <- lme4::ranef(model)

    species <- rownames(random$SPP)
    SPPValues <- unlist(random$SPP)
    SPPValues <- as.numeric(as.vector(SPPValues))

    plots <- rownames(random$Plot)
    PlotValues <- unlist(random$Plot)
    PlotValues <- as.numeric(as.vector(PlotValues))

    SPPTable   <- tidyr::tibble(species, SPPValues)
    PlotTable  <- tidyr::tibble(plots, PlotValues)

    trees$SPPcoef <- ifelse(trees$SPP %in% SPPTable$species,
                            SPPTable$SPPValues[match(trees$SPP, SPPTable$species)], 0)

    trees$PLOTcoef <- ifelse(trees$Plot %in% PlotTable$plots,
                             PlotTable$PlotValues[match(trees$Plot, PlotTable$plots)], 0)

    ### Final Model ###
    trees$HT1 <- (fixed[1] + trees$SPPcoef + trees$PLOTcoef) + (fixed[2]*trees$HTPred)

    ### Return either measured HT value or the adjusted HTPred Value ###
    trees$HT2 <- ifelse(is.na(trees$HT) == FALSE, trees$HT, trees$HT1)

    print(plot(model))
    print("Adjusted Acadian Model With Provided Hts (SPP and Plot Random Variables)")
    return(trees$HT2)

  } else if (length(unique(Stand)) >= 5 && length(unique(Plot)) >= 5 && length(unique(SPP)) >= 5) {

    ### Regression HT ~ HTPred with SPP and Plot random effects (1|SPP) + (1|Plot) ###
    trees <- tidyr::tibble(SPP, DBH, CSI, CCF, BAL, Plot, Stand, HT, HTPred)

    ### Create columns for matching coefficients in the tree tibble ###
    trees$PLOTSTAND <- paste(trees$Plot, trees$Stand, sep = ":")

    ### Second Height Model ###
    model2 <- lme4::lmer(HT ~ HTPred + (1|SPP) + (1|Stand/Plot), data = trees, na.action = na.omit)

    ### Draw Out Coef ###
    fixed2  <- lme4::fixef(model2)
    random2 <- lme4::ranef(model2)

    species2 <- rownames(random2$SPP)
    SPPValues2 <- unlist(random2$SPP)
    SPPValues2 <- as.numeric(as.vector(SPPValues2))

    plotstand2 <- rownames(random2$`Plot:Stand`)
    plotstandvalues2 <- unlist(random2$`Plot:Stand`)
    plotstandvalues2 <- as.numeric(as.vector(plotstandvalues2))

    stands2 <- rownames(random2$Stand)
    standvalues2 <- unlist(random2$Stand)
    standvalues2 <- as.numeric(as.vector(standvalues2))

    SPPTable2   <- tidyr::tibble(species2, SPPValues2)
    PlotStandTable2 <-tidyr::tibble(plotstand2, plotstandvalues2)
    StandTable2 <- tidyr::tibble(stands2, standvalues2)

    trees$SPPcoef2 <- ifelse(trees$SPP %in% SPPTable2$species2,
                             SPPTable2$SPPValues2[match(trees$SPP, SPPTable2$species2)], 0)

    trees$PlotStandcoef2 <- ifelse(trees$PLOTSTAND %in% PlotStandTable2$plotstand2,
                                   PlotStandTable2$plotstandvalues2[match(trees$PLOTSTAND,
                                                                          PlotStandTable2$plotstand2)], 0)
    trees$Standcoef2 <- ifelse(trees$Stand %in% StandTable2$stands2,
                               StandTable2$standvalues2[match(trees$Stand,
                                                              StandTable2$stands2)], 0)

    ### Final Model ###
    trees$HT3 <- (fixed2[1] + trees$SPPcoef2 + trees$PlotStandcoef2 + trees$Standcoef2) + (fixed2[2]*trees$HTPred)

    ### Return either measured HT value or the adjusted HTPred Value ###
    trees$HT4 <- ifelse(is.na(trees$HT) == FALSE, trees$HT, trees$HT3)

    print(plot(model2))
    print("Adjusted Acadian Model With Provided Hts (SPP, Stand/Plot Random Variables)")
    return(trees$HT4)

  } else if (length(unique(Stand)) < 5 && length(unique(Plot)) < 5 && length(SPP >= 5)){

    ### Only SPP as a Random Effect (HT ~ HTPred + (1|SPP)) ###
    trees <- tidyr::tibble(SPP, DBH, CSI, CCF, BAL, Plot, Stand, HT, HTPred)

    ### Third Height Model ###
    model3 <- lme4::lmer(HT ~ HTPred + (1|SPP), data = trees, na.action = na.omit)

    ### Draw Out Coef ###
    fixed3  <- lme4::fixef(model3)
    random3 <- lme4::ranef(model3)

    species3 <- rownames(random3$SPP)
    SPPValues3 <- unlist(random3$SPP)
    SPPValues3 <- as.numeric(as.vector(SPPValues3))

    SPPTable3   <- tidyr::tibble(species3, SPPValues3)

    trees$SPPcoef3 <- ifelse(trees$SPP %in% SPPTable3$species3,
                             SPPTable3$SPPValues3[match(trees$SPP, SPPTable3$species3)], 0)

    ### Final Model ###
    trees$HT5 <- (fixed3[1] + trees$SPPcoef3) + (fixed3[2]*trees$HTPred)

    ### Return either measured HT value or the adjusted HTPred Value ###
    trees$HT6 <- ifelse(is.na(trees$HT) == FALSE, trees$HT, trees$HT5)

    print(plot(model3))
    print("Adjusted Acadian Model With Provided Hts (SPP Random Variable)")
    return(trees$HT6)

  } else {

    ### No Random Effects (HT ~ HTPred) ###
    trees <- tidyr::tibble(SPP, DBH, CSI, CCF, BAL, Plot, Stand, HT, HTPred)

    ### Third Height Model ###
    model5 <- lm(HT ~ HTPred, data = trees, na.action = na.omit)

    ### Draw Out Coef ###
    Coef <- coef(model5)

    ### Final Model ###
    trees$HT7 <- (Coef[1]) + (Coef[2]*trees$HTPred)

    ### Return either measured HT value or the adjusted HTPred Value ###
    trees$HT8 <- ifelse(is.na(trees$HT) == FALSE, trees$HT, trees$HT7)

    print(plot(model5))
    print("Adjusted Acadian Model With Provided Hts using lm")
    return(trees$HT8)
  }
}

