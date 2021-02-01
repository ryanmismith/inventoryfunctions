data("tree_data")
trees <- tree_data
rm(tree_data)

### Extract CSI and BMGI Data ###

cord <- data.frame(trees$X, trees$Y)
sp::coordinates(cord) <- cord
sp::proj4string(cord) <- sp::CRS("+proj=longlat +datum=WGS84")

CSI <- raster::raster("/Users/ryansmith/Thesis/GIS/Climate Site Index/EastSI_ENSEMBLE_rcp60_2030.tif")
BMGI <- raster::raster("/Users/ryansmith/Thesis/GIS/BGI Raster/BGI_S2REP_ME_Update.tif")

CSIdata   <- raster::extract(CSI, cord, method = 'simple', df = TRUE)
BMGIdata  <- raster::extract(BMGI, cord, method = 'simple', df = TRUE)

trees$CSI <- CSIdata$EastSI_ENSEMBLE_rcp60_2030
trees$BMGI <- BMGIdata$BGI_S2REP_ME_Update
rm(CSI, BMGI, cord, CSIdata, BMGIdata)

### Tree and Plot Level Data ###
trees <- trees %>%
  mutate(
    EXPF = EXP.F(DBH, BAF),
    SDIPlot = SDI.Plot(Stand, Plot, Tree, DBH, EXPF),
    SDIMax = SDI.Max(Stand, Plot, Tree, SPP, DBH = DBH, EXPF = EXPF, CSI = CSI, X_Coord = X, Y_Coord = Y),
    BA = BA(DBH),
    CCF = CrownCompF(Stand, Plot, Tree, SPP, DBH, EXPF),
    ID = Unique.ID(Stand, Plot),
    RD = RD(SDIPlot, SDIMax),
    BAPH = BAPH(Stand, Plot, BA, EXPF),
    TPH = TPH(Stand, Plot, DBH, EXPF)
  )

### BAL Values

trees <- trees %>%
  group_by(ID) %>%
  arrange(desc(DBH), .by_group = TRUE)
trees <- trees %>%
  mutate(
    BAL = BA.Larger.Trees(ID, DBH, BA)
  )

### Height ###
trees$HT <-  HeightPredict(trees$SPP, trees$DBH, trees$CSI, trees$CCF, trees$BAL, trees$Plot, trees$HT)

### Volume Output (Tree Vol * EXPF)

tempvol <- mapply(KozakTreeVol, 'ib', trees$SPP, trees$DBH, trees$HT)
trees$TreeVol <- tempvol * trees$EXPF
rm(tempvol)

### CCFL Values

trees <- trees %>%
  group_by(ID) %>%
  arrange(desc(DBH), .by_group = TRUE)
trees <- trees %>%
  mutate(
    CCFL = CCF.Larger(ID, SPP, DBH, EXPF)
  )


trees <- trees %>%
  group_by(ID) %>%
  arrange(desc(HT), .by_group = TRUE)

trees$Tallest <- TallestTrees(trees$ID, trees$HT, trees$EXPF)
