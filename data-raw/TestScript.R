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
    TPH = TPH(Stand, Plot, DBH, EXPF),
    QMD = QMD(BAPH, TPH)
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
trees$HT <-  HeightPredict(trees$Stand, trees$Plot, trees$SPP, trees$DBH, trees$CSI,
                        trees$CCF, trees$BAL, trees$HT)

### Volume Output (Tree Vol * EXPF)
trees$Vol <- mapply(KozakTreeVol, 'ib', trees$SPP, trees$DBH, trees$HT)
trees$VolPerHctr <- trees$Vol * trees$EXPF


### CCFL Values
trees <- trees %>%
  group_by(ID) %>%
  arrange(desc(DBH), .by_group = TRUE)
trees <- trees %>%
  mutate(
    CCFL = CCF.Larger(ID, SPP, DBH, EXPF)
  )

### 100 Tallest Trees
trees$Tallest <- TallestTrees(trees$ID, trees$HT, trees$EXPF)

MeanVolPerAcreBySpecies <- trees %>% group_by(Stand, SPP) %>% summarize(mean(VolPerHctr)*35.315/2.47105) %>% rename(VolPerAcre = `mean(VolPerHctr) * 35.315/2.47105`)
MeanVolPerAcreBySpecies_byPlot <- trees %>% group_by(Plot, SPP) %>%
  summarize(mean(VolPerHctr)*35.315/2.47105) %>% rename(VolPerAcre = `mean(VolPerHctr) * 35.315/2.47105`)
MeanBAPA_by_Species <- trees %>% group_by(Stand, SPP) %>% summarize(mean(BAPH)*10.764/2.47105) %>% rename(BAPA = `mean(BAPH) * 10.764/2.47105`)

StandVol_By_Species <- MeanVolPerAcreBySpecies %>% mutate(StandVolume = VolPerAcre*25)
