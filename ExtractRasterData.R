### Extract CSI Data ###

maine_trees <- read_csv("maine_trees.csv")

cord <- data.frame(maine_trees$X_Cord, maine_trees$Y_Cord)
cord <- cord %>% rename(x = maine_trees.X_Cord, y = maine_trees.Y_Cord)
coordinates(cord) <- cord
proj4string(cord) <- CRS("+proj=longlat +datum=WGS84")

CSI <- raster("/Users/ryansmith/Thesis/GIS/Climate Site Index/EastSI_ENSEMBLE_rcp60_2030.tif")
BMGI <- raster("/Users/ryansmith/Thesis/GIS/BGI Raster/BGI_S2REP_ME_Update.tif")

CSIdata   <- extract(CSI, cord, method = 'simple', df = TRUE)
BMGIdata  <- extract(BMGI, cord, method = 'simple', df = TRUE)

maine_trees$CSI <- CSIdata$EastSI_ENSEMBLE_rcp60_2030
maine_trees$BMGI <- BMGIdata$BGI_S2REP_ME_Update

A <- get_elev_point(cord)
Elevations <- A[[3]]
