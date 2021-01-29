### Extract CSI Data ###

cord <- data.frame(trees$X, trees$Y)
coordinates(cord) <- cord
proj4string(cord) <- CRS("+proj=longlat +datum=WGS84")

CSI <- raster("/Users/ryansmith/Thesis/GIS/Climate Site Index/EastSI_ENSEMBLE_rcp60_2030.tif")
BMGI <- raster("/Users/ryansmith/Thesis/GIS/BGI Raster/BGI_S2REP_ME_Update.tif")

CSIdata   <- extract(CSI, cord, method = 'simple', df = TRUE)
BMGIdata  <- extract(BMGI, cord, method = 'simple', df = TRUE)

trees$CSI <- CSIdata$EastSI_ENSEMBLE_rcp60_2030
trees$BMGI <- BMGIdata$BGI_S2REP_ME_Update

A <- get_elev_point(cord)
Elevations <- A[[3]]
