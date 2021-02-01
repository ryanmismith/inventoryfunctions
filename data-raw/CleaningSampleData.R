tree <- readr::read_csv("data-raw/Tree.csv")
plot <- readr::read_csv("data-raw/Plot.csv")

tree <- tree %>% dplyr::select(PlotIndex, TRecIndex, SpeciesCode, SpeciesName, Dbh, HmObs, Ht, CC03, CC04)
plot <- plot %>% dplyr::select(PlotIndex, Longitude, Latitude, PwPlotSz)

# Join the Long, Lat, and PlotSz to the Tree Table
married <- left_join(tree, plot, "PlotIndex")

# Filter out trees with DBH < 4"

married <- married %>% filter(Dbh > 4)

# Make smaller for testing functions

married <- married %>% filter(PlotIndex <= 50)

# Rename Variables so they are easier to work with

married <- married %>% rename(Plot = PlotIndex, Tree = TRecIndex, SPP = SpeciesCode, DBH = Dbh, HT = Ht,
                              SawHeight = HmObs, BAF = PwPlotSz, Y = Latitude, X = Longitude,
                              Form = CC03, Risk = CC04)

# Clean Species Codes and Eliminate SpeciesName variable

married$SPP[married$SpeciesName == "Hard Maple"] <- "SM"
married$SPP[married$SpeciesName == "White Birch"] <- "PB"

married <- married %>% dplyr::select(-SpeciesName)

married <- married %>% mutate(
  Stand = ifelse(Plot > 25, 2, 1)
)

# Convert to metric

married <- married %>% mutate(
  DBH = DBH*2.54,                   # Cm
  HT = HT*.3048,                    # Meters
  SawHeight = SawHeight*.3048,      # Meters
  BAF = 4.5                         # BAF Acres to Hectares
)

tree_data <- married

usethis::use_data(tree_data, overwrite = TRUE)
