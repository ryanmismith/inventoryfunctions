trees <- read_csv('/Users/ryansmith/Thesis/Practice Data/PEF.Inv.Data.for.FVS.TREE.csv')
trees_2010 <- trees %>% filter(YEAR == 2010)

trees_2010 <- trees_2010 %>%
  mutate(
    DBH = DBH*2.54,
    HT = HT*0.3048,
    EXPF = EXPF*2.47,
    SDIPlot = SDI.Plot(STAND, PLOT, TREE, DBH, EXPF),
    SDIMax = SDI.Max(STAND, PLOT, TREE, SP, EXPF),
    BA = BA(DBH),
    CCF = CrownCompF(STAND, PLOT, TREE, SP, DBH, EXPF),
    ID = Unique.ID(STAND, PLOT),
    RD = RD(SDIPlot, SDIMax),
    BAPH = BAPH(STAND, PLOT, BA, EXPF),
    TPH = TPH(STAND, PLOT, DBH, EXPF)
  )

### Volume Output (Tree Vol * EXPF)

tempvol <- mapply(KozakTreeVol, 'ib', trees_2010$SP, trees_2010$DBH, trees_2010$HT)
trees_2010$TreeVol <- tempvol * trees_2010$EXPF


### BAL Values

trees_2010 <- trees_2010 %>%
  group_by(ID) %>%
  arrange(desc(DBH), .by_group = TRUE)
trees_2010 <- trees_2010 %>%
  mutate(
    BAL = BA.Larger.Trees(ID, DBH, BA)
  )

### CAFL Values

trees_2010 <- trees_2010 %>%
  group_by(ID) %>%
  arrange(desc(DBH), .by_group = TRUE)
trees_2010 <- trees_2010 %>%
  mutate(
    CCFL = CCF.Larger(ID, SP, DBH, EXPF)
  )

### 100 Tallest Trees

trees_2010 <- trees_2010 %>%
  group_by(ID) %>%
  arrange(desc(HT), .by_group = TRUE)
trees_2010 <- trees_2010 %>%
  mutate(
    Tall = TallestTrees(ID, HT, EXPF)
  )


### Examine outputs in two plots:

tree33 <- trees_2010 %>% filter(PLOT == 33)



