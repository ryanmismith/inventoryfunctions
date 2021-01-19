trees <- read_csv('/Users/ryansmith/Thesis/Practice Data/PEF.Inv.Data.for.FVS.TREE.csv')
trees_2010 <- trees %>% filter(YEAR == 2010)

trees_2010 <- trees_2010 %>%
  mutate(
  DBH = DBH*2.54,
  HT = HT*0.3048,
  EXPF = EXPF*2.47,
  Bark = c('ib'),
  Planted = FALSE,
  SDIPlot = SDI.Plot(STAND, PLOT, TREE, DBH, EXPF),
  SDIMax = SDI.Max(STAND, PLOT, TREE, SP, EXPF),
  BA = BA(DBH),
  CCF = CrownCompF(STAND, PLOT, TREE, SP, DBH, EXPF),
  ID = Unique.ID(STAND, PLOT),
  RD = (SDIPlot/SDIMax),
  BAPH = BAPH(STAND, PLOT, BA, EXPF)
  )

tempvol <- mapply(KozakTreeVol, 'ib', trees_2010$SP, trees_2010$DBH, trees_2010$HT)
trees_2010$vol <- tempvol * trees_2010$EXPF

trees_2010 <- trees_2010 %>%
  group_by(ID) %>%
  arrange(desc(DBH), .by_group = TRUE)
trees_2010 <- trees_2010 %>%
  mutate(
    BAL = BA.Larger.Trees(ID, DBH, BA),
  )

trees_2010 <- trees_2010 %>%
  group_by(ID) %>%
  arrange(desc(DBH), .by_group = TRUE)
trees_2010 <- trees_2010 %>%
  mutate(
    CCFL = CCF.Larger(ID, SP, DBH, EXPF)
  )


tree21 <- trees_2010 %>% filter(PLOT == c(11, 21, 22))

