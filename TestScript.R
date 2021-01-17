library(readr)
trees <- read_csv('/Users/ryansmith/Thesis/Practice Data/PEF.Inv.Data.for.FVS.TREE.csv')
trees_2010 <- trees %>% filter(YEAR == 2010)

trees_2010 <- trees_2010 %>%
  mutate(
  DBH = DBH*2.54,
  HT = HT*0.3048,
  EXPF = EXPF*2.47,
  SDI.Plot = SDI.Plot(STAND, PLOT, TREE, DBH, EXPF),
  SDI.Max = SDI.Max(STAND, PLOT, TREE, SP, EXPF),
  BA = BA(DBH),
  RD = RD(SDI.Plot, SDI.Max),
  CCF = CrownCompF(STAND, PLOT, TREE, SP, DBH, EXPF)
)


trees_2010 <- trees_2010 %>%
  mutate(
   BAL = BA.Larger.Trees(STAND, PLOT, TREE, DBH, BA)
  )

Temp <- trees_2010 %>%
  group_by(PLOT, STAND) %>%
  arrange(desc(DBH), .by_group = TRUE)

tree21 <- trees_2010 %>% filter(PLOT == c(11, 21, 22))

do()
