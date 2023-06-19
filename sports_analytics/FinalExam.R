library(tidyverse)

library(Lahman)

teams15_18 <- Teams %>%
  filter(yearID >= 2015, yearID <= 2018) %>%
  mutate(OBP = (H + BB + HBP)/(AB + BB + HBP + SF),
         SLG = (H + X2B + 2 * X3B + 3 * HR)/AB,
         OPS = OBP + SLG,
         WHIP = (BBA + HA)/(IPouts/3))

summary(lm(W ~ OPS+WHIP, teams15_18))














#q4
fielding <- Lahman::Fielding

fielding1 <- fielding %>%
  filter(yearID > 1950) %>%
  mutate(fielding_attempts = PO + A + E) %>%
  mutate(fielding_percentage = (PO + A)/fielding_attempts) %>%
  filter(fielding_attempts > 100) %>%
  group_by(POS) %>%
  summarise(avg_fp = round(mean(fielding_percentage),5)) %>%
  arrange(avg_fp)
fielding1








#q18

(.460778*5) + (1.923468*1.95) + (-3.887444*.74) + (3.455785*.24) + (14.803375*.682) + (-.494903*24) + (.005805*1) -.176970








