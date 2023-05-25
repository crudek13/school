library(tidyverse)


pga <- readxl::read_xlsx("sports_analytics/pga_2007.xlsx")

#Q1
summary(lm(Scoring ~ Driving + Fairways + Greens + Putts, pga))

0.013217*10


#Q3
summary(lm(Earnings ~ Driving + Fairways + Greens + Putts, pga))

pga_earnings_model <- lm(Earnings ~ Driving + Fairways + Greens + Putts, pga)

tiger <- pga %>%
  filter(Player == "Tiger Woods") 


predict(pga_earnings_model, tiger)

predicted = 2720706 


actual = 10867050

10867050 - 2720706




#Q4
library(Lahman)

View(Teams)

Teams %>%
  filter(yearID %in% c('2015', '2016', '2017', '2018', '2019'))

team_15_19 <- Teams %>%
  filter(yearID %in% c('2015', '2016', '2017', '2018', '2019'))


?Lahman


team_15_19 %>%
  filter(lgID %in% c("AL", "NL")) %>%
  summary(lm(R ~ as.factor(lgID)))


summary(lm(R ~ as.factor(lgID), team_15_19))



#Q5
summary(lm(R ~ as.factor(lgID), team_15_19))


#Q7
browns <- readxl::read_xlsx("sports_analytics/browns.xlsx")

summary(glm(Win ~ PassNYA, family = binomial, browns))
0.46341 * 6



#Q8
summary(glm(Win ~ PassNYA + RushYA + as.factor(Home) + DYP, family = binomial, browns))



#Q12
E(f) = .2 - .1q
E(c) = .25 + .2q

.2 - .1q = .25 + .2q
-.05=.3q

