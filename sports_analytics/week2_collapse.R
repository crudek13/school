#Prob SoCar wins in regulation
# P(X < -16.5)

wininreg <- pnorm(-16.5, 0, 9.622)
#Prob SoCar goes to OT
gotoot <- pnorm(-15.5, 0, 9.622) - pnorm(-16.5, 0, 9.622)

comeback <- wininreg + .5 * gotoot

#With team rating
wininreg <- pnorm(-16.5, 1.1, 9.622)
gotoot <- pnorm(-15.5, 1.1, 9.622) - pnorm(-16.5, 1.1, 9.622)

comeback <- wininreg + .5 * gotoot

#With team rating
sdev <- 16/sqrt(60/8)
wininreg <- 1-pnorm(10.5, 2*8/60, sdev)
gotoot <- pnorm(10.5, 2*8/60, sdev) - pnorm(9.5, 2*8/60, sdev)

comeback <- wininreg + .5 * gotoot








###################
### PROBLEM SET ###
###################

library(tidyverse)
library(nflreadr)

year <- 2022
nflpbp <- nflreadr::load_pbp(year)

#Q1: On passing plays WITHOUT A PENALTY in the regular season (season_type == "REG"), which team had the:

nflpbp %>%
  filter(season_type == "REG" & play_type == 'pass' & penalty == '0') %>%
  select(posteam, epa) %>%
  group_by(posteam) %>%
  summarise(med_epa = median(epa)) %>%
  arrange(med_epa)
  
nflpbp %>%
  filter(season_type == "REG" & play_type == 'pass' & penalty == '0') %>%
  select(posteam, epa) %>%
  group_by(posteam) %>%
  summarise(sd_epa = sd(epa)) %>%
  arrange(sd_epa)


#Q2: On rushing plays WITHOUT A PENALTY in the regular season (season_type == "REG"), which team had the:

nflpbp %>%
  filter(season_type == "REG" & play_type == 'run' & penalty == '0') %>%
  select(posteam, epa) %>%
  group_by(posteam) %>%
  summarise(metric = sd(epa)) %>%
  arrange(desc(metric))


nflpbp %>%
  filter(season_type == "REG" & play_type == 'run' & penalty == '0') %>%
  select(posteam, epa) %>%
  group_by(posteam) %>%
  summarise(metric= sd(epa)) %>%
  arrange(metric)


#Q3
nflpbp %>%
  filter(season_type == "REG" & play_type == 'pass' & penalty == '0') %>%
  select(passer, epa, pass_attempt) %>%
  group_by(passer) %>%
  summarise(metric = mean(epa), attempts = sum(pass_attempt)) %>%
  filter(attempts > 100) %>%
  arrange(metric)


nflpbp %>%
  filter(season_type == "REG" & play_type == 'run' & penalty == '0') %>%
  select(rusher, epa, rush_attempt) %>%
  group_by(rusher) %>%
  summarise(metric = mean(epa), attempts = sum(rush_attempt)) %>%
  filter(attempts > 100) %>%
  arrange(desc(metric))


#Q4
nflpbp %>%
  filter(season_type == "REG" & play_type == 'run' & penalty == '0') %>%
  select(rusher, epa, rush_attempt) %>%
  group_by(rusher) %>%
  summarise(metric = mean(epa), attempts = sum(rush_attempt)) %>%
  filter(attempts > 50) %>%
  arrange(desc(metric))

nflpbp %>%
  filter(season_type == "REG" & play_type == 'run' & penalty == '0') %>%
  select(rusher_player_name, epa, rush_attempt) %>%
  group_by(rusher_player_name) %>%
  summarise(metric = mean(epa), attempts = sum(rush_attempt)) %>%
  filter(attempts > 50) %>%
  arrange(desc(metric))

#Q5
(0*.45) + (1*.1) + (2*.4) + (3*.05)





#Q12
100p - 600(1-p) > 0 


