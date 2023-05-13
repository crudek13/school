#####################################
### Sean Lahman Baseball database ###
#####################################

install.packages("Lahman")
library(Lahman)
library(tidyverse)

#?Lahman

#Lahman::
  
#Top 10 seasons in Batting Average since 1970 (min 100 ABs)
View(Batting)

Batting %>%
  mutate(BA = H / AB) %>%
  filter(yearID >= 1970, AB >= 100) %>%
  top_n(10, BA) %>%
  left_join(People, by = "playerID") %>%
  select(nameFirst, nameLast, yearID, teamID, lgID, BA) %>%
  mutate(name = paste(nameFirst, nameLast, yearID)) %>%
  ggplot(aes(x=fct_reorder(name,BA), y=BA, fill=lgID)) + 
    geom_col() +
    coord_flip() +
    labs(title = "Top 10 Batting Averages since 1970 (min 100 ABs)")


# Trajectory of most strikeouts per year
view(Pitching)


Pitching %>%
  group_by(playerID, yearID) %>%
  summarise(totalk = sum(SO)) %>%
  ungroup() %>%
  group_by(yearID) %>%
  summarise(maxK = max(totalk)) %>%
  left_join(Pitching, by = c("yearID" = "yearID", "maxK" = "SO")) %>%
  group_by(yearID) %>%
  slice(1) %>%
  left_join(People, by = "playerID") %>%
  select(nameFirst, nameLast, yearID, maxK) %>%
  ggplot(aes(x = yearID, y = maxK, label=nameLast)) + geom_line() + geom_text()



########################
### NFLFASTR Package ###
########################

install.packages("nflfastR")
install.packages("tictoc")
library(nflfastR)
library(tictoc)

# SCRAPE the data
# 237.85 sec elapsed
tic()
games2020 <- nflfastR::fast_scraper_schedules(seasons = 2020)
nfl2020 <- nflfastR::fast_scraper(game_ids = games2020$game_id)
toc()

tic()
games2000 <- nflfastR::fast_scraper_schedules(seasons = 2000)
nfl2000 <- nflfastR::fast_scraper(game_ids = games2000$game_id)
toc()

# alternatively, you could read it from github
# 2 sec
# tic()
# nfl2000 <- readRDS(
#   url("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2000.rds")
# )
# toc()


nfl2020 %>%
  distinct(play_type)



# passing vs rushing on first down
nfl2020 %>%
  select (posteam, down, play_type) %>%
  filter(down == 1, play_type %in% c('pass', 'run')) %>%
  group_by(posteam) %>%
  summarize(pct_1st_down_pass = mean(play_type == 'pass')) %>%
  arrange(-pct_1st_down_pass)

nfl2020 <- nfl2020 %>%
  mutate(season = "Y2020")

nfl2000 %>%
  mutate(season = "Y2000") %>%
  bind_rows(nfl2020) %>%
  select (posteam, down, play_type, season) %>%
  filter(down == 1, play_type %in% c('pass', 'run')) %>%
  group_by(posteam, season) %>%
  summarize(pct_1st_down_pass = mean(play_type == 'pass')) %>%
  pivot_wider(names_from = season, values_from = pct_1st_down_pass) %>%
  ggplot(aes(x = Y2000, y=Y2020, label = posteam)) + geom_abline(intercept = 0, slope = 1) + geom_text()





######################
### Other Packages ###
######################

library(devtools)

# college basketball
devtools::install_github("jflancer/bigballR")
library(bigballR)

# basketball
devtools::install_github("rtelmore/ballr")
library(ballr)

pergamestats <- NBAPerGameStatistics(season = 2018)

# average rebounds
pergamestats %>%
  group_by(pos) %>%
  summarise(mean(trb))


# average rebounds
pergamestats %>%
  group_by(pos) %>%
  summarise(mean(pts), num = n())
