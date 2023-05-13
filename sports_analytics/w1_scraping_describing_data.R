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


### New Version FASTER

install.packages("nflreadr")

library(nflreadr)

pbp2021 <- load_pbp(2021)

ps2021 <- load_player_stats(2021)

test <- nflreadr::load_nextgen_stats()


### Week 1 In Class

ngs <- nflreadr::load_nextgen_stats()


qb_records <- ngs %>%
  filter(season == '2022' & season_type == 'REG' & week != 0) %>%
  group_by(team_abbr, player_display_name) %>%
  count() %>%
  arrange(desc(n))


ngs %>%
  filter(season == '2022' & season_type == 'REG' & week != 0) %>%
  inner_join(qb_records, by = c("team_abbr" = "team_abbr", "player_display_name" = "player_display_name")) %>%
  filter(n >= 9) %>%
  group_by(player_display_name, team_abbr) %>%
  summarise(acay = mean(avg_completed_air_yards), attt = mean(avg_time_to_throw)) %>%
  ggplot(aes(x=acay, y=attt, label=player_display_name)) + geom_text() + 
  labs(title = "Good O line or Strong Arm?", x = "Avg Completed Air Yards", y = "Avg Time To Throw")

library(ggrepel)

ngs %>%
  filter(season == '2022' & season_type == 'REG' & week != 0) %>%
  inner_join(qb_records, by = c("team_abbr" = "team_abbr", "player_display_name" = "player_display_name")) %>%
  filter(n >= 9) %>%
  group_by(player_display_name, team_abbr) %>%
  summarise(acay = mean(avg_completed_air_yards), attt = mean(avg_time_to_throw), atts = sum(attempts)) %>%
  ggplot(aes(x=acay, y=attt, label=player_display_name, size=atts)) + geom_point() + ggrepel::geom_label_repel()


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
