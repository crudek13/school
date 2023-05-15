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














##########################
### Week 1 Problem Set ###
##########################

library(Lahman)
library(tidyverse)

# Q1
# Which player in 2021 that faced MORE than 50 batters (BFP) had the LOWEST DICE? 
# Use the Lahman package Pitching table and the formula given on page 53 of Mathletics. You may use the 'Constant' of 3.1.

t < - Lahman::Pitching %>%
  left_join(People, by = "playerID")

View(Lahman::People)


dice <- Lahman::Pitching %>%
  filter(yearID == 2021 & BFP > 50) %>%
  left_join(People, by = "playerID") %>%
  mutate(DICE = 3.1 + (3 * (BB + HBP) + 13 * HR - 2 * SO)/(IPouts/3)) %>%
  arrange(DICE) %>%
  select(playerID, nameFirst, nameLast, DICE)
  


3.1 + (3*(11+2)+13*8-2*172)/149


3.1 + ((13*1+3)*(13+0)-2*64)/(110/3)


# Q2
# Examine the Lahman database (specifically the Batting table).  If you do not recognize the playerID, you may simply enter it as is.
# Most triples (X3B) in a season by a player after 1950.
# When entering the player, enter the first initial and last name.  For example, Babe Ruth would be typed as 'B. Ruth' (without the quotes).  
# If you don't recognize the player, you may enter the playerID exactly as is. Enter the year with 4 digits.

View(Lahman::Batting)

Lahman::Batting %>%
  filter(yearID > 1950) %>%
  arrange(desc(X3B))





# Q3
# Examine the Lahman database (specifically the Pitching table).  
# Most total strikeouts by a team in a season up to and including 2021--the last year in the Lahman package.  
# Note: strikeouts are listed as SO. Enter the teamID as it appears in the database and the year as a 4-digit number

View(Lahman::Pitching)

Lahman::Pitching %>%
  group_by(yearID, teamID) %>%
  summarise(totalso = sum(SO)) %>%
  arrange(desc(totalso))



# Q4
# In the shootout between the Rams and Chiefs, rank these play types from most expected points added  
# to fewest expected points added on average.  When using nflfastR, the GameID is 2018_11_KC_LA. 
# The code to download the data is below.

library(nflreadr)
library(nflfastR)
pbp18 <- nflreadr::load_pbp(seasons = 2018)
ramschiefs <- pbp18 %>%
  filter(game_id == "2018_11_KC_LA")
  

ramschiefs %>%
  select(play_type, epa) %>%
  group_by(play_type) %>%
  summarise(avg_epa = mean(epa)) %>%
  arrange(desc(avg_epa))



# Q5
# Looking at the play by play data from all of 2022, Pat Mahomes finished with the highest average expected points per pass (minimum 100 pass attempts).  
# How many expected points per pass was Mahomes better than his Superbowl opponent, Jalen Hurts on average? 
  
# Note: Please make sure to focus on pass plays only.  The 'passer' column will help but you need to filter on play_type.


pbp22 <- load_pbp(seasons = 2022)

pbp22 %>%
  filter(play_type == 'pass' & passer %in% c('J.Hurts', 'P.Mahomes') ) %>%
  select(passer, epa) %>%
  group_by(passer) %>% 
  summarise(avg = mean(epa)) %>%
  arrange(desc(avg))
  


#Q6
#Looking at the play by play data from 2022, which rusher with more than 150 carries created the highest median expected points per attempt?  
#In fact, NO ONE had a POSITIVE median expected points per rush!
  
#Make sure to only look at play_type == "run".  

rb_150 <- pbp22 %>%
  filter(play_type == 'run') %>%
  count(rusher_player_name) %>%
  arrange(desc(n)) %>%
  filter(n > 150)

rtbl <- pbp22 %>%
  filter(play_type == 'run') %>%
  inner_join(rb_150, by = "rusher_player_name") %>%
  select(rusher_player_name, epa) %>%
  group_by(rusher_player_name) %>%
  mutate(med = median(epa)) %>%
  select(rusher_player_name, med) %>%
  unique()





#Q7
#Create a side by side boxplot utilizing the basketball season data provided below of assists separated by position from 2022-23 data 
#(using hoopR to retrieve the data) only including people who have at least 1000 minutes played. 
#Only include the 3 major positions (center, guard, and forward).  The code below will create a dataset with the appropriate variables.
#Which position has a severe outlier?

install.packages('hoopR')
library(hoopR)

dat <- load_nba_player_box() 
seasondata <- dat %>%
  group_by(athlete_display_name, athlete_position_name)  %>%
  summarise(across(
    .cols = minutes:fouls, 
    .fns = list(sum = sum), na.rm = TRUE
  )) %>%
  mutate(position = case_when(athlete_position_name %in% c("Power Forward", "Small Forward")~ "Forward",
                              athlete_position_name %in% c("Point Guard", "Shooting Guard") ~ "Guard",
                              TRUE ~ athlete_position_name))




seasondata %>%
  filter(minutes_sum >= 1000) %>%
  ggplot(aes(x=position, y=assists_sum, label=athlete_display_name)) + geom_boxplot() + geom_text()




#Q8
#Using the hoopR package, download the NBA per game data for 2018-19.  Data to accomplish this is below.  
#Note: this is per game data (not totals).  How many MORE attempts per game (fga) did James Harden take than the person who attempted the 2nd most?


dat <- load_nba_player_box(2019) 


seasondata19 <- dat %>%
  group_by(athlete_display_name, athlete_position_name)  %>%
  summarise(across(
    .cols = minutes:fouls, 
    .fns = list(mean = mean), na.rm = TRUE
  ))

seasondata19 %>%
  arrange(desc(field_goals_attempted_mean))






dat %>%
  filter(did_not_play == FALSE) %>%
  group_by(athlete_display_name) %>%
  summarise(tot = mean(field_goals_attempted)) %>%
  arrange(desc(tot))


dat %>%
  filter(did_not_play == FALSE) %>%
  group_by(athlete_display_name) %>%
  summarise(tot = sum(field_goals_attempted)) %>%
  arrange(desc(tot))


seasondata19 %>%
  select(athlete_display_name, field_goals_attempted_mean) %>%
  arrange(desc(field_goals_attempted_mean))


seasondata19 %>%
  filter(athlete_display_name == 'James Harden')





#Q11
#Using the instructions in chapter 2 of Mathletics, find the runs created per game for all player seasons with at least 100 at bats (AB) 
#in the Lahman database.  Mathletics says Barry Bonds in 2004 has the top at 20.7 (he should by your numbers as well).  
#How many Runs created per game did the player/season in second have? Answer to the nearest 1 decimal place.

q11 <- Lahman::Batting %>%
  filter(yearID == 2016 & playerID == 'troutmi01') %>%
  mutate(TotalBases = (H-X2B-X3B-HR) + (2*X2B) + (3*X3B) + (4*HR)) %>%
  mutate(SluggingPercentage = TotalBases/AB) %>%
  mutate(RunsCreated = ((H+HBP+BB) * TotalBases) / (AB+BB+HBP)) %>%
  mutate(RCPG = ((RunsCreated / (((.982*AB) - H + GIDP + SF + SH + CS) / 26.83))))
         
q11 <- Lahman::Batting %>%
  filter(AB > 100) %>%
  mutate(TotalBases = (H-X2B-X3B-HR) + (2*X2B) + (3*X3B) + (4*HR)) %>%
  mutate(SluggingPercentage = TotalBases/AB) %>%
  mutate(RunsCreated = ((H+HBP+BB) * TotalBases) / (AB+BB+HBP)) %>%
  mutate(RCPG = ((RunsCreated / (((.982*AB) - H + GIDP + SF + SH + CS) / 26.83)))) %>%
  arrange(desc(RCPG))
         
         



