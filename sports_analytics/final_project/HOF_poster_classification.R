# packages
library(tidyverse)
library(ballr)


##### FINAL
# use 1950 to build the initial df, iterator will start at 1951
master_df <- NBAPerGameStatistics(season = 1950)
master_df$Year <- 1950


for (x in 1965:2022) {
  year <- x + 1 # this is my counter (starts at 1951)
  temp_df <- NBAPerGameStatistics(season = year)
  temp_df$Year <- year
  master_df <- rbind(master_df,temp_df)
  Sys.sleep(900) # wait 15 min
}

# stopped at 1964... too many queries?


# write.csv(data, "sports_analytics/final_project/nba_stats_1950_2023.csv")


### ANALYSIS

# read in data
bball <- read.csv("sports_analytics/final_project/nba_stats_1950_2023.csv")

bball <- bball %>%
  mutate(hof = if_else(
    substr(player, nchar(player) - 1 + 1, nchar(player)) == "*", 1, 0)) 


# *HOF NOTE*  
#Player: A player must be fully retired for four full seasons before 
#being eligible for Enshrinement. He/she may then be considered for Enshrinement in the fifth year of retirement.

# last HOF class = 2022
# last season must be 2018 or less

bball <- bball %>%
  group_by(player) %>%
  mutate(last_season = max(Year)) %>%
  mutate(first_season = min(Year)) %>%
  mutate(total_seasons = last_season-first_season) %>%
  mutate(hof_eligibility = if_else(
    last_season <= 2018, 1, 0
  )) %>%
  mutate(status = if_else(
    last_season < 2023, "retired", "current"
  ))


# position consolidation (from Nick)

bball <- bball %>%
  mutate(pos = if_else(pos == "G-F", "SF",
                       if_else(pos == "F-C", "PF",
                               if_else(pos == "F-G", "SF",
                                       if_else(pos == "G", "PG",
                                               if_else(pos == "F", "SF", pos)))))) %>% 
  mutate(position = sub("-.*", "", pos)) %>% 
  rename(pos_orig = pos) %>% 
  relocate(position, .after = "player")


# total games (for weighted avg)

bball <- bball %>%
  group_by(player) %>%
  mutate(total_games = sum(g))


# find most occurring position

main_position <- bball %>%
  group_by(player) %>%
  count(player, position) %>%
  arrange(desc(n)) %>%
  filter(row_number()==1) %>%
  select(player, position)



# final df v1

bball_clean <- inner_join(bball, main_position, by = "player") %>%
  select(-c("position.x", "X.1","X","rk", "pos_orig", "link")) %>%
  rename("position" = "position.y") %>% 
  relocate(position, .after = "player")




### Weighted Average Calculations

# step 1: multiply the number of games in that season by each metric. This is the main piece of the weighting
bball_wavg <- bball_clean %>%
  group_by(player) %>%
  mutate(
    across(
      .cols = 7:28,
      ~ g*.,
      .names = "{.col}_x_g"
    )
  ) %>%
  select(1,2,31:59) %>%
  ungroup()

# step 2: sum up each metric for player and include other grouping vars
bball_wavg <- bball_wavg %>%
  group_by(player, position, hof, last_season, first_season, total_seasons, hof_eligibility, status, total_games) %>%
  summarise(
    across(c(1:22), sum, na.rm=TRUE
    )
  ) %>%
  ungroup()

# step 3: divide by the total game for that player in that position
bball_wavg <- bball_wavg %>%
  mutate(
    across(
      .cols = 10:31,
      ~ ./total_games,
      .names = "{.col}_x_g"
    )
  ) %>%
  select(1:9, 32:53)

# rename the cols
names(bball_wavg) <- c('player', 'position', 'hof', 'last_season', 'first_season', 'total_seasons', 'hof_eligibility', 'status', 
                'total_games', 'fg', 'fga', 'fgpercent', 'x3p', 'x3pa', 'x3percent', 'x2p', 'x2pa', 'x2percent', 'efgpercent', 'ft', 'fta', 
                'ftpercent', 'orb', 'drb', 'trb', 'ast', 'stl', 'blk', 'tov', 'pf', 'pts')  



### analysis

library(caret)
library(ROCR)
library(glmnet)


# training dataset

hof_only <- bball_wavg  %>%
  filter(hof_eligibility == 1 & last_season >= 2009) %>%
  select(-c(last_season, first_season, status, hof_eligibility))
# make hof factor
hof_only$hof<- as.factor(hof_only$hof)
str(hof_only)
hof_only$position<- as.factor(hof_only$position)
str(hof_only)

# testing dataset

non_hof <- bball_wavg  %>%
  filter(hof_eligibility == 0) %>%
  select(-c(last_season, first_season, status, hof_eligibility))
# make hof factor
non_hof$hof<- as.factor(non_hof$hof)
str(non_hof)
non_hof$position<- as.factor(non_hof$position)
str(non_hof)




# STEPWISE
library(My.stepwise)

variable_list <- c("position", "total_games", "total_seasons", "fga", "fgpercent", "x3pa", "x3percent", "x2pa", "x2percent", "fta", 
                     "ftpercent", "orb", "drb", "ast", "stl", "blk", "tov", "pf", "pts")
  
My.stepwise.glm(Y = "hof", variable.list = variable_list, data = hof_only, sle = 0.25, sls = 0.25, myfamily = "binomial")

stepwise <- glm(formula = hof ~ fta + total_games + drb + position + ast, 
                family = binomial(logit), data = hof_only)
summary(stepwise)



## Predict hof using hof_model2 ####
pred_1 <- predict(stepwise, hof_only, type="response") # Gives Predicted probabilities and we are storing in pred_1

pred_1c  <- ifelse(pred_1 >0.5,1,0) # Convert probabilities to class 1 or 0
head(pred_1) # Show the first 6 predicted probabilities from the data set
head(pred_1c) # Show the first 6 predicted classes from the data set

table(Predicted=pred_1c, Actual=hof_only$hof) # Gives Confusion Matrix

# add column with predicted value
hof_only$pred <- pred_1c 
hof_only$predperc <- pred_1




## Predict hof using hof_model2 ####
pred_2 <- predict(stepwise, non_hof, type="response") # Gives Predicted probabilities and we are storing in pred_1

pred_2c  <- ifelse(pred_2 >0.5,1,0) # Convert probabilities to class 1 or 0
head(pred_2) # Show the first 6 predicted probabilities from the data set
head(pred_2c) # Show the first 6 predicted classes from the data set

table(Predicted=pred_2c, Actual=non_hof$hof) # Gives Confusion Matrix

# add column with predicted value
non_hof$pred <- pred_2c 
non_hof$predperc <- pred_2







