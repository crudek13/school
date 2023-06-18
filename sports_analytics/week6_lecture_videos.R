library(tidyverse)

map(c(9, 16, 25), sqrt)

map(iris, mean, na.rm = T)

# Chuck-a-Luck
# Choose a number
# Roll 3 dice.  Win $1 for each die that matches your number.  Lose $1 if no match
diceroll <- function(numroll = 3){
  sample(1:6, size = numroll, replace = T)
}
map(rep(1, 1000), diceroll)
map_int(rep(1,1000),diceroll)
hist(map_int(rep(1,1000),diceroll))
diceroll(3)

chuck <- function(pick = 1){
  matches <- sum(diceroll(5) == pick)
  ifelse(matches == 0, -1, matches)
}


chuck()


mean(map_dbl(rep(1,10000), chuck))

table(map_dbl(rep(1,10000),chuck))






# Pulling the goalie
# Sim in 10 second intervals 
# Assumptions:  - Only 1 goal possible in 10 second interval
#               - Full strength: Each team scores 0.05 goals/min
#               - Goalie pulled: You score 0.08 goals/min, they score 0.12 goals/min on empty net
#               - No penalties or power plays (simplest case)
#               - Goalie reinserted if game is tied.  Goalie remains pulled as long as team is down
#               - If leading team falls behind, they will pull their goalie immediately
# You're down 1 with 5 minutes left.  When to pull?

last5min <- function(pulltime = 300, evenGPM = 0.05, manupGPM = 0.08, emptynetGPM = 0.12){
  margin <- -1
  for(i in 1:30){
    my_prob_goal <- evenGPM/6
    opp_prob_goal <- evenGPM/6
    if(margin < 0 & pulltime >= 300 - 10*(i-1)){
      my_prob_goal <- manupGPM/6
      opp_prob_goal <- emptynetGPM/6
    }
    if(margin > 0){
      my_prob_goal <- emptynetGPM/6
      opp_prob_goal <- manupGPM/6
    }
    # Score?
    random <- runif(1)
    if(random < my_prob_goal){
      margin <- margin + 1
    }
    if(random > 1 - opp_prob_goal){
      margin <- margin - 1
    }
    #print(margin)
  }
  margin
}

last5min()

table(map_dbl(rep(60,1000),last5min))/1000

table(map_dbl(rep(300,1000),last5min))

margins <- matrix(rep(0, 51000), nrow = 1000)
for(i in 1:51){
  margins[,i]<- map_dbl(rep(10*(i-1),1000),last5min)
}
plot(colSums(margins>=0)/1000, type = 'l')






# question 2
distances <- c(-5,0,2,5,10)
probabilities <- c(.05, .25, .35, .25, .1)
num_simulations <- 10000

simulate_success <- function(distances, probabilities, num_simulations) {
  successes <- 0
  
  for (i in 1:num_simulations) {
    total_distance <- 0
    num_attempts <- 0
    
    while (num_attempts < 3 && total_distance < 10) {
      num_attempts <- num_attempts + 1
      outcome <- sample(distances, size = 1, prob = probabilities)
      total_distance <- total_distance + outcome
    }
    
    if (total_distance >= 10 && num_attempts <= 3) {
      successes <- successes + 1
    }
  }
  
  success_probability <- round(successes / num_simulations, 5)
  return(success_probability)
}

success_probability <- simulate_success(distances, probabilities, num_simulations)

# Print the success probability
print(success_probability)





#q3
probs <- c(.6, .55, .48, .45, .48, .55, .5)

simulate_series_success <- function(probs) {
  num_games <- 7
  wins <- 0
  
  for (game in 1:num_games) {
    outcome <- rbinom(1, size = 1, prob = probs[game])
    if (outcome == 1) {
      wins <- wins + 1
    }
    
    if (wins >= 4) {
      break
    }
  }
  
  success <- ifelse(wins >= 4, TRUE, FALSE)
  return(success)
}

success <- simulate_series_success(probs)

# Print whether the team achieved success
if (success) {
  print("The team achieved success!")
} else {
  print("The team did not achieve success.")
}







simulate_multiple_series <- function(probs, num_simulations) {
  successes <- 0
  
  for (i in 1:num_simulations) {
    success <- simulate_series_success(probs)
    
    if (success) {
      successes <- successes + 1
    }
  }
  
  success_rate <- successes / num_simulations
  return(success_rate)
}

# Run 1000 simulations and calculate success rate
num_simulations <- 1000
success_rate <- simulate_multiple_series(probs, num_simulations)

# Print the success rate
print(paste0("Success rate: ", success_rate))





#q4
simulate_seasons <- function(num_seasons, num_games, game_probability, min_streak_length) {
  seasons_with_streak <- 0
  
  for (season in 1:num_seasons) {
    win_streak <- 0
    streak_detected <- FALSE
    
    for (game in 1:num_games) {
      outcome <- rbinom(1, size = 1, prob = game_probability)
      
      if (outcome == 1) {
        win_streak <- win_streak + 1
        if (win_streak >= min_streak_length) {
          streak_detected <- TRUE
          break
        }
      } else {
        win_streak <- 0
      }
    }
    
    if (streak_detected) {
      seasons_with_streak <- seasons_with_streak + 1
    }
  }
  
  probability_with_streak <- seasons_with_streak / num_seasons
  return(probability_with_streak)
}

# Set simulation parameters
num_seasons <- 5000
num_games <- 162
game_probability <- 0.584
min_streak_length <- 14

# Run the simulation
probability_with_streak <- simulate_seasons(num_seasons, num_games, game_probability, min_streak_length)

# Print the probability of a season including a win streak of 14 games or longer
print(paste0("Probability of a season with a ", min_streak_length, "-game win streak or longer: ", probability_with_streak))








