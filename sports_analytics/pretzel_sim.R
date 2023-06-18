install.packages("tidyverse")


cost_to_buy <- .5
cost_to_sell <- 1.5
blowout <- 0.4
order <- 170

pretzel <- function(order,blowout = .4, cost_to_buy = .5, cost_to_sell = 1.5){
  demand <- ifelse(runif(1)<blowout, sample(180:200,1),sample(120:180, 1))
  profit <- min(demand,order) * cost_to_sell - order * cost_to_buy
  profit
  }

pretzel(170)
profit170 <- map_dbl(rep(170,10000),pretzel)

profit_all <- map_dbl(rep(c(170,190,210),each = 10000),pretzel)
results <- tibble(order = rep(c(170,190,210),each = 10000), 
                  profit = profit_all)

results %>%
  group_by(order) %>%
  summarize(mean = mean(profit), sd = sd(profit))


results %>%
  ggplot(aes(x = profit)) + geom_histogram() + facet_grid(order~.)

results %>%
  ggplot(aes(x = profit, group = factor(order),color = factor(order))) +geom_density()
