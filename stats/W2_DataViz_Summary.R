# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: 
install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest
library(tidytuesdayR)
# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2022-07-05')
tuesdata <- tidytuesdayR::tt_load(2022, week = 27)

rent <- tuesdata$rent




summary(rent$county)

rent %>%
  filter(year == 2018) %>%
  ggplot(aes(x=county, y=price)) +
  geom_boxplot() +
  geom_jitter(width = .35, alpha=.12) +
  ggtitle("San Francisco Rent Prices by County in 2018")

