#install/load libraries
install.packages("nycflights13")

library(tidyverse)
library(nycflights13)

#explore data
nycflights13::flights
head(flights)



#create new dataset
flight_delay <- select(flights, 
                       dep_delay, arr_delay, air_time,distance,month, carrier)

flight_delay <- mutate(flight_delay, loss = arr_delay - dep_delay, AverageSpeed = 60 * distance / air_time)

lateAA <- filter(flights, arr_delay > 0, carrier == "AA")

flights %>%
  filter(arr_delay > 0, carrier == "AA") %>%
  group_by(month) %>%
  summarise(count = n())

flights %>%
  mutate(AverageSpeed = 60 * distance / air_time) %>%
  filter(origin == "JFK", dest == "CMH", !is.na(AverageSpeed), !is.na(arr_delay)) %>%
  summarise(minspeed = min(AverageSpeed), maxspeed=max(AverageSpeed),
            mindelay = min(arr_delay), maxdelay = max(arr_delay))

flights %>%
  filter(!is.na(dep_delay)) %>%
  group_by(carrier) %>%
  summarise(meandelay = mean(dep_delay)) %>%
  arrange(meandelay)



flights %>%
  group_by(carrier, dest) %>%
  summarise(count = n()) %>%
  mutate(rank = rank(-count)) %>%
  filter(rank == 1)
