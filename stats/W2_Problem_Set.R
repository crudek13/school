library(nycflights13)

flights <- nycflights13::flights


cor(flights$arr_delay, flights$dep_delay,  use="complete.obs")




install.packages("Ecdat")
library(Ecdat)

Ecdat::Computers

Computers



mean(Computers$price)


install.packages("fivethirtyeight ")
library(fivethirtyeight)


  ggplot(classic_rock_song_list, aes(x=playcount)) +
  geom_histogram()




  classic_rock_song_list %>%
    filter(release_year > 1982) %>%
    summarise(median(playcount))

