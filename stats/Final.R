?tidyverse
?fivethirtyeight::US_births_2000_2014
?fivethirtyeight::classic_rock_song_list
?fivethirtyeight::fandango
?nycflights13::flights
?Lock5Data::RestaurantTips
?Lock5Data::Cereal
?Lock5Data::HollywoodMovies2011





moves <- Lock5Data::HollywoodMovies2011

summary(
lm(WorldGross ~ Budget + TheatersOpenWeek + Genre, moves)
)

mod <- lm(WorldGross ~ Budget + TheatersOpenWeek + Genre, moves)

plot(mod$residuals)


85.3035 + (6.6755*10)



mtcars

0.7446
summary(lm(mpg ~ wt + I(wt^2) + hp + wt:hp, mtcars))


summary(lm(mpg ~ wt + hp, mtcars))






cereal <- Lock5Data::Cereal
summary(lm(formula = Calories ~ Sugars, data = Cereal))


fivethirtyeight::US_births_2000_2014 %>%
  ggplot(aes(factor(date_of_month), births)) +
  geom_boxplot()



cor(Cereal[,-c(1,2)])


(2*.1)+(2.5*.2)+(3*.5)+(3.5*.2)



1 - pbinom(5, 10, .2)



pnorm(109999,105000,18200)



qnorm(.4,235000,30400)


qnorm(.6082152,235000,30400)



pnorm(4.99999, 4.8 ,1.3)




avg <- 5.1
sd <- 2.4
n <- 36
Con <- .95

lower <- avg - qt(1 - (1 - Con)/2, n - 1) * sd / sqrt(n)
upper <- avg + qt(1 - (1 - Con)/2, n - 1) * sd / sqrt(n)

paste(lower, ",", upper)



xbar <- 89.22
mu = 85
sd = 17.3
n = 85


t <- (xbar - mu) / (sd/sqrt(n))
















us_births <- fivethirtyeight::US_births_2000_2014 
  
  
t <- (xbar - mu) / (sd/sqrt(n))
us_births %>%
  group_by(month) %>%
  summarise(tot = sum(births)) %>%
  arrange(tot)


us_births %>%
  filter(date == '2001-04-13' | date == '2011-11-25' | date == '2010-01-01' | date == '2011-12-25')




movies <- fivethirtyeight::fandango
movies






songs <- fivethirtyeight::classic_rock_song_list
songs


songs %>%
  filter(release_year >= 1983) %>%
  summarise(med = median(playcount))




flights <- nycflights13::flights
flights


flights %>%
  filter(!is.na(dep_delay)) %>%
  IQR(dep_delay)


IQR(flights$dep_delay)



x <- flights$dep_delay %>%

x2 <- x %>%
  filter(!is.na(x))

summary(x)
IQR(x)
