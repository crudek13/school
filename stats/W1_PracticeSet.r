money <- 27


str(money)


(3 + 1 == 2) || (TRUE && (4 == 2 * 2))



x <- c(3, 1, 2, "Hello", 4)
x[3]


product <- c("iPod", "Zune", "Teddy Bear", "Ice Cream", "Sewing Machine")
product[2]



mtcars[mtcars$cyl == 4,]


library(tidyverse)

test <- mtcars$mpg
str(test)
test2 <- mtcars["mpg"]
str(test2)

mtcars(mpg)


mtcars_mpg

install.packages("gapminder")
library(gapminder)

gapminder %>%
  filter(year == 2007, country == "Japan") %>%
  summarise(avg = mean(lifeExp))



?gapminder


gapminder %>%
  filter(year == 2002) %>%
  group_by(continent) %>%
  summarise(meanGDP = mean(gdpPercap)) %>%
  arrange(desc(meanGDP))




install.packages("babynames")
library(babybnames)

babynames %>%
  filter(year == 2015, sex == "M") %>%
  arrange(desc(n))
  

top_m_names <- babynames %>% 
  filter(sex == "M", year > 2000) %>%
  group_by(year) %>%
  #top_n(10,n) %>%
  arrange(desc(year))



babynames %>%
  filter(sex == "M") %>%
  group
  


urban <- babynames %>%
  filter(sex == "M", name == "Urban") %>%
  arrange(desc(year))
