library(tidyverse)
mod <- lm(mpg ~ wt + disp + hp, mtcars)
summary(mod)

library(gapminder)

gap2007 <- gapminder %>%
  filter(year == 2007)

# intercept = lifeexp for Africa with 0 GDP
# better to see changes between the continents
mod <- lm(lifeExp ~ gdpPercap + continent, gap2007)
summary(mod)


# how it compares to 0
# better if you dont care about changes compared to continnent and just compared to 0
mod <- lm(lifeExp ~ gdpPercap + continent + 0, gap2007)
summary(mod)

#gdpPercap - this is for Africa
mod <- lm(lifeExp ~ gdpPercap * continent, gap2007)
summary(mod)


#are the parralel?
  # Africa, maybe Asia are different
gap2007 %>%
  ggplot(aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point() +
  geom_smooth(method = "lm", se=FALSE)


#continent is significant, cotinent is significant, the interaction is not that significant
#pvalue between .01(accept) and .1(reject), if you are in between its kinda a weird space
anova(mod)




#In Class Example, slide 3
#have to round up
#Part 1
# qnorm = you give me area, i give you cutoff
qnorm(.98, 313, 57)

#Part 2
#pnorm = you give me the cutoff i give you the area
#could do tail but 1- is easier
#prob of running out of hamburgers
ph <- 1-pnorm(400, 313, 57)

#prob of running out of chicken sandwhich
pc <- 1-pnorm(150, 93, 22)

#probability of pc or ph minus prob of both
pc + ph - (pc*ph)


#In Class Example, slide 4
### SOMETHING LIKE THIS WILL BE ON EXAM

## CENTRAL LIMIT THEOREM 










