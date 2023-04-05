library(Lock5Data)
library(tidyverse)
library(MASS)

view(HappyPlanetIndex)

#Q1
summary(lm(Happiness ~ LifeExpectancy + factor(Region), HappyPlanetIndex))


#Q2
lm(Happiness ~ factor(Region)  + LifeExpectancy, HappyPlanetIndex)


#Q6
summary(lm(Happiness ~ LifeExpectancy + factor(Region) + GDPperCapita + Population, HappyPlanetIndex))


#Q7
summary(lm(Happiness ~ Region * LifeExpectancy, HappyPlanetIndex))




#Q9
model <- lm(Happiness ~ LifeExpectancy + factor(Region) + GDPperCapita + Population + LifeExpectancy:GDPperCapita, HappyPlanetIndex)
summary(model)
step <- stepAIC(model)
summary(step)
