lawn <- read.csv("https://raw.githubusercontent.com/jddbucknole/SMB-A/master/demand-housedensity.csv")

library(tidyverse)

#Q1
cor(lawn$Density, lawn$Sales)
plot(lawn)



model <- lm(formula = Sales ~ Density, data = lawn)
model_sum <- summary(model)

#Q4
5*(-12.893)


#Q5
resid(model)

model$residuals


#Q7
y <- 141.525 + -12.893*4
y


#Q9
model_sum$sigma

model_sum



#Q10
plot(model)
plot(model$residuals)




#Q13
library(Lock5Data)

view(InkjetPrinters)

summary(lm(Price ~ PPM + PhotoTime + CostBW + CostColor, data = InkjetPrinters))

library(car)

#Q14
ink_model <- lm(Price ~ PPM + PhotoTime + CostBW + CostColor, data = InkjetPrinters)
vif(ink_model)



#Q16
view(EmployedACS)

summary(lm(Income ~ Age + Sex + HoursWk, data = EmployedACS))






#Q18
view(HollywoodMovies2013)

mod_mov <- lm(WorldGross ~ Budget + RottenTomatoes + TheatersOpenWeek, data = HollywoodMovies2013)

movie_model_sum <- 
  summary(lm(WorldGross ~ Budget + RottenTomatoes + TheatersOpenWeek, data = HollywoodMovies2013))

plot(mod_mov)



