mtcars
library(tidyverse)
mtcars %>%
  ggplot(aes(x = wt, y = mpg)) + geom_point() + geom_abline(slope = -5.34472, intercept = 37.285126)


carsmodel <- lm(mpg ~ wt, data = mtcars)
summary(carsmodel)
carsmodel$coefficients
plot(carsmodel)

carsmodel$residuals
carsmodel$fitted.values
summary(carsmodel)
predmpg <- 37.2851 - 5.3445 * 3
predmpg
newcars <- data.frame(wt = 3)
newcars
predict(carsmodel, newdata = newcars, interval = "predict")



carsmult <- lm(mpg ~ wt + hp, data = mtcars)
summary(carsmult)
anova(carsmult)
cor(mtcars$wt, mtcars$hp)
install.packages("car")
library(car)
vif(carsmult)
