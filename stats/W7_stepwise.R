library(MASS)
fit <- lm(mpg ~., data =mtcars)
step <- stepAIC(fit, direction = "both")
step$anova
summary(step)
