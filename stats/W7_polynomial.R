linmodel <- lm(mpg ~ wt , data = mtcars)
quadmodel <- lm(mpg ~ wt + I(wt^2), data = mtcars)
summary(linmodel)
summary(quadmodel)

ggplot(mtcars, aes(x = wt, y = mpg)) + 
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), col = "red", se = F)


cubicmodel <- lm(mpg ~ wt + I(wt^2) +I(wt^3), data = mtcars)
summary(cubicmodel)


ggplot(mtcars, aes(x = wt, y = mpg)) + 
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), col = "red", se = F) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2) +I(x^3), col = "green", se = F) 
