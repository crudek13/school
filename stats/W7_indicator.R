library(tidyverse)
library(Lock5Data)
head(CommuteAtlanta)
t.test(Time ~ Sex, data = CommuteAtlanta)

commute <- lm(Time ~ Sex, data = CommuteAtlanta)
summary(commute)

t.test(Time ~ Sex, data = CommuteAtlanta, var.equal = T)

head(EmployedACS)

incomemod <- lm(Income ~HoursWk + Race + Sex, data = EmployedACS)
summary(incomemod)
anova(incomemod)
newdata <- data.frame(HoursWk = 40, Race = "black", Sex = 0)
predict(incomemod, newdata = newdata, interval = "prediction")


mtmod <- lm(mpg ~ wt + cyl, data = mtcars)
summary(mtmod)


mtmod <- lm(mpg ~ wt + factor(cyl), data = mtcars)
summary(mtmod)
