
install.packages("GGally")
library(GGally)
library(tidyverse)


albhome <- read_csv("https://raw.githubusercontent.com/jddbucknole/SMB-A/master/albhome.csv")
plot(albhome)

ggpairs(albhome)

slr <- lm(PRICE ~ SQFT, data = albhome)
summary(slr)


albhome %>%
  ggplot(aes(x = SQFT, y = PRICE)) + geom_point() + geom_smooth(method = "lm")


albhomenoout <- albhome %>%
  filter(SQFT < 3000)

nooutmodel <- lm(PRICE ~ SQFT, data = albhomenoout)
summary(nooutmodel)


multmodel <- lm(PRICE ~ SQFT + AGE + TAX, data = albhome)
summary(multmodel)
library(car)
vif(multmodel)
