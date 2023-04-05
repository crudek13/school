bking <- read_csv("https://raw.githubusercontent.com/jddbucknole/SMB-A/master/BurgerKing.csv")

intmod <- lm(Calories ~ Carbs_g + Meat + Carbs_g:Meat, bking)
summary(intmod)

intmod2 <- lm(Calories ~ Carbs_g*Meat, bking)
summary(intmod2)
