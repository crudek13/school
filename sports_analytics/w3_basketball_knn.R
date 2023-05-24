library(tidyverse)
library(rvest)
library(xml2)
library(class)


year <- "2019"
url <- paste0("https://www.basketball-reference.com/leagues/NBA_",year,"_per_game.html")

test <- url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="per_game_stats"]') %>%
  html_table() %>%
  data.frame() %>%
  filter(Player !="Player", Tm != "TOT", MP > 25, G > 40) %>%
  select(-c(Rk,Tm)) %>%
  mutate_at(vars(-c(Player,Pos)),as.numeric) %>%
  select(-FG.,-X3P., -X2P.,-FT., -eFG.) %>%
  mutate(Pos = ifelse(Pos %in% c("PF-SF","SF-SG"),"SF",Pos))

year <- "1985"
url <- paste0("https://www.basketball-reference.com/leagues/NBA_",year,"_per_game.html")
train <- url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="per_game_stats"]') %>%
  html_table() %>%
  data.frame() %>%
  filter(Player !="Player", Tm != "TOT", MP >25, G > 40)%>%
  select(-c(Rk,Tm)) %>%
  mutate_at(vars(-c(Player,Pos)),as.numeric) %>%
  select(-FG.,-X3P., -X2P.,-FT., -eFG.) %>%
  mutate(Pos = ifelse(Pos %in% c("PF-SF","SF-SG"),"SF",Pos))


scale01 <- function(x){
  (x-min(x))/(max(x)-min(x))
}

scaled_train <- train %>%
  select(-Player,-Pos) %>%
  mutate_all(scale01)


scaled_test <- test %>%
  select(-Player,-Pos) %>%
  mutate_all(scale01)

pred <- knn(train = scaled_train, test = scaled_test, cl = train$Pos, k = 5)

conf <- table(test$Pos, pred)
conf

test %>%
  filter(Pos == "C",pred == "SG")
