library(tidyverse)
library(rvest)
library(xml2)


year <- "2019"
url <- paste0("https://www.basketball-reference.com/leagues/NBA_",year,"_per_game.html")

playerdata <- url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="per_game_stats"]') %>%
  html_table()

df <- data.frame(playerdata) %>%
  filter(Player !="Player")


#Get rid of non-TOT for 2 team players
#make numeric and remove charater
df1 <- df %>%
  distinct(Player, .keep_all = T) %>%
  select(-c(Rk,Tm)) %>%
  mutate_at(vars(-c(Player,Pos)),as.numeric) %>%
  filter(MP > 25, G > 30) %>%
  select(-FG.,-X3P., -X2P.,-FT., -eFG.) %>%
  mutate(Pos = ifelse(Pos %in% c("PF-SF","SF-SG"),"SF",Pos))

scaled <- df1 %>%
  select(-Player,-Pos) %>%
  mutate_all(scale)


clusters <- kmeans(scaled, 3, nstart = 10)

df_clust <- df1 %>%
   mutate(cluster = clusters$cluster)
 
df_clust %>%
  filter(cluster == 2) %>%
   View()


df_clust %>%
   ggplot(aes(x = cluster,fill = Pos)) + geom_bar()

df_clust %>%
  group_by(cluster) %>%
  summarize(MP = mean(MP), PTS = mean(PTS), TRB = mean(TRB),
            AST = mean(AST), TOV = mean(TOV), STL = mean(STL),BLK = mean(BLK),
            X2PA = mean(X2PA), X3PA = mean(X3PA), Age =mean(Age))
  
ratio_ss <- rep(0,20)
for (k in 1:20) {
  
  # Apply k-means 
  clusters <- kmeans(scaled,k,nstart = 20)
  
  # Save the ratio between of WSS to TSS in kth element of ratio_ss
  ratio_ss[k] <- clusters$tot.withinss / clusters$totss
  
}

# Make a scree plot with type "b" and xlab "k"
plot(ratio_ss, type = "b", xlab = "k")
