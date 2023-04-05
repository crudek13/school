### Part I i)  Install and load packages ###
install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)
library(tidyverse)

# Get the working directory
getwd()
setwd("")


### Part I) ii) Read the Groceries.csv data into R  ######

groceries<-read.transactions("Groceries.csv",format = "single",sep=",",header=TRUE,cols = 1:2)
inspect(head(groceries,3))

#### Part I iii) and iv) Determine the number of Carts  and unique items sold #####
length(groceries) # number of carts (Customers) 
length(itemFrequency(groceries))  #  number of unique Items sold


### Part I v) top 10 items sold and Bar plot  ####
itemFrequencyPlot(groceries,topN=10,type="absolute", main="Top 10 Sold Items") # BarPlot of Top 10 frequently sold Items. Note absolute=freq. If you want % use "relative"


####### Part I vi) Percentage of top 10 items  ###########
sort(itemFrequency(groceries),decreasing = TRUE)[1:10]  # Gives the proportion of each item in the cart for the top 10 items in decreasing order )



######################################################################
##################   Finding Rule 1 ##################################
######################################################################

#### Part II i) Default paramater setting of the association measures ######
### Association Rules ###
rules<-apriori(groceries) # Rules with the default setting
rules # How many rules

#### Part II ii) Let's modify the parameters of the rules###
mrules5<-apriori(groceries,parameter=list(supp=0.001, confi=0.8,minlen=2, maxlen=5)) # Finding association rules
mrules5 # How many rules

#### Part II iii) 6 rules and discuss the measures of association ###
inspect(mrules5[1:6]) # let's see the first 6 rules
itemFrequency(groceries)["other vegetables"] # Probability of getting (other vegetables) in a cart

##### Part II iv-vi) Sorting  by measure of association rules #####
inspect(sort(mrules5,by="lift",decreasing=TRUE)[1:6])  # Sort by lift 
inspect(sort(mrules5,by="confidence",decreasing=TRUE)[1:6])  # Sort by conf 
inspect(sort(mrules5,by="support",decreasing=TRUE)[1:6])  # Sort by support

#### Part II vii) Visualization ####
plot(mrules5[1:10],method="graph",engine = "htmlwidget" )



######################################################################
##################   Finding Rule 2 ##################################
######################################################################

### Part III i) Let's modify the parameters rule ######
mrules3 <-apriori(groceries,parameter = list(supp=0.001,conf=0.8,minlen=2,maxlen=3)) # Modified Rule 2
mrules3 # How many rules

### Part III ii)  First 6 rules  and discuss ####
inspect(mrules3[1:6]) # Shows the first 6 rules


##### Part III iII-v) Sorting  by measure of association rules #####
inspect(sort(mrules3,by="lift",decreasing=TRUE)[1:6])  # Sort by lift 
inspect(sort(mrules3,by="confidence",decreasing=TRUE)[1:6])  # Sort by conf 
inspect(sort(mrules3,by="support",decreasing=TRUE)[1:6])  # Sort by support



#### Part III vi) Visualization ####
plot(mrules3[1:10],method="graph",engine = "htmlwidget" )


######################################################################
##################   Finding Rule 3 ##################################
######################################################################

### Part IV i) Let's modify the parameters rule ######
mrules4 <-apriori(groceries,parameter = list(supp=0.001,conf=0.8,minlen=2,maxlen=4)) # Modified Rule 2
mrules4 # How many rules

### Part IV ii)  First 6 rules  and discuss ####
inspect(mrules4[1:6]) # Shows the first 6 rules


##### Part IV iII-v) Sorting  by measure of association rules #####
inspect(sort(mrules4,by="lift",decreasing=TRUE)[1:6])  # Sort by lift 
inspect(sort(mrules4,by="confidence",decreasing=TRUE)[1:6])  # Sort by conf 
inspect(sort(mrules4,by="support",decreasing=TRUE)[1:6])  # Sort by support



#### Part IV vi) Visualization ####
plot(mrules4[1:10],method="graph",engine = "htmlwidget")




