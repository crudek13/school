###########################################################################
##          BUSOBA7332-Predictive Analytics                              ##
##         Week 1 R Code  Auto data example:                            ##
##            R and Regression Refresher ##                              ##
###########################################################################

##########################################################################
#####            Answers to Questions Set A                       ########
##########################################################################

# Idnetifying your working directory #
getwd() # Working Directory 

 # Setting your working directory#

#setwd("C:/Users/talke.1/OneDrive - The Ohio State University/Desktop/SP23 Courses/BUSOBA7332/Week 1") # change this to your directory

# Part A i) Reading the auto.csv data into R #
auto <- read.csv("auto.csv")  # Run this line to read the Excel.csv file data into R and choose the file from your directory
 
# Part A ii) Type of the data or structure of the data  #
str(auto)  # structure of the data 

# Part A iii) Converting categorical variable into factor.  #
auto$Cylinders <- as.factor(auto$Cylinders)   # converting cylinder into factor
str(auto)
auto$Origin <- as.factor(auto$Origin ) # Converting char into factor to get summary stat
str(auto) 
head(auto)  # Displays the first 6 rows of the data
tail(auto)   # Displays the last 6 rows of the data

# Part A iv) checking # of missing values #
summary(auto) # Horsepower shows two missing values 
is.na(auto) # returns TRUE if missing values are detected and FALSE O.WISE
sum(is.na(auto)) # Total missing values

# Part A v) Identify the observations with missing values? #
auto[!complete.cases(auto),] # !complete.case function gives observations with NA 

## Part A vi) Dealing with missing values: Imputation (replacing NAs with the mean of HP)##

auto$Horsepower[is.na(auto$Horsepower)] <- mean(auto$Horsepower,na.rm=TRUE) 
summary(auto) # summary of auto after handling the Missing values

# Part A vii)install and load tidyverse package #

#install.packages("tidyverse") 
library(tidyverse) #load tidyverse package
### Another way to identify missing value ###
auto %>% filter(is.na(Horsepower)) # Which one are missing



##########################################################################
#####            Answers to Questions Set B                       ########
##########################################################################

# Part B i) Filter data by Auto origins for US or Europe #

datauseu <- auto %>%  filter(Origin=='US'|Origin=='Europe') 

# Part B  ii) Arrange the data in i) descending order of  MPG #
datauseudesc<-auto %>%  
  filter(Origin=='US'|Origin=='Europe') %>% 
  arrange(desc(MPG))

# Part B  iii) Find average Auto's MPG by origin for US or Europe in descending order of MPG
auto %>%  
  filter(Origin=='US'|Origin=='Europe') %>% 
  arrange(desc(MPG)) %>% 
  group_by(Origin) %>% 
  summarise(Average=mean(MPG)) 

# Part B  iv) Find the average, stddev, min, MPG of the cars and  n of auto's by origin in descending order of average 

auto %>%  
  group_by(Origin) %>% 
  summarise(Average=mean(MPG), Std.dev=sd(MPG), MIN=min(MPG), Count =n()) %>% 
  arrange(desc(Average))

      # Part B  v) Create a new variable called Gallon Per Mile (GPM) = 1/MPG
 datagpm <- auto %>% 
            mutate(GPM=1/MPG) 

   
     # Part B  vi) Select all the quantitative variables #
names(auto)
auto_quant <- select(auto,MPG,Displacement,Horsepower, Weight, Acceleration)

     # Graphical data exploration  #

# Part B  vii) Histogram of MPG for US or Europe autos
auto %>% 
  filter(Origin=='US'|Origin=='Europe') %>% 
  ggplot(aes(x=MPG)) +
  geom_histogram(bins=15)

# Histogram of MPG for US or Europe autos by adding title and color (fill=origin) to differentiate by color. 
auto %>% 
  filter(Origin=='US'|Origin=='Europe') %>% 
  ggplot(aes(x=MPG,fill=Origin)) +
  geom_histogram(bins=15) +
  ggtitle('Histogram of MPG For US and Europe ')


# Histogram of auto's  MPG between US and Europe Side by Side by adding facet_wrap 
auto %>% 
  filter(Origin=='US'|Origin=='Europe') %>% 
  ggplot(aes(x=MPG,fill=Origin)) +
  geom_histogram(bins=15) +
  facet_wrap(~Origin)  +  
ggtitle('Histogram of MPG For US and Europe ')

# Histogram of auto's MPG for each origin Side by Side  
auto %>% 
   ggplot(aes(x=MPG,fill=Origin)) +
  geom_histogram(bins=15) +
  facet_wrap(~Origin)  +
  ggtitle('Histogram of Autos MPG For Each Origin')+
theme(plot.title=element_text(hjust = 0.5))  ## last line is to center the title

# Part B  viii) Side by Side boxplot of Auto's MPG for each origin  
auto %>% 
ggplot(aes(x=MPG,fill=Origin)) +
  geom_boxplot() +
  ggtitle('Side by Side Boxplot of Autos MPG by origin')+
theme(plot.title=element_text(hjust = 0.5))  


##########################################################################
##### Answers to Questions Set C: Regression Refresher: MLR Model ########
##########################################################################
  
# Selecting the quantitative Variables from auto data
names(auto)
auto_quant <- select(auto,MPG,Displacement,Horsepower, Weight, Acceleration)
#auto[-c(1,3,8,9)] # or removing the non-quantitative variables gives the same result

##### Part C a) Create scatterplot matrix which includes all the quantitative
##### variables in the dataset and comment.

pairs(auto_quant)  # scatterplot matrix

install.packages("psych") 
library(psych)  # install psych package and load it
pairs.panels(auto_quant)  # Displays Scatterplot plus correlation

#### Part C b) Compute the correlation matrix between the variables. 
##### Is there indication of collinearity?

cor(auto_quant)  # as an option
cor(auto[-c(1,3,8,9)]) # or removing columns 1,3, 8 and 9 the non quantitative variables  and find correlation
round(cor(auto_quant),2) # round correlation to 2 decimal places 


###### Part C c) Build OLS Regression model MPG Vs Displacement, HP, WT and Acceleration  ############
model1 <- lm(MPG~.,data=auto_quant)

#### Part C d) summary(model1)  ########
summary(model1)

### Part C d i) Is the model significant? ##

######################################################################
## Part C d ii) Which predictors appear to have a statistically significant ## 
## relationship to the response?                                    ##
######################################################################


## Part C d iii) Calculate the VIF to check multicollinearity problem. Comment ##

install.packages("faraway")
library(faraway) #or package 'car'
vif(model1)  # Checking multicollinearity 


#### Part C e i) Build Regression model with insignificant variables excluded ### 
####  one at a time                                                   ###


model2 <- lm(MPG~.-Acceleration-Displacement,data=auto_quant)
summary(model2)

#### Interpret the regression coefficients.###

############################################################
#### Part C e ii) What would be the estimated MPG for a car    ####
#### with Horsepower=100 , Weight=2000, Acceleration=15,####
#### and Displacement=160 using the model?              ####
############################################################

predict(model2,newdata=data.frame(Horsepower=100,Weight=2000,Acceleration=15,Displacement=160 ))






