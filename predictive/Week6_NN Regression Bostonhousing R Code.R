##########################################################################
######     Synchronous Session 3 R code                            #######
######  NN Regression Demonstration Using Bostonhousing.csv data   #######
##########################################################################

## Part I i) ##
#### Reading Bostonhousing.csv data into R  #####
# Get the working directory
getwd()
# setwd("C:/Users/talke.1/Documents")


### Part I) i) Read the data of Bostonhousing.csv file ######
data <- read.csv("BostonHousing.csv")

str(data) # Structure of the data.
names(data) # gives names of the features (Variables)


### Part I iii) Normalization ###
### Data Normalization Max-Min  ###
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

### Part I iv) ###
normdata <- as.data.frame(lapply(data,normalize))
str(normdata)
summary(normdata)

####  load packages ####
library(caret)
library(neuralnet)
library(tidyverse)
names(normdata)
## Part I v) Partition the data into 80% Training and 20% Testing##

# For reproducibility set seed
set.seed(2021)
Index <- createDataPartition(normdata$medv,p=0.8,list=FALSE)
trdata <- normdata[Index,] # training data set 80%
tsdata <- normdata[-Index,]  # testing data set 20%

### Fitting Neural Network Regression Model using medv as response ###

round(cor(data),2)
set.seed(2021)
nnmodelr <- neuralnet(medv~rm+lstat+indus+tax,
                      trdata,
                      hidden=5,
                      err.fct = "sse",
                      rep=5,
                      lifesign = "full",
                      linear.output = FALSE)  # Neural Network Model

### Part II iii) ###
### Plot Neural Network Model###
plot(nnmodelr,"best")  # Shows Neural Network  plot, with weights, intercepts etc.  



###  Part V iii) Predict test data using NN Regression###
##### Predict Test data #####
pred_nnmr <-predict(nnmodelr, tsdata,rep=which.min(nnmodelr$result.matrix[1, ]))
head(pred_nnmr)
###  Part V iv) NN Regression  Model Performance ###
MSE_nnmr <- mean(pred_nnmr-tsdata$medv)^2  # MSE
Rsqr<- 1-sum((tsdata$medv-pred_nnmr)^2)/sum((tsdata$medv-mean(tsdata$medv))^2) # R-Square

nnModelrperf<-cbind(MSE=MSE_nnmr,RSquareAdj=Rsqr) # Model Performance Measures
nnModelrperf

