##########################################################
######   Third Synchronous Session code           ########
######   Neural Network Model for Wine.csv data   ########
##########################################################

# # Get the working directory
# getwd()
# 
# setwd("C:/Users/talke.1/Documents")

### Part I) i) Read the data of winery.csv file ######
data <- read.csv("Wine.csv")


### Part I) ii) Structure of the data  ######

str(data) # Structure of the data
names(data) # gives names of the features (Variables)


### Pre Normalization get the quantitative features ####
data1 <-data[-1] # remove Type to normalize the data


### Part I) iii) Data Normalization. Max-Min Function ###
# all the features are not between 0 and 1, so we need to normalize
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

### Normalized Data  ###
normdata <- as.data.frame(lapply(data1,normalize)) #Normalized data stored in normdata

### Part I) iv) summary Stat of the Normalized Data ###
str(normdata) # structure of the normalized data
summary(normdata) # summary of the normalized data

#### Adding Back Winery Type Into norm data ####
Type <- data$Type 
normdata <-cbind(Type,normdata) # adding Type back into the normdata
str(normdata)

#### Part I) v) load packages ####
library(caret)
library(neuralnet)
library(tidyverse)



### Part I vi Data Partition 80% Training and 20% Testing ###
# For reproducibility set seed
set.seed(2021)
Index <- createDataPartition(normdata$Type,p=0.8,list=FALSE)
trdata <- normdata[Index,]  # training data set 80%
tsdata <- normdata[-Index,] # testing data set 20%
#head(trdata) # Gives the first 6 rows of the training data
#head(tsdata) # Gives the first 6 rows of the testing data



###Part II: Fit NN Model for Multi-Classification, Prediction and Model Performance ###

############################################################
##### Fitting Neural Network For MultiClassification  #######
############################################################


### Part II i) Fitting Neural Network Model For Classification to predict Type wine ###
###   with hidden=2, rep=5    and try c(1,2)                                      ####
                                 
set.seed(2021)
nnmodel <- neuralnet(Type~.,
                     trdata,
                     hidden=2,
                     err.fct = "ce", #for regression, change ce to sse
                     rep=5,
                     lifesign = "full",
                     linear.output = FALSE)  # Neural Network Model

### Part II ii) Plot Neural Network Model###
plot(nnmodel,"best")  # Shows Neural Network  plot, with weights, intercepts etc.  

### Part II iii) Predict Testing data #####
pred_nn2 <-predict(nnmodel, tsdata,rep=which.min(nnmodel$result.matrix[1, ])) # Predicting test data and it gives predicted probabilities and others
head(pred_nn2) # shows the first 6 predicted probabilities from test data 
head(tsdata[1:6,])  # shows the first 6  rows of the testing data


### Assigning probabilities to Classes (Determining class assignment based on the probability) ###
n<-nrow(tsdata) # Number of Test Data
pred.nn <- data.frame()  # Storage to identify predicted classes

for(i in 1:n){
  pred.nn <-rbind(pred.nn, which.max(pred_nn2[i,]))
}

pred.nn$X1L <-gsub(1,"A",pred.nn$X1L)
pred.nn$X1L <-gsub(2,"B",pred.nn$X1L)
pred.nn$X1L <-gsub(3,"C",pred.nn$X1L)
pred.nn$X1L

test_prediction <-as.factor(pred.nn$X1L) # Predicted Classes
test_actual <- as.factor(tsdata$Type)    # Actual Classes
conf_mat<- table(test_prediction,test_actual) # Confusion Matrix
conf_mat

### Part II iv) Overall accuracy and misclasification in the testing data  ###

test_nnoacc <- sum(diag(conf_mat))/sum(conf_mat )  # Overall accuracy
test_nnoacc # Overall accuracy
test_ernn <- 1-test_nnoacc  # Misclassification Error 
test_ernn  # Misclassification Error 

######### Or more efficiently Overall model performance ############
confusionMatrix(test_prediction,test_actual)  # or confusionMatrix(conf_mat)

