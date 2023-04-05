##########################################################
######     Synchronous Session 3 R code           ########
######   Using iris data FROM R                   ########
##########################################################


## Part I i) ##
#### Access iris data from R  and it's ready for use #####
data(iris) # accessing the IRIS data
names(iris)
Species <- iris$Species
## Part I ii) ##
?iris # to read more about the data
data <-iris
## Part I iii) ##
str(data) # Checking the structure of the data
summary(data)
data1 <-data[-5]

### Part I iii) Normalization ###
### Data Normalization Max-Min  ###
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

### Part I iv) ###
normdata <- as.data.frame(lapply(data1,normalize))
str(normdata)
summary(normdata)

normdata1 <- cbind(normdata,Species)


#### Install and load packages ####

#install.packages("neuralnet")
# load packages #
library(tidyverse)
library(caret)
library(neuralnet)
library(nnet)
library(party)
library(randomForest)

## Part I v) Partition the data into 80% Training and 20% Testing##

# For reproducibility set seed
set.seed(2021)
Index <- createDataPartition(normdata1$Species,p=0.8,list=FALSE)
trdata <- normdata1[Index,] # training data set 80%
tsdata <- normdata1[-Index,]  # testing data set 20%


### Fitting Neural Network Model For Classification using Species as response ###

set.seed(2021)
nnmodel <- neuralnet(Species~.,
                     trdata,
                     hidden=2,
                     err.fct = "ce",
                     rep=5,
                     lifesign = "full",
                     linear.output = FALSE)  # Neural Network Model

### Part II iii) ###
### Plot Neural Network Model###
plot(nnmodel,"best")  # Shows Neural Network  plot, with weights, intercepts etc.  

### Part II iv) ###

##### Predict Testing data #####
pred_nn <-predict(nnmodel, tsdata,rep=which.min(nnmodel$result.matrix[1, ])) # Predicting test data. 
# Gives predicted probabilities and others
head(pred_nn) # shows the first 6 predicted probabilities from test data 
head(tsdata[1:6,])  # shows the first 6  rows of the testing data

### Part II vii) ###
n<-nrow(tsdata)
pred.nn1 <- data.frame()

for(i in 1:n){
  pred.nn1 <-rbind(pred.nn1, which.max(pred_nn[i,]))
}

### How to decide how to assign the predicted species (1,2,3) to the actual Species Name ####
tsdata$Species<- as.factor(tsdata$Species)
pred.nn1$X1L<-as.factor(pred.nn1$X1L)
data.frame(Species=tsdata$Species,Predicted=pred.nn1$X1L) # due to the format of iris data data.frame works better than cbind. Note that for data loaded into R from excel cbind works just fine. 

### Replacing the predicted species numbers to actual species name ####
pred.nn1$X1L <-gsub(1,"setosa",pred.nn1$X1L)
pred.nn1$X1L <-gsub(2,"versicolor",pred.nn1$X1L)
pred.nn1$X1L <-gsub(3,"virginica",pred.nn1$X1L)
pred.nn1$X1L
test_prediction <-as.factor(pred.nn1$X1L)
test_actual <- tsdata$Species
conf_mat<- table(test_prediction,test_actual)
### Overall accuracy and misclasification in the testing data  ###
test_prediction <-as.factor(pred.nn1$X1L)
test_actual <- as.factor(tsdata$Species)
conf_mat<- table(test_prediction,test_actual) # Confusion Matrix

test_nnoacc <- sum(diag(conf_mat))/sum(conf_mat )  # Overall accuracy
test_nnoacc # Overall accuracy
test_ernn <- 1-test_nnoacc  # Misclassification Error 
test_ernn # Misclassification Error

######### Or more efficiently Overall model performance ############
confusionMatrix(test_prediction,test_actual)