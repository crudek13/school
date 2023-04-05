###########################################################
######     Week 5 R code                           ########
######   Neural Network Using Heart Failure DATA   ########
###########################################################

### Part I i) ###
## Source: Heart Failure data from UC Irvine Machine Learning Repository###

#### Read Data into R ####
#getwd()
#setwd("C:/Users/talke.1/Desktop")
#setwd("C:/Users/talke.1/Documents")
#data <- read.csv("C:/Users/talke.1/Desktop/HeartFailure.csv")


library(tidyverse)

data <- read.csv("HeartFailure.csv")

### Part I ii) ###
str(data) # Checking the structure of the data
names(data)

### Part I iii) Normalization ###
### Data Normalization. We will use Max-Min  ###
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

### Part I iv) ###
normdata <- as.data.frame(lapply(data,normalize))
str(normdata)
summary(normdata)

### Part I v) ###
#### Install and load packages ####

#install.packages("neuralnet")
# load packages #
library(caret)
library(neuralnet)

############################################################
##### Fitting Neural Network For Classification  #######
############################################################

### Part II i) ###
### Data Partition 80% Training and 20% Testing ###

# For reproducibility set seed
set.seed(2021)
Index <- createDataPartition(normdata$DEATH_EVENT,p=0.8,list=FALSE)
trdata <- normdata[Index,]  # training data set 80%
tsdata <- normdata[-Index,] # testing data set 20%
head(trdata) # Gives the first 6 rows of the training data
head(tsdata) # Gives the first 6 rows of the testing data

names(normdata)


### Fitting Neural Network Model For Classification using DEATH_EVENT as response and all Predictors ### 
#set.seed(2021)
#nnmodel <- neuralnet(DEATH_EVENT~ age+anaemia+CPK+diabetes+
#                                  EF+HBP+platelets+SC+SS+sex+
#                                  smoking+time,
#                                  data=trdata,
#                                  hidden=5,
#                                  err.fct = "ce",rep=5
#                                  linear.output = FALSE)  # Neural Network Model

### Part II ii) ###
### Fitting Neural Network Model For Classification using DEATH_EVENT as response ###
### and inputes using the top 5 important features identified from Random Forest  ###                              ### 
set.seed(2021)
nnmodel <- neuralnet(DEATH_EVENT~ time+SC+EF+SS+age,
                     trdata,
                     hidden=1,
                     err.fct = "ce",
                     rep=5,
                     lifesign = "full",
                     linear.output = FALSE)  # Neural Network Model

### Part II iii) ###
### Plot Neural Network Model###
plot(nnmodel,"best")  # Shows Neural Network  plot, with weights, intercepts etc.  

### Part II iv) ###
##### Predict Training data #####
pred_clanntr <- predict(nnmodel,trdata,rep=5)#Predicting training data.
pred_clanntr <-predict(nnmodel, trdata,which.min(nnmodel$result.matrix[1, ])) #use this efficient line code instead of the code in line 85
head(pred_clanntr) # shows the first 6 predicted probabilities from training data 
head(trdata[1:6,])  # shows the first 6  rows of the training data

### Part II v) ###
pred_classnntr<- ifelse(pred_clanntr >0.5,1,0) # Assigning probabilities to classes
conf_clanntr<-table(Predicted=pred_classnntr,Actual=trdata$DEATH_EVENT) # confusion matrix from nnmodel
conf_clanntr  # confusion Matrix from nn model

### Overall accuracy and misclasification in the training data  ###
tr_oacc <- sum(diag(conf_clanntr))/sum(conf_clanntr )  # Overall accuracy
tr_oacc  # training overall accuracy
1-tr_oacc # training misclasification


### Part II vi) ###
##### Predict Testing data #####
pred_clannts <-predict(nnmodel, tsdata,rep=5) # Predicting test data. 
pred_clannts <-predict(nnmodel, tsdata,which.min(nnmodel$result.matrix[1, ]))#use this efficient line code instead of the code in line 103
# Gives predicted probabilities and others
head(pred_clannts) # shows the first 6 predicted probabilities from test data 
head(tsdata[1:6,])  # shows the first 6  rows of the testing data

### Part II vii) ###
pred_classnnts<- ifelse(pred_clannts>0.5,1,0) # Assigning probabilities to classes
conf_clannts<-table(Predicted=pred_classnnts,Actual=tsdata$DEATH_EVENT) # confusion matrix from nnmodel
conf_clannts  # confusion Matrix from nn model

### Overall accuracy and misclasification in the testing data  ###
ts_oacc <- sum(diag(conf_clannts))/sum(conf_clannts )  # Overall accuracy
ts_oacc  # training overall accuracy
1-ts_oacc # training misclasification

### Overall accuracy comparison between Training and Testing  data ###
Comp_trts <-data.frame(Training=tr_oacc,Testing=ts_oacc)
Comp_trts


##### Note The codes for Part III, IV, AND V are the same. The only change is  #########
##### in the specification of the number of hidden Layers and nodes           ########## 

