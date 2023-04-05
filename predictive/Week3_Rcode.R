##########################################################
######     Week 3 R code                          ########
######    Social Network  Purchase                ########
##########################################################

#########  Install and load the packages ############
library(tidyverse)
library(caret)
#install.packages(ROCR)
library(ROCR)
library(glmnet)



# Get the working directory
# getwd()
# setwd("C:/Users/talke.1/Documents")

### Part I) i) Read the data of Social_Network_Purchase.csv file ######
data <- read.csv("Social_Network_Purchase.csv")
str(data) # Structure of the data
names(data) # gives names of the features (Variables)


#### Part I) ii) Data Pre-Process and Preparation ########

## covert variables to factor  ##
data$Gender <- as.factor(data$Gender)
data$Purchased<- as.factor(data$Purchased)

## Checking missing value      ## 
sum(is.na(data))

str(data) # Gives structure of the data after converting variables to factor
summary(data)# Summary stat of each variables 


data <- data[-1] # Removing the first column User.ID


## Part I) iii) Why Regression is NOT Used for binary or categorical outcome?  #####
##  Why Logistic Regression?
##              visual proof see lines 43-68                                  ## 
## Let's first build simple Logistic Regression model with Age as a predictor ##
model1 <- glm(Purchased~Age,data, family="binomial")
summary(model1)

## Predict Purchase using model1 ####
pred_1 <- predict(model1,data,type="response") # Gives Predicted probabilities and we are storing in pred_1

pred_1c  <- ifelse(pred_1 >0.5,1,0) # Convert probabilities to class 1 or 0
head(pred_1) # Show the first 6 predicted probabilities from the data set
head(pred_1c) # Show the first 6 predicted classes from the data set

table(Predicted=pred_1c, Actual=data$Purchased ) # Gives Confusion Matrix

data$pred <- pred_1 # will add another column in data predicted probabilities

### Visual comparison of Logistic vs Linear Regression plot #####

data %>% 
  ggplot(aes(x=Age,y=pred_1c))+
  geom_point(color="red")+
  geom_smooth(aes(x=Age, y=pred_1),color="green")+
  geom_smooth(method="lm", se=0, color="blue")+
  ggtitle('Logistic Vs Linear Regression')+
  labs(x="Age",y="Probability",color="Legend")+
  theme_minimal()+
  theme(plot.title=element_text(hjust = 0.5))
##############################################################################


###### Let's Build Predictive Logistic Regression Model ###########

data <-data[-5]# Now let's remove the added predicted probability from the data

names(data)

## Part I) iv) What's the proportion of each class Purchase (category)? ##
table(data$Purchased)  # Gives counts of each class
prop.table(table(data$Purchased)) # Gives proportion of each class

#### Part I) v) What level of accuracy is desired for the model to use? ######
prop.table(table(data$Purchased)) # Highest is the benchmark accuacy? 

#### Part I) vi) Fit logistic regression model with the entire data and ######
######   discuss the summary of the model.                              #####
modelall <- glm(Purchased~.,data, family="binomial")
summary(modelall)

modelall1 <- glm(Purchased~.-Gender,data, family="binomial")
summary(modelall1)


####### Part II i) Data splitting into 70% Training and 30% Testing ##########

# For reproducibility set seed
set.seed(2021)
#data partitioning using Stratified random sample into train and test data
Index <- createDataPartition(data$Purchased,p=0.7,list=FALSE)
trdata <- data[Index,] # training data set 70%
tsdata <- data[-Index,]  # testing data set 30%
head(trdata) # Gives the first 6 rows of the training data
head(tsdata) # Gives the first 6 rows of the testing data


## Part II ii) Fit logistic regression model using the training data set #####
## and discuss the summary of the model results.                        #####
## Is there any predictor that is not significant?                       ####

logisticm <- glm(Purchased~.,data=trdata, family="binomial")
summary(logisticm)  ## Gives model summary and shows Gender is not statistically significant

## Part II iii)Find the logistic model with all its predictors in the model ###
## significant using the training data set.                                 ###                                         ###
flogisticm <- glm(Purchased~.-Gender,data=trdata, family="binomial")
summary(flogisticm)
round(coef(flogisticm),5)
### Part II iV)Interpret the regression coefficients? #####





### Part III i) Predict the training dataset using the model  and            ####
### discuss overall accuracy, misclassification, Sensitivity and Specificity? ###

pred_probtr <- predict(flogisticm,trdata,type="response") # Gives predicted probabilities of training data and store it in pred_tr
pred_ctr  <- ifelse(pred_probtr>0.5,1,0) #Converting the predicted probabilities into class 1 or 0
head(pred_ctr) # shows the first 6 predicted classes
head(trdata)  # shows the first 6 rows of training data

### Confusion Matrix using Training Set####
conf_tr <- table(Predicted=pred_ctr, Actual=trdata$Purchased )
conf_tr # confusing matrix from Training set
sum(diag(conf_tr))/sum(conf_tr)   ### Overall accuracy using the training set
1-sum(diag(conf_tr))/sum(conf_tr)  # Overall Missclassfication in Training Data


### Part III ii) Predict the testing dataset using the model  and         ####
### discuss overall accuracy, misclassification, Sensitivity and Specificity? ###


## Predict Test data ####   
pred_probts <- predict(flogisticm,tsdata,type="response") # Gives predicted probabilities of testing data and  store it in pred_ts
pred_cts <- ifelse(pred_probts>0.5,1,0) #Converting the predicted probabilities into class 1 or 0
head(pred_cts)
head(tsdata)

### Confusion Matrix using Testing Set####
conf_ts<- table(pred_cts, tsdata$Purchased)
conf_ts # confusing matrix from Testing set
sum(diag(conf_ts))/sum(conf_ts)  # Overall accuracy in Testing set
1-sum(diag(conf_ts))/sum(conf_ts)  # Overall Missclassfication in Testing Data



#### Part III iii) Is the accuracy good? Why or why not? ####
  

### Part III iv) Plot the ROC curve for the testing data and discuss what you see?  ####
pred_probts <- predict(flogisticm,tsdata,type = 'response')
pred_probts <- prediction(pred_probts,tsdata$Purchased)
perf_eval <- performance(pred_probts,"acc") # Cutoff Vs Accuracy metric
plot(perf_eval)  # Plots Cutoff Vs Accuracy metric


#### Model Performance ROC  #####
ROC <- performance(pred_probts,"tpr","fpr") # Saves True positive rate=sensitivity and 1-Specificity = fpr
plot(ROC,xlab="1-Specificity",ylab="Sensitivity",main="Logistic Regression ROC",colorize=TRUE)  # Plots ROC Curve
abline(a=0,b=1,col="red")  # Benchmark 



#### Part III v) What's is AUC?  #####

AUC <- performance(pred_probts,"auc")
AUC<- unlist(slot(AUC,"y.values")) # Unlist from the stored slot
AUC # Area under the curve
AUC <- round(AUC,4)  # Round it to 4 decimal places
AUC
plot(ROC,xlab="1-Specificity",ylab="Sensitivity",main="Logistic Regression AUC",colorize=TRUE)
abline(a=0,b=1,col="red")
legend(0.3,0.8,AUC,title="AUC",cex=1)

