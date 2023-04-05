###########################################################
######      BUSOBA7332-Predictive Analytics         #######
######         Week 2 R code                        #######
######      CatalogMarketing Data                   #######
###########################################################

## Question Part I i) Reading data into R   ###

# Get the working directory
#getwd()
#setwd("C:/Users/talke.1/OneDrive - The Ohio State University/Desktop/SP23 Courses/BUSOBA7332/Week 2") # change this to your directory

# Part I i) Reading the catalogMarketing.csv data into R #
data <- read.csv("CatalogMarketing.csv")  # Run this line to read the Excel.csv file data into R and choose the file from your directory

str(data)   # structure of the data
summary(data)   # summary statistics for each feature

## Question Part I ii) Data Pre-Processing or Preparation ##
# covert coded variables to factor
data$Gender <- as.factor(data$Gender)
data$Home <- as.factor(data$Home)
data$Marital <- as.factor(data$Marital)


str(data)  # structure of the data
summary(data) # Gives summary stat of all the variables

#Checking missing value
sum(is.na(data))  # Number of missing values

## Question Part I iii )Preliminary data exploration-  ###
###  Correlation between quantitative etc              ###
data <- data[-1] #remove first column (customer Number)

#Quantitative Variables
data1 <- data[-c(2,3,4)]  # accessing quantitative variavles

#install.packages("psych")  
library(psych)  # load psych package
pairs(data1)   # provide scatterplot matrix
round(cor(data1),2)  # provides correlation between variables rounded to 2 decimal
pairs.panels(data1)  # provides more than correlarion and scatter plot


## Question Part I iv)Install Caret Package and Data Partition  ##

#install.packages("caret") # Install caret package - classification and regression tree
library(caret)   # load caret package

######## Data Splitting or data partitioning using Stratified random sample##
### into 80% train and 20% test data using a function from caret package##

set.seed(2121)  #For reproducibility (to get the same result like mine) set seed number
#stratified random sample
Index <- createDataPartition(data$AmtSpent,p=0.8,list=FALSE) #amount spent is the target variable
trdata <- data[Index,] # training data set
tsdata <- data[-Index,]  # testing data set
head(trdata) # shows the first 6 rows of training data
head(tsdata)  # shows the first 6 rows of testing data


####### Building Predictive Regression Model ##############

###### Fit OLS Regression model K-Fold cross validation   ####
#######       (10-Fold CV)                                ####


### Question Part I v) Setting the traincontrol with CV with k=10 folds and repeated 5 times ####
set.seed(2121) #for reproducibility
cntrl <- trainControl(method="repeatedcv",number = 10, repeats = 5)

### Question Part I vi) Fit OLS model using the training data ###
### and k=10 cross validation. Discuss Results.                ##

####Specify OLS MLR  using  train_data,  k=10 fold CV ####
set.seed(2121) #for reproducibility
model_ols <- train(AmtSpent~., 
                   data=trdata,
                   method="lm",
                   trControl=cntrl)

#### print Model to see results from K=10 CV  train_data
print(model_ols)  # model performance from K=10 fold CV get an idea how the model will perform on new data

model_ols$results  # gives more info

plot(varImp(model_ols,scale=T), main="Variable importance from OLS Model")
varImp(model_ols)

#### Summary of the model which shows the result of the model using entire train_data
summary(model_ols)

#### or model regression coefficients can be printed as follows
model_ols$finalModel$coefficients

## Question Part I vii) Using the OLS model predict the test data ###
 

###  Predict outcomes from OLS Model using Testing data  
model_olspred_ts <- predict(model_ols,newdata=tsdata)

## OLS Model performance Test data  
model_olsperformance_ts <- data.frame(RMSE=RMSE(model_olspred_ts,tsdata$AmtSpent),
                                      Rsquare=R2(model_olspred_ts,tsdata$AmtSpent))
print(model_olsperformance_ts)


#### More Information ####

## Visualization of OLS model performance 
plot(model_olspred_ts,type="l",col="red",ylab ="AmtSpent",main = "OLS Model Performance")
lines(tsdata$AmtSpent,col="blue")
legend(10,10,legend=c("Predicted","Actual"),col=c("red","blue"),lty=1:2,cex=0.8)


#### Question Part II Fitting LASSO Regression #####

########## LASSO Regression ########

# Part II Question i) Specify the lambda grids (sequence of tuning parameter) ##
###lambda to use for Ridge, Lasso and Elastic net ##.

# set seed for reproduciblity ###
set.seed(2121)
lambda_grid <- 10^seq(2.5,-2.5,length=100) # sequence of lambda values

##### Fitting LASSO Regression Model using training data and 10-fold cv ###
##### Note alpha=1 for lasso #####

### Part II Question ii)  Fit Lasso model using the training data ###
### and k=10 cross validation. Discuss Results.                   ###
set.seed(2121)
model_laso <- train(AmtSpent~.,
                    data=trdata,
                    method="glmnet",
                    tuneGrid=expand.grid(alpha=1,lambda=lambda_grid),
                    trControl=cntrl)

model_laso$bestTune   # Gives best tuning parameter

##Visual inspection of LASSO Regularization Parameter and RMSE
plot(model_laso)

model_laso


## OR Visual inspection of log(lambda) and RMSE 
plot(log(model_laso$result$lambda),model_laso$result$RMSE,xlab="log(lambda)",ylab="RMSE",xlim=c(6,-6),main = " LASSO log Reguralization Parameter Vs RMSE")
log(model_laso$bestTune$lambda)


## Visual inspection of log(lambda) and Rsquared
plot(log(model_laso$result$lambda),model_laso$result$Rsquared,xlab="log(lambda)",ylab="Rsquared",xlim=c(6,-6),main = " LASSO log Reguralization Parameter Vs Rsquare")


### Part II Question iii) What's the best tuning parameter. ####

model_laso$bestTune   # Gives best tuning parameter


### Part II Question iv) Which variables coefficients shrunk to zero? ###
####  List them                                                      ####

## LASSO REG model coefficient - USED AS VARIABLE SELECTION CRITERIA ALSO - when shrunk to 0, not relevant
coef(model_laso$finalModel,model_laso$bestTune$lambda)



### Part II Question v) What are the three most important variables?  ###

#### Variable Selection  ###

plot(model_laso$finalModel,xvar="lambda", label=TRUE,main="LASSO REGRESSION")
varImp(model_laso)
plot(varImp(model_laso,Scale=T),main="LASSO REGRESSION")




#####Visualization of varImp using ggplot2
library(ggplot2)
ggplot(varImp(model_laso))

### Part II Question vi)  Using the Lasso model predict the test data and ###
#### report RMSE and R-square.                                            ###



##  Predict outcome from LASSO Regression Model using test_data
model_lasopred_ts <- predict(model_laso,newdata=tsdata)


## ### Lasso Regression model performance /accuracy test data
model_lasoperformance_ts <- data.frame(RMSE=RMSE(model_lasopred_ts,tsdata$AmtSpent),
                                       Rsquare=R2(model_lasopred_ts,tsdata$AmtSpent))
print(model_lasoperformance_ts)



###### Questions: Part III Fitting Ridge Regression  #######

########## Ridge Regression #############


##### Specify Ridge Regression Model with alpha=0 to be estimated  #####
##### using training data and 10-fold cv                         #####



#### Part III Question i)Fit Ridge model using the training data ####
#### and k=10 cross validation. Discuss Results.                ####
set.seed(2121) # For reproduciblity
model_rdg <- train(AmtSpent~.,
                   data=trdata,
                   method="glmnet",
                   tuneGrid=expand.grid(alpha=0,lambda=lambda_grid),
                   trControl=cntrl)



model_rdg$bestTune  # Gives best tuning parameter

##Visual inspection of RIDGE Regularization Parameter and RMSE
plot(model_rdg)

##OR Visual inspection of RIDGE log(lambda) and RMSE
plot(log(model_rdg$result$lambda),model_rdg$result$RMSE,xlab="log(lambda)",ylab="RMSE",xlim=c(6,-6),main=" Ridge log Reguralization Parameter Vs RMSE")
log(model_rdg$bestTune$lambda)


## OR Visual inspection of RIDGE  log(lambda) and Rsquared
plot(log(model_rdg$result$lambda),model_rdg$result$Rsquared,xlab="log(lambda)",ylab="Rsquared",xlim=c(6,-6))

#### Part III Question ii) What is the best tuning parameter ###

model_rdg$bestTune  # Gives best tuning parameter

#### Part III Question iii) What are the three most important variables? ###

#### Variable Importance
varImp(model_rdg)
plot(varImp(model_rdg, Scale = T),main=" Ridge Regression")



plot(model_rdg$finalModel,xvar="lambda", label=TRUE)

## Ridge Regression model coefficient 
coef(model_rdg$finalModel,model_rdg$bestTune$lambda)
plot(model_rdg$finalModel,xvar="lambda", label=TRUE,main="Ridge Regression")


#### Part III Question iii) What's the difference between Ridge ######
####  and Lasso Regression?                                     ######

#Answer
#Ridge is not used as a variable selection criteria, but laso is .
#Ridge only shrink variables towards 0. Laso reduces to 0


#### Part III Question iv) Using the Ridge model predict the test data ###
#####  and report RMSE and R-square.                                   ###

## predict outcome from Ridge Regression Model using test_data
model_rdgpred_ts <- predict(model_rdg,newdata=tsdata)


## Ridge Regression Model performance /accuracy  test data
model_rdgperformance_ts <- data.frame(RMSE=RMSE(model_rdgpred_ts,tsdata$AmtSpent),
                                      Rsquare=R2(model_rdgpred_ts,tsdata$AmtSpent))
print(model_rdgperformance_ts)


## Questions: Part IV Fitting Elastic Net Regression and Model Comparisons ##

########## Elastic Net Regression #############

## Questions: Part IV question i)                                          ###
#### Specify alpha=seq(0,1,length=10) and fit Elastic Net regression model ###
####using the training data and k=10 cross validation. Discuss Results.    ###

# set seed for reproduciblity 
set.seed(2021)
##### Specify Elastic net Regression Model with seq of alpha #####
##### to be estimated using training data and 10-fold cv  ######
model_net <- train(AmtSpent~.,
                   data=trdata,
                   method="glmnet",
                   tuneGrid=expand.grid(alpha=seq(0,1,length=10),lambda=lambda_grid),
                   trControl=cntrl)

model_net$bestTune  ## Gives best tuning parameter

##Visual inspection of Elastic Net Regularization Parameter and RMSE
plot(model_net)
model_net


#### Part Iv Question ii) What is the best tuning parameter ###
model_net$bestTune  ## Gives best tuning paramete

#### Part Iv Question iii) What are the three most important variables? ###


#### Variable Importance
plot(model_net$finalModel,xvar="lambda", main="Elastic Net Regression",label=TRUE)
varImp(model_net)
plot(varImp(model_net),main=" Net Regression")



## Elastic Regression model coefficient 
coef(model_net$finalModel,model_net$bestTune$lambda)


#### Part Iv Question iv)Using the Elastic model predict the test data ###
#### and report RMSE and R-square.                                  ####


## predict outcome from Elastic Net Regression Model using test_data
model_netpred_ts <- predict(model_net,newdata=tsdata)


## Elastic Regression Model performance /accuracy  test data
model_netperformance_ts <- data.frame(RMSE=RMSE(model_netpred_ts,tsdata$AmtSpent),
                                      Rsquare=R2(model_netpred_ts,tsdata$AmtSpent))
print(model_netperformance_ts)


#### Part Iv Question v) Finally Compare the model performance ####
##### between LASSO, RIDGE and ELASTIC NET Models. Which one is the best? ###


######Comparison of RIDGE, LASSO and Elastic Net Regression Models performance for the test data######
comp_ts <- matrix(c(model_lasoperformance_ts,model_rdgperformance_ts,model_netperformance_ts ),ncol=2,byrow=TRUE)
colnames(comp_ts) <- c("RMSE","Rsquare")
rownames(comp_ts) <- c("LASSO","RIDGE","Net")
print(comp_ts, digits=3)# round to 4 decimal places



