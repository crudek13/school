##########################################################
######     Week 4 R code                           ########
######   Heart Failure DATA                        ########
##########################################################

## Part I i) ###
## Access Heat Failure data from UC Irvine Machine Learning Repository###

data <- read.csv("HeartFailure.csv")

### Part I ii) ###
str(data) # Checking the structure of the data
names(data)

### Part I iii) ###
### Convert the categorical coded features to factors  ###
data$diabetes <- as.factor(data$diabetes)
data$anaemia <- as.factor(data$anaemia)
data$HBP <- as.factor(data$HBP)
data$sex <- as.factor(data$sex)
data$smoking <- as.factor(data$smoking)
data$DEATH_EVENT <- as.factor(data$DEATH_EVENT)

str(data)
### Part I iv) ###
summary(data)

### Part I v) ###
#### Install and load packages ####
#install.packages("party")
#install.packages("randomForest")

### Part I vi) ###
# load packages #
library(party)
library(caret)
library(randomForest)



############################################################
##### Classification Tree, Bagging and Random Forest #######
############################################################

### Part II i) ###

### Data Partition 80% Training and 20% Testing ###

# For reproducibility set seed
set.seed(2021)
Index <- createDataPartition(data$DEATH_EVENT,p=0.8,list=FALSE)
trdata <- data[Index,]  # training data set 80%
tsdata <- data[-Index,] # testing data set 20%
head(trdata) # Gives the first 6 rows of the training data
head(tsdata) # Gives the first 6 rows of the testing data

names(data)


### Part II ii) ###

            SINGLE CLASSIFCATION (DECISION) TREE

### Fitting Classification Tree using DEATH_EVENT as response ### 
set.seed(2021)
clatree <- ctree(DEATH_EVENT~.,trdata)  # Classification Tree Model
print(clatree) # Shows the decision input and output variables as well as nodes and rules

### Part II iii) plot classfication tree ###
plot(clatree)  # Decision Tree plot with bar plot showing as predicted probablity value in the terminal node
plot(clatree,type="simple") # Decision Tree plot showing predicted probability value in the terminal node


### Part II iv) ###
##### Predict Test data #####
pred_clatree <-predict(clatree, tsdata) # Predicting test data
conf_clatree<-table(Predicted=pred_clatree,Actual=tsdata$DEATH_EVENT) # confusion matrix and strong it in conf_clatree
conf_clatree  # confusion Matrix from Classification tree

### Part II v) ###
test_ercltree <- 1-sum(diag(conf_clatree))/sum(conf_clatree )  # test error rate
test_ercltree  # test error rate


### Part III i) ###

            CLASSIFICATION RANDOM FOREST - MANY TREES 

### Fitting Random Forest Classification Tree using DEATH_EVENT as response ### 
set.seed(2021)
clarf <- randomForest(DEATH_EVENT~.,trdata,importance=TRUE)  # Classification Tree Model - DEFAULT is 500 TREES, SPLIT 3 TIMEs
print(clarf) # Shows the decision input and output variables as well as nodes and rules

### Part III ii) ###
plot(clarf,main="Random Forest Error rate vs ntrees" )  # Decision Tree plot with bar plot showing as predicted probablity value in the terminal node
legend(450,0.6,colnames(clarf$err.rate),col=1:3,cex=0.8,fill=1:3)
### Part III iii) ###
#### Variable Importance ######
varImpPlot(clarf,main="Random Forest-Variable Importance" )
importance(clarf) # Gives numerical values of the variable Importance


### Part III iv) ###
##### Predict Test data #####
pred_clarf  <-predict(clarf , tsdata) # Predicting test data
conf_clarf <-table(Predicted=pred_clarf ,Actual=tsdata$DEATH_EVENT) # confusion matrix and strong it in conf_clatree
conf_clarf  # confusion Matrix from Classification tree


### Part III v) ###
test_erclarf <- 1-sum(diag(conf_clarf ))/sum(conf_clarf )  # test error rate
test_erclarf # test error rate
1-test_erclarf #accuracy


### The following code gets you more than the test error rate ####
confusionMatrix(pred_clarf,tsdata$DEATH_EVENT)  # Gives confusion Matrix and other statistics values

add this in homework4?
,positive="1")

### Part Iv i) ###

             BAGGING - CLASSIFICATION

### Fitting Bagging Classification Tree using DEATH_EVENT as response ###
set.seed(2021)
clabag <- randomForest(DEATH_EVENT~.,trdata,mtry=12,importance=TRUE)  # Classification Tree Model
    #mtry = all teh predictors because bagging
print(clabag) # Shows the decision input and output variables as well as nodes and rules

### Part Iv ii) ###
plot(clabag,main="Bagging Error rate vs ntrees")  # Decision Tree plot with bar plot showing as predicted probablity value in the terminal node
legend(450,0.35,colnames(clarf$err.rate),col=1:3,cex=0.8,fill=1:3)

### Part Iv iii) ###
#### Variable Importance ######
varImpPlot(clabag, main="Bagging-Variable Importance ") # Feature Importance Plot
importance(clabag) # Gives numerical values of the variable Importance

### Part Iv iv) ###
##### Predict Test data #####
pred_clabag <-predict(clabag, tsdata) # Predicting test data
conf_clabag<-table(Predicted=pred_clabag,Actual=tsdata$DEATH_EVENT) # confusion matrix and strong it in conf_clatree
conf_clabag # confusion Matrix from Classification tree

### Part Iv v) ###

test_erclabag <- 1-sum(diag(conf_clabag))/sum(conf_clabag)  # test error rate
test_erclabag # test error rate (MISCLASSIFICATION)

### The following code gets you more than the test error rate ####
confusionMatrix(pred_clabag,tsdata$DEATH_EVENT)  # Gives confusion Matrix and other statistics values


### Part Iv vi) Comparisons ###

### Comparing test errors of Single Decision Tree, Bagging and Random Forest For Classification ####
Test_erclacomp <- data.frame(test_ercltree,test_erclabag ,test_erclarf)
Test_erclacomp


            CLASSIFCATION RANDOM FOREST IS THE BEST


############################################################
##### Regression Tree,  Random Forest and Bagging ###########
############################################################

### Part v i) ###
            
            REGRESSION SINGLE DECISON TREE

### Fitting Regression Tree using serum_sodium =SS as response ### 
set.seed(2021)
regtree <- ctree(SS~.,trdata)  # Regression Tree Model
print(regtree) # Shows the decision input and output variables as well as nodes and rules

### Part V ii) ###

plot(regtree)  # Decision Tree plot with Boxplot as predicted value in the terminal node
plot(regtree,type="simple") # Decision Tree plot showing predicted value in the terminal node

### Part V iii) ###
##### Predict Test data #####
pred_regtree <-predict(regtree, tsdata)

### Part V iv) ###
MSE_regtree <- mean(pred_regtree-tsdata$SS)^2 # Calculating test MSE
MSE_regtree  # test MSE


### Part VI i) ###

            RANDOM FOREST FOR REGRESSION
            

### Fitting aggregate tree Random Forest for Regression using SS=serum_sodium as response ### 
set.seed(2021)
rfreg <- randomForest(SS~.,trdata,importance=TRUE)  # Bagging 
print(rfreg) # shows type of random forest, #splits, MSE and RSquare 

  randome forest starts with p/3 predictors. so with 12 predictors, it starts with 3 predictors 

### Part VI ii) ###
plot(rfreg,main="RandomForest")  # Shows the plot of error rate and ntrees 

  

### Part VI iii) ###
##### Predict Test data #####
pred_rfreg <-predict(rfreg, tsdata)

### Part VI iv) ###
MSE_rfreg<- mean(pred_rfreg -tsdata$SS)^2 # Calculating test MSE
MSE_rfreg # test MSE 


### Part VII i) ###

            BAGGING FOR REGRESSION 
              bagging uses all the predictors
              random forest only uses 1/3 of the predictors

### Fitting aggregate tree Bagging for Regression using serum_sodium=SS as response ### 
set.seed(2021)
bagreg <- randomForest(SS~.,trdata,mtry=12,importance=TRUE)  # Bagging 
print(bagreg) # shows type of random forest, #splits, MSE and RSquare 

### Part VII ii) ###
plot(bagreg,main="Bagging")  # Shows the plot of error rate vs ntrees 

### Part VII iii) ###

##### Predict Test data #####
pred_bagreg <-predict(bagreg, tsdata)

### Part VII iv) ###
MSE_bagreg <- mean(pred_bagreg -tsdata$SS)^2 # Calculating test MSE
MSE_bagreg # test MSE

### Part VII v) Comparisons###
### Comparing test MSE of Single Decision Tree, Bagging and Random Forest For Regression ####
MSE_regcomp <- data.frame(MSE_regtree,MSE_bagreg,MSE_rfreg)
MSE_regcomp # test MSE Comparison

  BAGGING  IS THE BEST



