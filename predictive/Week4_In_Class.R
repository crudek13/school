##########################################################
######     Synchronous Session 2 R code           ########
######   Using iris data FROM R                   ########
##########################################################


## Part I i) ##
#### Access iris data from R  and it's ready for use #####
data(iris) # accessing the IRIS data
names(iris)

## Part I ii) ##
?iris # to read more about the data

## Part I iii) ##
str(iris) # Checking the structure of the data
summary(iris)


## Part I iv) Install and load Packages##
#### Install and load packages ####
install.packages("party")
install.packages("randomForest")
install.packages("nnet")
library(party)
library(caret)
library(randomForest)
library(nnet)

## Part I v) Partition the data into 80% Training and 20% Testing##

# For reproducibility set seed
set.seed(2021)
Index <- createDataPartition(iris$Species,p=0.8,list=FALSE)
trdata <- iris[Index,] # training data set 80%
tsdata <- iris[-Index,]  # testing data set 20%
head(trdata) # Gives the first 6 rows of the training data
head(tsdata) # Gives the first 6 rows of the testing data


##################################################################################
##### Multinomial Logistic, Classification Tree, Random Forest and RF-Bagging #######
##################################################################################


## Part I vi) Fit Multinomial Logistic Model using Species as response ###
### summary of the model and Interpret Model coefficients              ###

### Fitting Multinomial logistic model using Species as response ### 
set.seed(2021)
mlogistm <-multinom(Species~.,data=trdata) # note R selects Setosa as a reference (baseline)
summary(mlogistm) 


#interpret the coefficent for sepal.width
for every 1 cm increase in sepal.width
the log odds of belonging to versicolar species decreases by -37.06668
while holding sepal length, pedal lengths and pedal widths constant




##  Part I viii) Predicting test data and model performance ###

##### Predict Test data #####
pred_probmlogist<- predict(mlogistm,tsdata,type="prob") # predicts the probablities of classes of species
round(pred_probmlogist,4)  # gives the predicted probability of each class in the test data rounded to 4 decimal places
pred_mlogist<- predict(mlogistm,tsdata) # predicts the classes of species
#http://127.0.0.1:15667/graphics/plot_zoom_png?width=1179&height=900
conf_mlogistm<-table(pred_mlogist,tsdata$Species) # confusion matrix from multinomial logistics model
conf_mlogistm # confusion Matrix from multinomial logistics model
test_mlogistm <- 1-sum(diag(conf_mlogistm))/sum(conf_mlogistm)  # test error rate
test_mlogistm  # test error rate
confusionMatrix(conf_mlogistm)

###  Part II i) Fit Classification Decision Tree ###

### Fitting Classification Tree using Species as response ### 
set.seed(2021)
clatree <- ctree(Species~.,trdata)  # Classification Tree Model
print(clatree) # Shows the decision input and output variables as well as nodes and rules

###  Part II ii) Plot the decision Decision Tree ###
plot(clatree)  # Decision Tree plot with bar plot showing as predicted probability value in the terminal node
plot(clatree,type="simple") # Decision Tree plot showing predicted probability value in the terminal node

###  Part II iii) Fit Classification Decision Tree ###
##### Predict Test data #####
pred_clatree <-predict(clatree, tsdata) # Predicting test data


###  Part II iv) Decision Tree Model Performance ###
conf_clatree<-table(Predicted=pred_clatree,Actual=tsdata$Species) # confusion matrix and strong it in conf_clatree
conf_clatree
test_erclatree <- 1-sum(diag(conf_clatree ))/sum(conf_clatree ) # test error 
test_erclatree # test error rate
1-test_erclatree




###  Part III i) Fit Random Forest Classification Tree using Species as response  ###

### Fitting Random Forest Classification Tree using Species as response ### 
set.seed(2021)
clarf <- randomForest(Species~.,trdata,importance=TRUE)  # Classification Tree Model
print(clarf ) # Shows the decision input and output variables as well as nodes and rules

###  Part III ii) Plot Random Forest model ### 
plot(clarf,main="Random Forest" )  # Random forest plot Error vs ntrees 
legend(300,0.13,colnames(clarf$err.rate),col=1:4,cex=0.8,fill=1:4)

###  Part III iii) feature selection or Importance ###

### Variable Importance (feature selection) #######
importance(clarf )
varImpPlot(clarf )

###  Part III iv) Predicting test data using Random Forest Model ###
##### Predict Test data #####
pred_clarf  <-predict(clarf , tsdata) # Predicting test data

###  Part III v) Random Forest Model Performance ###
conf_clarf <-table(Predicted=pred_clarf ,Actual=tsdata$Species) # confusion matrix and strong it in conf_clatree
conf_clarf  # confusion Matrix from Classification tree
test_erclarf <- 1-sum(diag(conf_clarf ))/sum(conf_clarf )  # test error rate
test_erclarf # test error rate

1-test_erclarf




















# Part IV: Fitting Aggregate Tree (Bagging) For Classification (Slide 21)
# i.Fit Bagging for classification. Use set.seed(2021)
# ii.Plot the Bagging model 
# iii.Use the importance() function to determine which variables are most important.
# iv.Predict the response on the test data and produce a confusion matrix comparing the test labels to the predicted test labels. 
# v.What is the test overall accuracy and  misclassification (error rate)?
# vi.Which classification method is best? 

set.seed(2021)
clabag <- randomForest(Species~.,trdata,mtry=12,importance=TRUE)  # Classification Tree Model
print(clabag) # Shows the decision input and output variables as well as nodes and rules

plot(clabag,main="Bagging Error rate vs ntrees")  # Decision Tree plot with bar plot showing as predicted probablity value in the terminal node
legend(450,0.35,colnames(clarf$err.rate),col=1:3,cex=0.8,fill=1:3)

varImpPlot(clabag, main="Bagging-Variable Importance ") # Feature Importance Plot
importance(clabag) # Gives numerical values of the variable Importance

pred_clabag <-predict(clabag, tsdata) # Predicting test data
conf_clabag<-table(Predicted=pred_clabag,Actual=tsdata$Species) # confusion matrix and strong it in conf_clatree
conf_clabag # confusion Matrix from Classification tree

test_erclabag <- 1-sum(diag(conf_clabag))/sum(conf_clabag)  # test error rate
test_erclabag # test error rate
confusionMatrix(pred_clabag,tsdata$Species)  # Gives confusion Matrix and other statistics values

Test_erclacomp <- data.frame(test_mlogistm, test_erclatree, test_erclabag ,test_erclarf)
Test_erclacomp2 <- data.frame(1-test_mlogistm, 1-test_erclatree, 1-test_erclabag ,1-test_erclarf)
Test_erclacomp
Test_erclacomp2


#Part V: Fitting Decision Tree For Regression (Slide 22)
# i.Fit Regression Tree using Sepal.width as response to the training dataset. Use set.seed(2021)
# ii.Plot the regression tree and Interpret the results. 
# iii.Predict the response of the test data using the model.
# iv.Report the test MSE? 

set.seed(2021)
regtree <- ctree(Sepal.Width~.,trdata)  # Regression Tree Model
print(regtree) # Shows the decision input and output variables as well as nodes and rules

plot(regtree)  # Decision Tree plot with Boxplot as predicted value in the terminal node
plot(regtree,type="simple") # Decision Tree plot showing predicted value in the terminal node

pred_regtree <-predict(regtree, tsdata)

MSE_regtree <- mean(pred_regtree - tsdata$Sepal.Width)^2 # Calculating test MSE
MSE_regtree  # test MSE




#Part VI: Fitting Random Forest For Regression (Slide 23)
# i.Use random forests for regression using Sepal.width as response to the training dataset. Use set.seed(2021)
# ii.Plot the fitted random forest model
# iii.Predict the response of the test data using the model.
# iv.Report the calculated test MSE? 
  

set.seed(2021)
rfreg <- randomForest(Sepal.Width~.,trdata,importance=TRUE)  # Bagging 
print(rfreg) # shows type of random forest, #splits, MSE and RSquare 

plot(rfreg,main="RandomForest")  # Shows the plot of error rate and ntrees 

pred_rfreg <-predict(rfreg, tsdata)

MSE_rfreg<- mean(pred_rfreg -tsdata$Sepal.Width)^2 # Calculating test MSE
MSE_rfreg # test MSE 


#Part VII: Fitting Random Forest-Bagging For Regression (Slide 24)
# i.Use Bagging for regression using Sepal.width as response to the training dataset. Use set.seed(2021)
# ii.Predict the response of the test data using the model.
# iii.Report the calculated test MSE? 
# iv.Which Regression method is better? Why


### Fitting aggregate tree Bagging for Regression using serum_sodium=SS as response ### 
set.seed(2021)
bagreg <- randomForest(Sepal.Width~.,trdata,mtry=4,importance=TRUE)  # Bagging 
print(bagreg) # shows type of random forest, #splits, MSE and RSquare 

### Part VII ii) ###
plot(bagreg,main="Bagging")  # Shows the plot of error rate vs ntrees 

### Part VII iii) ###

##### Predict Test data #####
pred_bagreg <-predict(bagreg, tsdata)

### Part VII iv) ###
MSE_bagreg <- mean(pred_bagreg -tsdata$Sepal.Width)^2 # Calculating test MSE
MSE_bagreg # test MSE

### Part VII v) Comparisons###
### Comparing test MSE of Single Decision Tree, Bagging and Random Forest For Regression ####
MSE_regcomp <- data.frame(MSE_regtree,MSE_bagreg,MSE_rfreg)
MSE_regcomp # test MSE Comparison











