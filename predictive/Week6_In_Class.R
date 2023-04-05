#### W6 in class
library(caret)
library(neuralnet)
library(tidyverse)



data(iris) # accessing the IRIS data
names(iris)
str(iris)


data <- iris[-5]

#### i.Fit neural network to the training dataset with 2 hidden layer and 5 repetitions.  Use set.seed(2021). 
### Data Normalization Max-Min  ###
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

normdata <- as.data.frame(lapply(data,normalize))
str(normdata)
summary(normdata)

Species <- iris$Species 
normdata <-cbind(Species,normdata) # adding Type back into the normdata
str(normdata)


set.seed(2021)
Index <- createDataPartition(normdata$Species,p=0.8,list=FALSE)
trdata <- normdata[Index,]  # training data set 80%
tsdata <- normdata[-Index,] # testing data set 20%

set.seed(2021)
nnmodel <- neuralnet(Species~.,
                     trdata,
                     hidden=2,
                     err.fct = "ce", #for regression, change ce to sse
                     rep=5,
                     lifesign = "full",
                     linear.output = FALSE)  # Neural Network Model


#### ii.Plot the neural network model. 

plot(nnmodel,"best")  # Shows Neural Network  plot, with weights, intercepts etc.


#### iii.Predict the response on the test data and produce a confusion matrix comparing the actual test data to the predicted. 

pred_nn2 <-predict(nnmodel, tsdata,rep=which.min(nnmodel$result.matrix[1, ])) # Predicting test data and it gives predicted probabilities and others
head(pred_nn2) # shows the first 6 predicted probabilities from test data 
head(tsdata[1:6,])

n<-nrow(tsdata) # Number of Test Data
pred.nn <- data.frame()  # Storage to identify predicted classes

for(i in 1:n){
  pred.nn <-rbind(pred.nn, which.max(pred_nn2[i,]))
}

pred.nn$X1L <-gsub("A","setosa",pred.nn$X1L)
pred.nn$X1L <-gsub("B","versicolor",pred.nn$X1L)
pred.nn$X1L <-gsub("C","virginica",pred.nn$X1L)
pred.nn$X1L

test_prediction <-as.factor(pred.nn$X1L) # Predicted Classes
test_actual <- as.factor(tsdata$Species)    # Actual Classes
conf_mat<- table(test_prediction,test_actual) # Confusion Matrix
conf_mat


#### iv.What is the overall accuracy and  misclassification (error rate) using the test data?

test_nnoacc <- sum(diag(conf_mat))/sum(conf_mat )  # Overall accuracy
test_nnoacc # Overall accuracy
test_ernn <- 1-test_nnoacc  # Misclassification Error 
test_ernn  # Misclassification Error 

confusionMatrix(test_prediction,test_actual)  # or confusionMatrix(conf_mat)


#**ACCURACY of 96.67%**
  
#### v.Compare the misclassification error found in the previous question part iv) with what was found using decision tree, Random Forest, Random Forest-Bagging, and the logistic model during the second in person session?  Which model is best?  
  
#Decision Tree CLASSIFICATION TREE = 93.3%
#Random Forest CLASIFICATION = 93.3%
#RF BAGGIG = 93.3%
#LOGISTIC = 96.67%

  