library(tidyverse)

getwd()

data <- read.csv("predictive/datasets/Broadwayshow.csv")

#Q1
sum(is.na(data)) 

#Q2
#Replaces the NAs in Receipts with average of Receipts
data$Receipts[is.na(data$Receipts)] <- mean(data$Receipts, na.rm=TRUE) 
#will print the summary statistics for each variable in the data
summary(data)

#Q3
data %>% 
  ggplot(aes(x=Receipts)) +
  geom_histogram(bins=30,color="red",fill="lightgreen") +
  ggtitle('Histogram of Broadwayshow Receipts ') +
  theme(plot.title=element_text(hjust = 0.5))  # last line is to center the title


#Q4
#correlation matrix
round(cor(data[-1]),2)


#Q5
#A MLR model was fit to predict Receipt using the three predictors
model1 <- lm(Receipts~.,data[-1])
summary(model1)


#Q7
model2 <- lm(Receipts~.-Avg.Ticket.Price,data[-1])
summary(model2)

predict(model2, newdata = data.frame(Paid.Attendance=200,Shows=30,Avg.Ticket.Price=70))
