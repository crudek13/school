##########################################################
######   Week 6 R code                            ########
######    Ohio Croplands                          ########
##########################################################

## Part I i)  Reading data ###
## Ohio Croplands data accessed from R cluster.datasets package ###
# getwd()
# setwd("C:/Users/talke.1/Desktop")
data <- read.csv("ohiocropland.csv")

### Part I ii) Structure of the data ###
str(data) # Checking the structure of the data
names(data) # gives names of the features (Variables)


### Part I iii) Summary Stat ###
summary(data)

### Part I iv) Does the data need standardization? ###
#normdata <- scale(data[-1]) # standardize

### Part I v) Scatterplot of corn vs wheat labeled by county ###
# Scatter plot of corn vs wheat labeled by county ##
plot(data$corn~data$wheat, data=data)
with(data,text(data$corn~data$wheat, labels=data$county,pos=4))


## Part I vi) Installing and Loading packages ##

# install.packages("cluster")
# install.packages("factoextra")
# install.packages("NbClust")
library(cluster)
library(factoextra)
library(NbClust)
library(tidyverse)



### Part II i) calculate the distance measure (Default=Euclidean) #####

#### distance measure #####

distance <- dist(data) # Euclidean Distance
print(distance,digits=3) # Distances rounded to 3 decimal places

data1 <- data[-1] # to perform K-means remove the categorical feature in this case county


### Part II ii) Non-Hierarchical Cluster (K-means) #####
### Non-Hierarchical Cluster (K-means)###
set.seed(2021)  # K means algortihm uses a random set of intial points 
kmc3 <- kmeans(data1,center=3,nstart = 10) # Kmeans with 3 cluster repeated with 10 different random intial points
kmc3 # prints clusters and others

### Part II iii) Non-Hierarchical Cluster (K-means)  ###

print(kmc3$cluster) # prints cluster assignment of each observation (county) 


### Part II iv) Non-Hierarchical Cluster (K-means)  ###
### Extraction of Between and Total Sum of Squares  ###

Betweenss <- kmc3$betweenss  # between clusters sum of squares
Betweenss

Totalss <- kmc3$totss  # Total sum of squares
Totalss

## Quality of Clustering##
(Betweenss/Totalss)*100    # the higher the better



### Part II v) Non-Hierarchical Cluster (K-means)  ###

####################################
## Determining number of clusters ##
####################################

## Elbow Method ##
fviz_nbclust(data1,kmeans,method = "wss")+ 
  theme(plot.title=element_text(hjust = 0.5)) ## Elbow method to select # cluster sample



### Part II vi) Silhouette Method ###
## Silhouette Method ##

fviz_nbclust(data1,kmeans,method="silhouette")+
  labs(subtitle = "Silhouette Method")+
  theme(plot.title=element_text(hjust = 0.5)) 

### Part II vii) NbClust() Method ##
nbclustm<- NbClust(data=data1,min.nc = 2, max.nc = 5, distance="euclidean",method="kmeans")


### Part III i) K-means cluster  with10 random initial points ###
set.seed(2121)
kmc4f <-kmeans(data1,4,nstart=10)

silht <- silhouette(kmc4f$cluster,dist(data1))

### Part III ii) silhouette plot and describe it ###
fviz_silhouette(silht)  # silhouette plot shows visual inspection how well the observations are grouped


### Part III iii) Plot cluster using the first 2 principal component analysis ###
####Cluster Plot  ######
par(mfrow=c(1,1)) 
clusplot(data1,kmc4f$cluster,color=TRUE,shade=TRUE,labels=2,line=0,main = "Visual Inspection")


kmsclust <- data.frame(data,cluster=as.factor(kmc4f$cluster)) # adding cluster column into the data 
head(kmsclust)



### Part IV i) Hierarchical Clustering  distance measure###

distance <- dist(data) # Euclidean Distance

### Part IV ii) Hierarchical Clustering ###

### Hierarchical  Clustering   ###
hierclust <- hclust(dist(data1)) # hierarchical clustering

### Part IV iii) Dendrogram Plot HCA ###
plot(hierclust)  # Dendrogram plot
plot(hierclust, labels=data$county,hang=-1) # Dendrogram with labels

## Part IV iv) Where should we cut the dendrogram in order to obtain clusters?  ###
plot(hierclust, labels=data$county,hang=-1) # Dendrogram with labels
abline(h=22,col="red")

### Part IV V) Hierarchical Clustering optimal number of clusters look for a large drop  #
barplot(hierclust$height,names.arg = (nrow(data1)-1):1) # shows barplots and in hierarchical clustering
                                                        # this plot allows you to choose the optimal clusters.
                                                        # Look for a large drop in the height of the bar

### Part IV Vi) Plot the dendrogram with county labels. Box the clusters ###

plot(hierclust, labels=data$county) # Dendrogram with labels
abline(h=22, col="red") # cutting the height at height 22 results in 4 clusters
rect.hclust(hierclust,k=4,border="red")  # Boxing the clusters D


#Classification of observations into Clusters or groups #

group <-cutree(hierclust,4) # Gives classification of observations into clusters
table(group) # table shows how many observations in each cluster


hiercluster <- data.frame(data,group) # adding cluster column into the data 
head(hiercluster)

### Part IV Vii)  Find the average of each feature in each cluster (group)? Describe the clusters? ##

#How to name or Characterize clusters #
aggregate(data1,list(group),mean) # original data averages of each feature by cluster
#aggregate(normdata,list(member),mean) # normalized data




