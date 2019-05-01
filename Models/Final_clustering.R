library(caret)
library(glmnet)
library(party)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(NbClust)
library(dendextend)

mydata<-read.csv("listings.csv")
str(mydata)

newdata<-mydata[c(61,29,53,54,83,64,65,87,91,106,56,57)]
str(newdata)
newdata<-na.omit(newdata)
newdata<-newdata[which(newdata$price >10),]

newdata$price<-as.numeric(newdata$price)
newdata$cleaning_fee<-as.numeric(newdata$cleaning_fee)
newdata$security_deposit<-as.numeric(newdata$security_deposit)
newdata$number_of_reviews<-as.numeric(newdata$number_of_reviews)

#Building training and testing data
testidx<-which(1:nrow(newdata)%%2==0)
testidx
traindata<-newdata[-testidx,]
testdata<-newdata[testidx,]

#Creating subset of two variables - price & number of reviews
df<-traindata[,c(1,5)]
str(df)
df<-na.omit(df)
df<-scale(df)
head(df)

#Kmeans-------------------------------

# Elbow method to find optimal k value
set.seed(123)
# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(df, k, nstart = 10 )$tot.withinss
}
# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15
# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#Silhoutte method to find optimal k value
# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(df, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(df))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")


#Performing clustering with the optimal k value found ( k=5 )
k5 <- kmeans(df, centers = 5, nstart = 25)
print(k5)
k5$tot.withinss
k5$betweenss
k5$withinss

k7 <- kmeans(df, centers = 7, nstart = 25)
print(k7)
k7$tot.withinss
k7$betweenss
k7$withinss

#Visualizing the clusters
fviz_cluster(k5,data=df)
fviz_cluster(k7,data=df)

#Hierarchichal clustering---------
sub1<-newdata[which(newdata$price<=150),]
sub2<-newdata[which(newdata$price >150 & newdata$price<=500),]
sub3<-newdata[which(newdata$price>500 & newdata$price<=800),]

d_sub1<-sub1[,c(1,5)]
d_sub1<-scale(d_sub1)
d1 <- dist(d_sub1, method = "euclidean")
hc1 <- hclust(d1, method = "ward.D" )
plot(hc1, cex = 0.6, hang = -1)
hc1$ac

d_sub2<-sub2[,c(1,5)]
d_sub2<-scale(d_sub2)
d2 <- dist(d_sub2, method = "euclidean")
hc2 <- hclust(d2, method = "ward.D" )
plot(hc2, cex = 0.6, hang = -1)
hc2$ac

d_sub3<-sub3[,c(1,5)]
d_sub3<-scale(d_sub3)
d3 <- dist(d_sub3, method = "euclidean")
hc3 <- hclust(d3, method = "ward.D" )
plot(hc3, cex = 0.6, hang = -1)
hc3$ac

#DBSCAN----------------------------------
library(fpc)
library(dbscan)

# Compute DBSCAN using fpc package
set.seed(123)

#DETERMINING OPTIMAL MIN PTS
dbscan::kNNdistplot(df, k = 50)
abline(h = 0.15, lty = 2) 

db <- fpc::dbscan(df, eps = 0.15, MinPts = 50)

# Plot DBSCAN results
fviz_cluster(db, df, geom = "point")
print(db)

