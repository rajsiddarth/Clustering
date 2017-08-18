# Removing all the variables from the workspace
rm(list = ls(all.names = T))

# Set working directory
setwd("J:/Analytics/Batch 22/CSE7405c/Custering PCA/Lab Activity/")

# Reding the Cereals data
cereals_Data = read.csv('Cereals.csv',header = T)
#=> header = T to let the first row is not a numeric and do not analyst it.
summary(cereals_Data)

# Ignoring the dependent attribute
data = as.matrix(cereals_Data[,-c(1)])# drop the ID, sometime is useful

###??? IN CASE WE STILL KEEP THE STRUCTURE OF DATA FRAME, CAN WE STILL APPLY ALL OTHER STEPS

rownames(data) = cereals_Data$name
###??? WHY DO WE NEED THIS STEP?

sqrt(sum((data[1,]-data[2,])^2))
#=> formula to calculate the distance base on L2 method (Eulecian)
# Cheking for number or NA values in data
sum(is.na(data))

library(DMwR) #> Library to data mining
data <- knnImputation(data, k = 5)
#> WHY IS APPLY THIS k=5 in KNN method,given that we are having 13 attributes / dimension

# Scale the attributes
library(vegan)
data <- decostand(data, method = "range")
###???> WHY SHOULD WE DO THIS STEP, AND WHAT IS THE PURPOSE OF NORMALIZE AND STANDARDLIZE

# Calculate the euclidean distance
d <- dist(data,method = "euclidean")
d
d1 = as.data.frame(d)
###??? STILL NOT YET CLEAR ABOUT THE PURPOSE OF THIS CODE, AND HAVING AN ERROR PROMPT ONCE RUNNING THIS CODE,
###> ONLY CAN CONVERT TO DATA FRAME AFTER CONVERT IT FIRST TO NUMERIC

# Ward Hierarchical Clustering
fit <- hclust(d, method = "ward.D")

 # display dendogram
plot(fit)

# Cut the tree to 5 custers
groups <- cutree(fit,k=5)
groups

# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=5, border = "red")

# Custering the data using K-means, creating 5 clusters
fit <- kmeans(data, centers = 5)

# Getting the withiness of the error of all the clusters
sum(fit$withinss)
fit$centers
fit$cluster

# K-means:  Determine number of clusters
wss <- 0
for (i in 2:15) {
  wss[i] <- sum(kmeans(data,centers=i)$withinss)
}

# Plot the cluster number and withinness error
plot(1:15, wss, 
     type="b", 
     xlab="Number of Clusters",
     ylab="Within groups sum of squares") 

