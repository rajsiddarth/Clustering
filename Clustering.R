
rm(list = ls(all.names = T))

library(RCurl)

# Reading  data
cereals = read.table(text = getURL("https://raw.githubusercontent.com/rajsiddarth/Unsupervised_Learning_Clustering/master/Cereals.csv"),quote ="" ,sep = ",",header = T)

str(cereals)

# Cheking for number or NA values in data
sum(is.na(cereals))

cereals=cereals[,-1]

library(DMwR) 
#Knn imputation for filling NA values

cereals =knnImputation(cereals, k = 5)
sum(is.na(cereals))

# Normalizing the values
install.packages("vegan")
library(vegan)
data= decostand(cereals, method = "range")

# Using K-means.Select the k value having minimum within sum of clusters

within_sum=0
#k from 2 TO 20
for (i in 2:20) {
  within_sum[i] = sum(kmeans(data,centers=i)$withinss)
}

# Plot k and sum of withinness error
plot(1:20, within_sum,type="b",xlab="Number of clusters(k)",ylab="Within groups sum of squares") 

#Select k where there is a knee in the plot

#For hclust input a dissimilarity distance measure.
#Selecting Euclidean as the distance measure
d=dist(data,method = "euclidean")

# Hierarchical Clustering
model_hclust= hclust(d, method = "ward.D")

 # display dendogram
plot(model_hclust)

# Cut the tree to 5 custers
groups= cutree(model_hclust,k=5)

# draw dendogram with red borders around the 5 clusters
rect.hclust(model_hclust, k=5, border = "red")

# Custering the data using K-means, creating 5 clusters
model_kmeans= kmeans(data, centers = 5)

# Getting the withiness of the error of all the clusters
sum(model_kmeans$withinss)
model_kmeans$centers
