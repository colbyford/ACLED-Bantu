data(iris)
library(reshape)

###############
#UPGMA Example
hirisclustermelt <- melt(iris, id = c("Species")) #Reshape the data
hirisclustermeans <- cast(hirisclustermelt, Species~variable, mean) #Pivot and summarize
hirisclustermeans[is.na(hirisclustermeans)] <- 0 #Impute 0's for NaNs


#Plot Dendrogram using Distance Matrix
plot(hclust(dist(hirisclustermeans[, 2:5]), method = "average"),
     labels = hirisclustermeans[,1],
     main = "UPGMA Hierarchical Clustering Dendrogram")

################
#k-Means Example
kclusters <- kmeans((iris[, 1:4]), 3, iter.max = 10)

kiris <- data.frame(iris, cluster = kclusters$cluster) #Append the cluster assignment to the dataframe
kirisclustermelt <- melt(kiris, id = c("Species", "cluster")) #Reshape the data
kirisclustermeans <- cast(kirisclustermelt, Species~cluster+variable, mean) #Pivot and summarize
kirisclustermeans[is.na(kirisclustermeans)] <- 0 #Impute 0's for NaNs

#Plot Dendrogram using Cluster Averages
plot(hclust(dist(kirisclustermeans[, 2:4]), method = "average"),
     labels = kirisclustermeans[,1],
     main = "k-Means Clustering Dendrogram")
