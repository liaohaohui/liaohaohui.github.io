# -------------------------------------------------------------------
# Purpose: Unsupervised Learning (Part 2)
# Detail: Clustering
# Author : Liew How Hui (2026)
# References: 
#  1. https://hastie.su.domains/ISLR2/Labs/R_Labs/Ch12-unsup-lab.R
#  2. https://www.statlearning.com/resources-second-edition
#  3. https://altaf-ali.github.io/ISLR/chapter10/lab.html
# Data: ISLR2, https://liaohaohui.github.io/UECM3993/clustering.csv
# License: BSD-3
# Software: R 4.1+
# Duration: 1 hour
# -------------------------------------------------------------------

#-------------------------------------------------------------------------
#  Practical : Basic K-Means with simulated 2D two-clusters
#-------------------------------------------------------------------------

# set 2 clusters simulated data
set.seed(2)
x=matrix(rnorm(50*2), ncol=2)
# shift the first 25 rows to centre at (3,-4)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4

#
# K-Means Clustering with 2 generated clusters
# Different starting points may lead to different outcome
#
par(mfrow=c(1,2))
seedno = 2
set.seed(seedno)   # k-Means take initial centroids randomly!
km.out=kmeans(x,2)
km.out$cluster
plot(x, col=km.out$cluster, main=paste0("K-Means (K=2,seed=",seedno,")"), 
  xlab="x", ylab="y", pch=20, cex=2)
seedno = 7
set.seed(seedno)
km.out=kmeans(x,2)
km.out$cluster
plot(x, col=km.out$cluster, main=paste0("K-Means (K=2,seed=",seedno,")"), 
  xlab="x", ylab="y", pch=20, cex=2)

#
# K-Means Clustering with 3 generated clusters
#
dev.new()
par(mfrow=c(1,2))
seedno = 2
set.seed(seedno)
# nstart=20: 20 random centroids to avoid local minimum
# Note: May be too slow for large data
km.out=kmeans(x,3)
plot(x, col=km.out$cluster, main=paste0("K-Means (K=3,seed=",seedno,")"),
  xlab="x", ylab="y", pch=20, cex=2)
seedno = 4  # or 7
set.seed(seedno)
km.out=kmeans(x,3)
plot(x, col=km.out$cluster, main=paste0("K-Means (K=3,seed=",seedno,")"),
  xlab="x", ylab="y", pch=20, cex=2)

#
# Comparing the two, we can see "different" set of clusters !!!
#

#-------------------------------------------------------------------------
#  Lesson learn from the previous part : K-means algorithm may lead
#  to different "local minimum" clusters!   We WANT GLOBAL MINIMUM.
#
#  Practical : Working with "clustering.csv" using K-means nstart=20, 
#  which uses 20 sets of initial points and compare all WSS (Within 
#  Sum of Squares) to choose the lowest!  Hopefully, the GLOBAL MINIMUM
#  can be obtained.  PAM will also be explored.
#-------------------------------------------------------------------------

#https://liaohaohui.github.io/UECM3993/clustering.csv
X = read.csv("clustering.csv", row.names="id")

# Visual inspection: The Data has 4 clusters
par(mfrow=c(1,1))
plot(X)

#
# Without nstart=20 (or a larger number), we may end up in local minimum.
#
dev.new()
par(mfrow=c(1,2))
seedno = 2
set.seed(seedno)
km.out = kmeans(X, 4)
plot(X,col=km.out$cluster+1,xlab = "x1",ylab = "x2", pch=15, cex=2)
title(paste0("K-Means (K=4,seed=",seedno,")"))
points(km.out$centers, col="black", cex=2, pch=19)
seedno = 22
set.seed(seedno)
km.out = kmeans(X, 4)
plot(X,col=km.out$cluster+1,xlab = "x1",ylab = "x2", pch=15, cex=2)
title(paste0("K-Means (K=4,seed=",seedno,")"))
points(km.out$centers, col="black", cex=2, pch=19)

#
# With nstart=20 (it will be slow for large data)
#
dev.new()
par(mfrow=c(1,2))
seedno = 2
set.seed(seedno)
km.out = kmeans(X, 4, nstart=20)
plot(X,col=km.out$cluster+1,xlab = "x1",ylab = "x2", pch=15, cex=2)
title(paste0("K-Means (nstart=20,K=4,seed=",seedno,")"))
points(km.out$centers, col="black", cex=2, pch=19)
seedno = 22
set.seed(seedno)
km.out = kmeans(X, 4, nstart=20)
plot(X,col=km.out$cluster+1,xlab = "x1",ylab = "x2", pch=15, cex=2)
title(paste0("K-Means (nstart=20,K=4,seed=",seedno,")"))
points(km.out$centers, col="black", cex=2, pch=19)

#
# WSS (Within Sum of Squares) vs number of clusters, k 
#
# => trying to understand Elbow method when the data has a particular 
#    number of clusters.  k=4 is at the Elbow.
#
# If no elbow, that means "no obvious clusters"
# 
#for k=1
total.wss = (nrow(X)-1)*sum(apply(X,2,var))
for (k in 2:15) {
  total.wss[k] = kmeans(X, centers=k, nstart=20)$tot.withinss
}
# Without using for loop:
#total.wss = sapply(1:15, function(k){kmeans(X, centers=k, nstart=20)$tot.withinss})
plot(total.wss, type="b", xlab="Number of Clusters", ylab="WSS")


par(mfrow=c(1,2))
library(cluster)
# PAM (k-Medoids)
kmd = pam(X, 4)   # cluster is in the Base R
plot(X, col=kmd$clustering, pch=16, cex=2, main="PAM (k=4)")
points(kmd$medoids, pch=15, cex=2, col="brown")
km  = kmeans(X, 4, nstart=20)
plot(X, col=km$cluster, pch=16, cex=2, main="kmeans (k=4)")
points(km$centers, pch=15, cex=2, col="brown")

#
# Centres/Centroids (k-means) vs Medoids (PAM)
#
par(mfrow=c(1,2))
km = kmeans(X, 2, nstart=20)
plot(X, col=km$cluster+1, pch=16, cex=2, main="kmeans (k=2)")
points(km$centers, pch=18, cex=3.2, col="black")
kmd = pam(X, 2, nstart=20)
plot(X, col=kmd$clustering+1, pch=16, cex=2, main="PAM (k=2)")
points(kmd$medoids, pch=18, cex=3.2, col="black")

#
# GMM (Gaussian Mixture Model)
#
#install.packages("mclust")
dev.new()
library(mclust, quietly=TRUE)
gmm.model = Mclust(X, 2)
plot(gmm.model, what="classification", pch=16, cex=2) # others: BIC, uncertainty, density


#-------------------------------------------------------------------------
#  Practical : Hierarchical clustering with simulated 2D two-clusters
#
#  Note: Hierarchical clustering does not work with large data.
#-------------------------------------------------------------------------

# simulated 2D two-clusters from earlier part
set.seed(2)
x=matrix(rnorm(50*2), ncol=2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4

# Hierarchical Clustering with Euclidean distances and 3 linkages
dist.mat = dist(x)
hc.complete = hclust(dist.mat, method="complete")
hc.average  = hclust(dist.mat, method="average")
hc.single   = hclust(dist.mat, method="single")
# Dendrograms
par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage", xlab="", sub="", cex=.9, hang=-1)
plot(hc.average, main="Average Linkage",  xlab="", sub="", cex=.9, hang=-1)
plot(hc.single,  main="Single Linkage",   xlab="", sub="", cex=.9, hang=-1)

#
# Dendrogram is useful in DNA classification, etc. but not useful in
# business data.  We need to perform cutting on the dendrograms
# to get useful clusters.
#
# Cut the dendrograms into k clusters: Two ways:
# (1) specify how many clusters we want
# (2) cut at a specific height
#

#
# (1)
#
par(mfrow=c(2,3))
km.out=kmeans(x,2,nstart=20)
plot(x, col=km.out$cluster, main="K-Means(K=2)", pch=20, cex=2)
cls1 = cutree(hc.complete, 2)
plot(x, col=cls1, pch=15, cex=1.5, main="Complete-2")
cls2 = cutree(hc.average, 2)
plot(x, col=cls2, pch=15, cex=1.5, main="Average-2")
cls5 = cutree(hclust(dist.mat, method="ward.D2"), 2)
plot(x, col=cls5, pch=15, cex=1.5, main="Ward-2")
cls3 = cutree(hc.single, 2)
plot(x, col=cls3, pch=15, cex=1.5, main="Single-2")
cls4 = cutree(hc.single, 4)
plot(x, col=cls4, pch=15, cex=1.5, main="Single-4")
mtext("(1) specify how many clusters we want", side=3, line=-1.5, outer=TRUE)

#
# (2)
#
par(mfrow=c(2,2))
cls1 = cutree(hc.complete, h=4)
plot(x, col=cls1, pch=15, cex=1.5, main="Complete-2 (height=4)")
cls2 = cutree(hc.average,  h=3)
plot(x, col=cls2, pch=15, cex=1.5, main="Average (height=3)")
hc.ward2 = hclust(dist.mat, method="ward.D2")
cls5 = cutree(hc.ward2, h=5)
plot(x, col=cls5, pch=15, cex=1.5, main="Ward (height=3)")
cls4 = cutree(hc.single,  h=1)
plot(x, col=cls4, pch=15, cex=1.5, main="Single (height=1)")
mtext("(2) Cluster by height", side=3, line=-1.5, outer=TRUE)


#-------------------------------------------------------------------------
#  Practical : Working with ISLR or ISLR2's NCI60 using hierarchical 
#  clustering
#
#  Ref: https://rstudio-pubs-static.s3.amazonaws.com/14007_8294de3bde644c18bf7c45c2f75bd16f.html
#-------------------------------------------------------------------------

library(ISLR2)
nci.labs=NCI60$labs    # Mentioned in Practical 11
nci.data=NCI60$data

# EDA on real-world data
table(nci.labs)  # EDA on output
dim(nci.data)    # -> 64 x 6830 (n < p)
summary(apply(nci.data, 2, sd))

#
# Is it necessary for scaling?  In real-world data, standard deviations
# of the columns can be large.  So scaling is necessary.
#
summary(apply(nci.data, 2, sd))
nci.norm = apply(nci.data, 2, function(x){ (x-min(x))/(max(x)-min(x)) })

# Euclidean distance is used.
nci.dist = dist(nci.norm)   # Distance matrix: around 64 x 64
hc.complt = hclust(nci.dist, method="complete")
hc.averag = hclust(nci.dist, method="average")
hc.single = hclust(nci.dist, method="single")
par(mfrow=c(1,3))
plot(hc.complt, labels=nci.labs, main="Complete Linkage", xlab="",ylab="",sub="")
plot(hc.averag, labels=nci.labs, main="Average Linkage",  xlab="",ylab="",sub="")
plot(hc.single, labels=nci.labs, main="Single Linkage",   xlab="",ylab="",sub="")

dev.new()   # x11()
par(mfrow=c(2,2))
#
# Recognising clusters by projecting high-dim data to 2D biplot
#
pca = prcomp(nci.norm)
set.seed(2)
km.out = kmeans(nci.norm, 4, nstart=20)
plot(pca$x[,1:2], col=km.out$cluster, pch=16, cex=1.8, main="kMeans(k=4)-labelled biplot")

hc.c.clusters=cutree(hc.complt,4)
hc.a.clusters=cutree(hc.averag,4)
hc.s.clusters=cutree(hc.single,4)
plot(pca$x[,1:2], col=hc.c.clusters, pch=16, cex=1.8, main="AGNES(Complete-link) biplot")
plot(pca$x[,1:2], col=hc.a.clusters, pch=16, cex=1.8, main="AGNES(Average-link) biplot")
plot(pca$x[,1:2], col=hc.s.clusters, pch=16, cex=1.8, main="AGNES(Single-link) biplot")
text(pca$x[,1:2], nci.labs, cex=0.6)

#
# Compare unsupervised learning results (clusters) to actual label
#
table(km.out$cluster,nci.labs)
table(hc.c.clusters,nci.labs)
table(hc.a.clusters,nci.labs)
table(hc.s.clusters,nci.labs)


