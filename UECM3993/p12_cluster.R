# -------------------------------------------------------------------
# Purpose: Practical for Clustering in R
# Author : Liew How Hui (2022)
# Reference: 
#  1. http://faculty.marshall.usc.edu/gareth-james/ISL/Chapter%2010%20Labs.txt
#  2. https://altaf-ali.github.io/ISLR/chapter10/lab.html
# License: BSD-3
# Software: R 3.6 & R 4.0
# Duration: 1 hour
# -------------------------------------------------------------------

# Normalization
#library(scales)
rescale = function(x, to = c(0, 1), from = range(x, na.rm = TRUE, finite = TRUE), ...) {
  (x - from[1]) / diff(from) * diff(to) + to[1]
}

#library(BBmisc) # BBmisc::normalize
#X_rsc <- normalize(X,method="range",range=c(0,1))
#X_std <- BBmisc::normalize(X,method="standardize")
normalise.by.range = function(x, range) {
  (x - min(x, na.rm = TRUE)) / diff(range(x, na.rm = TRUE)) * 
  diff(range) + range[1L]
}

#-------------------------------------------------------------------------
# Example 1: Basic K-Means with 2 clusters (simulated)
#-------------------------------------------------------------------------

# set 2 clusters simulated data
set.seed(2)
x=matrix(rnorm(50*2), ncol=2)
# shift the first 25 rows to centre at (3,-4)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4

# K-Means Clustering with 2 generated clusters
set.seed(2)   # k-Means take initial centroids randomly!
km.out=kmeans(x,2)
km.out$cluster
plot(x, col=km.out$cluster, main="K-Means Clustering Results with K=2", 
  xlab="x", ylab="y", pch=20, cex=2)

set.seed(3)
km.out=kmeans(x,3,nstart=20)  # 20 random centroids get the one with min WSS
plot(x, col=km.out$cluster, main="K-Means Clustering Results with K=3", 
  xlab="x", ylab="y", pch=20, cex=2)


#-------------------------------------------------------------------------
# Example 2: K-Means Clustering for clustering.xlsx.
#-------------------------------------------------------------------------

#https://liaohaohui.github.io/UECM3993/clustering.csv
X = read.csv("clustering.csv")
names(X)
# exclude variable "id"
X = X[,-1]   # Alternatives: X = X[,2:3] or X$id = NULL

# plot scatterplot of x1 vs x2
plot(X) #plot(X$x1,X$x2)

# k-means clustering with number of clusters = 4
set.seed(2)
# set.seed(22)  # gives a different clustering which is funny
km.out = kmeans(X, 4)  # With/Without nstart=20

# get cluster means 
km.out$centers

X_group = data.frame(X, km.out$cluster)
head(X_group)

# plot data with cluster assigned
plot(X,col=km.out$cluster+1,xlab = "x1",ylab = "x2", pch=15, cex=2)

# WSS = Within Sum of Squares
# get the best k
total.wss = (nrow(X)-1)*sum(apply(X,2,var)) #for k=1
for (k in 2:15) {
  total.wss[k] <- sum(kmeans(X, centers=k, nstart=20)$withinss)
}
# Shorter: total.wss = sapply(1:15, function(k){kmeans(X, centers=k, nstart=20)$tot.withinss})
plot(total.wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
# -> Elbow method

# standardisation is not going to eliminate the `local' minimum problem
X_std = as.data.frame(lapply(X, scale))
head(X_std)
plot(X_std)

set.seed(3)
par(mfrow=c(2,2))
#group_norm = list()
for(k in c(2,3,4,6)) {
	km = kmeans(X_std,k)  # nstart=20
	#group_norm[[sprintf("k%d",k)]] <- km
	plot(X_std, col=km$cluster, pch=16, cex=1.5,
      main=sprintf("k-Means with k=%d",k))
}

# min-max scaling
X_rsc = as.data.frame(lapply(X, normalise.by.range, range=c(0,1)))
head(X_rsc)
x11()  # Same thing as dev.new()
plot(X_rsc)

set.seed(5)  # Compare to seed.no=3 as well
par(mfrow=c(2,2))
#group_norm = list()
for(k in c(2,3,4,6)) {
	km = kmeans(X_rsc,k,nstart=20)  # nstart is to avoid `local minimum'
	plot(X_rsc, col=km$cluster, pch=16, cex=1.5,
      main=sprintf("k-Means with k=%d",k))
}

# PAM (k-Medoids)
kmd = cluster::pam(X, 4)   # cluster is in the Base R
plot(X, col=kmd$clustering, pch=16, cex=2)

## GMM (Gaussian Mixture Model)
#library(mclust, quietly=TRUE)  #install.packages("mclust")
#gmm.model = Mclust(X, 4)
#plot(gmm.model, what="classification", pch=16, cex=2) # others: BIC, uncertainty, density


#-------------------------------------------------------------------------
# Example 3: Hierarchical clustering with 2 clusters
#-------------------------------------------------------------------------

# set 2 clusters simulated data (same as Example 1)
set.seed(2)
x=matrix(rnorm(50*2), ncol=2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4

# Hierarchical Clustering
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
# Cut the dendrograms into k clusters: Two ways:
# (1) specify how many clusters we want
# (2) cut at a specific height
#
# (1) specify how many clusters we want
par(mfrow=c(2,3))
km.out=kmeans(x,2,nstart=20)
plot(x, col=km.out$cluster, main="K-Means Clustering Results with K=2", 
  xlab="x", ylab="y", pch=20, cex=2)
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

# (2) Cluster by height
par(mfrow=c(2,2))
cls1 = cutree(hc.complete, h=4)
plot(x, col=cls1, pch=15, cex=1.5, main="Complete-2 (height=4)")
cls2 = cutree(hc.average,  h=3)
plot(x, col=cls2, pch=15, cex=1.5, main="Average (height=3)")
cls5 = cutree(hclust(dist.mat, method="ward.D2"), h=3)
plot(x, col=cls5, pch=15, cex=1.5, main="Ward (height=3)")
cls4 = cutree(hc.single,  h=1)
plot(x, col=cls4, pch=15, cex=1.5, main="Single (height=1)")

# Scaled data: No difference compare to the original data
x.norm = rescale(x, to=c(0,1))
par(mfrow=c(1,2))
plot(hc.complete,main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hclust(dist(x.norm), method="complete"),
  main="Normalized Complete Linkage", xlab="", sub="", cex=.9)
# hc.norm = hclust(dist(x.norm), method="complete")
# For comparing: table(cutree(hc.complete,5),cutree(hc.norm, 5))
# min-max scaling has no effect in this example


#-------------------------------------------------------------------------
# Title: NCI60 with hierarchical clustering
# Reference: https://rstudio-pubs-static.s3.amazonaws.com/14007_8294de3bde644c18bf7c45c2f75bd16f.html
#-------------------------------------------------------------------------

library(ISLR)
nci.labs=NCI60$labs    # Mentioned in Practical 6
nci.data=NCI60$data
dim(nci.data)  # -> 64 x 6830 (n < p)
table(nci.labs)

nci.norm = rescale(nci.data, to=c(0,1))

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
# Recognising clusters using human vision(?)
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

# Compare unsupervised learning results (clusters) to actual label
table(hc.c.clusters,nci.labs)


#-------------------------------------------------------------------------
# https://bradleyboehmke.github.io/HOML/kmeans.html
#-------------------------------------------------------------------------

fraud = read.csv("fraud.csv")
col_fac = c("gender", "status", "employment", "account_link", "supplement", "tag")
### change data type from numeric to categorical
fraud[col_fac] = lapply(fraud[col_fac], factor)
fraud[c("id_person", "tag")] = NULL

# Get stats range of each column
apply(fraud,2,range)
library(gower)
# For gower distance (https://jamesmccaffrey.wordpress.com/2020/04/21/example-of-calculating-the-gower-distance/):
# numeric:     abs(diff) / range 
# non-numeric: 0 if equal, 1 if different
# E.g. Consider Item 1 vs Item 2 & 3
# Calculation for Item 1 vs Item 3:
# For the categorical columns: I(1!=1)+I(2!=3)+I(3!=1)+I(0!=0)+I(1!=0) => 3
# For age: abs(32-21)/(57-21) => 0.3055556
# For base_value: abs(729.3-683.8)/(729.3-384.1) => 0.1318076
# gower distance = (3 + 0.3055556 + 0.1318076)/7 = 0.4910519  # 7 columns
gower_dist(fraud[1,], fraud[2:3,])  # Item 1 vs 3 => 0.4910519

library(cluster)
gower_dst = daisy(fraud, metric="gower")  # Dissimilarity Matrix
                                          # Generalisation of distance matrix

# We can now feed the results into any clustering algorithm that accepts a
# distance matrix. This primarily includes cluster::pam(), cluster::diana(),
# and cluster::agnes() (stats::kmeans() and cluster::clara() do not accept
# distance matrices as inputs).

pam_gower   = pam(x = gower_dst, k = 8, diss = TRUE)
agnes_gower = agnes(x = gower_dst, diss = TRUE)  # Can be slow (1 minute?)
diana_gower = diana(x = gower_dst, diss = TRUE)  # Diana is very slow
# Dendrogram plot is not feasible for agnes & diana (>2000: too many points)!
# pam_gower$clustering
# table(cutree(diana_gower, 8), pam_gower$clustering)
# cutree(diana_gower, 8)  # 8 clusters ???
# table(cutree(diana_gower, 8), cutree(agnes_gower, 8))


