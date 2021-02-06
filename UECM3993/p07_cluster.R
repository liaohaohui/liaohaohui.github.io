# -------------------------------------------------------------------
# Purpose: Practical for Clustering in R
# Author : Liew How Hui (2021)
# Reference: 
#  1. http://faculty.marshall.usc.edu/gareth-james/ISL/Chapter%2010%20Labs.txt
#  2. https://altaf-ali.github.io/ISLR/chapter10/lab.html
# License: BSD-3
# Software: R 3.6 & R 4.0
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
set.seed(2)   # k-Means take initial centroids randoms!
km.out=kmeans(x,2)
km.out$cluster
plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results with K=2", 
  xlab="x", ylab="y", pch=20, cex=2)

set.seed(3)
km.out=kmeans(x,3,nstart=20)  # 20 random centroids get the one with min WSS
plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results with K=3", 
  xlab="x", ylab="y", pch=20, cex=2)


#-------------------------------------------------------------------------
# Example 2: K-Means Clustering for clustering.xlsx.
#-------------------------------------------------------------------------

# you should have imported clustering.xlsx before running the following codes
X = read.csv("DataLab/clustering.csv")
names(X)
# exclude variable "id"
X = X[,-1]

# plot scatterplot of x1 vs x2
plot(X) #plot(X$x1,X$x2)

# k-means clustering with number of clusters = 4
km.out = kmeans(X, 4)  # With/Without nstart=20

# get cluster means 
km.out$centers

X_group = data.frame(X, km.out$cluster)
head(X_group)

# plot data with cluster assigned
plot(X,col=km.out$cluster+1,xlab = "x1",ylab = "x2", pch=15, cex=2)

# get the best k
wss = (nrow(X)-1)*sum(apply(X,2,var)) #for k=1
for (k in 2:15) {
  wss[k] <- sum(kmeans(X, centers=k, nstart=20)$withinss)
}
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

# standardisation
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
x11()
plot(X_rsc)

set.seed(5)
par(mfrow=c(2,2))
#group_norm = list()
for(k in c(2,3,4,6)) {
	km = kmeans(X_rsc,k)  # ,nstart=20
	plot(X_rsc, col=km$cluster, pch=16, cex=1.5,
      main=sprintf("k-Means with k=%d",k))
}

# PAM (k-Medoids)
kmd = cluster::pam(X, 4)
plot(X, col=kmd$clustering)

# GMM (Gaussian Mixture Model)
library(mclust, quietly=TRUE)  #install.packages("mclust")
gmm.model = Mclust(X, 4)
plot(gmm.model, what="classification") # others: BIC, uncertainty, density


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
# Cut the dendrograms into k clusters
par(mfrow=c(2,3))
km.out=kmeans(x,2,nstart=20)
plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results with K=2", 
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

# Cluster by height
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
nci.labs=NCI60$labs
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

x11()
# Recognising clusters using human vision(?)
pca = prcomp(nci.norm)
set.seed(2)
par(mfrow=c(2,2))
km.out = kmeans(nci.norm, 4, nstart=20)
plot(pca$x[,1:2], col=km.out$cluster, pch=16, main="kMeans-labelled biplot")

hc.c.clusters=cutree(hc.complt,4)
hc.a.clusters=cutree(hc.averag,4)
hc.s.clusters=cutree(hc.single,4)
plot(pca$x[,1:2], col=hc.c.clusters, pch=16, main="AGNES(Complete-link) biplot")
plot(pca$x[,1:2], col=hc.a.clusters, pch=16, main="AGNES(Average-link) biplot")
plot(pca$x[,1:2], col=hc.s.clusters, pch=16, main="AGNES(Single-link) biplot")

# Compare unsupervised learning results (clusters) to actual label
table(hc.c.clusters,nci.labs)


#-------------------------------------------------------------------------
# https://bradleyboehmke.github.io/HOML/kmeans.html
#-------------------------------------------------------------------------

# Most real life data sets contain a mixture of numeric, categorical, and
# ordinal variables; and whether an observation is similar to another
# observation should depend on these data type attributes. There are a few
# options for performing clustering with mixed data and we'll demonstrate on
# the full Ames housing data set (minus the response variable Sale_Price). To
# perform k-means clustering on mixed data we can convert any ordinal
# categorical variables to numeric and one-hot encode the remaining nominal
# categorical variables.

library(AmesHousing)
ames_full = make_ames()
dim(ames_full)
# Full ames data set --> recode ordinal variables to numeric
# ames_full <- AmesHousing::make_ames() %>%
#   mutate_if(str_detect(names(.), 'Qual|Cond|QC|Qu'), as.numeric)
### Using the `dplyr' technology, R `command' may be shorter
for(i in colnames(ames_full)){
	if(grepl("Qual|Cond|QC|Qu", i)) {
		cat(i,"\n")
		ames_full[[i]] = as.numeric(ames_full[[i]])
	}
}

### caret provides `dummyVars' which is very difficult to achieve
### using plain R --- in such cases, it is reasonable to use `caret'
### But for the situation where simple R will do, don't install
### too many packages.
# One-hot encode --> retain only the features and not sale price
full_rank = caret::dummyVars(Sale_Price~., data=ames_full, fullRank=TRUE)
ames_1hot = predict(full_rank, ames_full)
ames_1hot_scaled = as.data.frame(scale(ames_1hot))
# Note: ames_1hot[,"Neighborhood.Hayden_Lake"] are all zero,
# after scaling, it becomes NaN!
ames_1hot_scaled$Neighborhood.Hayden_Lake = NULL
dim(ames_1hot_scaled)  # Large dimensions than the original data `ames_full'

wss = rep(0,25)
Ks = 1:25
for(k in Ks){
	cat("Calculating k=",k," ...\n")
	km = kmeans(ames_1hot_scaled, k, nstart=20)
	wss[k] = sum(km$withinss)
}
plot(Ks, wss, xlab="Number of clusters k", ylab="Total Within Sum of Squares")
lines(Ks, wss)

# Unfortunately, this is a common issue. As the number of features expand,
# performance of k-means tends to break down and both k-means and hierarchical
# clustering (Chapter 21) approaches become slow and ineffective. This happens,
# typically, as your data becomes more sparse. An additional option for heavily
# mixed data is to use the Gower distance (Gower 1971) measure, which applies a
# particular distance calculation that works well for each data type. The
# metrics used for each data type include:
#
#   quantitative (interval): range-normalized Manhattan distance;
#   ordinal: variable is first ranked, then Manhattan distance is used with a special adjustment for ties;
#   nominal: variables with \(k\) categories are first converted into \(k\) binary columns (i.e., one-hot encoded) and then the Dice coefficient is used. To compute the dice metric for two observations \(\left(X, Y\right)\), the algorithm looks across all one-hot encoded categorical variables and scores them as:
#       a — number of dummies 1 for both observations
#       b — number of dummies 1 for \(X\) and 0 for \(Y\)
#       c — number of dummies 0 for \(X\) and 1 for \(Y\)
#       d — number of dummies 0 for both
#
#and then uses the following formula:
#
#        D = \frac{2a}{2a + b + c}

# We can use the cluster::daisy() function to create a Gower distance matrix
# from our data; this function performs the categorical data transformations so
# you can supply the data in the original format.

library(cluster)
ames_full = make_ames()
ames_full$Sale_Price = NULL
gower_dst = daisy(ames_full, metric="gower")

# We can now feed the results into any clustering algorithm that accepts a
# distance matrix. This primarily includes cluster::pam(), cluster::diana(),
# and cluster::agnes() (stats::kmeans() and cluster::clara() do not accept
# distance matrices as inputs).

pam_gower   = pam(x = gower_dst, k = 8, diss = TRUE)
diana_gower = diana(x = gower_dst, diss = TRUE)
agnes_gower = agnes(x = gower_dst, diss = TRUE)
# Dendrogram plot is not feasible!
# pam_gower$clustering
# cutree(diana_gower, 8)  # 8 clusters ???
# table(cutree(diana_gower, 8), cutree(agnes_gower, 8))
# table(cutree(diana_gower, 8), pam_gower$clustering)


