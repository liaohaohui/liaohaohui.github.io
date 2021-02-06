# -------------------------------------------------------------------
# Purpose: Practical for PCA in R
# Author : Liew How Hui (2021)
# Reference: 
#  1. http://faculty.marshall.usc.edu/gareth-james/ISL/Chapter%2010%20Labs.txt
#  2. https://uc-r.github.io/pca
#  3. https://altaf-ali.github.io/ISLR/chapter10/lab.html
# Data   : http://faculty.marshall.usc.edu/gareth-james/ISL/data.html
# License: BSD-3
# Software: R 4.x
# -------------------------------------------------------------------

#-------------------------------------------------------------------------
#  Analysis of the `USArrests' Dataset (in R) with PCA (n>p)
#-------------------------------------------------------------------------
data(USArrests)
names(USArrests)              # ncol(USArrests) -> 4
states=row.names(USArrests)
apply(USArrests, 2, var)      # Check if `scaling' is necessary
pca = prcomp(USArrests, scale=TRUE)
names(pca)

# keep in mind that there may be instances where scaling is not desirable. An
# example would be if every variable in the data set had the same units and the
# analyst wished to capture this difference in variance for his or her results.
# Since Murder, Assault, and Rape are all measured on occurrences per 100,000
# people this may be reasonable depending on how you want to interpret the
# results. But since UrbanPop is measured as a percentage of total population
# it wouldn’t make sense to compare the variability of UrbanPop to Murder,
# Assault, and Rape.

# The important thing to remember is PCA is influenced by the magnitude of each
# variable; therefore, the results obtained when we perform PCA will also
# depend on whether the variables have been individually scaled.

# Relate to Linear Algebra Theory --- Eigenvectors
pca$center    # compare it to apply(USArrests, 2, mean)
pca$scale
pca$rotation  # eigenvectors
dim(pca$x)    # principle components of the original row
biplot(pca, scale=0)
# The scale = 0 argument to biplot ensures that the arrows are scaled to
# represent the loadings; other values for scale give slightly different
# biplots with different interpretations.

# Eigenvalues --- Measuring ``representativeness'' when $n >> p$
pr.var=pca$sdev^2
pve=pr.var/sum(pr.var)
plot(pve, ylim=c(0,1), type='b', xlab="Principal Component",
  ylab="Proportion of Variance Explained")
lines(cumsum(pve), ylim=c(0,1), type='b', xlab="Principal Component",
  ylab="Cumulative Proportion of Variance Explained")

# Alternative: screeplot(pca, type="line")


#-------------------------------------------------------------------------
#  ISLR: Analysis of the `NCI60' Dataset with PCA (n<p)
#-------------------------------------------------------------------------
library(ISLR)
class(NCI60)
nci.labs=NCI60$labs
nci.data=NCI60$data
dim(nci.data)            # -> 64 x 6830

# Biplot -> for identification of ``patterns'' or ``clusters''?
pca = prcomp(nci.data, scale=TRUE)
summary(pca)
plot(pca)   # -> screeplot
pve = 100*pca$sdev^2/sum(pca$sdev^2)
par(mfrow=c(1,2))
plot(pve,  type="o", ylab="PVE", xlab="Principal Component", col="blue")
plot(cumsum(pve), type="o", ylab="Cumulative PVE", 
  xlab="Principal Component", col="brown3")
par(mfrow=c(1,1))
biplot(pca, col=c(1,0), scale=0)  # make colour of the axes white
# plot(pca$x[,1:2],cex=0.1); text(pca$x[,1:2])


#-------------------------------------------------------------------------
#  Q2. PCA on Rooms.csv
#-------------------------------------------------------------------------

# Q2 (a)
Room = read.csv("DataLab/Room.csv")
head(Room)
temp = Room[,2:5]
apply(temp, 2, mean)
apply(temp, 2, sd)
plot(temp)  # -> `pair' scatter plot
# FrontLeft and FrontRight are highly correlated.  BackLeft and
# BackRight are highly correlated.  Two variables are expected to be used.
# First eigenvector captures the direction of majority of the variation.

# Q2 (b)
pca = prcomp(temp)
biplot(pca, scale=0)
# The biplot shows similar vectors for FrontLeft and FrontRight, and
# similar vectors for BackLeft and BackRight.  The first two PCs
# explained most (>90%) variation from (a).

# Q2 (c)
screeplot(pca, type='line')
pr.var=pca$sdev^2
pve=pr.var/sum(pr.var)
plot(pve, xlab="PC", ylab="PVE", ylim=c(0,1),type='b')
#sum(pve[1:2])
# 2 PCs should be considered (elbow method), which explained 93.5\%
# variation.

#-------------------------------------------------------------------------
#  Q3. PCA on foodtexture.csv (https://www.geo.fu-berlin.de/en/v/soga/Geodata-analysis/Principal-Component-Analysis/Principal-component-analysis-in-R/index.html)
#-------------------------------------------------------------------------

#food = read.csv("https://userpage.fu-berlin.de/soga/300/30100_data_sets/food-texture.csv")
food = read.csv("DataLab/foodtexture.csv")
food$X = NULL   # Remove the first column
apply(food, 2, mean)
apply(food, 2, sd)   # this tells us why the scale=TRUE
pca=prcomp(food, scale=TRUE)  # Normalised data to var=1
summary(pca)
pve = pca$sdev^2/sum(pca$sdev^2)
plot(pve, xlab="PC", ylab="PVE", ylim=c(0,1),type='b')
# From the PCA summary, 2 PCs are required for 85\% PVE. Total PVE is 86.54\%.
biplot(pca, scale=0)


#-------------------------------------------------------------------------
# From: https://aaronschlegel.me/principal-component-analysis-r-example.html
# Data: https://www.wiley.com/en-my/Methods+of+Multivariate+Analysis,+3rd+Edition-p-9780470178966
#-------------------------------------------------------------------------

pilots = read.table('DataLab/AlvinC.Rencher/T5_6_PILOT.DAT', col.names=c('Group', 
  'Intelligence', 'Form Relations', 'Dynamometer', 'Dotting', 
  'Sensory Motor Coordination', 'Perservation'))
pilots$Group = ifelse(pilots$Group == 1, 'Apprentice', 'Pilot')
S = cov(pilots[,2:7])
s.eigen = eigen(S)
s.eigen$values/sum(s.eigen$values)
plot(s.eigen$values, xlab = 'Eigenvalue Number', ylab = 'Eigenvalue Size', main = 'Scree Graph')
lines(s.eigen$values)
# The elements of the eigenvectors of S are the 'coefficients' or 'loadings' of
# the principal components.
s.eigen$vectors
pilots.pca = prcomp(pilots[,2:7])
summary(pilots.pca)

#To recreate the graph generated by autoplot(), scale the data using the
#standard deviations of the principal components multiplied by the square root
#of the number of observations. The principal components are then computed for
#each observation vector. Note the first eigenvector is multiplied by a scaling
#factor of −1 so the signs what was reported by the prcomp() function.

scaling = pilots.pca$sdev[1:2] * sqrt(nrow(pilots))
pc1 = rowSums(t(t(sweep(pilots[,2:7], 2 ,colMeans(pilots[,2:7]))) * s.eigen$vectors[,1] * -1) / scaling[1])
pc2 = rowSums(t(t(sweep(pilots[,2:7], 2, colMeans(pilots[,2:7]))) * s.eigen$vectors[,2]) / scaling[2])
d.f <- data.frame(pc1, pc2, c(rep('Apprentice', 20), rep('Pilot', 20)))
colnames(d.f) <- c('PC1', 'PC2', 'Group')
plot(d.f[,1:2], col=as.integer(d.f$Group), pch=15+as.integer(d.f$Group), cex=2)
text(d.f[,1], d.f[,2]+0.025, d.f$Group)


#-------------------------------------------------------------------------
# Try comparing to
#  https://bio723-class.github.io/Bio723-book/principal-components-analysis.html
#-------------------------------------------------------------------------

#bioenv = read.table('https://github.com/Bio723-class/example-datasets/raw/master/bioenv.txt',sep="\t",header=TRUE) 
bioenv = read.table('DataLab/bioenv.txt',sep="\t",header=TRUE) 
# The first column corresponds to the sampling sites. Before we move on let's
# give this column a more meaningful name:
names(bioenv)[1] = "Site"
abundance = bioenv[,1:6]
head(abundance)
abundance.only = abundance[,2:6]
boxplot(abundance.only,xlab="Species",ylab="Count", main="Distribution of\nSpecies Counts per Site")
sapply(abundance.only,mean)
sapply(abundance.only,var)
cor(abundance.only)
pairs(abundance.only)  # The bio723 website's plot is prettier
abundance.pca = prcomp(abundance.only)  # center=TRUE, retx=TRUE are default
summary(abundance.pca)
# We see that approximately 59% of the variance in the data is capture by 
# the first PC, and approximately 90% by the first three PCs.
par(mfrow=c(2,2))
biplot(abundance.pca, scale=0)
biplot(abundance.pca, scale=0, choices=c(1,3))
biplot(abundance.pca, scale=0, choices=c(2,3))

#-------------------------------------------------------------------------
#  Biomedical Data Science: http://genomicsclass.github.io/book/
#-------------------------------------------------------------------------

### https://github.com/genomicsclass/tissuesGeneExpression/tree/master/data
load("DataLab/tissuesGeneExpression.rda")
ls()
dim(e)
x = t(e)
pc = prcomp(x)
plot(pc$x[,1], pc$x[,2], col=as.integer(factor(tab$Tissue)), 
  pch=15, cex=2, main = "PCA", xlab = "PC1", ylab = "PC2")

# https://github.com/genomicsclass/GSE5859/tree/master/data
# exprs_GSE5859.csv: https://github.com/cs109/2014_data
# https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE5859
# Allelic Differences Account for Gene Expression Differences Among Population
# Keywords: Comparison of Gene Expression Profiles from Lymphoblastoid cells
gse5859 = read.csv("DataLab/exprs_GSE5859.csv",row.names=1,header=TRUE)
sampleinfo = read.csv("DataLab/sampleinfo_GSE5859.csv")
idx = data.frame(
	gse5859 = order(names(gse5859)), samplei = order(sampleinfo$filename)
)
gse5859 = gse5859[,idx[order(idx$samplei),"gse5859"]]
all(names(gse5859)==sampleinfo$filename)

### http://genomicsclass.github.io/book/pages/eda_with_pca.html
# We start by exploring the sample correlation matrix and noting that one pair
# has a correlation of 1. This must mean that the same sample was uploaded
# twice to the public repository, but given different names. The following code
# identifies this sample and removes it.
cors = cor(gse5859)
Pairs = which(abs(cors)>0.9999,arr.ind=TRUE)
out = Pairs[which(Pairs[,1]<Pairs[,2]),,drop=FALSE]
if(length(out[,2])>0) {
	gse5859 = gse5859[,-out[2]]
	sampleinfo = sampleinfo[-out[2],]
}

pc = prcomp(t(gse5859))
#summary(pc)  # too many, not nice
plot(pc$sdev[1:50]/sum(pc$sdev),type="b")
groups = factor(sampleinfo$ethnicity)
kmcls = kmeans(pc$x[,1:2], length(levels(groups)), nstart=20)
plot(pc$x[,1:2],col=as.integer(groups),pch=15+kmcls$cluster,cex=2.3)
x11()  # dev.new()
biplot(pc,col=c(1,0),scale=FALSE,cex=0.5)

cat("
* 
* Rtsne takes a long time to calculate and converge.
* You can skip this if your computer is slow with < 8G memory.
*
")
library(Rtsne)  # Uses Barnes-Hut-TSNE algorithm instead of the slower t-SNE
### tSNE does not allow duplicate data
tsne = Rtsne(t(gse5859), dims=2, perplexity=18, verbose=TRUE, max_iter=900)
plot(tsne$Y, t='n', main="tsne")
text(tsne$Y, labels=as.factor(sampleinfo$ethnicity), col=as.integer(as.factor(groups)))


#-------------------------------------------------------------------------
#  https://github.com/mkinsey/digit-classifier
#-------------------------------------------------------------------------
library(MASS)
train = read.csv("DataLab/mnist_train.csv")
# We need to remove those columns where there are less than 4 values
# some of them are alway 0 (indicating a black dot?) => Feature selection
train = train[,apply(train, 2, function(col) {length(unique(col))>4})]

cat("
* 
* The K-fold CV with LDA takes really long time to calculate.
* You can skip this if your computer is slow with < 8G memory.
*
")
# K-fold CV with LDA (K=10 is slow, K=5 may be faster)
N = nrow(train)
K = 5   # 10
S = N/K   # Size of 1-fold
results = vector(length=K)
set.seed(1)
idx = sample(N)
for (i in c(1:K)){
  # form testing and training sets
  X.train = train[-idx[((i-1)*S+1):(i*S)], -1]
  y.train = train[-idx[((i-1)*S+1):(i*S)],  1]
  X.test  = train[ idx[((i-1)*S+1):(i*S)], -1]
  y.test  = train[ idx[((i-1)*S+1):(i*S)],  1]

  lda.fit = lda(X.train, y.train)
  yhat = predict(lda.fit, X.test)
  cfmat = table(yhat$class, y.test)
  results[i] = sum(diag(cfmat))/sum(cfmat)
  cat("Run", i, ": LDA classifier had ", 100*results[i],"% accuracy.\n")
}

cat("
* 
* The K-fold CV with PCA+kNN takes super long time to calculate.
* You can skip this if your computer is slow with < 8G memory.
*
")
# K=5 K-fold CV with PCA (feature selection) + kNN (k=1)
train = read.csv("DataLab/mnist_train.csv"); N = nrow(train)
K = 5
S = N/K   # Size of 1-fold
results = vector(length=K)
set.seed(1)
idx = sample(N)
library(class)
for (i in c(1:K)){
  X.train = train[-idx[((i-1)*S+1):(i*S)], -1]
  y.train = train[-idx[((i-1)*S+1):(i*S)],  1]
  X.test  = train[ idx[((i-1)*S+1):(i*S)], -1]
  y.test  = train[ idx[((i-1)*S+1):(i*S)],  1]
  pca = prcomp(X.train)
  R = pca$rotation[ , 1:150]  # columns are eigenvectors, use first 150 PCs
  X.train.pca = as.matrix(X.train) %*% R
  X.test.pca  = as.matrix(X.test)  %*% R
  yhat = knn(X.train.pca, X.test.pca, y.train)
  cfmat = table(yhat, y.test)
  results[i] = sum(diag(cfmat))/sum(cfmat)
  cat("Run", i, ": The KNN + PCA classifier had ", 100*results[i],"% accuracy.\n")
}

