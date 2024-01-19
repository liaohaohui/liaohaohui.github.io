# -------------------------------------------------------------------
# Purpose: Practical for PCA in R for simple dimensional reduction
#          and using it a preprocessing stage for supervised models
# Author : Liew How Hui (2024)
# Reference: 
#  1. http://faculty.marshall.usc.edu/gareth-james/ISL/Chapter%2010%20Labs.txt
#  2. https://uc-r.github.io/pca
#  3. https://altaf-ali.github.io/ISLR/chapter10/lab.html
# Data   : install.packages("ISLR2")
# License: BSD-3
# Software: R 4.x
# Duration: 1 hour
# -------------------------------------------------------------------

#-------------------------------------------------------------------------
#  Analysis of the `USArrests' Dataset (in R) with PCA (n>p)
#-------------------------------------------------------------------------
#data(USArrests)              # Default in R
names(USArrests)              # ncol(USArrests) -> 4
#states = rownames(USArrests)
apply(USArrests, 2, var)      # Step 0: Check if `scaling' is necessary
pca = prcomp(USArrests, scale=TRUE)   # Step 1 & 2 -> shift to centre & scale
names(pca)                            # pca contains results from Step 4
# Older PCA tool: princomp

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

#
# Relate to Linear Algebra Theory --- Eigenvectors
#
pca$center    # compare it to apply(USArrests, 2, mean)
pca$scale
pca$rotation  # eigenvectors
dim(pca$x)    # principle components of the original row data
biplot(pca, scale=0)
# The scale = 0 argument to biplot ensures that the arrows are scaled to
# represent the loadings; other values for scale give slightly different
# biplots with different interpretations.

# Eigenvalues --- Measuring ``representativeness'' when $n >> p$
# Step-by-step method to plot PVE vs component order
pr.var=pca$sdev^2
pve=pr.var/sum(pr.var)
# Plotting screeplot manual
par(mfrow=c(1,2))
plot(pr.var, xlab = 'i', ylab = expression(paste(lambda,'_i',sep='')), main = 'Scree Graph')
plot(pve, ylim=c(0,1), type='b', xlab="Principal Component",
  ylab="Proportion of Variance Explained") #-> PVE instead of Variances
lines(cumsum(pve), ylim=c(0,1), type='b', xlab="Principal Component",
  ylab="Cumulative Proportion of Variance Explained")
par(mfrow=c(1,1))

# Alternative: screeplot(pca, type="line") -> Variances


#-------------------------------------------------------------------------
#  Food Texture Data (https://www.geo.fu-berlin.de/en/v/soga/Geodata-analysis/Principal-Component-Analysis/Principal-component-analysis-in-R/index.html)
#-------------------------------------------------------------------------

### https://openmv.net/info/food-texture
#Description:	Texture measurements of a pastry-type food.
#Data source:	
#    Simulated data, but has the characteristics from an industrial problem.
#    Oil: percentage oil in the pastry
#    Density: the product’s density (the higher the number, the more dense the product)
#    Crispy: a crispiness measurement, on a scale from 7 to 15, with 15 being more crispy.
#    Fracture: the angle, in degrees, through which the pasty can be slowly bent before it fractures.
#    Hardness: a sharp point is used to measure the amount of force required before breakage occurs. 
#Data shape:	50 rows and 5 columns

# Original Source: https://userpage.fu-berlin.de/soga/300/30100_data_sets/food-texture.csv
#https://liaohaohui.github.io/UECM3993/foodtexture.csv
food = read.csv("foodtexture.csv")
food$X = NULL   # Remove the first column
apply(food, 2, sd)   # this tells us why the scale=TRUE
pca=prcomp(food, scale=TRUE)  # Normalised data to var=1
summary(pca)
pve = pca$sdev^2/sum(pca$sdev^2)
plot(pve, xlab="PC", ylab="PVE", ylim=c(0,1),type='b')
# From the PCA summary, 2 PCs are required for 85\% PVE. Total PVE is 86.54\%.
biplot(pca, scale=0)


#-------------------------------------------------------------------------
#  Eigenface (https://en.wikipedia.org/wiki/Eigenface)
#  Ref: https://rstudio-pubs-static.s3.amazonaws.com/543854_eac98275af7d461792a3523fae88be22.html
#  Ref: https://github.com/k41m4n/eigenfaces
#  Ref: https://laid.delanover.com/explanation-face-recognition-using-eigenfaces/
#-------------------------------------------------------------------------

# Load data of face images taken between April 1992 and April 1994 
# at AT&T Laboratories Cambridge in USA
### Large file (35M): https://raw.githubusercontent.com/k41m4n/eigenfaces/master/olivetti_X.csv
#Compressed: https://liaohaohui.github.io/UECM3993/olivetti_X.csv.gz
faceX = read.csv(gzfile("olivetti_X.csv.gz"), header=FALSE)
# faceX is a data.frame, convert it to matrix
faceX = matrix(unlist(faceX),nrow=nrow(faceX),ncol=ncol(faceX))
par(mfrow=c(4, 10))
par(mar=c(0.05, 0.05, 0.05, 0.05))
for (i in 1:40) {
  # Original image is bottom to top
  img = matrix(faceX[i, ], nrow=64)
  # Reverse the image in y-direction
  image(img[,64:1], col=grey(seq(0, 1, length=256)), xaxt="n", yaxt="n")
}

# Create a sequence of label numbers from 1 to 40 corresponding to 40 persons
Nfigs = 40
dataY = data.frame(label=rep(1:Nfigs,each=10))

set.seed(2024)
K = 10
M = 7
idx.train = rep(sample(K,M), Nfigs) + rep(K*(0:(Nfigs-1)), each=M)

face.train = faceX[ idx.train,]
face.test  = faceX[-idx.train,]
Y.train = dataY$label[ idx.train]
Y.test  = dataY$label[-idx.train]
rm(faceX)
pca = prcomp(face.train)

#
# Average Face
#
par(mfrow=c(1,1))
face_avg = matrix(pca$center, nrow=64)
image(face_avg[,64:1], col=grey(seq(0, 1, length=256)), xaxt="n", yaxt="n")

plot(pca$x[,1], pca$x[,2], cex=0.05, main="Biplot of the faces")
text(pca$x[,1], pca$x[,2], dataY$label[idx.train], cex=2, col=dataY$label[idx.train])

# PCs for test data
PCs = predict(pca, face.test)
points(PCs[,1], PCs[,2], pch=16, cex=2, col=dataY$label[-idx.train], main="Biplot of the faces")

#screeplot(pca, npcs=20, type="lines")
CPVEs = cumsum(pca$sdev**2) / sum(pca$sdev**2)
thres = min(which(CPVEs > 0.95))   # 95% of eigenfaces are used

par(mfrow=c(4,4))
par(mar=c(0.05,0.05,0.05,0.05))
for (i in 1:16) {
  # Show the eigenfaces
  img = matrix(pca$rotation[,i], nrow=64)
  # Turn up-side-down image back to usual side
  img = img[,64:1]
  image(img, col=grey(seq(0, 1, length=256)), xaxt="n", yaxt="n")
}

#
# This part tries to `recognise' the face using the mathematical 
# formulation for PCA prediction:
#
#   PCs = t(t(face.test) - c(face_avg))
#   PCs = PCs %*% pca$rotation
#
# which is built into predict().
#

img_test = PCs    # 120 test images with 280 PCs
# We just take the PCs which contribute to 95% using 1:thres
yhat = c()
dmin = c()
dmax = c()
for(i in 1:nrow(img_test)){
  lookup = apply(t(pca$x[,1:thres]) - img_test[i,1:thres], 2, function(r){sum(r^2)})
  dmin[i] = min(lookup)
  dmax[i] = max(lookup)
  yhat[i] = Y.train[which.min(lookup)]
}
print(data.frame(yhat, dmin, dmax))
print(table(yhat, Y.test))
cat("Accuracy=", sum(yhat == Y.test)/nrow(img_test), "\n")
# A bit lower than the one predicted by https://github.com/k41m4n/eigenfaces
# Is 100 a good threshold for the face in database?
#ifelse(any(lookup < 100), which.min(lookup), "Not in database")

# Reset plotting parameters
dev.off()

#-------------------------------------------------------------------------
#  ISLR or ISLR2' NCI60 Bio-Dataset with PCA (n<p)
#-------------------------------------------------------------------------
library(ISLR2)
class(NCI60)   # it tells me that it is a list, so we can names(NCI60)
nci.labs=NCI60$labs      # labs = labels?
nci.data=NCI60$data
dim(nci.data)            # -> 64 x 6830

# Biplot -> for identification of ``patterns'' or ``clusters''?
pca = prcomp(nci.data, scale=TRUE)
#summary(pca)    # Not useful because we have 64 data
plot(pca)   # -> screeplot
#
# Manual plot of the PVEs
#
pve = 100*pca$sdev^2/sum(pca$sdev^2)
par(mfrow=c(1,2))
plot(pve,  type="o", ylab="PVE", xlab="Principal Component", col="blue")
plot(cumsum(pve), type="o", ylab="Cumulative PVE", 
  xlab="Principal Component", col="brown3")
#
# Regardless of the PVE, we still project the data to 2D to
# find any clustering or interesting patterns using biplot
#
par(mfrow=c(1,1))
biplot(pca, col=c(1,0), scale=0)  # make colour of the axes white
#
# Manual biplot
#
cc = c("black", "red", "green", "blue", "brown", "yellow", "cyan", "violet", "violetred", "seagreen", "royalblue", "purple", "plum1")
plot(pca$x[,1:2], col=cc[as.integer(as.factor(nci.labs))], pch=16, cex=2)
#text(t(t(pca$x[,1:2])+c(0,3)), labels=nci.labs, cex=0.8)
text(pca$x[,1], pca$x[,2]+2, nci.labs, cex=0.8)

#-------------------------------------------------------------------------
#  More Bio-Dataset with PCA (n<p)
#  Ref: https://bio723-class.github.io/Bio723-book/principal-components-analysis.html
#-------------------------------------------------------------------------

# Original Source: https://github.com/Bio723-class/example-datasets/raw/master/bioenv.txt
#https://liaohaohui.github.io/UECM3993/bioenv.txt
bioenv = read.table('bioenv.txt',sep="\t",header=TRUE) 
# read.csv works fine with Tab (above your Caps Lock on your left keyboard) symbol
names(bioenv)[1] = "Site"  # This is only required for read.table
abundance = bioenv[,1:6]
#head(abundance)
## The first column corresponds to the sampling sites. Before we move on let's
## give this column a more meaningful name:
abundance.only = abundance[,2:6]

#rownames(bioenv) = bioenv$X
#bioenv$X = NULL
#abundance.only = bioenv[,1:5]

#
# Exploratory Data Analysis (EDA)
#
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
biplot(abundance.pca, scale=0)  # Default to PC1 vs PC2
biplot(abundance.pca, scale=0, choices=c(1,3))  # Choose PC1 & PC3
biplot(abundance.pca, scale=0, choices=c(2,3))


#-------------------------------------------------------------------------
#  MNIST Data: PCA vs TSNE
#  Ref: https://rpubs.com/sumangv/TSNE
#-------------------------------------------------------------------------

#https://liaohaohui.github.io/UECM3993/mnist_train.psv
train = read.csv("mnist_train.psv", sep="|", as.is=TRUE, header=FALSE)
train$V1 = factor(train$V1)
#summary(train)
## Controls for plotting
colours = rainbow(length(unique(train$V1)))
names(colours) = unique(train$V1)

#
# Comparing PCA to t-SNE
#
par(mfrow=c(1,2))
pca = prcomp(train[,-1])
# Manual biplot is better
plot(pca$x[,1], pca$x[,2], pch=16, col=colours[train$V1], main="PCA")

library(Rtsne)
tsne = Rtsne(train[,-1], dims=2, perplexity=30, verbose=TRUE, max_iter=500)
plot(tsne$Y, t='n', main="tSNE")
text(tsne$Y, labels=train$V1, col=colours[train$V1])


#-------------------------------------------------------------------------
#  MNIST Data with "Principal Component Classifier = PCA -> Classifier"
#  Ref: https://github.com/mkinsey/digit-classifier
#-------------------------------------------------------------------------
library(MASS)
#https://liaohaohui.github.io/UECM3993/mnist_train.psv
train = read.csv("mnist_train.psv", sep="|", as.is=TRUE, header=FALSE)
# We need to remove those columns where there are less than 4 values
# some of them are alway `white/black' => Feature selection
train = train[,apply(train, 2, function(col) {length(unique(col))>4})]

cat("
* 
* The K-fold CV with LDA takes really long time to calculate.
* You can skip this if your computer is slow with < 8G memory.
*
")
# K-fold CV (K=10 is very slow, K=5 may be faster) on LDA 
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
* The K-fold CV on PCA+kNN takes super long time to calculate.
* You can skip this if your computer is slow with < 8G memory.
*
")
#
# K=5 K-fold CV on PCA (feature selection) + kNN (k=1)
# Data: https://liaohaohui.github.io/UECM3993/mnist_train.psv
#
train = read.csv("mnist_train.psv", sep="|", as.is=TRUE, header=FALSE)
N = nrow(train)
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
  cat("Run", i, ": The PCA + KNN classifier had ", 100*results[i],"% accuracy.\n")
}

