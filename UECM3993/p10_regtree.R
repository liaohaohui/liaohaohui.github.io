# -------------------------------------------------------------------
# Purpose: Practical for Regression Trees & Regressive Predictive Models in R
# Author : Liew How Hui (2022)
# References: 
#  1. http://faculty.marshall.usc.edu/gareth-james/ISL/Chapter%208%20Lab.txt
#  2. https://rstudio-pubs-static.s3.amazonaws.com/446972_323b4475ff0749228fe4057c4d7685f5.html
#  3. http://www.di.fc.ul.pt/~jpn/r/tree/tree.html
#  4. https://uc-r.github.io/regression_trees
# Data   : http://faculty.marshall.usc.edu/gareth-james/ISL/data.html
# License: BSD-3
# Software: R 3.6 & R 4.x
# Duration: 1 hour
# -------------------------------------------------------------------

# Performance measure for regression problem -> RSS, R2, etc.

# Residue Sum of Squares or Mean Square Error
performance.regression.RSS = function(y, yhat){
  mean((y - yhat)^2)
}
# R^2 Error
performance.regression.R2 = function(y, yhat){
  cor(y, yhat)^2
}

#-------------------------------------------------------------------------
# Carseats (in the earlier practical, we transform it to `classification')
#-------------------------------------------------------------------------
library(ISLR)
set.seed(2022)
idx = sample(nrow(Carseats), 0.7*nrow(Carseats))
data.train = Carseats[ idx, ]
data.test  = Carseats[-idx, ]

Model.list = c()
Train.list = c()
Test.list  = c()

library(tree)   # CART = Classification & Regression Trees
reg.model  = tree(Sales ~ ., data.train)
plot(reg.model)
text(reg.model)
Model.list = c(Model.list, "tree")
yhat = predict(reg.model, data.train)
Train.list = c(Train.list, performance.regression.RSS(data.train$Sales, yhat))
yhat = predict(reg.model, data.test)
Test.list  = c(Test.list, performance.regression.RSS(data.test$Sales, yhat))

library(rpart)   # CART
reg.model  = rpart(Sales ~ ., data.train)
Model.list = c(Model.list, "rpart")
yhat = predict(reg.model, data.train)
Train.list = c(Train.list, performance.regression.RSS(data.train$Sales, yhat))
yhat = predict(reg.model, data.test)
Test.list  = c(Test.list, performance.regression.RSS(data.test$Sales, yhat))

# C5.0 tree only supports classification problems!!!

library(Cubist)  # Extension of Quinlan's M5 tree
reg.model = cubist(x=as.matrix(data.train[,2:11]), y=data.train$Sales)
Model.list = c(Model.list, "cubist")
yhat = predict(reg.model, data.train)
Train.list = c(Train.list, performance.regression.RSS(data.train$Sales, yhat))
yhat = predict(reg.model, data.test)
Test.list  = c(Test.list, performance.regression.RSS(data.test$Sales, yhat))

library(partykit)
reg.model  = ctree(Sales ~ ., data.train)   # Conditionally Inference Tree
Model.list = c(Model.list, "ctree")
yhat = predict(reg.model, data.train)
Train.list = c(Train.list, performance.regression.RSS(data.train$Sales, yhat))
yhat = predict(reg.model, data.test)
Test.list  = c(Test.list, performance.regression.RSS(data.test$Sales, yhat))

cat("Processing cforest ...\n")
reg.model  = cforest(Sales ~ ., data.train)    # Very slow
Model.list = c(Model.list, "cforest")
yhat = predict(reg.model, data.train)
Train.list = c(Train.list, performance.regression.RSS(data.train$Sales, yhat))
yhat = predict(reg.model, data.test)
Test.list  = c(Test.list, performance.regression.RSS(data.test$Sales, yhat))

cat("Processing randomForest ...\n")
library(randomForest)
reg.model  = randomForest(Sales ~ ., data.train)
Model.list = c(Model.list, "randomForest")
yhat = predict(reg.model, data.train)
Train.list = c(Train.list, performance.regression.RSS(data.train$Sales, yhat))
yhat = predict(reg.model, data.test)
Test.list  = c(Test.list, performance.regression.RSS(data.test$Sales, yhat))

cat("Processing Gradient Boosting Trees ...\n")
library(gbm)
reg.model  = gbm(Sales ~ ., "gaussian", data=data.train)  # n.tree = 100
Model.list = c(Model.list, "gbm")
yhat = predict(reg.model, data.train)
Train.list = c(Train.list, performance.regression.RSS(data.train$Sales, yhat))
yhat = predict(reg.model, data.test)
Test.list  = c(Test.list, performance.regression.RSS(data.test$Sales, yhat))

print(data.frame(tree.model=Model.list,
  train.RSS = Train.list, test.RSS = Test.list
))

cat("
# -------------------------------------------------------------------
#    Analysis of the `Boston' Dataset with Regression Tree
# -------------------------------------------------------------------
")
library(MASS)  # Provides the Boston data
sapply(Boston,class)
set.seed(1)
train.idx = sample(nrow(Boston), nrow(Boston)/2)

tree.boston = tree(medv~.,Boston[train.idx, ])
summary(tree.boston)
plot(tree.boston)
text(tree.boston,pretty=0)

#
# Unpruned tree has 7 leaves
#
cv.boston=cv.tree(tree.boston)  # K = 10 (default) fold & pruning
dev.new()
plot(cv.boston$size,cv.boston$dev,type='b')
#
# Cross Validation says that unpruned tree is the best
#
dev.new()
#
# Try to prune to 5 leaves to see why less leaves is worse
#
prune.boston=prune.tree(tree.boston,best=5)
plot(prune.boston)
text(prune.boston,pretty=0)

yhat=predict(tree.boston,newdata=Boston[-train.idx,])
boston.test=Boston[-train.idx,"medv"]
plot(yhat,boston.test)
abline(0,1)
cat("RSS =", performance.regression.RSS(yhat, boston.test), "\n")
cat("R^2 =", performance.regression.R2(yhat, boston.test), "\n")

cat("
# -------------------------------------------------------------------
#    Analysis of the `California Real Estate' Dataset with Regression Tree
# -------------------------------------------------------------------
")
# Original Source: https://raw.githubusercontent.com/jbryer/CompStats/master/Data/cadata.dat
#https://liaohaohui.github.io/UECM3993/cadata.dat
real.estate = read.table("cadata.dat", header=TRUE)
tree.model = tree(log(MedianHouseValue) ~ Longitude + Latitude, data=real.estate)
plot(tree.model)
text(tree.model, cex=.75)

# We can compare the predictions with the dataset (darker is more expensive)
# which seem to capture the global price trend:
price.deciles = quantile(real.estate$MedianHouseValue, 0:10/10)
cut.prices    = cut(real.estate$MedianHouseValue, price.deciles, include.lowest=TRUE)
plot(real.estate$Longitude, real.estate$Latitude, col=grey(10:2/11)[cut.prices], pch=20, xlab="Longitude",ylab="Latitude")
partition.tree(tree.model, ordvars=c("Longitude","Latitude"), add=TRUE)

summary(tree.model)
# Deviance means here the mean squared error.

# Tune the number of leaves of the tree model
tree.model2 = tree(log(MedianHouseValue) ~ Longitude + Latitude, data=real.estate, mindev=0.001)
plot(tree.model2)
text(tree.model2, cex=.75)
plot(real.estate$Longitude, real.estate$Latitude, col=grey(10:2/11)[cut.prices], pch=20, xlab="Longitude",ylab="Latitude")
partition.tree(tree.model2, ordvars=c("Longitude","Latitude"), add=TRUE)

summary(tree.model2)

# -------------------------------------------------------------------
#  Example from Week 1 Slide => 1D Regression
# -------------------------------------------------------------------
NS = 30   # Number of data samples
Xbeg = 0
Xend = pi
#Xbeg = -pi
#Xend = 2*pi
# Deterministic input
X = seq(Xbeg,Xend,length.out=NS)
# Random input
set.seed(2022)
X = Xbeg + runif(NS,min=0,max=Xend)
# Output without noise
y = sin(X)
### Output with noise ~ N(0, sig^2)
sig = 0.2
y = y + rnorm(NS,mean=0,sd=sig)

Nx = 200
Xx = seq(Xbeg,Xend,length.out=Nx)
yy = sin(Xx)

original.data = function(s) {
	plot(X,y,pch='+',col=2,cex=3,main=s,xlim=c(Xbeg,Xend),ylim=c(-1.2,1.2))
	lines(Xx,yy,lwd=1.5)
}

# Global line wide for models
glw = 6

#-------------------------------------------------------------------------
#  Linear Regression
#-------------------------------------------------------------------------

# random input data above
d.f.train = data.frame(X = X, y = y)

model = lm(y ~ X, d.f.train)
predictions = predict(model, new=data.frame(X = Xx), type="response")
par(mfrow=c(1,3))
original.data("linear regression")
lines(Xx, predictions, pch='.', col=3, lwd=glw)

#-------------------------------------------------------------------------
#  Polynomial Regression (???)
#-------------------------------------------------------------------------

# Quadratic
model = lm(y ~ poly(X,2), d.f.train)
predictions = predict(model, new=data.frame(X = Xx), type="response")
original.data("quadratic regression")
lines(Xx, predictions, pch='.', col=4, lwd=glw)

# Cubic
model = lm(y ~ poly(X,3), d.f.train)
predictions = predict(model, new=data.frame(X = Xx), type="response")
original.data("cubic regression")
lines(Xx, predictions, pch='.', col=5, lwd=glw)

#-------------------------------------------------------------------------
#  kNN
#-------------------------------------------------------------------------

# Using sklearn
library(FNN)
knnk = 5
predictions = knn.reg(d.f.train$X, test=data.frame(X=Xx), y=d.f.train$y, k=knnk)$pred
dev.new()
par(mfrow=c(1,3))
original.data(paste("kNN regression", paste("(k=", knnk, ")", se="")))
lines(Xx, predictions, pch='.', col=5, lwd=glw)

#-------------------------------------------------------------------------
#  Neural Network Regressor
#-------------------------------------------------------------------------
library(neuralnet)
model = neuralnet(y ~ ., data=d.f.train, hidden=c(10))
print(model)
predictions = predict(model,newdata=data.frame(X=Xx))
original.data("1L(10) neural net")
lines(Xx, predictions, pch='.', col=6, lwd=glw)


#-------------------------------------------------------------------------
#  Regression Tree
#-------------------------------------------------------------------------

model = tree(y ~ ., d.f.train)
predictions = predict(model, data.frame(X=Xx))
original.data("tree regression")
lines(Xx, predictions, pch='.', col=6, lwd=glw)



