# -------------------------------------------------------------------
# Purpose: Practical for Regression Trees & Regressive Predictive Models in R
# Author : Liew How Hui (2024)
# References: 
#  1. http://faculty.marshall.usc.edu/gareth-james/ISL/Chapter%208%20Lab.txt
#  2. https://rstudio-pubs-static.s3.amazonaws.com/446972_323b4475ff0749228fe4057c4d7685f5.html
#  3. http://www.di.fc.ul.pt/~jpn/r/tree/tree.html
#  4. https://uc-r.github.io/regression_trees
# Data   : http://faculty.marshall.usc.edu/gareth-james/ISL/data.html
# License: BSD-3
# Software: R 4.1+
# Duration: 1 hour
# -------------------------------------------------------------------

#
# Revision:
# Practical 1 & 2
#   => Basic R Data Processing (Data Frame ~ Excel Table)
#   => Alternative: Dplyr technology (using pipeline)
# Practical 3 - 11
#   => All about classification problems (classifiers)
# Practical 12:
#   => Try to explore a little about regression problems
# For classification problems, key performance measures:
# 1. Contingency table / Confusion matrix
# 2. accuracy
# For binary classification (output is binary):
# 3. sensitivity, specificity, PPV, NPV
# 4. ROC / AUC
# 5. Kappa (used in imbalanced data)

# Performance measures for regression problem (output is continuous):
# 1. MSE (Mean Square Error)
# 2. R2
# 3. MAE = Mean Absolute Error (Not working well with differentiation)

#
# MSE = SSE / n => https://en.wikipedia.org/wiki/Mean_squared_error
#
performance.regression.MSE = function(yhat, y){
  mean((y - yhat)^2)
}

#
# R^2 Error => https://en.wikipedia.org/wiki/Coefficient_of_determination
# 1. If the modeled values exactly match the observed values, 
#    SS_res = 0 => R^2 = 1.
# 2. For a baseline model, which always predicts y - mean(y), will 
#    have R^2 = 0. 
# 3. Models that have worse predictions than the baseline will have 
#    a negative R^2.
#
performance.regression.R2  = function(yhat, y){
  SS_res = sum((y-yhat)**2)
  SS_tot = sum((y-mean(y))**2)
  1 - SS_res/SS_tot
}

#-------------------------------------------------------------------------
# Carseats (in the earlier practical, we transform it to `classification')
# A simulated data set containing *sales* of child car seats at 
# 400 different stores
#-------------------------------------------------------------------------
library(ISLR2)
set.seed(2024)
idx = sample(nrow(Carseats), 0.7*nrow(Carseats))
data.train = Carseats[ idx, ]
data.test  = Carseats[-idx, ]

# Keep a record to compare different predictive models
Model.list = c()
Train.list = c()
Test.list  = c()

#
# CART = Classification & Regression Trees
# We have explored the classification capability of CART in Practical 9
# and we are exploring the regression capability in this practical class
#
library(tree)
reg.model  = tree(Sales ~ ., data.train)
# Visual representation
plot(reg.model)
text(reg.model)
# Text representation
print(reg.model)
Model.list = c(Model.list, "tree")
yhat = predict(reg.model, data.train)
# Performance of the training data (theoretically it should be high)
Train.list = c(Train.list, performance.regression.MSE(yhat, data.train$Sales))
yhat = predict(reg.model, data.test)
# Data are sampled from Y = f(X) + Random, E[Random] = 0 (Regression)
# Generalise: Can we capture f(X)?
# Performance of the testing data (theoretically it should be low if overfit)
Test.list  = c(Test.list, performance.regression.MSE(yhat, data.test$Sales))

#
# CART (recursive partitioning)
#
library(rpart)
reg.model  = rpart(Sales ~ ., data.train)
Model.list = c(Model.list, "rpart")
yhat = predict(reg.model, data.train)
Train.list = c(Train.list, performance.regression.MSE(yhat, data.train$Sales))
yhat = predict(reg.model, data.test)
Test.list  = c(Test.list, performance.regression.MSE(yhat, data.test$Sales))

# C5.0 tree only supports classification problems!!!
# C5.0 is an extension of C4.5 which is an extension of ID3 by Quinlan

#install.packages("Cubist")
library(Cubist)  # Extension of Quinlan's M5 tree
reg.model = cubist(x=as.matrix(data.train[,2:11]), y=data.train$Sales)
Model.list = c(Model.list, "cubist")
yhat = predict(reg.model, data.train)
Train.list = c(Train.list, performance.regression.MSE(yhat, data.train$Sales))
yhat = predict(reg.model, data.test)
Test.list  = c(Test.list, performance.regression.MSE(yhat, data.test$Sales))

#
# partikit requires libcoin, mvtnorm, Formula, inum
#
# Bias problem => regression (less in classification)
# CART (based on information theory => Info Gain, Gini Impurity) => more bias
# Conditionally Inference Tree (probability theory) => less bias
#
library(partykit)
reg.model  = ctree(Sales ~ ., data.train)
Model.list = c(Model.list, "ctree")
yhat = predict(reg.model, data.train)
Train.list = c(Train.list, performance.regression.MSE(yhat, data.train$Sales))
yhat = predict(reg.model, data.test)
Test.list  = c(Test.list, performance.regression.MSE(yhat, data.test$Sales))

#
# SLOW: Random Forest of Conditional Inference Trees
#
cat("Processing cforest ...\n")
reg.model  = cforest(Sales ~ ., data.train)    # ntree=500, Very slow
Model.list = c(Model.list, "cforest")
yhat = predict(reg.model, data.train)
Train.list = c(Train.list, performance.regression.MSE(yhat, data.train$Sales))
yhat = predict(reg.model, data.test)
Test.list  = c(Test.list, performance.regression.MSE(yhat, data.test$Sales))

# Usual Random Forest with CARTs (Gini Impurity, Info Gain)
cat("Processing randomForest ...\n")
library(randomForest)
reg.model  = randomForest(Sales ~ ., data.train)  # ntree=500
Model.list = c(Model.list, "randomForest")
yhat = predict(reg.model, data.train)
Train.list = c(Train.list, performance.regression.MSE(yhat, data.train$Sales))
yhat = predict(reg.model, data.test)
Test.list  = c(Test.list, performance.regression.MSE(yhat, data.test$Sales))

cat("Processing Gradient Boosting Trees ...\n")
library(gbm)
reg.model  = gbm(Sales ~ ., "gaussian", data=data.train)  # n.trees = 100
Model.list = c(Model.list, "gbm")
yhat = predict(reg.model, data.train)
Train.list = c(Train.list, performance.regression.MSE(yhat, data.train$Sales))
yhat = predict(reg.model, data.test)
Test.list  = c(Test.list, performance.regression.MSE(yhat, data.test$Sales))

# Multivariate Linear Regression Models
lr = lm(Sales ~ ., data.train)   # categorical => one-hot encoded
Model.list = c(Model.list, "linear regression")
yhat = predict(lr, data.train)
Train.list = c(Train.list, performance.regression.MSE(yhat, data.train$Sales))
yhat = predict(lr, data.test)
Test.list  = c(Test.list, performance.regression.MSE(yhat, data.test$Sales))

print(data.frame(
  tree.model = Model.list,
  train.MSE  = Train.list,
  test.MSE   = Test.list
))

#
# Hyperparameter Tuning for Trees: Pruning
#
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
#dev.new()
plot(cv.boston$size,cv.boston$dev,type='b')
#
# Cross Validation says that unpruned tree is the best
#
#dev.new()
#
# Try to prune to 5 leaves to see why less leaves is worse
#
prune.boston=prune.tree(tree.boston,best=5)
plot(prune.boston)
text(prune.boston,pretty=0)

yhat=predict(tree.boston,newdata=Boston[-train.idx,])
boston.test=Boston[-train.idx,"medv"]
cat("MSE =", performance.regression.MSE(yhat, boston.test), "\n")
cat("R^2 =", performance.regression.R2(yhat, boston.test), "\n")
plot(yhat,boston.test)
abline(0,1)

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
dev.new()
plot(real.estate$Longitude, real.estate$Latitude, col=grey(10:2/11)[cut.prices], pch=20, xlab="Longitude",ylab="Latitude")
partition.tree(tree.model, ordvars=c("Longitude","Latitude"), add=TRUE)

summary(tree.model)
# Deviance means here the mean squared error.

# Tune the number of leaves of the tree model
tree.model2 = tree(log(MedianHouseValue) ~ Longitude + Latitude, data=real.estate, mindev=0.001)
plot(tree.model2)
text(tree.model2, cex=.75)
dev.new()
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
#X = seq(Xbeg,Xend,length.out=NS)
# Random input
set.seed(2024)
X = Xbeg + runif(NS,min=0,max=Xend)
# Output without noise
y = sin(X)
### Output with noise ~ N(0, sig^2)
sig = 0.2
y = y + rnorm(NS,mean=0,sd=sig)

# Theoretical values of the 'deterministic model' f(X)
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
#dev.new()
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

