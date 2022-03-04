# -------------------------------------------------------------------
# Purpose: Practical for Regression Tree Based Predictive Models in R
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

library(tree)  # CART = Classification & Regression Trees

#
# Carseats (in the previous part, we transform it to `classification')
#
Carseats = ISLR::Carseats
set.seed(2022)
idx = sample(nrow(Carseats), 0.7*nrow(Carseats))
data.train = Carseats[ idx, ]
data.test  = Carseats[-idx, ]
reg.model  = tree(Sales ~ ., data.train)
plot(reg.model)
text(reg.model)
# Performance measure for regression problem is "RSS"
yhat = predict(reg.model, data.test)
RSS = mean((data.test$Sales - yhat)^2)
#plot(yhat, data.test$Sales, xlim=c(0,14))   or comparing output?

cat("
# -------------------------------------------------------------------
#    Analysis of the `Boston' Dataset with Regression Tree
# -------------------------------------------------------------------
")
library(MASS)  # Provides the Boston data
sapply(Boston,class)
set.seed(1)
train.idx = sample(nrow(Boston), nrow(Boston)/2)

tree.boston = tree(medv~.,Boston[train.idx, ])  # Alternative: subset=train.idx
summary(tree.boston)
plot(tree.boston)
text(tree.boston,pretty=0)

cv.boston=cv.tree(tree.boston)
dev.new()
plot(cv.boston$size,cv.boston$dev,type='b')
prune.boston=prune.tree(tree.boston,best=5)
plot(prune.boston)
text(prune.boston,pretty=0)

yhat=predict(tree.boston,newdata=Boston[-train.idx,])
boston.test=Boston[-train.idx,"medv"]  # Actual response
plot(yhat,boston.test)
abline(0,1)
# Performance measure for regression problem -> RSS, R2, etc.
cat("RSS =", mean((yhat-boston.test)^2), "\n")
# RSS = Residue Sum of Squares = Mean Square Errors (MSE)

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


