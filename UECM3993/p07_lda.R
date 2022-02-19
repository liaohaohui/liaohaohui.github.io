# -------------------------------------------------------------------
# Purpose: Practical for LDA & QDA Models in R
# Author : Liew How Hui (2022)
# Reference: 
#  1. http://faculty.marshall.usc.edu/gareth-james/ISL/Chapter%204%20Lab.txt
#  2. http://euler.stat.yale.edu/~tba3/stat665/lectures/lec11/script11.html
# Data   : http://faculty.marshall.usc.edu/gareth-james/ISL/data.html
# License: BSD-3
# Software: R 3.6 & R 4.x & install.packages(c("ISLR", "FNN"))
# Duration: 1 hour
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# Taken from p03_knn1.R (Only works properly for binary classification)
# -------------------------------------------------------------------
performance = function(xtab, desc=""){
    cat(desc,"\n")
    ACR = sum(diag(xtab))/sum(xtab)
    TPR = xtab[1,1]/sum(xtab[,1]); TNR = xtab[2,2]/sum(xtab[,2])
    PPV = xtab[1,1]/sum(xtab[1,]); NPV = xtab[2,2]/sum(xtab[2,])
    FPR = 1 - TNR                ; FNR = 1 - TPR
    # https://standardwisdom.com/softwarejournal/2011/12/confusion-matrix-another-single-value-metric-kappa-statistic/
    RandomAccuracy = (sum(xtab[,2])*sum(xtab[2,]) + 
      sum(xtab[,1])*sum(xtab[1,]))/(sum(xtab)^2)
    Kappa = (ACR - RandomAccuracy)/(1 - RandomAccuracy)
    print(xtab)
    cat("\n      Accuracy :", ACR)
    if(nrow(xtab)==2){
    cat("\n\n         Kappa :", Kappa, "\n")
    cat("\n   Sensitivity :", TPR,   "\n   Specificity :", TNR, "\n")
    cat("Pos Pred Value :", PPV,     "\nNeg Pred Value :", NPV, "\n")
    cat("           FPR :", FPR,     "\n           FNR :", FNR, "\n")
    }else{
    cat("\n\nTable is ", dim(xtab), " other performance metrics are unavailable\n")
    }
}

#
# LDA & QDA `only' work PROPERLY with `numeric' features
#

# -------------------------------------------------------------------
#  Analysis of the `Smarket' Dataset with LDA & QDA Classifier
# -------------------------------------------------------------------
library(ISLR)

# Split data into train set and validation set
# train set = data from Year 2001-2004
# validation set = data from Year 2005
train = (Smarket$Year < 2005)
Smarket.2005 = Smarket[!train,]
Direction.2005 = Smarket$Direction[!train]

library(MASS)  # supports LDA & QDA
#lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket[train,])
lda.fit

# Make prediction based on LDA model
yhat = predict(lda.fit,Smarket.2005)
#names(yhat)
# yhat$posterior -> posterior `probability'
cfmat = table(yhat$class, Direction.2005)
performance(cfmat, "Smarket data analysis with LDA")

qda.fit=qda(Direction~Lag1+Lag2,data=Smarket,subset=train)
qda.fit
qda.class=predict(qda.fit,Smarket.2005)$class
cfmat = table(qda.class,Direction.2005)
performance(cfmat, "Smarket data analysis with QDA")

# -------------------------------------------------------------------
#  Analysis of the `Fraud' Dataset with LDA Classifier ???
# -------------------------------------------------------------------

# Impossible due to the data is not normally distributed ->
# many are factors & integers

# Or we can try using kNN's approach?

# -------------------------------------------------------------------
#  Analysis of the `Weekly' Dataset with LDA Classifier
# -------------------------------------------------------------------
# Q2(a)
train = (Weekly$Year < 2009)
Weekly.0910 = Weekly[!train, ]   # 2009 & 2010
Direction.0910 = Weekly$Direction[!train]
#lda.fit = lda(Direction ~ Lag2, data=Weekly[train,])
#lda.fit = lda(Direction ~ . - Year, data=Weekly[train,])
lda.fit = lda(Direction ~ . - Today - Year, data=Weekly[train,])

# Q2(b)
lda.pred = predict(lda.fit, Weekly.0910)
cfmat = table(lda.pred$class, Direction.0910)
performance(cfmat, "Weekly data analysis using LDA")

# Q2(c): We can compare to kNN, LR or decision tree.
library(class)
# Lag2 = column 3; Direction = column 9
#yhat = knn(as.matrix(Weekly[train,3]), as.matrix(Weekly.0910[,3]), Weekly[train,9]) #k=1
#yhat = knn(as.matrix(Weekly[train,2:8]), as.matrix(Weekly.0910[,2:8]), Weekly[train,9]) #k=1
yhat = knn(as.matrix(Weekly[train,2:7]), as.matrix(Weekly.0910[,2:7]), Weekly[train,9]) #k=1
cfmat = table(yhat, Direction.0910)
performance(cfmat, "Weekly data analysis using kNN(k=1)")

# Q2(d) For the Weekly Data, there is no best result when using Lag2
# Q2(d) For the Weekly Data, LDA performs better than kNN (k=1)
# maybe due to the data fits the multivariate normal distribution

# -------------------------------------------------------------------
#  Analysis of the `Auto' Dataset with LDA Classifier
# -------------------------------------------------------------------
# Q3(a)
#library(ISLR)
# New Target for classification: Feul efficient
Auto$mpg01 = factor(ifelse(Auto$mpg > median(Auto$mpg), 1, 0))

# Q3(b): Split train:test -> 70%:30%
#set.seed(1)
#set.seed(100)
set.seed(101)
n = nrow(Auto)
train.idx = sample(n, size=round(n*0.7))
# library(caTools)
#train.idx = sample.split(Auto$mpg01, SplitRatio=0.7)

# year, name, origin, acceleration from columns 6:9 should not contribute
# to efficiency of a car, so we remove them
# We also don't need mpg since we created `mpg01' (categorical data)
X_y.train = Auto[ train.idx, -c(1, 6:9)]  # removing columns 1,6,7,8,9
X_y.test  = Auto[-train.idx, -c(1, 6:9)]  # column 1 = mpg (Original)

# Q3(c): I will try with LDA and kNN only, you can try others.
#library(MASS)
lda.fit  = lda(mpg01~., data=X_y.train)  # 2:5 are all numeric columns
lda.pred = predict(lda.fit, X_y.test)
cf.mat = table(lda.pred$class, X_y.test$mpg01)
performance(cf.mat, "\nAuto Data with LDA")

library(class)
for (k in c(1,10,100)){
  # Column 3 = Lag2, Column 9 = Direction
  knn.pred = knn(X_y.train[,1:4], X_y.test[,1:4], X_y.train$mpg01, k=k)
  cf.mat.knn = table(knn.pred, X_y.test$mpg01)
  print(cf.mat.knn)
  accuracy = sum(diag(cf.mat.knn))/sum(cf.mat.knn)
  cat("Accuracy (kNN, k=",k,") = ",accuracy,"\n",sep="")
}

# Conclusion: LDA and kNN (larger k) seem to be good classifiers
# because the data is linear classifiable.  But LDA is better
# because kNN will fit ``noise'' as data and predict slight poorer.


# -------------------------------------------------------------------
#  Analysis of the `MNIST' Dataset with LDA Classifier
# -------------------------------------------------------------------

# Download mnist_train.csv (~7M) & mnist_test.csv (~2M) from
#  https://github.com/statsmaths/stat665/find/gh-pages

#https://liaohaohui.github.io/UECM3993/mnist_train.psv
train = read.csv("mnist_train.psv", sep="|", as.is=TRUE, header=FALSE)
#https://liaohaohui.github.io/UECM3993/mnist_test.psv
test  = read.csv("mnist_test.psv",  sep="|", as.is=TRUE, header=FALSE)
dim(train)  # ncol(train) = 1 + 16^2
# To look at ONE ROW.  Note: First column is the `label'.
X = matrix(as.matrix(train[3400,-1]),16,16,byrow=TRUE)
X = 1 - (X + 1)*0.5
plot(0,0)
rasterImage(X,-1,-1,1,1)

iset <- sample(nrow(train),5*7)   # Take 35 samples from training data
par(mar=c(0,0,0,0))
par(mfrow=c(5,7))
for (j in iset) {
  y = matrix(as.matrix(train[j,-1]),16,16,byrow=TRUE)
  y = 1 - (y + 1)*0.5
  plot(0,0,xlab="",ylab="",axes=FALSE)
  rasterImage(y,-1,-1,1,1)
  box()
  text(-0.8,-0.7, train[j,1], cex=3, col="red")
}

ytest  = test[,1]

lda.fit = lda(V1 ~ ., train)
yhat = predict(lda.fit, test)
cfmat = table(yhat$class, ytest)
performance(cfmat, "MNIST performance with LDA")

# qda? --- not working --- requires filtering of features
# qda.fit = qda(V1 ~ ., train)

# Compare to other models
#predKnn = FNN::knn(Xtrain,Xtest,ytrain,k=3)  # quite slow
library(class)
# Splitting the Data (X,y) to X and y seperately.
Xtrain = as.matrix(train[,-1])
Xtest  = as.matrix(test[,-1])
ytrain = train[,1]
predKnn = knn(Xtrain,Xtest,ytrain,k=3)  # quite slow
performance(table(predKnn, ytest), "\nkNN with k=3")

#library(glmnet)
## super-slow: more than 5 minutes?
#outLm  = cv.glmnet(Xtrain, ytrain, alpha=0, nfolds=3, family="multinomial")
#predLm = apply(predict(outLm, Xtest, s=outLm$lambda.min,
#                  type="response"), 1, which.max) - 1L
#performance(table(predLm, ytest), "\nRidge Regression")

#
# Ensemble methods
#
## Random Forest is also slow but it should finish in 2 minutes. Bad performance
#outRf = randomForest::randomForest(Xtrain, factor(ytrain), maxnodes=10)
#outRf20 = randomForest::randomForest(Xtrain, factor(ytrain), maxnodes=20) => 85%
#predRf = predict(outRf, Xtest)
#performance(table(predRf, ytest), "\nMNIST with Random Forest")

#---------------------------------------------------------------
# Do not run the following three lines if your computer is slow
# The `SVM' predictive model is very very very slow.
#---------------------------------------------------------------

#outSvm  = e1071::svm(Xtrain,  factor(ytrain), kernel="radial", cost=1)
#predSvm = predict(outSvm, Xtest)
#performance(table(predSvm, ytest), "\nMNIST with SVM")

