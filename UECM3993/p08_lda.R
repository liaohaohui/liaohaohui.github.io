# -------------------------------------------------------------------
# Purpose: Application using Statistical Software (Part 3)
# Detail: Case study 2: Segmentation with Discriminant Analysis
# Author : Liew How Hui (2026)
# Reference: 
#  1. http://faculty.marshall.usc.edu/gareth-james/ISL/Chapter%204%20Lab.txt
#  2. http://euler.stat.yale.edu/~tba3/stat665/lectures/lec11/script11.html
# Data   : http://faculty.marshall.usc.edu/gareth-james/ISL/data.html
# License: BSD-3
# Software: R 4.x & install.packages("ISLR2")
# Duration: 1 hour
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# performance() is taken from p03_knn1.R with minor adjustment
# -------------------------------------------------------------------

performance = function(xtab, desc="", digits=7){
    cat("\n", desc,"\n", sep="")
    print(xtab)

    ACR = sum(diag(xtab))/sum(xtab)
    CI  = binom.test(sum(diag(xtab)), sum(xtab))$conf.int
    cat("\n        Accuracy :", ACR)
    cat("\n          95% CI : (", CI[1], ",", CI[2], ")\n")

    if(nrow(xtab)>2){
        # e1071's classAgreement() in matchClasses.R
        # Ref: https://stats.stackexchange.com/questions/586342/measures-to-compare-classification-partitions
        n  = sum(xtab)
        ni = apply(xtab, 1, sum)
        nj = apply(xtab, 2, sum)
        p0 = sum(diag(xtab))/n
        pc = sum(ni * nj)/n^2
        Kappa = (p0 - pc)/(1 - pc)
        cat("\n           Kappa :", Kappa, "\n")
        cat("\nStatistics by Class:\n")
        # Levels of the actual data
        lvls = dimnames(xtab)[[2]]
        sensitivity = c()
        specificity = c()
        ppv         = c()
        npv         = c()
        for(i in 1:length(lvls)) {
            sensitivity[i] = xtab[i,i]/sum(xtab[,i])
            specificity[i] = sum(xtab[-i,-i])/sum(xtab[,-i])
            ppv[i]         = xtab[i,i]/sum(xtab[i,])
            npv[i]         = sum(xtab[-i,-i])/sum(xtab[-i,])
        }
        b = data.frame(rbind(sensitivity,specificity,ppv,npv))
        names(b) = lvls
        print(round(b,digits))
    } else {
         #names(dimnames(xtab)) = c("Prediction", "Actual")
         TPR = xtab[1,1]/sum(xtab[,1]); TNR = xtab[2,2]/sum(xtab[,2])
         PPV = xtab[1,1]/sum(xtab[1,]); NPV = xtab[2,2]/sum(xtab[2,])
         FPR = 1 - TNR                ; FNR = 1 - TPR
         # https://standardwisdom.com/softwarejournal/2011/12/confusion-matrix-another-single-value-metric-kappa-statistic/
         RandomAccuracy = (sum(xtab[,2])*sum(xtab[2,]) + 
           sum(xtab[,1])*sum(xtab[1,]))/(sum(xtab)^2)
         Kappa = (ACR - RandomAccuracy)/(1 - RandomAccuracy)
         cat("\n           Kappa :", Kappa, "\n")
         cat("\n     Sensitivity :", TPR)
         cat("\n     Specificity :", TNR)
         cat("\n  Pos Pred Value :", PPV)
         cat("\n  Neg Pred Value :", NPV)
         cat("\n             FPR :", FPR)
         cat("\n             FNR :", FNR, "\n")
         cat("\n'Positive' Class :", dimnames(xtab)[[1]][1], "\n")
    }
}


#
# LDA & QDA assumes the input data (X1,X2,...,Xp) needs to be NUMERIC 
# and the data is segmented using multivariate NORMAL distribution
#

# -------------------------------------------------------------------
#   Case Study 2 : Segment the `Smarket' Dataset with LDA & QDA Classifiers
#   following the main reference book
# -------------------------------------------------------------------

library(ISLR2)

### 
### Holdout Method on Smarket data (refer to Practical 3)
### 
train = (Smarket$Year < 2005)
Smarket.test = Smarket[!train,]
Direction.test = Smarket.test$Direction

#
# Unlike Practical 3, we consider all columns except Today
# because Direction is obtained from Today.
#
# We can check this using
# 
# table(ifelse(Smarket$Today>=0,"Up","Down"), Smarket$Direction)
#
# If we use Today, we are cheating, rather than predicting.
#
library(MASS)
lda.fit=lda(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket[train,])
### The matrix of the LDA model is not shown but rather a summary of it
print(lda.fit)

#
# For binary classification, the "projection" plot is interesting
# and we can compare it to the Simlpe Bivariate Analysis.
#
# We just show the Simlpe Bivariate Analysis on the Lag1 column,
# the rest will be left as exercise.
#
par(mfrow=c(2,1))
attach(Smarket)
h1 = hist(Lag1[Direction=="Up"], plot=F)
h2 = hist(Lag1[Direction=="Down"], plot=F)
plot(h1,col="green",ylim=c(0,250))
plot(h2,col=rgb(1,0,0,0.5),add=TRUE)

plot(Lag1~Direction, Smarket)
detach(Smarket)

#
# Make prediction using the trained LDA model
#
yhat = predict(lda.fit,Smarket.test)
# yhat$posterior -> posterior `probability'
cfmat = table(yhat$class, Direction.test)
performance(cfmat, "Smarket data analysis with LDA")

qda.fit=qda(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket[train,])
### The matrix of the QDA model is not shown at all
qda.fit
qda.class=predict(qda.fit,Smarket.test)$class
cfmat = table(qda.class,Direction.test)
performance(cfmat, "Smarket data analysis with QDA")


# -------------------------------------------------------------------
#   Case Study 2 : Segment of the `Fraud' Dataset with LDA Classifier
#   assuming the categorical data are encoded as integers.
# -------------------------------------------------------------------
#https://liaohaohui.github.io/UECM3993/fraud.csv
fraud = read.csv("fraud.csv", row.names=1)
fraud$tag = factor(fraud$tag)

# EDA
par(mfrow=c(2,4))
header = names(fraud)
for(i in 1:7){
   hist(fraud[,i], main=header[i])
}

#
# Holdout method on Fraud Data as in Practical 3
#
set.seed(123)
fraud_tag0 = fraud[fraud$tag=="0", ]
fraud_tag1 = fraud[fraud$tag=="1", ]
tag0_idx = sample(nrow(fraud_tag0), size=round(0.7*nrow(fraud_tag0)))
tag1_idx = sample(nrow(fraud_tag1), size=round(0.7*nrow(fraud_tag1)))
fraud.train = rbind(fraud_tag0[ tag0_idx,], fraud_tag1[ tag1_idx,])
fraud.test  = rbind(fraud_tag0[-tag0_idx,], fraud_tag1[-tag1_idx,])
normalise.vec <- function(column,ref.col) {
    return ((column - mean(ref.col)) / sd(ref.col))
}
fraud.train.knn     = fraud.train
fraud.test.knn      = fraud.test
fraud.train.knn$age = normalise.vec(fraud.train.knn$age,fraud.train$age)
fraud.test.knn$age  = normalise.vec(fraud.test.knn$age, fraud.train$age)
fraud.train.knn$base_value = normalise.vec(
    fraud.train.knn$base_value,fraud.train$base_value)
fraud.test.knn$base_value  = normalise.vec(
    fraud.test.knn$base_value, fraud.train$base_value)

#
# Training the LDA model
#
lda.fit = lda(tag ~ ., fraud.train.knn)
#
# Exercise: Looking at the trained LDA model
#

yhat = predict(lda.fit, fraud.test.knn)$class
cfmat = table(yhat, fraud.test.knn$tag)
performance(cfmat, "LDA")

#
# Training the QDA model
#
qda.fit = qda(tag ~ ., fraud.train.knn)

yhat = predict(qda.fit, fraud.test.knn)$class
cfmat = table(yhat, fraud.test.knn$tag)
performance(cfmat, "QDA")

#
# Comparing LDA to Logistic Regression because both are linear models
#
lr.fit = glm(tag ~ ., fraud.train.knn, family=binomial)

prob = predict(lr.fit, fraud.test.knn, type="response")
yhat = ifelse(prob>=0.5, 1, 0)
cfmat = table(yhat, fraud.test.knn$tag)
performance(cfmat, "LR (with numeric input)")



# -------------------------------------------------------------------
#   Case Study 2 : Segment the `Auto' Dataset from ISLR2 with 
#   LDA Classifier and comparing it with kNN (which requires
#   input to be all numeric)
# -------------------------------------------------------------------
# New Target for classification: Petrol efficiency
Auto$mpg01 = factor(ifelse(Auto$mpg > median(Auto$mpg), 1, 0))

#
# EDA's correlation analysis
#
#cor(Auto$mpg, Auto$acceleration)  # Not really correlated

# Q3(b): Split train:test -> 70%:30%
#set.seed(1)
#set.seed(100)
set.seed(101)
n = nrow(Auto)
#
# Holdout method: Train-Test split (70% vs 30%)
#
train.idx = sample(n, size=round(n*0.7))
# library(caTools)
#train.idx = sample.split(Auto$mpg01, SplitRatio=0.7)

# year, name, origin, acceleration from columns 6:9 should not contribute
# to efficiency of a car, so we remove them
# We also don't need mpg since we created `mpg01' (categorical data)
#toremove.columns = c(1, 6:9)
toremove.columns = c(1, 7:9)
X_y.train = Auto[ train.idx, -toremove.columns]  # removing columns 1,6,7,8,9
X_y.test  = Auto[-train.idx, -toremove.columns]  # column 1 = mpg (Original)

# Q3(c): I will try with LDA and kNN only, you can try others.
#library(MASS)
lda.fit  = lda(mpg01~., data=X_y.train)
lda.pred = predict(lda.fit, X_y.test)
cf.mat = table(lda.pred$class, X_y.test$mpg01)
performance(cf.mat, "\nAuto Data with LDA")

qda.fit  = qda(mpg01~., data=X_y.train)
qda.pred = predict(qda.fit, X_y.test)
cf.mat = table(qda.pred$class, X_y.test$mpg01)
performance(cf.mat, "\nAuto Data with QDA")


#
# Comparing LDA to kNN with various k
#
library(class)
for (k in c(1,5, 10,20,100)){
  # Column 3 = Lag2, Column 9 = Direction
  p = ncol(X_y.train)-1
  knn.pred = knn(X_y.train[,1:p], X_y.test[,1:p], X_y.train$mpg01, k=k)
  cf.mat.knn = table(knn.pred, X_y.test$mpg01)
  print(cf.mat.knn)
  accuracy = sum(diag(cf.mat.knn))/sum(cf.mat.knn)
  cat("Accuracy (kNN, k=",k,") = ",accuracy,"\n",sep="")
}

#
# Conclusion: LDA and kNN (larger k) seem to be good classifiers
# because the data is linear classifiable.  But LDA is better
# because kNN will fit ``noise'' as data and the prediction is 
# slightly poorer.
#

# -------------------------------------------------------------------
#  Case Study 2 : Segment the `MNIST' Dataset with LDA Classifier
#  (and other supervised learning models).
#  `MNIST' Dataset is a beginner's image recognition example.
#  NOTE: the TIME for training/computation is MOSTLY VERY VERY LONG
#
#  The MNIST data mnist_train.psv (~7M) & mnist_test.psv (~2M) are
#  obtained from https://github.com/statsmaths/stat665/find/gh-pages
# -------------------------------------------------------------------

#https://liaohaohui.github.io/UECM3993/mnist_train.psv
train = read.csv("mnist_train.psv", sep="|", as.is=TRUE, header=FALSE)
#https://liaohaohui.github.io/UECM3993/mnist_test.psv
test  = read.csv("mnist_test.psv",  sep="|", as.is=TRUE, header=FALSE)
dim(train)  # ncol(train) = 1 + 16x16
#
# Each row is an image with a label at Column 1
# To look at ONE ROW.  Note: First column is the `label'.
#
par(mfrow=c(1,1))
X = matrix(as.matrix(train[3400,-1]),16,16,byrow=TRUE)
X = 1 - (X + 1)*0.5
plot(0,0)
rasterImage(X,-1,-1,1,1)

set.seed(2026)
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

#
# First column (V1) is the label of the MNIST images
#
ytest  = test[,1]

lda.fit = lda(V1 ~ ., train)
yhat = predict(lda.fit, test)
cfmat = table(Prediction=yhat$class, Actual=ytest)
#
# The performance function is only for binary classification
# not appropriate for classification of 10 classes
#
performance(cfmat, digits=2, "MNIST performance with LDA")

# qda? --- not working --- requires filtering of features
# E.g. Perform PCA to the data???
# qda.fit = qda(V1 ~ ., train)

#
# Compare to other supervised learning models
#
#predKnn = FNN::knn(Xtrain,Xtest,ytrain,k=3)  # quite slow
library(class)
# Splitting the Data (X,y) to X and y seperately.
Xtrain = as.matrix(train[,-1])
Xtest  = as.matrix(test[,-1])
ytrain = train[,1]
predKnn = knn(Xtrain,Xtest,ytrain,k=3)  # quite slow
cfmat = table(Prediction=predKnn, Actual=ytest)
#
# The performance function is only for binary classification
# not appropriate here.
#
performance(cfmat, "\nkNN with k=3", digits=2)
#
# glmnet is an extension of Multivariate logit
#
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


