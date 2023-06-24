# -------------------------------------------------------------------
# Purpose: Practical for LDA & QDA Models in R
# Author : Liew How Hui (2023)
# Reference: 
#  1. http://faculty.marshall.usc.edu/gareth-james/ISL/Chapter%204%20Lab.txt
#  2. http://euler.stat.yale.edu/~tba3/stat665/lectures/lec11/script11.html
# Data   : http://faculty.marshall.usc.edu/gareth-james/ISL/data.html
# License: BSD-3
# Software: R 4.x & install.packages("ISLR2")
# Duration: 1 hour
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# performance() is taken from p03_knn1.R.  It is a simple 
# implementation of caret::confusionMatrix()
# -------------------------------------------------------------------

performance = function(xtab, desc=""){
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
        print(b)
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
# and the data is separable or is related multivariate NORMAL distribution
#

# -------------------------------------------------------------------
#  Analysis of the `Smarket' Dataset with LDA & QDA Classifier
# -------------------------------------------------------------------
library(ISLR2)

# Split data into train set and validation set
# train set = data from Year 2001-2004
# validation set = data from Year 2005
train = (Smarket$Year < 2005)
Smarket.2005 = Smarket[!train,]
Direction.2005 = Smarket$Direction[!train]

#
# EDA (histogram) tells us that the numeric data seems normal
# LDA (& QDA) may not be a bad choice.
# Note that LDA works with non-normal distributed data as well!
#
par(mfrow=c(2,4))
hist(Smarket$Lag1)
hist(Smarket$Lag2)
hist(Smarket$Lag3)
hist(Smarket$Lag4)
hist(Smarket$Lag5)
hist(Smarket$Volume)
hist(Smarket$Today)
#
# EDA's correlation analysis is IMPORTANT
#
table(ifelse(Smarket$Today>=0,"Up","Down"), Smarket$Direction)
# Today is actually used to obtain the Direction !!!!
# So Today needs to be removed from the input to Direction

library(MASS)  # supports LDA & QDA
#lda.fit=lda(Direction~Today,data=Smarket[train,])
#
# Year is something like `index', not useful as a predictor
#
# We usually start of with all VARIABLES which are useful
#
#lda.fit=lda(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket[train,])
#
# Other choices (some features may `confuse' the predictive model)
#
lda.fit=lda(Direction~Lag1+Lag2+Lag3+Lag4+Lag5,data=Smarket[train,])
#
# Performance analysis seems to suggest that volume is confusing
# the predictive model.
#
#lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
#lda.fit=lda(Direction~Lag1+Lag2,data=Smarket[train,])

lda.fit

# Make prediction based on LDA model
yhat = predict(lda.fit,Smarket.2005)
#names(yhat)
# yhat$posterior -> posterior `probability'
cfmat = table(yhat$class, Direction.2005)
performance(cfmat, "Smarket data analysis with LDA")

qda.fit=qda(Direction~Lag1+Lag2+Lag3+Lag4+Lag5,data=Smarket,subset=train)
#qda.fit=qda(Direction~Lag1+Lag2,data=Smarket,subset=train)
qda.fit
qda.class=predict(qda.fit,Smarket.2005)$class
cfmat = table(qda.class,Direction.2005)
performance(cfmat, "Smarket data analysis with QDA")

# -------------------------------------------------------------------
#  Analysis of the `Fraud' Dataset with LDA Classifier ???
# -------------------------------------------------------------------

# Impossible due to the data is not normally distributed ->
# many are factors & integers

# -------------------------------------------------------------------
#  Analysis of the `Weekly' Dataset with LDA Classifier
#  Very similar to the `Smarket' data
# -------------------------------------------------------------------
# Q2(a)
train = (Weekly$Year < 2009)
Weekly.0910 = Weekly[!train, ]   # 2009 & 2010
Direction.0910 = Weekly$Direction[!train]
#lda.fit = lda(Direction ~ Lag2, data=Weekly[train,])
#lda.fit = lda(Direction ~ . - Year, data=Weekly[train,])
#lda.fit = lda(Direction ~ . - Today - Year, data=Weekly[train,])
lda.fit = lda(Direction ~ . - Volume - Today - Year, data=Weekly[train,])

lda.pred = predict(lda.fit, Weekly.0910)
cfmat = table(lda.pred$class, Direction.0910)
performance(cfmat, "Weekly data analysis using LDA")

#qda.fit = qda(Direction ~ . - Today - Year, data=Weekly[train,])
qda.fit = qda(Direction ~ . - Volume - Today - Year, data=Weekly[train,])
qda.pred = predict(qda.fit, Weekly.0910)
cfmat = table(qda.pred$class, Direction.0910)
performance(cfmat, "Weekly data analysis using QDA")

#
# Q2(c): We can compare LDA to various models such as kNN, LR, etc.
#
library(class)
# Lag2 = column 3; Direction = column 9
#yhat = knn(as.matrix(Weekly[train,3]), as.matrix(Weekly.0910[,3]), Weekly[train,9]) #k=1
#yhat = knn(as.matrix(Weekly[train,2:8]), as.matrix(Weekly.0910[,2:8]), Weekly[train,9]) #k=1
# 2..6: Lag1..Lag5, 7=Volume, 9=Direction
kidx = seq(1,200,2)
accu = 0
for(i in 1:length(kidx)) {
  #yhat = knn(as.matrix(Weekly[train,2:7]), as.matrix(Weekly.0910[,2:7]), Weekly[train,9], k=kidx[i]) #k=1
  yhat = knn(as.matrix(Weekly[train,2:6]), as.matrix(Weekly.0910[,2:6]), Weekly[train,9], k=kidx[i]) #k=1
  cfmat = table(yhat, Direction.0910)
  #performance(cfmat, paste("Weekly data analysis using kNN(k=",kidx[i],")",sep=""))
  accu[i] = sum(diag(cfmat))/sum(cfmat)
}
par(mfrow=c(1,1))
plot(kidx, accu, type="b", xlab="kNN's k", ylab="Accuracy", 
  main="Performance of kNN(Direction~Lag1+...+Lag5)")

# Q2(d) For the Weekly Data, there is no best result when using Lag2
# Q2(d) For the Weekly Data, LDA performs better than kNN (k=1)??
# kNN with larger k seems to perform better than LDA maybe due to 
# the data cancelling the "noise" in the data.

# -------------------------------------------------------------------
#  Analysis of the `Auto' Dataset from ISLR2 with LDA Classifier
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

#
# Try standardisation to see if the trained model would be better
#

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
#  Analysis of the `MNIST' Dataset with LDA Classifier.
#  `MNIST' Dataset is a beginner's image recognition example.
#  Learn that the TIME for training/computation is MOSTLY VERY VERY LONG
# -------------------------------------------------------------------

# Download mnist_train.psv (~7M) & mnist_test.psv (~2M) from
#  https://github.com/statsmaths/stat665/find/gh-pages

#https://liaohaohui.github.io/UECM3993/mnist_train.psv
train = read.csv("mnist_train.psv", sep="|", as.is=TRUE, header=FALSE)
#https://liaohaohui.github.io/UECM3993/mnist_test.psv
test  = read.csv("mnist_test.psv",  sep="|", as.is=TRUE, header=FALSE)
dim(train)  # ncol(train) = 1 + 16x16
#
# Each row is an image with a label at Column 1
# To look at ONE ROW.  Note: First column is the `label'.
X = matrix(as.matrix(train[3400,-1]),16,16,byrow=TRUE)
X = 1 - (X + 1)*0.5
plot(0,0)
rasterImage(X,-1,-1,1,1)

set.seed(2023)
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
# not appropriate here.
#
#performance(cfmat, "MNIST performance with LDA")
cfmat
cat("Accuracy =", sum(diag(cfmat))/length(ytest), "\n")

# qda? --- not working --- requires filtering of features
# E.g. Perform PCA to the data???
# qda.fit = qda(V1 ~ ., train)

#
# Compare to other models
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
#performance(table(predKnn, ytest), "\nkNN with k=3")
cfmat
cat("Accuracy =", sum(diag(cfmat))/length(ytest), "\n")

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

#---------------------------------------------------------------
# Do not run the following three lines if your computer is slow
# The `SVM' predictive model is very very very slow.
#---------------------------------------------------------------

#outSvm  = e1071::svm(Xtrain,  factor(ytrain), kernel="radial", cost=1)
#predSvm = predict(outSvm, Xtest)
#performance(table(predSvm, ytest), "\nMNIST with SVM")

