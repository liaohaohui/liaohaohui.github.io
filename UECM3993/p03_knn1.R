# -------------------------------------------------------------------
# Purpose: Practical for kNN (k-Nearest Neighbour) Models in R (Part 1)
# Author : Liew How Hui (2022)
# Reference: 
#  1. http://faculty.marshall.usc.edu/gareth-james/ISL/Chapter%204%20Lab.txt
# Data   : http://faculty.marshall.usc.edu/gareth-james/ISL/data.html
# License: BSD-3
# Software: R 3.6 & R 4.x
# Duration: 1 hour
# -------------------------------------------------------------------

#install.packages("ISLR2")
library(ISLR2)   # provides the Smarket (US Stock Market 2001-2005 data)

# -------------------------------------------------------------------
# Split data into train set and validation set
# train set = data from Year 2001-2004
# validation set = data from Year 2005
# -------------------------------------------------------------------
### Explore the dataset
#View(Smarket)
names(Smarket)
dim(Smarket)
train = (Smarket$Year < 2005)
Smarket.2005 = Smarket[!train,]   # MATLAB: Masking / Python: Boolean indexing
# , for picking rows in 2D data

# -------------------------------------------------------------------
#  Analysis of the reduced `Smarket' Dataset with kNN Classifier
# -------------------------------------------------------------------
library(class)   # for kNN
#library(FNN)    # also provides the kNN we can use here.
attach(Smarket)
train.X= cbind(Lag1,Lag2)[ train,]  # cbind = Column Binding
train.y = Direction[train]
test.X = cbind(Lag1,Lag2)[!train,]
test.y = Direction[!train]  # Direction.2005, associated with Smarket.2005
detach(Smarket)
#set.seed(1)
knn.pred = knn(train.X,test.X,train.y)  # Computer Default: k=1
table(knn.pred,test.y)  # k=1: (83+43)/252
knn.pred = knn(train.X,test.X,train.y,k=3)
table(knn.pred,test.y)
accuracy = mean(knn.pred==test.y)

# Summary: Financial Stochastic Data is ``not'' predictable
# More advanced maths like SDE (Financial Econ II) to model the risk
# to develop asset (stock, futures, etc.) derivatives

# -------------------------------------------------------------------
#  Analysis of the `Fraud' Dataset with kNN Classifier
# -------------------------------------------------------------------
#https://liaohaohui.github.io/UECM3993/fraud.csv
fraud = read.csv("fraud.csv")
sapply(fraud,class)
#colnames(fraud)
col_fac = c("gender", "status", "employment", "account_link", "supplement", "tag")
### change data type from numeric to categorical
fraud[col_fac] = lapply(fraud[col_fac], factor)
# "After type conversion .....
sapply(fraud,class)

# -------------------------------------------------------------------
#  Stratified sampling for Binary Classification Problem
# -------------------------------------------------------------------

set.seed(123)
cat("Option 1: Manual stratified sampling using Base R ...\n")
### https://stackoverflow.com/questions/23479512/stratified-random-sampling-from-data-frame
fraud_tag0 = fraud[fraud$tag=="0", ]
fraud_tag1 = fraud[fraud$tag=="1", ]
tag0_idx = sample(nrow(fraud_tag0), size=round(0.7*nrow(fraud_tag0)))
tag1_idx = sample(nrow(fraud_tag1), size=round(0.7*nrow(fraud_tag1)))
fraud.train = rbind(fraud_tag0[ tag0_idx,], fraud_tag1[ tag1_idx,])
fraud.test  = rbind(fraud_tag0[-tag0_idx,], fraud_tag1[-tag1_idx,])
summary(fraud.test)

#cat("Option 2: Using splitstackshape ...\n")
#library(splitstackshape)  # depends on data.table
#set.seed(123)
#fraud.train = stratified(fraud,"tag",size=0.7,keep.rownames=TRUE)
#fraud.test  = fraud[-as.integer(fraud.train$rn),]
#summary(fraud.test)
#fraud.train = as.data.frame(fraud.train[,-c("rn")])
## If I don't convert fraud.train to data.frame, I will get error
## with kNN because splitstackshape convert data.frame to data.table

#cat("Option 3: caTools (use to be called by dplyr) ...\n")
#library(caTools)
#split = sample.split(fraud$tag, SplitRatio=0.7)
#fraud.train = fraud[ split,]
#fraud.test  = fraud[!split,]

# -------------------------------------------------------------------
#  Feature Scaling with Standardisation
#  => Data standardisation with respect to the TRAINING DATA
# -------------------------------------------------------------------

cat("\nData Preparation/Preprocessing: Data standardisation ...\n")
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

# -------------------------------------------------------------------
#  Modelling with kNN & Evaluation for Binary Classification
# -------------------------------------------------------------------
cat("\nTraining and validation with kNN ...\n\n")
yhat = knn(fraud.train.knn[,2:8], fraud.test.knn[,2:8], fraud.train.knn[,9], k=3)
cftable.std = table(yhat, fraud.test.knn$tag)

# You can replace the following with caret::confusionMatrix
performance = function(xtab, description=""){
    cat("\n\n",description,"\n",sep="")
    ACR = sum(diag(xtab))/sum(xtab)
    TPR = xtab[1,1]/sum(xtab[,1]); TNR = xtab[2,2]/sum(xtab[,2])
    PPV = xtab[1,1]/sum(xtab[1,]); NPV = xtab[2,2]/sum(xtab[2,])
    FPR = 1 - TNR                ; FNR = 1 - TPR
    # https://standardwisdom.com/softwarejournal/2011/12/confusion-matrix-another-single-value-metric-kappa-statistic/
    RandomAccuracy = (sum(xtab[,2])*sum(xtab[2,]) + 
      sum(xtab[,1])*sum(xtab[1,]))/(sum(xtab)^2)
    Kappa = (ACR - RandomAccuracy)/(1 - RandomAccuracy)
    print(xtab)
    cat("\n      Accuracy :", ACR, "\n\n         Kappa :", Kappa, "\n")
    cat("\n   Sensitivity :", TPR,   "\n   Specificity :", TNR, "\n")
    cat("Pos Pred Value :", PPV,     "\nNeg Pred Value :", NPV, "\n")
    cat("           FPR :", FPR,     "\n           FNR :", FNR, "\n")
}

performance(cftable.std, "Confusion matrix and performance with kNN")

# -------------------------------------------------------------------
#  On the "More Performance Evaluation" in Lecture Slide
#  => validation set approach / holdout method is sensitive
#     to the stratified sampling / linear sampling
# -------------------------------------------------------------------

fraud_tag0 = fraud[fraud$tag=="0", ]
fraud_tag1 = fraud[fraud$tag=="1", ]
# (1) Take only the odd numbers
tag0_idx = seq(1,nrow(fraud_tag0),2)
tag1_idx = seq(1,nrow(fraud_tag1),2)
fraud.train     = rbind(fraud_tag0[ tag0_idx,], fraud_tag1[ tag1_idx,])
fraud.test.knn  = rbind(fraud_tag0[-tag0_idx,], fraud_tag1[-tag1_idx,])
fraud.train.knn = fraud.train
# Data Preprocessing
fraud.train.knn$age = normalise.vec(fraud.train.knn$age,fraud.train$age)
fraud.test.knn$age  = normalise.vec(fraud.test.knn$age, fraud.train$age)
fraud.train.knn$base_value = normalise.vec(
    fraud.train.knn$base_value,fraud.train$base_value)
fraud.test.knn$base_value  = normalise.vec(
    fraud.test.knn$base_value, fraud.train$base_value)
yhat = knn(fraud.train.knn[,2:8], fraud.test.knn[,2:8], fraud.train.knn[,9], k=3)
cftable.std = table(yhat, fraud.test.knn$tag)
performance(cftable.std, "Odd index for training, even index for testing")

# (2) Now, take only the even numbers
tag0_idx = seq(2,nrow(fraud_tag0),2)
tag1_idx = seq(2,nrow(fraud_tag1),2)
fraud.train     = rbind(fraud_tag0[ tag0_idx,], fraud_tag1[ tag1_idx,])
fraud.test.knn  = rbind(fraud_tag0[-tag0_idx,], fraud_tag1[-tag1_idx,])
fraud.train.knn = fraud.train
# Data Preprocessing
fraud.train.knn$age = normalise.vec(fraud.train.knn$age,fraud.train$age)
fraud.test.knn$age  = normalise.vec(fraud.test.knn$age, fraud.train$age)
fraud.train.knn$base_value = normalise.vec(
    fraud.train.knn$base_value,fraud.train$base_value)
fraud.test.knn$base_value  = normalise.vec(
    fraud.test.knn$base_value, fraud.train$base_value)
yhat = knn(fraud.train.knn[,2:8], fraud.test.knn[,2:8], fraud.train.knn[,9], k=3)
cftable.std = table(yhat, fraud.test.knn$tag)
performance(cftable.std, "Even index for training, odd index for testing")

