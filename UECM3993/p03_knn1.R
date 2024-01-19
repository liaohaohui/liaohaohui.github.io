# -------------------------------------------------------------------
# Purpose: Practical for kNN (k-Nearest Neighbour) Models in R (Part 1)
# Author : Liew How Hui (2024)
# Reference: 
#  1. http://faculty.marshall.usc.edu/gareth-james/ISL/Chapter%204%20Lab.txt
# Data   : http://faculty.marshall.usc.edu/gareth-james/ISL/data.html
# License: BSD-3
# Software: R 4.1+
# Duration: 1 hour
# -------------------------------------------------------------------

#install.packages("ISLR2")   # if you on your own laptop / PC
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
library(class)   # for knn() --- converts categorical data to integer
#library(FNN)    # alternative for knn() --- no automatic conversion
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
fraud_fac = fraud    # create a copy
### change data type from numeric to categorical
fraud_fac[col_fac] = lapply(fraud[col_fac], factor)
# "After type conversion .....
sapply(fraud,class)
sapply(fraud_fac,class)

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

ACR = sum(diag(cftable.std))/sum(cftable.std)
TPR = cftable.std[1,1]/sum(cftable.std[,1])
TNR = cftable.std[2,2]/sum(cftable.std[,2])
PPV = cftable.std[1,1]/sum(cftable.std[1,])
NPV = cftable.std[2,2]/sum(cftable.std[2,])
FPR = 1 - TNR
FNR = 1 - TPR
RandomAccuracy = (sum(cftable.std[,2])*sum(cftable.std[2,]) + 
      sum(cftable.std[,1])*sum(cftable.std[1,]))/(sum(cftable.std)^2)
Kappa = (ACR - RandomAccuracy)/(1 - RandomAccuracy)
print(cftable.std)
cat("\n      Accuracy :", ACR, "\n")
cat("\n         Kappa :", Kappa, "\n")
cat("\n   Sensitivity :", TPR)
cat("\n   Specificity :", TNR)
cat("\nPos Pred Value :", PPV)
cat("\nNeg Pred Value :", NPV)
cat("\n           FPR :", FPR)
cat("\n           FNR :", FNR, "\n")

#
# Comparing with caret's confusionMatrix()
#
#> library(caret)
#> print(confusionMatrix(yhat, factor(fraud.test.knn$tag)))
#

#Confusion Matrix and Statistics
#
#          Reference
#Prediction   0   1
#         0 322  53
#         1  45 181
#                                          
#               Accuracy : 0.8369          
#                 95% CI : (0.8049, 0.8656)
#    No Information Rate : 0.6106          
#    P-Value [Acc > NIR] : <2e-16          
#                                          
#                  Kappa : 0.6549          
#                                          
# Mcnemar's Test P-Value : 0.4795          
#                                          
#            Sensitivity : 0.8774          
#            Specificity : 0.7735          
#         Pos Pred Value : 0.8587          
#         Neg Pred Value : 0.8009          
#             Prevalence : 0.6106          
#         Detection Rate : 0.5358          
#   Detection Prevalence : 0.6240          
#      Balanced Accuracy : 0.8254          
#                                          
#       'Positive' Class : 0               


# -------------------------------------------------------------------
#  Practical: Defining a function to deal with repetitive process
#  In R, a function has the format:
#  function_name = function(x1, x2, x3) {
#     ... process (to be filled in) ...
#  }
# -------------------------------------------------------------------

# You can replace the following with caret::confusionMatrix
performance = function(xtab, desc=""){
    cat(desc,"\n")
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

performance(cftable.std, "Confusion matrix and performance of kNN")

# -------------------------------------------------------------------
#  On the "More Performance Evaluation" in Lecture Slide
#  => validation set approach / holdout method is sensitive
#     to the stratified sampling / linear sampling
# -------------------------------------------------------------------

fraud_tag0 = fraud[fraud$tag=="0", ]
fraud_tag1 = fraud[fraud$tag=="1", ]

#
# (1) Take only the odd numbers
#
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

#
# (2) Now, take only the even numbers
#
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

# -------------------------------------------------------------------
#  Analysis of the IRIS Flower Dataset (3-class output) with kNN Classifier
# -------------------------------------------------------------------

# Linear Sampling 70% for training, 30% for testing
idx.train = sample(nrow(iris), 0.7*nrow(iris))

X.train = iris[ idx.train, 1:4]
y.train = iris[ idx.train, 5]
X.test  = iris[-idx.train, 1:4]
y.test  = iris[-idx.train, 5]

yhat = knn(X.train, X.test, y.train, k=1)   # Exercise: practise with k>1
performance(table(yhat, y.test), "Iris flower with kNN")

#
# Compare to caret's confusionMatrix:
#
#> print(confusionMatrix(yhat, y.test))
#
#Confusion Matrix and Statistics
#
#            Reference
#Prediction   setosa versicolor virginica
#  setosa         12          0         0
#  versicolor      0         15         1
#  virginica       0          3        14
#
#Overall Statistics
#                                          
#               Accuracy : 0.9111          
#                 95% CI : (0.7878, 0.9752)
#    No Information Rate : 0.4             
#    P-Value [Acc > NIR] : 9.959e-13       
#                                          
#                  Kappa : 0.8655          
#                                          
# Mcnemar's Test P-Value : NA              
#
#Statistics by Class:
#
#                     Class: setosa Class: versicolor Class: virginica
#Sensitivity                 1.0000            0.8333           0.9333
#Specificity                 1.0000            0.9630           0.9000
#Pos Pred Value              1.0000            0.9375           0.8235
#Neg Pred Value              1.0000            0.8966           0.9643
#Prevalence                  0.2667            0.4000           0.3333
#Detection Rate              0.2667            0.3333           0.3111
#Detection Prevalence        0.2667            0.3556           0.3778
#Balanced Accuracy           1.0000            0.8981           0.9167


