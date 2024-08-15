# -------------------------------------------------------------------
# Purpose: Practical for kNN (k-Nearest Neighbour) Models in R (Part 1)
# Author : Liew How Hui (2024)
# Reference: 
#  1. http://faculty.marshall.usc.edu/gareth-james/ISL/Chapter%204%20Lab.txt
# Data   : http://faculty.marshall.usc.edu/gareth-james/ISL/data.html
# License: BSD-3
# Software: R 4.x
# Duration: 1 hour
# -------------------------------------------------------------------

# -------------------------------------------------------------------
#  Gower Distance ....
# -------------------------------------------------------------------

#https://liaohaohui.github.io/UECM3993/fraud.csv
fraud = read.csv("fraud.csv",row.names=1)
col_fac = c("gender", "status", "employment", "account_link", "supplement", "tag")
### change data type from numeric to categorical
fraud[col_fac] = lapply(fraud[col_fac], factor)
Y = fraud$tag

# Get stats range of each column
apply(fraud,2,range)
library(gower)
fraud$tag = NULL
# For gower distance (https://jamesmccaffrey.wordpress.com/2020/04/21/example-of-calculating-the-gower-distance/):
# numeric:     abs(diff between two items) / range 
# non-numeric: 0 if equal, 1 if different
# E.g. Consider Item 1 vs Item 2 & 3
# Calculation for Item 1 vs Item 3:
# For the categorical columns: I(1!=1)+I(2!=3)+I(3!=1)+I(0!=0)+I(1!=0) => 3
# For age: abs(32-21)/(57-21) => 0.3055556
# For base_value: abs(729.3-683.8)/(729.3-384.1) => 0.1318076
# gower distance = (3 + 0.3055556 + 0.1318076)/7 = 0.4910519  # 7 columns
gower_dist(fraud[1,], fraud[2:4,])  # item 1 vs item 3 => 0.4910519

# -------------------------------------------------------------------
#  Work on Lecture Slide Case Study 1 (Example 1)
# -------------------------------------------------------------------

d.f.train = read.csv(text="
Student,Weight,Height,Group
A,29,118,A
B,53,137,B
C,38,127,B
D,49,135,B
E,28,111,A
F,24,111,A
G,30,121,A
",row.names=1,stringsAsFactor=T)

d.f.test = read.csv(text="
Student,Weight,Height
H,35,120
I,47,131
J,22,115
K,38,119
L,31,136
",row.names=1)

library(class)
yhat = knn(d.f.train[,1:2], d.f.test, d.f.train[,3], k=3)
print(cbind(d.f.test, Prediction=yhat))

# Construct confusion matrix
Actual = c("A", "B", "A", "B", "B")
print(table(yhat, Actual))


# -------------------------------------------------------------------
#  Work on Lecture Slide Case Study 2 (Example 2)
# -------------------------------------------------------------------

d.f.train = read.csv(text="
X,Y
 5,4
 8,1
15,10
22,16
30,30")

d.f.new = data.frame(X=12)

library(FNN)
#yhat = knn.reg(matrix(d.f.train[,1],nrow=nrow(d.f.train)), 
#                  d.f.new, matrix(d.f.train[,2],nrow=nrow(d.f.train)), k=3)
yhat = knn.reg(data.frame(X=d.f.train[,1]), d.f.new, d.f.train[,2], k=3)


# -------------------------------------------------------------------
#  Analysis of the reduced `Smarket' Dataset with kNN Classifier
#
#  Split data into train set and validation set
#  train set = data from Year 2001-2004
#  validation set = data from Year 2005
# -------------------------------------------------------------------
#
# Time-based Holdout Method / Train-Test Split / Validation Set approach / ...
#
library(ISLR2)
train = (Smarket$Year < 2005)
attach(Smarket)
train.X= cbind(Lag1,Lag2)[ train,]  # cbind = Column Binding
train.y = Direction[train]
test.X = cbind(Lag1,Lag2)[!train,]
test.y = Direction[!train]  # Direction.2005, associated with Smarket.2005
detach(Smarket)
#set.seed(1)
library(class)   # for knn() --- converts categorical data to integer
#library(FNN)    # alternative for knn() --- no automatic conversion
knn.pred = knn(train.X,test.X,train.y)  # Computer Default: k=1
table(knn.pred,test.y)  # k=1: (83+43)/252
knn.pred = knn(train.X,test.X,train.y,k=3)
table(knn.pred,test.y)
accuracy = mean(knn.pred==test.y)   # Evaluation for Classification: Accuracy

# Summary: Financial Stochastic Data is ``not'' predictable
# More advanced maths like Stochastic Differential Equations to 
# model the fluctuation of stocks as well as the fluctuations
# related to derivatives of the financial assets.

# -------------------------------------------------------------------
#  Analysis of the `Fraud' Dataset with kNN Classifier using 
#  "Ordinal encoding" for "categorical data".
# -------------------------------------------------------------------
# Already loaded above from Lecture Slide Example 1
fraud_fac = cbind(fraud, tag=Y)    # create a copy
fraud = read.csv("fraud.csv",row.names=1)
sapply(fraud,class)
sapply(fraud_fac,class)

# Linear sampling:  Just sample a portion (e.g. 70%) from the data
# Stratified sampling:  Split the data based on the output class C1, C2
#                       Sample 70% from C1, 70% from C2, then combine.

# -------------------------------------------------------------------
#  Stratified sampling for Binary Classification Problem
# -------------------------------------------------------------------

# Target for fraud: tag (0=fraud, 1=no fraud)
# 2003 x 9 (p = 8, n = 2003)

set.seed(123)   # Reduce randomness by allowing repetition
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
yhat = knn(fraud.train.knn[,1:7], fraud.test.knn[,1:7], fraud.train.knn[,8], k=3)
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
        recalls     = c()
        mcspecificity = c()
        ppv         = c()
        npv         = c()
        for(i in 1:length(lvls)) {
            recalls[i] = xtab[i,i]/sum(xtab[,i])
            mcspecificity[i] = sum(xtab[-i,-i])/sum(xtab[,-i])
            ppv[i]         = xtab[i,i]/sum(xtab[i,])
            npv[i]         = sum(xtab[-i,-i])/sum(xtab[-i,])
        }
        b = data.frame(rbind(recalls,mcspecificity,ppv,npv))
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
#  Analysis of the IRIS Flower Dataset (3-class output) with kNN Classifier
#  Input:  iris[ , c("Sepal.Length", ..., "Petal.Width")]
#  Target: iris$Species
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


