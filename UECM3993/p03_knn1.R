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
#  Practical : Analyse Smarket data with kNN Classifier
#
#  Smarket is a time series data and the linear split is based
#  on a cut-off time.  Be careful about similar data in assignment.
# -------------------------------------------------------------------

### Simple EDA on Smarket data
#View(Smarket)
names(Smarket)
dim(Smarket)
summary(Smarket)

#
# For Holdout Method / Train-Test Split / Validation Set approach / ...,
# split data into train set and validation set
# train set = data from Year 2001-2004
# validation set = data from Year 2005
#

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

#
# Lesson Learn from Smarket data: Financial Stochastic Data is ``not'' 
# predictable
#
# More advanced maths like SDE (Financial Econ II) to model the risk
# to develop asset (stock, futures, etc.) derivatives
#

# -------------------------------------------------------------------
#  Practical : Analysis of the Fraud data with kNN Classifier
#
#  The target of Fraud data is binary and imbalanced.  So we try
#  stratified sampling.
# -------------------------------------------------------------------

#https://liaohaohui.github.io/UECM3993/fraud.csv
fraud = read.csv("fraud.csv",row.names=1)

### Simple EDA
### Data is 2003 x 8 (p = 7, n = 2003)
### Target for fraud: tag (0=fraud, 1=no fraud)
dim(fraud)
sapply(fraud,class)
summary(fraud)

### Simple EDA tells us that some columns need to be converted to categorical
col_fac = c("gender", "status", "employment", "account_link", "supplement", "tag")
fraud_fac = fraud    # create a copy
### change data type from integer to categorical
fraud_fac[col_fac] = lapply(fraud[col_fac], factor)
#
# Check type conversion and compare the data before and after type conversion
#
sapply(fraud,class)
sapply(fraud_fac,class)
summary(fraud)
summary(fraud_fac)

#
# Stratified sampling for Binary Classification Problem : 
#   1. Split the data based on the output class C1, C2
#   2. Sample (without replacement) 70% from C1, 70% from C2, then 
#      combine as training data
#   3. Take the remainder 30% from C1 and 30% from C2 to form
#      the testing data
#

#
# Option 1: Manual stratified sampling using Base R ...
#
# Ref: https://stackoverflow.com/questions/23479512/stratified-random-sampling-from-data-frame
#
set.seed(123)   # Reduce randomness by allowing repetition
fraud_tag0 = fraud[fraud$tag=="0", ]
fraud_tag1 = fraud[fraud$tag=="1", ]
tag0_idx = sample(nrow(fraud_tag0), size=round(0.7*nrow(fraud_tag0)))
tag1_idx = sample(nrow(fraud_tag1), size=round(0.7*nrow(fraud_tag1)))
fraud.train = rbind(fraud_tag0[ tag0_idx,], fraud_tag1[ tag1_idx,])
fraud.test  = rbind(fraud_tag0[-tag0_idx,], fraud_tag1[-tag1_idx,])
summary(fraud.test)

#
# Option 2: caTools (use to be called by dplyr) ...
#
#library(caTools)
#split = sample.split(fraud$tag, SplitRatio=0.7)
#fraud.train = fraud[ split,]
#fraud.test  = fraud[!split,]

#
# Option 3: Using splitstackshape ...
#
# Note: We are not using it because UTAR blocks R from assessing Internet
#
#library(splitstackshape)  # depends on data.table
#set.seed(123)
#fraud.train = stratified(fraud,"tag",size=0.7,keep.rownames=TRUE)
#fraud.test  = fraud[-as.integer(fraud.train$rn),]
#summary(fraud.test)
#fraud.train = as.data.frame(fraud.train[,-c("rn")])
## If I don't convert fraud.train to data.frame, I will get error
## with kNN because splitstackshape convert data.frame to data.table

#
#  Data Preparation : Feature Scaling with Standardisation
#  => Data standardisation with respect to the TRAINING DATA
#
#  Rationale : age and base_value have large standard deviations
#  compare to other columns
#

normalise.vec <- function(column,ref.column) {
    return ((column - mean(ref.column)) / sd(ref.column))
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
#  Modelling with kNN (k=3)
# 
cat("\nTraining and validation with kNN ...\n\n")
yhat = knn(fraud.train.knn[,1:7], fraud.test.knn[,1:7], fraud.train.knn[,8], k=3)

#
# Evaluation for Binary Classification using formulas for Topic 1
#
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
#
# If you are using caret in your assignment, you can just use
# confusionMatrix() rather than the performance()
#


# -------------------------------------------------------------------
#  Practical : Analysis of the Iris Flower data (3-class output) 
#  with kNN Classifier.
#
#  Iris only has has 150 rows of data.  So it is small (e.g. < 500)
#  We can consider using K-Fold CV or LOOCV.  knn.cv() performs
#  LOOCV and we will try it here.
#
#  Note that when we use LOOCV, we don't need to split data.
#
#  Input:  iris[ , c("Sepal.Length", ..., "Petal.Width")]
#  Target: iris$Species
# -------------------------------------------------------------------

### Simple EDA
dim(iris)
table(iris$Species)

#
# Performance Measurement for kNN(k=1) using LOOCV
#
yhat = knn.cv(iris[,1:4], iris[,5], k=1)
performance(table(yhat, iris[,5]), "Iris flower with kNN(k=1)")

ACC = c()
ir = 1:25
for (i in ir) {
  yhat = knn.cv(iris[,1:4], iris[,5], k=2*i+1)
  ACC[i] = sum(diag(table(yhat, iris[,5])))/150
}
k = 2*ir+1
plot(k, ACC, "b")


