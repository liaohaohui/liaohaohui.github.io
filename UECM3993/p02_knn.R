# -------------------------------------------------------------------
# Purpose: Practical for kNN (k-Nearest Neighbour) Models in R
# Author : Liew How Hui (2020)
# Reference: 
#  1. http://faculty.marshall.usc.edu/gareth-james/ISL/Chapter%204%20Lab.txt
# Data   : http://faculty.marshall.usc.edu/gareth-james/ISL/data.html
# License: BSD-3
# Software: R 3.6 & R 4.0
# -------------------------------------------------------------------

#install.packages("ISLR")
library(ISLR)

# -------------------------------------------------------------------
# Split data into train set and validation set
# train set = data from Year 2001-2004
# validation set = data from Year 2005
# -------------------------------------------------------------------
train = (Smarket$Year < 2005)
Smarket.2005   = Smarket[!train,]
Direction.2005 = Smarket$Direction[!train]

# -------------------------------------------------------------------
#  Analysis of the reduced `Smarket' Dataset with kNN Classifier
# -------------------------------------------------------------------
library(class)
attach(Smarket)
train.X= cbind(Lag1,Lag2)[ train,]
test.X = cbind(Lag1,Lag2)[!train,]
train.y = Direction[train]
detach(Smarket)
#set.seed(1)
knn.pred = knn(train.X,test.X,train.y)  # Computer Default: k=1
table(knn.pred,Direction.2005)  # (83+43)/252
knn.pred = knn(train.X,test.X,train.y,k=3)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)

# -------------------------------------------------------------------
#  Analysis of the `Fraud' Dataset with kNN Classifier
# -------------------------------------------------------------------
fraud = read.csv("DataLab/fraud.csv")
sapply(fraud,class)
#colnames(fraud)
col_fac <- c("gender", "status", "employment", "account_link", "supplement", "tag")
### change data type from numerical to categorical
fraud[col_fac] <- lapply(fraud[col_fac], factor)
# "After type conversion .....
sapply(fraud,class)

cat("
# -------------------------------------------------------------------
#  Stratified sampling & Standardisation
# -------------------------------------------------------------------
")
cat("Option 1: Using splitstackshape ...\n")
library(splitstackshape)  # depends on data.table
set.seed(123)
fraud.train = stratified(fraud,"tag",size=0.7,keep.rownames=TRUE)
fraud.test  = fraud[-as.integer(fraud.train$rn),]
summary(fraud.test)
fraud.train = as.data.frame(fraud.train[,-c("rn")])
# If I don't convert fraud.train to data.frame, I will get error
# with kNN because splitstackshape convert data.frame to data.table

#set.seed(123)
#cat("Option 2: Using R only ...\n")
#### https://stackoverflow.com/questions/23479512/stratified-random-sampling-from-data-frame
#fraud_tag0 = fraud[fraud$tag=="0", ]
#fraud_tag1 = fraud[fraud$tag=="1", ]
#tag0_idx = sample(nrow(fraud_tag0), size=round(0.7*nrow(fraud_tag0)))
#tag1_idx = sample(nrow(fraud_tag1), size=round(0.7*nrow(fraud_tag1)))
#fraud.train = rbind(fraud_tag0[ tag0_idx,], fraud_tag1[ tag1_idx,])
#fraud.test  = rbind(fraud_tag0[-tag0_idx,], fraud_tag1[-tag1_idx,])
#summary(fraud.test)

# Option 3: dplyr (6 dependencies?)
#library(dplyr)
#split = sample.split(fraud$tag, SplitRatio=0.7)
#fraud.train = fraud[ split,]
#fraud.test  = fraud[!split,]

cat("\nData standardisation ...\n")
normalise.vec <- function(data,ref.data) {
    return ((data - mean(ref.data)) / sd(ref.data))
}
fraud.train.knn     = fraud.train
fraud.test.knn      = fraud.test
fraud.train.knn$age = normalise.vec(fraud.train.knn$age,fraud.train$age)
fraud.test.knn$age  = normalise.vec(fraud.test.knn$age, fraud.train$age)
fraud.train.knn$base_value = normalise.vec(
    fraud.train.knn$base_value,fraud.train$base_value)
fraud.test.knn$base_value  = normalise.vec(
    fraud.test.knn$base_value, fraud.train$base_value)

cat("\nTraining and validation with kNN ...\n\n")
yhat = knn(fraud.train.knn[,2:8], fraud.test.knn[,2:8], fraud.train.knn[,9], k=3)
cftable = table(yhat, fraud.test.knn$tag)

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
    cat("\n      Accuracy :", ACR, "\n\n         Kappa :", Kappa, "\n")
    cat("\n   Sensitivity :", TPR,   "\n   Specificity :", TNR, "\n")
    cat("Pos Pred Value :", PPV,     "\nNeg Pred Value :", NPV, "\n")
    cat("           FPR :", FPR,     "\n           FNR :", FNR, "\n")
}

performance(cftable, "Confusion matrix and performance with kNN")

library(kknn) # tons of dependencies: igraph, Matrix, graphics
cat("\nTraining and validation with wkNN ...\n\n")
fraud.kknn = kknn(tag~.-id_person, fraud.train.knn, fraud.test.knn, k=3)
#summary(fraud.kknn)
yhat = fitted(fraud.kknn)
cftable = table(yhat, fraud.test.knn$tag)
performance(cftable, "Confusion matrix and performance with wkNN")

cat("
# -------------------------------------------------------------------
#  Analysis of the `Boston' Dataset with kNN Regressor
# -------------------------------------------------------------------
")
### https://daviddalpiaz.github.io/r4sl/knn-reg.html
library(FNN)   # Fast Nearest Neighbor Search Algorithms and Applications
library(MASS)  # Boston data
#data(Boston)  # The Boston Housing Dataset has 506 rows
set.seed(42)
idx = sample(nrow(Boston), size=250)
boston.train = Boston[ idx,]
boston.test  = Boston[-idx,]
X = boston.train['lstat'] # lstat: % lower status of the population
y = boston.train['medv']  # medv: Median of owner-occupied homes in $1000's
X.test = boston.test['lstat']
y.test = boston.test['medv']
lstat_grid = data.frame(lstat=seq(min(X), max(X), by = 0.01))
pm.001 = knn.reg(X, test=lstat_grid, y=y, k=  1)
pm.005 = knn.reg(X, test=lstat_grid, y=y, k=  5)
pm.010 = knn.reg(X, test=lstat_grid, y=y, k= 10)
pm.050 = knn.reg(X, test=lstat_grid, y=y, k= 50)
pm.100 = knn.reg(X, test=lstat_grid, y=y, k=100)
pm.250 = knn.reg(X, test=lstat_grid, y=y, k=250)

rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}
make_knn_pred = function(k, training, predicting) {
  pred = FNN::knn.reg(train = training["lstat"], 
                      test = predicting["lstat"], 
                      y = training$medv, k = k)$pred
  act  = predicting$medv
  rmse(predicted = pred, actual = act)
}
k_list = c(1, 5, 10, 25, 50, 250)
# get requested train RMSEs
knn_trn_rmse = sapply(k_list, make_knn_pred, boston.train, boston.train)
# get requested test RMSEs
knn_tst_rmse = sapply(k_list, make_knn_pred, boston.train, boston.test)

# determine "best" k
best_k = k_list[which.min(knn_tst_rmse)]

# find overfitting, underfitting, and `best' k
fit_status = ifelse(k_list<best_k, "Over",
                ifelse(k_list==best_k, "Best", "Under"))

knn_results = data.frame(k_list, round(knn_trn_rmse, 2),
                round(knn_tst_rmse, 2), fit_status)
colnames(knn_results) = c("k", "Train RMSE", "Test RMSE", "Fit?")
print(knn_results)

