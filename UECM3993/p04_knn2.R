# -------------------------------------------------------------------
# Purpose: Practical for kNN (k-Nearest Neighbour) Models in R (Part 2)
# Author : Liew How Hui (2023)
# Reference: 
#  1. http://faculty.marshall.usc.edu/gareth-james/ISL/Chapter%204%20Lab.txt
# Data   : http://faculty.marshall.usc.edu/gareth-james/ISL/data.html
# License: BSD-3
# Software: R 4.x & install.packages(c("kknn", "FNN"))
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


# -------------------------------------------------------------------
#  Manual stratified sampling using Base R & Standardising Fraud data
#  as in Practical 3
# -------------------------------------------------------------------

#https://liaohaohui.github.io/UECM3993/fraud.csv
fraud = read.csv("fraud.csv")
col_fac = c("gender", "status", "employment", "account_link", "supplement", "tag")
### change data type from numeric to categorical
fraud[col_fac] = lapply(fraud[col_fac], factor)
set.seed(123)
# Option 1 stratified sampling
fraud_tag0 = fraud[fraud$tag=="0", ]
fraud_tag1 = fraud[fraud$tag=="1", ]
tag0_idx = sample(nrow(fraud_tag0), size=round(0.7*nrow(fraud_tag0)))
tag1_idx = sample(nrow(fraud_tag1), size=round(0.7*nrow(fraud_tag1)))
fraud.train = rbind(fraud_tag0[ tag0_idx,], fraud_tag1[ tag1_idx,])
fraud.test  = rbind(fraud_tag0[-tag0_idx,], fraud_tag1[-tag1_idx,])
# Data standardisation with respect to the TRAINING DATA
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
#  Weighted kNN
# -------------------------------------------------------------------

library(kknn) # tons of dependencies: igraph, Matrix, graphics
cat("\nTraining and validation with wkNN ...\n\n")
#
# kernel='optimal', not the same as (standard unweighted) kNN
#
fraud.kknn = kknn(tag~.-id_person, fraud.train.knn, fraud.test.knn, k=3)
#summary(fraud.kknn)
yhat.kknn = fitted(fraud.kknn)
performance(table(yhat.kknn, fraud.test.knn$tag), "Confusion matrix and performance with wkNN(k=3,kernel=optimal)")

#
# Trying out other kernels
#
fraud.kknn = kknn(tag~.-id_person, fraud.train.knn, fraud.test.knn, k=3, kernel='inv')
yhat.kknn = fitted(fraud.kknn)
performance(table(yhat.kknn, fraud.test.knn$tag), "Confusion matrix and performance with wkNN(k=3,kernel=inv)")

fraud.kknn = kknn(tag~.-id_person, fraud.train.knn, fraud.test.knn, k=3, kernel='triangular')
yhat.kknn = fitted(fraud.kknn)
performance(table(yhat.kknn, fraud.test.knn$tag), "Confusion matrix and performance with wkNN(k=3,kernel=triangular)")

#
# Trying to compare with starndard kNN (kernel="rectangular"):
#
fraud.kknn.rect = kknn(tag~.-id_person, fraud.train.knn, fraud.test.knn, k=3, kernel='rectangular')
yhat.rect = fitted(fraud.kknn.rect)
performance(table(yhat.rect, fraud.test.knn$tag), "wkNN.Rectangular")

#
# Fraud data can be predicted. 
# 1. class::knn regards the data as numeric and will convert 
#    the categorical data to the corresponding `integers'.
# 2. FNN::knn requires us to perform the conversion ourselves.
# 3. Weighted kNN, which theoretically better, is not performing better
#    than the usual kNN for high dimension input (e.g. p=7)
#
# Leason 1: summary of table is important (e.g. summary(fraud))
# Leason 2: for numeric columns with large variations, then 
#           standardisation may improve the training of the model
# Leason 3: Stratified sampling + Model Training + Performance Evaluation
#

# -------------------------------------------------------------------
#  Applying kNN Regressor to Regression Problem
#  Case Study: `Boston' Dataset
# -------------------------------------------------------------------

### https://daviddalpiaz.github.io/r4sl/knn-reg.html
library(FNN)   # Fast Nearest Neighbor Search Algorithms and Applications
library(MASS)  # Boston data
#data(Boston)  # The Boston Housing Dataset has 506 rows
set.seed(42)
idx = sample(nrow(Boston), size=250)   # Linear Sampling
boston.train = Boston[ idx,]
boston.test  = Boston[-idx,]
X = boston.train['lstat'] # lstat: % lower status of the population
y = boston.train['medv']  # medv: Median of owner-occupied homes in $1000's
X.test = boston.test['lstat']
y.test = boston.test['medv']
lstat_grid = data.frame(lstat=seq(min(X), max(X), by = 0.01))

#
# kknn::kknn regression
#
wk.001 = kknn(medv~., cbind(X,y), test=lstat_grid, k= 10)
yhat   = fitted(wk.001)
print(class(lstat_grid[,1]))
print(class(yhat))
plot(cbind(X,y), main="kkNN regression (k=10)")
lines(as.vector(lstat_grid[,1]),yhat)

#
# FNN::knn.reg
#
pm.001 = knn.reg(X, test=lstat_grid, y=y, k=  1)
plot(cbind(X,y))
lines(lstat_grid[,1],pm.001$pred)
pm.005 = knn.reg(X, test=lstat_grid, y=y, k=  5)
pm.010 = knn.reg(X, test=lstat_grid, y=y, k= 10)
pm.050 = knn.reg(X, test=lstat_grid, y=y, k= 50)
plot(cbind(X,y))
lines(lstat_grid[,1],pm.050$pred)
pm.100 = knn.reg(X, test=lstat_grid, y=y, k=100)
pm.250 = knn.reg(X, test=lstat_grid, y=y, k=250)
plot(cbind(X,y))
lines(lstat_grid[,1],pm.250$pred)

# -------------------------------------------------------------------
# (1) Performance Measurement for Regression Problems using
#     RMSE (Root Mean Sum of Square Error)
# (2) Hyperparameter analysis of kNN's k using the Holdout Method
# -------------------------------------------------------------------

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
#k_list = seq(1,200,20)
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


