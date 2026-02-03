# -------------------------------------------------------------------
# Purpose: Application using Statistical Software (Part 1)
# Detail: Case Study 1: Prediction and Estimation
# Author : Liew How Hui (2026)
# Reference:
#   1. https://hastie.su.domains/ISLR2/Labs/R_Labs/Ch4-classification-lab.R
#   2. https://www.statlearning.com/resources-second-edition
# Data: ISLR2, https://liaohaohui.github.io/UECM3993/fraud.csv
# License: BSD-3
# Software: R 4.1+
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
#  Practical : Working with Similarity Measures other than
#  Euclidean distance.  philentrophy library provides distance
#  measures between two vectors.  They don't work with data.frame
#
#  Ref: 
#    1. https://rstudio-pubs-static.s3.amazonaws.com/1059197_cabed5b55c88477582c1816435e3b1ce.html
#    2. https://www.youtube.com/watch?v=w2FIg2-ln8s (Part 2 - KNN Solved 
#       Example Cosine Similarity Manhattan Distance Mahesh Huddar)
# -------------------------------------------------------------------

library(philentropy)
# getDistMethods()

#
# Working with Example 2.1.3
#
# Exercise: Compare them to the formulas in Topic 2
#
Calculus1 = c(3626, 1446, 915, 798, 552, 556)
Calculus2 = c( 926,  476, 317, 356, 283, 146)
print(euclidean(Calculus1, Calculus2, testNA=F))
# Or: distance(rbind(Calculus1,Calculus2), method="euclidean")
print(manhattan(Calculus1, Calculus2, testNA=F))
print(chebyshev(Calculus1, Calculus2, testNA=F))
print(minkowski(Calculus1, Calculus2, n=3.5, testNA=F))
# The cosine dissimilarity lack the complement `1-'
print(1 - cosine_dist(Calculus1, Calculus2, testNA=F))

#
# Working with Example 2.1.4
#
x = c(1, 0, 1, 1, 0, 0, 0, 0, 0, 0)
y = c(0, 0, 1, 1, 0, 0, 1, 0, 0, 1)
print(intersection_dist(x,y,testNA=F)/length(x))
print(jaccard(x,y,testNA=F))
print(tanimoto(x,y,testNA=F))


# -------------------------------------------------------------------
#  Case Study 1 : Using Fraud data using kNN with Gower similarity
#  measurement for Prediction & Estimation
# -------------------------------------------------------------------

#
#  Gower distance
#  * numeric     : abs(diff between two items) / range 
#  * non-numeric : 0 if equal, 1 if different
#
#  Note: philentropy's gower is NOT for the Gower distance but for
#  difference between two probability vectors.  Gower distance
#  for a mixed of numeric and categorical data is available in 
#  gower library (StatMatch has too many dependecies)
#
#  Example from lecture notes using fraud data (row 1 vs rows 2--4):
#
#  n | gender | age | status | employment | acclink | supplement | base  
#  --|--------|-----|--------|------------|---------|------------|-------
#  1 |      1 |  32 |      2 |          3 |       0 |          1 | 729.3 
#  2 |      1 |  57 |      1 |          3 |       0 |          0 | 384.1 
#  3 |      1 |  21 |      3 |          1 |       0 |          0 | 683.8 
#  4 |      1 |  27 |      1 |          3 |       0 |          0 | 143.0 
#
# Gower_dist(row 1, row 2) 
# = 1/7 * ((1!=1) + abs((32-57)/(57-21)) + (2!=1) + (3!=3) + (0!=0) + (1!=0) + 
#     abs((729.3-384.1)/(729.3-143.0)))
#
# Gower_dist(row 1, row 3)
# = 1/7 * ((1!=1) + abs((32-21)/(57-21)) + (2!=3) + (3!=1) + (0!=0) + (1!=0) +
#     abs((729.3-683.8)/(729.3-143.0)))
#
# Gower_dist(row 1, row 4)
# = 1/7 * ((1!=1) + abs((32-27)/(57-21)) + (2!=1) + (3!=3) + (0!=0) + (1!=0) +
#     abs((729.3-143.0)/(729.3-143.0)))
#
#  Ref:
#    1. https://jamesmccaffrey.wordpress.com/2020/04/21/example-of-calculating-the-gower-distance/):
#    2. https://bradleyboehmke.github.io/HOML/kmeans.html
#

#https://liaohaohui.github.io/UECM3993/fraud.csv
fraud = read.csv("fraud.csv",row.names=1)
col_fac = c("gender", "status", "employment", "account_link", "supplement", "tag")
fraud[col_fac] = lapply(fraud[col_fac], factor)
set.seed(123)
fraud_tag0 = fraud[fraud$tag=="0", ]
fraud_tag1 = fraud[fraud$tag=="1", ]
tag0_idx = sample(nrow(fraud_tag0), size=round(0.7*nrow(fraud_tag0)))
tag1_idx = sample(nrow(fraud_tag1), size=round(0.7*nrow(fraud_tag1)))
fraud.train = rbind(fraud_tag0[ tag0_idx,], fraud_tag1[ tag1_idx,])
fraud.test  = rbind(fraud_tag0[-tag0_idx,], fraud_tag1[-tag1_idx,])

#
# Based on what we learned in the lecture and first two practical classes, 
# here's a simple implementation.
#
n_clust = 3
d.train = fraud.train
d.test  = fraud.test
X.train = d.train[,1:7]
X.test  = d.test[,1:7]
Y.train = d.train[,8]
Y.test  = d.test[,8]
Yhat = c()
library(gower)
gdist = matrix(rep(0,nrow(d.train)*nrow(d.test)), nrow=nrow(d.train))
for (j in 1:nrow(d.test)) {
  gdist[,j] = gower_dist(X.test[j,], X.train)
  # Get k smallest distances
  Yhat[j] = names(sort(-table(Y.train[order(gdist[,j])[1:n_clust]])))[1]
}
performance(table(Yhat, Y.test))

#
# dprep provides an implementation of KNN with Gower distance listed below.
#
moda <- structure(function (x, na.rm = TRUE) {
    if (na.rm == TRUE) 
        m1 = rev(sort(table(x[])))
    else
        m1 = rev(sort(table(x, exclude = NULL)))
    moda = names(m1[m1 == m1[1]])
    if (is(x, "numeric")) 
        moda = as.numeric(moda)
    return(moda)
}, source = c("function(x,na.rm=TRUE)", "{", "  ", "#Function that finds the mode of vector x", 
"", "  if(na.rm==TRUE) m1=rev(sort(table(x[])))", "    else m1=rev(sort(table(x,exclude=NULL)))", 
"  moda=names(m1[m1==m1[1]])", "  if (is(x,\"numeric\")) moda=as.numeric(moda)", 
"  return(moda)", "}"))

knngow <- function (train,test,k) {
  p=dim(train)[2]
  ntest=dim(test)[1]
  ntrain=dim(train)[1]
  classes=rep(0,ntest)
  if(ntest==ntrain) {
    for(i in 1:ntest) {
      tempo = order(gower_dist(test[i,-p],train[-i,-p]))[1:k]
      classes[i] = moda(train[tempo,p])[1]
    }
  } else {
    for(i in 1:ntest) {
      tempo = order(gower_dist(test[i,-p],train[,-p]))[1:k] 
      classes[i] = moda(train[tempo,p])[1]}
  }
  classes
}

yhat2 = knngow(fraud.train, fraud.test, k=n_clust)
performance(table(Yhat, Y.test))



# -------------------------------------------------------------------
#  Case Study 1 : Using Fraud data using Weighted kNN with
#  different kernels and Euclidean distance for Prediction & Estimation
# -------------------------------------------------------------------

#
# Loading Fraud data and applying data preparation as in Practical 3
#
fraud = read.csv("fraud.csv")
col_fac = c("gender", "status", "employment", "account_link", "supplement", "tag")
fraud[col_fac] = lapply(fraud[col_fac], factor)
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
#  Case Study 1 : Applying kNN Regressor to a Regression Problem
#  using the "Boston" Dataset for Prediction and Estimation
#
#  Outcome: Getting familiar with underfitting and overfitting issues
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

#
# (1) Performance Measurement for Regression Problems using
#     RMSE (Root Mean Sum of Square Error)
# (2) Hyperparameter analysis of kNN's k using the Holdout Method
#

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


