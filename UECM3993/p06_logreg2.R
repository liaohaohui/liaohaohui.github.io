# -------------------------------------------------------------------
# Purpose: Practical for Logistic Regression Models in R (Part 2)
# Author : Liew How Hui (2022)
# Reference: 
#  1. http://faculty.marshall.usc.edu/gareth-james/ISL/Chapter%204%20Lab.txt
# Data   : http://faculty.marshall.usc.edu/gareth-james/ISL/data.html
# License: BSD-3
# Software: R 3.6 & R 4.x & install.packages(c("glmnet","ISLR"))
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
    cat("\n      Accuracy :", ACR, "\n\n         Kappa :", Kappa, "\n")
    cat("\n   Sensitivity :", TPR,   "\n   Specificity :", TNR, "\n")
    cat("Pos Pred Value :", PPV,     "\nNeg Pred Value :", NPV, "\n")
    cat("           FPR :", FPR,     "\n           FNR :", FNR, "\n")
}

# -------------------------------------------------------------------
# Loading and transforming Fraud data
# -------------------------------------------------------------------

fraud = read.csv("DataLab/fraud.csv")
col_fac = c("gender", "status", "employment", "account_link", "supplement", "tag")
### change data type from numeric to categorical
fraud[col_fac] = lapply(fraud[col_fac], factor)
set.seed(123)
# Option 2 Stratified sampling
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

#
# May be in future consider comparing to feature-scaled data
#

#cat("
# -------------------------------------------------------------------
#  Analysis of the `Fraud' Dataset using Multinomial LR nnet::multinom
# -------------------------------------------------------------------
#")

library(nnet)   # Already inside Base R
# Multinomial Logistic Regression can be regarded as ANN with single layer
mlr_model = multinom(tag ~ .-id_person, data=fraud.train)
summary(mlr_model)
# Z-statistic & p-values are not provided.  From down below
#z = summary(mlr_model)$coefficients/summary(mlr_model)$standard.errors
#p = (1 - pnorm(abs(z), 0, 1)) * 2

yhat = predict(mlr_model, newdata=subset(fraud.test,select=c(1:8)),
  type='class')
performance(table(yhat, fraud.test$tag), "Multinomial LR with K=2")
# K = number of classes in the output

#
# Include an example of ANN in future?
# Problem: ANN takes super long time to calculate for any useful applications
#

# -------------------------------------------------------------------
# https://stats.stackexchange.com/questions/72251/an-example-lasso-regression-using-glmnet-for-binary-outcome
# -------------------------------------------------------------------
library(glmnet)   # install.packages("glmnet")
#
# GLMNET -> blending `feature selection' & `parameter estimation'
# Problem: Categorical inputs are regarded as integers
#
# alpha = 1 (default) => lasso penalty, ||beta||_1  (feature selection)
# alpha = 0           => ridge penalty, ||beta||_2^2 (smoothing ???)
# lambda -> 0 (close to original GLM)
#glmmod = glmnet(x=as.matrix(fraud.train[,2:8]), y=fraud.train[,9], alpha=0, family="binomial", standardize=FALSE)
X = model.matrix(tag ~ .-id_person, data=fraud.train)[,-1]
glmmod = glmnet(x=X, y=fraud.train[,9], alpha=1, family="binomial", lambda=1e-5)
print(coef(glmmod))   # Same as glmmod$beta
# Close but not exactly the same as Logistic Regression due to numerical errors

glmmod = glmnet(x=X, y=fraud.train[,9], alpha=1, family="binomial")
plot(glmmod)

# -------------------------------------------------------------------
#  Exercise: Analysis of the `Auto' Dataset
# -------------------------------------------------------------------

library("ISLR")
summary(Auto)   # From ISLR, n=392, p=9 (it is a cleanup of Auto.data)
attach(Auto)    # We have seen it in Practical 1
mpg01 = rep(0, length(mpg))
mpg01[mpg > median(mpg)] = 1
Auto = data.frame(Auto, mpg01)
cor(Auto[, -9])
pairs(Auto)
# ... build the logistic models, multinomial LR, ...


### https://stats.idre.ucla.edu/stata/dae/multinomiallogistic-regression/
### No longer available: https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/
cat("
# -------------------------------------------------------------------
#  Analysis of the `hsbdemo' Dataset using Multinomial LR nnet::multinom
# -------------------------------------------------------------------
")
# d.f = read.csv("https://stats.idre.ucla.edu/stat/data/hsbdemo.csv", stringsAsFactors=T)
d.f = read.csv("DataLab/hsbdemo.csv", stringsAsFactors=TRUE)
# Dummy variables `general', `vocation(al)' against `academic'
#d.f$prog2 <- relevel(d.f$prog, ref="academic")
#model1 <- multinom(prog2 ~ ses + write, data=d.f)
#
# Target `prog' has 3 levels, so glm(..., family=binomial) is not working
# correctly.  The prediction of glm is giving us only 0 (academic) &
# 1 (general).
#
# prog = program type
# ses = social economic status
# write = writing score
#
model1 = multinom(prog ~ ses + write, data=d.f)
summary(model1)
z <- summary(model1)$coefficients/summary(model1)$standard.errors
# 2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
print(p)

