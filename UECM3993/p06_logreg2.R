# -------------------------------------------------------------------
# Purpose: Application using Statistical Software (Part 2)
# Detail: Case Study 1: Prediction and Estimation with Logistic Regression
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
#  Case Study 1 : Prediction and Estimation of the `Fraud' Dataset 
#  using Multinomial LR nnet::multinom
# -------------------------------------------------------------------

### 
### Working with Fraud data (refer to Practical 3)
### 

#https://liaohaohui.github.io/UECM3993/fraud.csv
fraud = read.csv("fraud.csv", row.names=1)
col_fac = c("gender", "status", "employment", "account_link", "supplement", "tag")
fraud[col_fac] = lapply(fraud[col_fac], factor)
set.seed(123)
fraud_tag0 = fraud[fraud$tag=="0", ]
fraud_tag1 = fraud[fraud$tag=="1", ]
tag0_idx = sample(nrow(fraud_tag0), size=round(0.7*nrow(fraud_tag0)))
tag1_idx = sample(nrow(fraud_tag1), size=round(0.7*nrow(fraud_tag1)))
fraud.train = rbind(fraud_tag0[ tag0_idx,], fraud_tag1[ tag1_idx,])
fraud.test  = rbind(fraud_tag0[-tag0_idx,], fraud_tag1[-tag1_idx,])

###
### Multinomial Logistic Regression can be regarded as ANN with single layer,
### i.e. no hidden layer.
###
library(nnet)   # Already in Base R
mlr_model = multinom(tag ~ ., data=fraud.train)
###
### Z-statistic & p-values are not provided.  We can obtain them
### using concepts from statistics
###
model.stats = summary(mlr_model)
# coef(mlr_model), mlr_model$wts, mlr_model$coefnames
betas = model.stats$coefficients
SE    = model.stats$standard.errors
Z     = betas/SE
# 2-tailed Z test
p     = (1 - pnorm(abs(Z), 0, 1)) * 2
print(p)
#
# Note: p-value may not be meaningful when there are more than two
# classes because there is no Hypothesis Testing for multiple class case.
#

#
# Similar to glm with family=binomial (logistic regression) but
# not exact due to numeric approximation errors
#
yhat = predict(mlr_model, newdata=fraud.test, type='class')
performance(table(yhat, fraud.test$tag), "Multinomial LR with K=2")
# K = number of classes in the output

#
# Comparing the multinom results to GLM model results (from Practical 5)
#
lr_model = glm(tag~., data=fraud.train, family=binomial)
summary(lr_model)
probs = predict(lr_model, newdata=fraud.test[,1:8], type='response')
yhat = ifelse(probs<0.5, 0, 1)
performance(table(yhat, fraud.test$tag), "LR (GLM with binomial) with K=2")


# -------------------------------------------------------------------
#  Case Study 1 : Prediction and Estimation of the `Fraud' Dataset 
#  using 1-hidden layer Neural Network nnet().
#
#  According to
#  https://stackoverflow.com/questions/64835527/plot-neural-network-of-nnet-object
#  we can visualise it using NeuralNetTools but this package
#  depends on ggplot2() and has many dependencies and will be skipped in 
#  the practical class.
# -------------------------------------------------------------------

set.seed(123)    # For the random initial weights
the.hidden.size = 20
nnm = nnet(tag~., data=fraud.train, size=the.hidden.size)
summary(nnm)

yhat = predict(nnm, newdata=fraud.test, type='class')
performance(table(yhat, fraud.test$tag), paste0("NN(hidden=",the.hidden.size,")"))

###https://beckmw.wordpress.com/2013/11/14/visualizing-neural-networks-in-r-update/
# install.packages("NeuralNetTools")
# library(NeuralNetTools)
# plotnet(nnm)


# -------------------------------------------------------------------
#  Case Study 1 : Prediction and Estimation of the `Iris flower' 
#  Dataset (K = 3 classes) using Multinomial LR nnet::multinom
# -------------------------------------------------------------------

# Since the ratio is 50:50:50, we can do linear sampling
set.seed(123)
idx = sample(nrow(iris), 0.7*nrow(iris))
iris.train = iris[ idx, ]
iris.test  = iris[-idx, ]

mlr_model2 = multinom(Species ~ ., iris.train)
summary(mlr_model2)
#probs = predict(mlr_model2, newdata=iris.test, type='probs')
yhat2 = predict(mlr_model2, newdata=iris.test, type='class')
# performance() only works for binary classification
confusion.matrix = table(yhat2, iris.test$Species)
performance(confusion.matrix)

#
# glm() won't work correctly for data with more than 2 classes as 
# it only take two classes
#


# -------------------------------------------------------------------
#  Case Study 1 : Prediction and Estimation of the `Iris flower' 
#  data using 1-hidden layer Neural Network nnet().
# -------------------------------------------------------------------

set.seed(123)    # For the random initial weights
the.hidden.size = 20
nnm = nnet(Species~., data=iris.train, size=the.hidden.size)

yhat = predict(nnm, newdata=iris.test, type='class')
performance(table(yhat, iris.test$Species), paste0("NN(hidden=",the.hidden.size,")"))


# -------------------------------------------------------------------
#  Case Study 1 : Prediction and Estimation of the `Fraud' Dataset 
#  using ElasticNet
#
#  ElasticNet = GLM + Constraints on the coefficients b0, b1, ..., bp
#  Combining `feature selection' & `parameter estimation'
#  Can be useful for `feature selection' when alpha=1.
#  Supported by GLMNET package
#  Standardisation is necessary when using GLMNET to make sure
#  the constraint part has reasonable scales.
#
#  Ref: https://stats.stackexchange.com/questions/72251/an-example-lasso-regression-using-glmnet-for-binary-outcome
# -------------------------------------------------------------------

# install.packages("glmnet")
library(glmnet)

#
# Problem: Categorical inputs are regarded as integers
#
# alpha = 1 (default) => lasso penalty, ||beta||_1  (feature selection)
# alpha = 0           => ridge penalty, ||beta||_2^2 (smoothing ???)
# lambda -> 0 (close to original GLM)
# Problem with directly using fraud.train with glmnet => factors are convert to integers
#glmmod = glmnet(x=as.matrix(fraud.train[,2:8]), y=fraud.train[,9], alpha=0, family="binomial", standardize=FALSE)
#
# model.matrix : one-hot encoding
# Just being lazy: no standardization.  Need to include it in future.
#
# remove the first column with `intersection' data
X = model.matrix(tag~., data=fraud.train)[,-1]
glmmod = glmnet(x=X, y=fraud.train[,8], alpha=1, family=binomial, lambda=1e-5)
print(coef(glmmod))   # Same as glmmod$beta
# Close but not exactly the same as Logistic Regression due to numerical errors

#
# When lambda is not set, it is a sequence
#
glmmod = glmnet(x=X, y=fraud.train[,8], alpha=1, family="binomial")
plot(glmmod, lwd=5)   # The x-axis defaults to L1-Norm
plot(glmmod, lwd=5, xvar="lambda")


