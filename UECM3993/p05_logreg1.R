# -------------------------------------------------------------------
# Purpose: Practical for Logistic Regression Models in R (Part 1)
# Author : Liew How Hui (2025)
# Reference: 
#  1. http://faculty.marshall.usc.edu/gareth-james/ISL/Chapter%204%20Lab.txt
# Data   : http://faculty.marshall.usc.edu/gareth-james/ISL/data.html
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
#  Practical : Logistic Regression Analysis of the `Smarket' Dataset
#
#  Exercise: Analysis of the `Weekly' data set using Logistic Regression
#  Note: `Weekly' data set is more or less the same as the `Smarket' (daily)
# -------------------------------------------------------------------

library(ISLR2)   # For Smarket data

### 
### Working with Smarket data (refer to Practical 3)
### 
train = (Smarket$Year < 2005)
Smarket.test = Smarket[!train,]
Direction.test = Smarket.test$Direction

### Fit the train set into logistic regression (parametric predictive model)
# Output / Target / Response = Direction (Interested to see how price go)
# Inputs / Factors / Predictors / Independent Variables = various variations
logreg.fits = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
                  data=Smarket, subset=train, family=binomial)
### Equivalent expression
#logreg.fits = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
#                  data=Smarket[train,], family=binomial)
summary(logreg.fits)

### Apply model into validation set and predict the probability to be Class 1
# Response = P(Y=1 | X=x) = 1/(1 + exp(-(DefaultT)))
# DefaultT = beta0 + beta1*x1 + ... + betap*xp
logreg.probs = predict(logreg.fits, newdata=Smarket.test, type="response")
# contrasts is used for the construction of one-hot encoding
contrasts(Smarket$Direction)    # To show the value (1/0) for level (Up/Down)
### Make prediction based on the probability computed (>=0.5 is Up)
predicted = ifelse(logreg.probs < 0.5, "Down", "Up")

# logreg.probs[2] same as
# 1/(1+exp(-sum(coef(logreg.fits) * c(1,unlist(Smarket.test[2,2:7])))))

### Construct confusion matrix and performance measures
cfmat  = table(predicted,Direction.test)
performance(cfmat, "Performance of Logistic Regression Model on Smarket Data")


# -------------------------------------------------------------------
#  Practical : Analysis of the `Fraud' Dataset using Logistic Regression
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
tag0_idx = sample(nrow(fraud_tag0), size=0.7*nrow(fraud_tag0))
tag1_idx = sample(nrow(fraud_tag1), size=0.7*nrow(fraud_tag1))
fraud.train = rbind(fraud_tag0[ tag0_idx,],fraud_tag1[ tag1_idx,])
fraud.test  = rbind(fraud_tag0[-tag0_idx,],fraud_tag1[-tag1_idx,])
summary(fraud.test)

###
### logistic regression
### Data preprossing may not be necessary because the coefficients
### may auto-adjust.  However, we still need to be careful when 
### optimisation converges badly.
###
logreg_model = glm(tag~., data=fraud.train, family=binomial)
print(summary(logreg_model))

#
# There are two ways to perform classification.  Here, we follow 
# the lecture slides using the conditional probability (type='response')
#
fraud.test.prob = predict(logreg_model, newdata=fraud.test[ ,1:7], type='response')
yhat = ifelse(fraud.test.prob < 0.5, "pred_0", "pred_1")
cfmat = table(yhat, fraud.test$tag)
performance(cfmat, "Performance of the Logistic Regression Model")


# -------------------------------------------------------------------
#  Practical : Analysis of the standardised `Fraud' Dataset using 
#  Logistic Regression.  The Logistic Regression trained on 
#  the unscaled Fraud data has an interesting comparison to 
#  the Logistic Regression with the scaled Fraud data.
# -------------------------------------------------------------------

summary(fraud)

fraud.tran.std     = fraud.train
fraud.test.std     = fraud.test
# Standardisation
mu_age = mean(fraud.train$age)
si_age = sd(  fraud.train$age)
fraud.tran.std$age = scale(fraud.tran.std$age,mu_age,si_age)[,1]
fraud.test.std$age = scale(fraud.test.std$age,mu_age,si_age)[,1]

mu_bsv = mean(fraud.train$base_value)
si_bsv = sd(  fraud.train$base_value)
fraud.tran.std$base_value = scale(fraud.tran.std$base_value, mu_bsv, si_bsv)[,1]
fraud.test.std$base_value = scale(fraud.test.std$base_value, mu_bsv, si_bsv)[,1]

model.for.scaleddata = glm(tag~., data=fraud.tran.std, family=binomial)
print(summary(model.for.scaleddata))

#
# Logit can be used instead of probability in prediction
#
fraud.test.logit = predict(model.for.scaleddata, fraud.test.std[,1:7])
yhat = ifelse(fraud.test.logit < 0, "pred_0", "pred_1")
cfmat = table(yhat, fraud.test$tag)
performance(cfmat, "Performance of the Logistic Regression Model")


