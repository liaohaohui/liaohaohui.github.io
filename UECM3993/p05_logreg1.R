# -------------------------------------------------------------------
# Purpose: Practical for Logistic Regression Models in R (Part 1)
# Author : Liew How Hui (2022)
# Reference: 
#  1. http://faculty.marshall.usc.edu/gareth-james/ISL/Chapter%204%20Lab.txt
# Data   : http://faculty.marshall.usc.edu/gareth-james/ISL/data.html
# License: BSD-3
# Software: R 3.6 & R 4.x & install.packages("ISLR2")
# Duration: 1 hour
# -------------------------------------------------------------------

library(ISLR2)

#
# We are not working with caret in Physical Lab because the dependencies
# in caret library is more than 20 and a lot of packages need to be
# downloaded manual which is a pain which can't be endured.
#
### caret provides `dummyVars' which is very difficult to achieve
### using plain R --- in such cases, it is reasonable to use `caret'
### But for the situation where simple R will do, don't install
### too many packages.
# One-hot encode --> retain only the features and not sale price
#full_rank = caret::dummyVars(Sale_Price~., data=???, fullRank=TRUE)

## Topic 3 page 21
#oneh = caret::dummyVars(~ student, data=Default, fullRank=TRUE)
#data_1hot = data.frame(predict(oneh, newdata=Default[,c("student"),drop=FALSE]))
## compare to Default$student
#print(names(data_1hot))
##data_1hot_scaled = as.data.frame(scale(data_1hot))
##dim(data_1hot_scaled)

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
#  Logistic Regression Analysis of the `Smarket' Dataset
# -------------------------------------------------------------------

### Explore the dataset
#View(Smarket)   # From ISLR
names(Smarket)   # or colnames
summary(Smarket) # Except for the 1st & last columns, the rests are numerics
pairs(Smarket)   # Practical 1: scatter plots of all columns
#cor(Smarket)        # Won't work, Direction is numeric
cor(Smarket[,-9])    # Remove variable Direction
# `Some' correlation between Year and Volume
plot(Smarket$Year,Smarket$Volume)

### Split data into train set and validation set
### For time series, we always split the data into `past' and `present'
### train set = data from Year 2001-2004
### validation set = data from Year 2005
train = (Smarket$Year < 2005)     # Practical 2
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)     # Check the dimension of the testing data table
Direction.2005 = Smarket.2005$Direction

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
logreg.probs = predict(logreg.fits, newdata=Smarket.2005, type="response")
# contrasts is used for the construction of one-hot encoding
contrasts(Smarket$Direction)    # To show the value (1/0) for level (Up/Down)
### Make prediction based on the probability computed (>=0.5 is Up)
predicted = ifelse(logreg.probs < 0.5, "Down", "Up")

### Construct confusion matrix and performance measures
cfmat  = table(predicted,Direction.2005)
performance(cfmat, "Performance of Logistic Regression Model on Smarket Data")

# -------------------------------------------------------------------
#  Exercise: Analysis of the `Weekly' data set using Logistic Regression
#  Note: `Weekly' data set is more or less the same as the `Smarket' (daily)
# -------------------------------------------------------------------

summary(Weekly)
pairs(Weekly)
cor(Weekly[, -9])
# ... build the logistic models ...
bool.idx = Weekly$Year < 2005   # Roughly 70%~75% vs 30%~25%
Weekly.train = Weekly[bool.idx, ]
lr.model = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, Weekly.train,
	family=binomial)
print(summary(lr.model))
# ... do the prediction & check the performance ...

# -------------------------------------------------------------------
#  Analysis of the `Fraud' Dataset using Logistic Regression glm
# -------------------------------------------------------------------

###
###  Manual stratified sampling using Base R & Standardising Fraud data
###  as in Practical 3
###

#https://liaohaohui.github.io/UECM3993/fraud.csv
fraud = read.csv("fraud.csv")
col_fac = c("gender", "status", "employment", "account_link", "supplement", "tag")
fraud[col_fac] = lapply(fraud[col_fac], factor)
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
logreg_model = glm(tag~.-id_person, data=fraud.train, family=binomial)
print(summary(logreg_model))

#
# There are two ways to perform classification.  Here, we follow 
# the lecture slides using the conditional probability (type='response')
#
fraud.test.prob = predict(logreg_model,
  newdata=subset(fraud.test,select=c(1:8)), type='response')
#fraud.test.prob = predict(logreg_model, newdata=fraud.test[ ,1:8], type='response')
yhat = ifelse(fraud.test.prob < 0.5, "pred_0", "pred_1")
cfmat = table(yhat, fraud.test$tag)
performance(cfmat, "Performance of the Logistic Regression Model")

# -------------------------------------------------------------------
#  Analysis of the standardised `Fraud' Dataset using Logistic Regression
#  We now working with `scaled' data just to compare it to kNN models.
# -------------------------------------------------------------------

summary(fraud)

normalise.vec <- function(column,ref.col) {
    return ((column - mean(ref.col)) / sd(ref.col))
}
fraud.tran.std     = fraud.train
fraud.test.std     = fraud.test
#fraud.tran.std$age = normalise.vec(fraud.tran.std$age, fraud.train$age)
#fraud.test.std$age = normalise.vec(fraud.test.std$age, fraud.train$age)
mu_age = mean(fraud.train$age)
si_age = sd(  fraud.train$age)
fraud.tran.std$age = scale(fraud.tran.std$age,mu_age,si_age)[,1]
fraud.test.std$age = scale(fraud.test.std$age,mu_age,si_age)[,1]

mu_bsv = mean(fraud.train$base_value)
si_bsv = sd(  fraud.train$base_value)
fraud.tran.std$base_value = scale(fraud.tran.std$base_value, mu_bsv, si_bsv)[,1]
fraud.test.std$base_value = scale(fraud.test.std$base_value, mu_bsv, si_bsv)[,1]

model.for.scaleddata = glm(tag~., data=fraud.tran.std[,2:9], family=binomial)
print(summary(model.for.scaleddata))

#
# Logit can be used instead of probability in prediction
#
fraud.test.logit = predict(model.for.scaleddata, fraud.test.std[,2:8])
yhat = ifelse(fraud.test.logit < 0, "pred_0", "pred_1")
cfmat = table(yhat, fraud.test$tag)
performance(cfmat, "Performance of the Logistic Regression Model")


