# -------------------------------------------------------------------
# Purpose: Practical for Logistic Regression Models in R
# Author : Liew How Hui (2021)
# Reference: 
#  1. http://faculty.marshall.usc.edu/gareth-james/ISL/Chapter%204%20Lab.txt
# Data   : http://faculty.marshall.usc.edu/gareth-james/ISL/data.html
# License: BSD-3
# Software: R 3.6 & R 4.0
# -------------------------------------------------------------------

### Taken from p_knn.R
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

cat("
# -------------------------------------------------------------------
#  Logistic Regression Analysis of the `Smarket' Dataset
# -------------------------------------------------------------------
")

library(ISLR)  # the Stock Market Data from ISLR package

### Explore the dataset
#View(Smarket)
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
#cor(Smarket)        # Won't work, Direction is numeric
cor(Smarket[,-9])    # Remove variable Direction
# Higher correlation between Year and Volume
plot(Smarket$Year,Smarket$Volume)

### Split data into train set and validation set
### train set = data from Year 2001-2004
### validation set = data from Year 2005
train = (Smarket$Year < 2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005 = Smarket$Direction[!train]

### Fit the train set into logistic regression
logreg.fits = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
                  data=Smarket, family=binomial, subset=train)
summary(logreg.fits)

### Apply model into validation set and predict the probability to be Class 1
logreg.probs = predict(logreg.fits, newdata=Smarket.2005, type="response")
contrasts(Smarket$Direction)    # To show the value (1/0) for level (Up/Down)
### Make prediction based on the probability computed (>=0.5 is Up)
logreg.pred <- ifelse(logreg.probs < 0.5, "Down", "Up")

### Construct confusion matrix and performance measures
cfmat  = table(logreg.pred,Direction.2005)
performance(cfmat)

# -------------------------------------------------------------------
#  Exercise: Analysis of the `Weekly' Dataset using Logistic Regression glm
# -------------------------------------------------------------------

summary(Weekly)
pairs(Weekly)
cor(Weekly[, -9])
# ... build the logistic models ...

cat("
# -------------------------------------------------------------------
#  Analysis of the `Fraud' Dataset using Logistic Regression glm
# -------------------------------------------------------------------
")

set.seed(123)
fraud = read.csv("DataLab/fraud.csv")
### change data type from numerical to categorical
col_fac = c("gender", "status", "employment", "account_link", "supplement", "tag")
fraud[col_fac] = lapply(fraud[col_fac], factor)
sapply(fraud,class)

### stratified sampling --- Two methods
#library(splitstackshape)
#fraud.train = stratified(fraud,"tag",size=0.7,keep.rownames=TRUE)
#fraud.test  = fraud[-as.integer(fraud.train$rn),]
#fraud.train = as.data.frame(fraud.train[,-c("rn")])
### https://stackoverflow.com/questions/23479512/stratified-random-sampling-from-data-frame
fraud_tag0 = fraud[fraud$tag=="0", ]
fraud_tag1 = fraud[fraud$tag=="1", ]
tag0_idx = sample(nrow(fraud_tag0), size=0.7*nrow(fraud_tag0))
tag1_idx = sample(nrow(fraud_tag1), size=0.7*nrow(fraud_tag1))
fraud.train = rbind(fraud_tag0[ tag0_idx,],fraud_tag1[ tag1_idx,])
fraud.test  = rbind(fraud_tag0[-tag0_idx,],fraud_tag1[-tag1_idx,])
summary(fraud.test)

### logistic regression (use back the data without normalization)
logreg_model <- glm (tag ~ .-id_person, data=fraud.train, family = binomial)
summary(logreg_model)

fraud.test.prob = predict(logreg_model,
  newdata=subset(fraud.test,select=c(1:8)), type='response')
fraud.test.pred <- ifelse(fraud.test.prob >= 0.5,"pred_1","pred_0")
cfmat <- table(fraud.test.pred, fraud.test$tag)
performance(cfmat)

### Deployment: score new data (fraud_new.xlsx) using `best' LR model
fraud_new = read.csv("DataLab/fraud_new.csv")
col_fac_new <- c("gender", "status", "employment", "account_link", "supplement")
fraud_new[col_fac_new] <- lapply(fraud_new[col_fac_new], factor)
fraud_new.prob <- predict(logreg_model,fraud_new,type = "response")
fraud_new.pred <- ifelse(fraud_new.prob >= 0.5, "pred_1","pred_0")
fraud_new.result <- data.frame(fraud_new,round(fraud_new.prob,4),fraud_new.pred)

cat("
# -------------------------------------------------------------------
#  Analysis of the `Fraud' Dataset using Multinomial LR nnet::multinom
# -------------------------------------------------------------------
")

library(nnet)
mlr_model = multinom(tag ~ .-id_person, data=fraud.train)
summary(mlr_model)

yhat = predict(mlr_model, newdata=subset(fraud.test,select=c(1:8)),
  type='class')
cfmat <- table(yhat, fraud.test$tag)
performance(cfmat)

# -------------------------------------------------------------------
# https://stats.stackexchange.com/questions/72251/an-example-lasso-regression-using-glmnet-for-binary-outcome
# -------------------------------------------------------------------
library(glmnet)
# alpha = 1 (default) => lasso penalty, ||beta||_1  (better approximation)
# alpha = 0           => ridge penalty, ||beta||_2^2
# lambda -> 0 (close to original GLM)
#glmmod = glmnet(x=as.matrix(fraud.train[,2:8]), y=fraud.train[,9], alpha=0, family="binomial", standardize=FALSE)
X = model.matrix(tag ~ .-id_person, data=fraud.train)[,-1]
glmmod = glmnet(x=X, y=fraud.train[,9], alpha=1, family="binomial", lambda=1e-5)
print(coef(glmmod))
plot(glmmod)

# -------------------------------------------------------------------
#  Exercise: Analysis of the `Auto' Dataset
# -------------------------------------------------------------------

summary(Auto)
attach(Auto)
mpg01 = rep(0, length(mpg))
mpg01[mpg > median(mpg)] = 1
Auto = data.frame(Auto, mpg01)
cor(Auto[, -9])
pairs(Auto)
# ... build the logistic models ...


### https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/
cat("
# -------------------------------------------------------------------
#  Analysis of the `hsbdemo' Dataset using Multinomial LR nnet::multinom
# -------------------------------------------------------------------
")
# d.f = read.csv("https://stats.idre.ucla.edu/stat/data/hsbdemo.csv", stringsAsFactors=T)
d.f = read.csv("DataLab/hsbdemo.csv")
# Dummy variables `general', `vocation(al)' against `academic'
#d.f$prog2 <- relevel(d.f$prog, ref="academic")
d.f$prog2 <- d.f$prog
model1 <- multinom(prog2 ~ ses + write, data=d.f)
summary(model1)
z <- summary(model1)$coefficients/summary(model1)$standard.errors
# 2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
print(p)

