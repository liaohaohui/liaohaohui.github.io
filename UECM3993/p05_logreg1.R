# -------------------------------------------------------------------
# Purpose: Practical for Logistic Regression Models in R (Part 1)
# Author : Liew How Hui (2022)
# Reference: 
#  1. http://faculty.marshall.usc.edu/gareth-james/ISL/Chapter%204%20Lab.txt
# Data   : http://faculty.marshall.usc.edu/gareth-james/ISL/data.html
# License: BSD-3
# Software: R 3.6 & R 4.x & install.packages("ISLR")
# Duration: 1 hour
# -------------------------------------------------------------------

# Topic 1 Slide
    oneh = caret::dummyVars( ~ ., data=df)
    final_df = data.frame(predict(oneh, newdata=df))

### caret provides `dummyVars' which is very difficult to achieve
### using plain R --- in such cases, it is reasonable to use `caret'
### But for the situation where simple R will do, don't install
### too many packages.
# One-hot encode --> retain only the features and not sale price
full_rank = caret::dummyVars(Sale_Price~., data=ames_full, fullRank=TRUE)
ames_1hot = predict(full_rank, ames_full)
ames_1hot_scaled = as.data.frame(scale(ames_1hot))
# Note: ames_1hot[,"Neighborhood.Hayden_Lake"] are all zero,
# after scaling, it becomes NaN!
ames_1hot_scaled$Neighborhood.Hayden_Lake = NULL
dim(ames_1hot_scaled)  # Large dimensions than the original data `ames_full'


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

cat("
# -------------------------------------------------------------------
#  Logistic Regression Analysis of the `Smarket' Dataset
# -------------------------------------------------------------------
")

library(ISLR)  # the Stock Market Data from ISLR package

### Explore the dataset
#View(Smarket)
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

cat("
# -------------------------------------------------------------------
#  Analysis of the `Fraud' Dataset using Logistic Regression glm
# -------------------------------------------------------------------
")

fraud = read.csv("DataLab/fraud.csv")
### change data type from numerical to categorical
col_fac = c("gender", "status", "employment", "account_link", "supplement", "tag")
fraud[col_fac] = lapply(fraud[col_fac], factor)
sapply(fraud,class)   # or summary(fraud)

### Stratified sampling --- Three methods from Practical Lab 2
set.seed(123)
#library(caTools)
# ...
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
logreg_model = glm(tag~.-id_person, data=fraud.train, family=binomial)
summary(logreg_model)

fraud.test.prob = predict(logreg_model,
  newdata=subset(fraud.test,select=c(1:8)), type='response')
#fraud.test.prob = predict(logreg_model, newdata=fraud.test[ ,1:8], type='response')
yhat = ifelse(fraud.test.prob < 0.5, "pred_0", "pred_1")
cfmat = table(yhat, fraud.test$tag)
performance(cfmat, "Performance of the Logistic Regression Model")

### Deployment: score new data (fraud_new.xlsx) using `best' LR model
fraud_new = read.csv("DataLab/fraud_new.csv")
col_fac_new <- c("gender", "status", "employment", "account_link", "supplement")
fraud_new[col_fac_new] <- lapply(fraud_new[col_fac_new], factor)
fraud_new.prob <- predict(logreg_model,fraud_new,type = "response")
fraud_new.pred <- ifelse(fraud_new.prob < 0.5, "pred_0", "pred_1")
fraud_new.result <- data.frame(fraud_new,round(fraud_new.prob,4),fraud_new.pred)


