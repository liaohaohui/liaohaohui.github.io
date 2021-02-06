# -------------------------------------------------------------------
# Purpose: Practical for Tree Ensemble Predictive Models in R
# Author : Liew How Hui (2021)
# References: 
#  1. https://daviddalpiaz.github.io/r4sl/ensemble-methods.html
#  2. http://faculty.marshall.usc.edu/gareth-james/ISL/Chapter%208%20Lab.txt
# Data   : http://faculty.marshall.usc.edu/gareth-james/ISL/data.html
# License: BSD-3
# Software: R 3.6 & R 4.0
# -------------------------------------------------------------------

# From p_knn.R
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
#  Analysis of the `credit_data' with RuleQuest C5.0 with ``Boosting''
# -------------------------------------------------------------------
")
library(modeldata)
data(credit_data)

set.seed(2411)
in_train   = sample(1:nrow(credit_data), size = 3000)
train_data = credit_data[ in_train,]
test_data  = credit_data[-in_train,]

library(C50)
# Depends on mvtnorm, libcoin, inum, Formula, partykit, Rcpp, plyr,
# stringi, glue, stringr, reshape2, Cubist.
#names(train_data)
C50tree_model = C5.0(x=train_data[, c("Home", "Seniority")], y=train_data$Status, trials=3)
summary(C50tree_model)
#plot(C50tree_model)  # depends on partykit for plotting

cat("
# -------------------------------------------------------------------
#  Analysis of the `Carseats' Dataset with Bagging and Random Forest
# -------------------------------------------------------------------
")
library(ISLR)

### Initial Exploration of the Dataset ``Carseats''
sapply(Carseats,class)
 
### Sales is the ``response'' variable with respect to the rest
### To turn it to a ``classification problem'', we need to discretise
### it: "High" = "Yes" if "Sales" > 8 and "No" if "Sales"<= 8
Carseats$High  = as.factor(ifelse(Carseats$Sales<=8,"No","Yes"))
Carseats$Sales = NULL  # To delete a column ``Sales'' from Carseats
sapply(Carseats,class)

### Validation Set Approach: split data into training and testing sets
set.seed(2)
cs_tag0 = Carseats[Carseats$High=="No", ]
cs_tag1 = Carseats[Carseats$High=="Yes", ]
tag0_idx = sample(1:nrow(cs_tag0), size=0.7*nrow(cs_tag0))
tag1_idx = sample(1:nrow(cs_tag1), size=0.7*nrow(cs_tag1))
Carseats.train = rbind(cs_tag0[ tag0_idx,],cs_tag1[ tag1_idx,])
Carseats.test  = rbind(cs_tag0[-tag0_idx,],cs_tag1[-tag1_idx,])
### Alternative way of splitting data:
#library(splitstackshape)
#Carseats.train <- stratified(Carseats,"High",size=0.7,keep.rownames=TRUE)
#Carseats.test  = Carseats[-as.integer(Carseats.train$rn),]
#Carseats.train = Carseats.train[,-c("rn")]

### Training Bagging Tree
suppressMessages(library(randomForest))  # dependencies: None
set.seed(1)
p = ncol(Carseats.train)-1
bag.carseats = randomForest(High~.,data=Carseats.train,mtry=p,importance=TRUE)
tree.pred = predict(bag.carseats,Carseats.test,type="class")
cf.mat.bag = table(tree.pred,Carseats.test$High)
performance(cf.mat.bag, "\n*** Bagging Tree Strategy (RF with m=p) Performace")

### Training Random Forest
set.seed(1)
rf.carseats = randomForest(High~.,data=Carseats.train,mtry=6,importance=TRUE)
tree.pred = predict(rf.carseats,Carseats.test,type="class")
cf.mat.rf = table(tree.pred,Carseats.test$High)
performance(cf.mat.rf, "\n*** Random Forest (RF) Strategy (m=6) Performance")

### https://www.displayr.com/how-is-variable-importance-calculated-for-a-random-forest/
cat("
After training a random forest, it is natural to ask which variables 
have the most predictive power. Variables with high importance are 
drivers of the outcome and their values have a significant impact on 
the outcome values.
")
print(importance(rf.carseats))
# Dotchart of variable importance as measured by a Random Forest
varImpPlot(rf.carseats)

cat("
# -------------------------------------------------------------------
#  Analysis of the `Carseats' Dataset with Boosting Strategies
# -------------------------------------------------------------------
")
### http://uc-r.github.io/gbm_regression
suppressMessages(library(gbm))   # dependencies: gtable, gridExtra
set.seed(1)
### For gbm with bernoulli distribution, the output is assumed to be
### logistic regression for 0-1 outcomes
Carseats.train$High2=ifelse(Carseats.train$High=="Yes",1,0)
boost.carseats = gbm(High2~.-High,data=Carseats.train,
  distribution="adaboost",n.trees=5000,interaction.depth=4)
#print(summary(boost.carseats))  # Summary of the Importance of Features
boost.probs = predict(boost.carseats,newdata=Carseats.test,type="response",
  n.trees=5000)
boost.pred = ifelse(boost.probs>=0.5,"Yes","No")
cf.mat.boost = table(boost.pred,Carseats.test$High)
performance(cf.mat.boost, "\n*** Adaptive Boosting Performance\n")

boost.carseats = gbm(High2~.-High,data=Carseats.train,
  distribution="bernoulli",n.trees=5000,interaction.depth=4)
#print(summary(boost.carseats))  # Summary of the Importance of Features
boost.probs = predict(boost.carseats,newdata=Carseats.test,type="response",
  n.trees=5000)
boost.pred = ifelse(boost.probs>=0.5,"Yes","No")
cf.mat.boost2 = table(boost.pred,Carseats.test$High)
performance(cf.mat.boost2, "\n*** Bernoulli Gradient Boosting Performance\n")

cat("
# -------------------------------------------------------------------
#  Analysis of the `Boston' Dataset with Regression Tree Ensemble Methods
# -------------------------------------------------------------------
")
library(MASS)  # Provides the Boston data

set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
boston.test = Boston[-train,"medv"]

cat("\n*** Bagging Tree Strategy (RF with m=p) Performace\n")
set.seed(1)
p = ncol(Boston)-1
### randomForest default to ntree=500
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=p,importance=TRUE)
bag.boston
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
cat("MSE(Baggining with ntree=500) =", mean((yhat.bag-boston.test)^2), "\n")
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=p,ntree=25)
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
cat("MSE(Baggining with ntree= 25) =", mean((yhat.bag-boston.test)^2), "\n")

cat("\n*** Random Forest (RF) Strategy (m=6) Performance\n")
set.seed(1)
rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=6,importance=TRUE)
yhat.rf = predict(rf.boston,newdata=Boston[-train,])
cat("MSE(Random Forest with m=6) =", mean((yhat.rf-boston.test)^2), "\n")
importance(rf.boston)
varImpPlot(rf.boston)

cat("\n*** Boosting Strategy (m=6) Performance\n")
set.seed(1)
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(boost.boston)
par(mfrow=c(1,2))
plot(boost.boston,i="rm")
plot(boost.boston,i="lstat")
yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
cat("MSE(Generalised Boosting with interaction.depth=1) = ",
  mean((yhat.boost-boston.test)^2), "\n")
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.2,verbose=F)
yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
cat("MSE(Generalised Boosting with interaction.depth=4) = ",
  mean((yhat.boost-boston.test)^2), "\n")

