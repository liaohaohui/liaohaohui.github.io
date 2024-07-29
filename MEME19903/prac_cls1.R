# ===================================================================
# Purpose: Practical for Working with Logistic Regression Model 
#          (and its extensions) for Classification Problems in R
# Author : Liew How Hui (2024)
# Reference & Data: 
#  1. https://www.statlearning.com/resources-second-edition
# License: BSD-3
# Software: R 4.x
# Duration: 1 hour
# Data1: Default (from ISLR2, simulated default data on credit card)
# ===================================================================

cat("
# -------------------------------------------------------------------
#  Part 1: Trying to understand the theory --- finding coefficients
#  (Lecture Slide Example 2)
# -------------------------------------------------------------------
")

### Install the ISLR2 library if the loading fails
#install.packages("ISLR2")
library(ISLR2)

# Default data is very imbalance: 97% No, 3% Yes
data.No  = Default[Default$default=="No", ]
data.Yes = Default[Default$default=="Yes",]
set.seed(3)
# Taking from Default 7 data with No as output, 3 data with Yes as output
mydata = rbind(data.No[sample(nrow(data.No),7),], 
               data.Yes[sample(nrow(data.Yes),3),])
m = glm(default ~ balance, mydata, family=binomial)
x = mydata$balance
y = ifelse(mydata$default=="No",0,1)  # Convert to binary
coeff.beta = m$coefficients

# Compare to Equation (4) in s41_logreg.pdf
lnL = function(betas) {
  beta0 = betas[1]
  beta1 = betas[2]
  sum(y*(beta0 + beta1*x)) - sum(log(1+exp(beta0 + beta1*x)))
}
# Max lnL = Min (-LnL)
neg.lnL = function(betas) { -lnL(betas) }
#res = optim(c(1,0), neg.lnL, method="BFGS")
res = optim(c(1,0), neg.lnL)
if (res$convergence == 0) {
  print(res$par)
  # compare to coeff.beta
}

null.neg.lnL = function(beta0) {
  - (sum(y*beta0) - length(y)*(log(1+exp(beta0))))
}

res.null = optim(1, null.neg.lnL, method="BFGS")
if (res.null$convergence == 0) {
  print(res.null$par)
}

# According to https://stats.stackexchange.com/questions/184753/in-a-glm-is-the-log-likelihood-of-the-saturated-model-always-zero
# Saturated model for LR with ungrouped data is 0
null.coeffs = rep(0,length(res$par))
null.coeffs[1] = res.null$par
cat("Null Deviance = 2(LL(saturated)-LL(null)) =", 2*(0-lnL(null.coeffs)), "\n")
cat("Residue Deviance = 2(LL(saturated)-LL(fitted)) =", 2*(0-lnL(res$par)), "\n")

print(summary(m))     # Summary of the fitted Logistic Regression model


cat("
# -------------------------------------------------------------------
#  Part 2: Working with Lecture's Example 4 (single numeric input)
#  Improvement in Deviance => Well fitted model
# -------------------------------------------------------------------
")

lr.fit = glm(default ~ balance, data=Default, family=binomial)
print(summary(lr.fit))

#
# The null deviance is larger than residual deviance, it seems
# that the logistic regression model with 1 input fits the data quite well.
#

#
# We can try to find the contingency table for the training data.
#
prob1  = predict(lr.fit, Default, type="response")   # P(Y=1|balance)
# If we don't specify type="response", we will get link, i.e.
# ln( P(Y=1|balance)/P(Y=0|balance) )
link1  = predict(lr.fit, Default)
pred   = ifelse(prob1>=0.5, 1, 0)
actual = Default$default
compare = data.frame(pred, actual)  # Compare like Excel
print(head(compare))     # Show the first few rows
print(tail(compare))     # Show the last few rows
print(table(compare))    # Ask R to summarise the comparison

#
# The fitting below is actually not very good since specificity is low.
#
#      actual
#  pred   No  Yes
#     0 9625  233
#     1   42  100
#

#
# Do you know how to calculate the conditional probabilities using R?
#
# (b) P(default=1|balance=1000) = 0.005752145
#     P(default=1|balance=2000) = 0.5857694
#

cat("
# -------------------------------------------------------------------
#  Part 3: Working with Lecture's Example 5 (single categorical input)
#  Lack of Improvement in Deviance => Poorly fitted model
#
# The small difference between null deviance and residual deviance
# suggest the model to be a poor fit despite the p-values
# Pr(>|z|) are small:
#
# Call:
# glm(formula = default ~ student, family = binomial, data = Default)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -0.2970  -0.2970  -0.2434  -0.2434   2.6585  
# 
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -3.50413    0.07071  -49.55  < 2e-16 ***
# studentYes   0.40489    0.11502    3.52 0.000431 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 2920.6  on 9999  degrees of freedom
# Residual deviance: 2908.7  on 9998  degrees of freedom
# AIC: 2912.7
# 
# Number of Fisher Scoring iterations: 6
#
# -------------------------------------------------------------------
")

# First, make sure the input is categorical by either:
class(Default$student)
summary(Default$student)

lr.fit = glm(default ~ student, data=Default, family=binomial)
print(summary(lr.fit))

#
# Do you know why we have a new variable studentYes?  Where is 
# the variable student?
#


#
# We can see the terrible specificity despite the accuracy is OK.
#
#      actual
#  pred   No  Yes
#     0 9667  333
#
# This confirms that the LR is a poor model for default ~ student.
#

prob1  = predict(lr.fit, Default, type="response")   # P(Y=1|student)
pred   = ifelse(prob1>=0.5, 1, 0)
actual = Default$default
compare = data.frame(pred, actual)  # Compare like Excel
print(head(compare))     # Show the first few rows
print(tail(compare))     # Show the last few rows
print(table(compare))    # Ask R to summarise the comparison

#
# The P(default=1 | student = Yes) can be calculated by
#
predict(lr.fit, data.frame(student="Yes"), type="response")
#
# Do you know how to calculate P(default=1 | student = Yes)?
# Do you know how to calculate using mathematical formula?  You need
# to know because it will come out in exam.
#

cat("
# -------------------------------------------------------------------
#  Part 4: Logistic Regression Analysis of the ISLR2's `Smarket' Dataset
#  (unpredictable data) following the main reference book
# -------------------------------------------------------------------
")

### Univariate Analysis of Tabular Dataset
#View(Smarket)       # From ISLR
dim(Smarket)         # Check the dimension of the data
names(Smarket)       # Show the column names
summary(Smarket)     # Except for the 1st & last columns, the rests are numerics
par(mfrow=c(2,4))
for (column in 1:8) {    # Exclude the output Direction which is not numeric
  hist(Smarket[,column], main=names(Smarket)[column], xlab="")
}
par(mfrow=c(1,1))

### Bivariate Analysis of Tabular Dataset
# (1) Numeric statistical analysis => correlation coefficients, cor()
# (2) Visual statistics => pairs = scatter plots of every pair of the columns

#cor(Smarket)        # Won't work: Direction is categorical
cor(Smarket[,-9])    # Remove variable Direction
pairs(Smarket)       # Scatter plots of all columns
# Pair plot is only OK when we have less than 15 or so numeric columns

# Focusing into absolute correlation >=0.5 using scatter plot:
# E.g. Year and Volume
plot(Smarket$Year,Smarket$Volume)

### Time series data are always split into `past' and `present' !!!
### Linear sampling and stratified sampling are not applicable.
### train set = data from Year 2001-2004
### validation set = data from Year 2005
train = (Smarket$Year < 2005)     # For Boolean selection
Smarket.2005 = Smarket[!train,]   # !TRUE == FALSE
Y.2005 = Smarket.2005$Direction

### Fit the train set into logistic regression (parametric predictive model)
# Output / Target / Response = Direction (Interested to see how price go)
# Inputs / Factors / Predictors / Independent Variables = various variations
#
# Why no 'Today'?  Because Up / Down is BASED on Today & they are correlated
#
lr.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
                  data=Smarket[train,], family=binomial)
summary(lr.fit)
anova(lr.fit)      # analysis of variance of input in sequential order

### Apply model into validation set and predict the probability to be Class 1
# Response = P(Y=1 | X=x) = 1/(1 + exp(-(DefaultT)))
# DefaultT = beta0 + beta1*x1 + ... + betap*xp
# 1 = Up; 0 = Down
probs.of.Y1 = predict(lr.fit, newdata=Smarket.2005, type="response")

### Make prediction based on the probability computed (>=0.5 is Up)
yhat = ifelse(probs.of.Y1 >= 0.5, "Up", "Down")

### Construct confusion matrix and performance measures
cfmat  = table(yhat,Y.2005)

### We can use caret's confusionMatrix() to show the performance
### measurements for classification problem.  But the caret library
### has many dependencies, so I define a function using the definitions 
### from Week 1 to avoid the need to install too many R libraries.

performance = function(xtab, desc=""){
    cat(desc,"\n")
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

performance(cfmat, "Performance of Logistic Regression Model on Smarket Data")


cat("
# -------------------------------------------------------------------
#  Part 5: Logistic Regression Analysis of `Fraud' Dataset
#  (predictable data)
# -------------------------------------------------------------------
")

#https://liaohaohui.github.io/MEME19903/fraud.csv
fraud = read.csv("fraud.csv")
### change data type from integer to categorical
col_fac = c("gender", "status", "employment", "account_link", "supplement", "tag")
fraud[col_fac] = lapply(fraud[col_fac], factor)

#
# Manual stratified sampling
# Ref: https://stackoverflow.com/questions/23479512/stratified-random-sampling-from-data-frame
#
set.seed(123)
fraud_tag0 = fraud[fraud$tag=="0", ]
fraud_tag1 = fraud[fraud$tag=="1", ]
tag0_idx = sample(nrow(fraud_tag0), size=0.7*nrow(fraud_tag0))
tag1_idx = sample(nrow(fraud_tag1), size=0.7*nrow(fraud_tag1))
fraud.train = rbind(fraud_tag0[ tag0_idx,],fraud_tag1[ tag1_idx,])
fraud.test  = rbind(fraud_tag0[-tag0_idx,],fraud_tag1[-tag1_idx,])
summary(fraud.test)

### logistic regression (use the data without normalization)
logreg_m = glm(tag~.-id_person, data=fraud.train, family=binomial)
print(summary(logreg_m))

#
# Perform binary classification using conditional probability
#
fraud.test.prob = predict(logreg_m, newdata=fraud.test[ ,1:8], type='response')

yhat = ifelse(fraud.test.prob >= 0.5, "pred_1", "pred_0")
cfmat = table(yhat, fraud.test$tag)
performance(cfmat, "Performance of the Logistic Regression Model")


### When a model does not fit the data, the model should be discarded.

cat("
# -------------------------------------------------------------------
#  Part 6a: Model Comparison for ISLR's Default Data (Case Study 1)
# -------------------------------------------------------------------
")

m = glm(default ~ ., Default, family=binomial)
print(anova(m, test="Chisq"))

m0 = glm(default ~ 1, Default, family=binomial)   # Null Model
m1 = glm(default ~ student, Default, family=binomial)
m2 = glm(default ~ student + balance, Default, family=binomial)
m3 = glm(default ~ student + balance + income, Default, family=binomial)
print(anova(m0,m1,m2,m3,test="Chisq"))

cat("
# -------------------------------------------------------------------
#  Part 6b: Model Comparison for Fraud Data (nicely fitted)
# -------------------------------------------------------------------
")

reduced.m = glm(tag~ gender + status + employment + account_link + supplement, data=fraud.train[,2:9], family=binomial)
print(summary(reduced.m))
# What about removing employment as well?
rm2 = glm(tag~ gender + status + account_link + supplement, data=fraud.train[,2:9], family=binomial)
print(summary(rm2))

#
# Chisq, Rao tests are OK for binary output with p-value given.
# Cp is AIC when GLM is LR (deviance improvement, larger better)
#
anv0 = anova(rm2, reduced.m, logreg_m)
anv1 = anova(rm2, reduced.m, logreg_m, test="LRT")    # Likelihood Ratio Test
anv2 = anova(rm2, reduced.m, logreg_m, test="Chisq")  # Same as LRT for LR
anv3 = anova(rm2, reduced.m, logreg_m, test="Rao")
anv4 = anova(rm2, reduced.m, logreg_m, test="Cp")     # Cp == AIC

print(anv0)
cat("-----------------------------------------\n")
print(anv1)
cat("-----------------------------------------\n")
print(anv2)
cat("-----------------------------------------\n")
print(anv3)
cat("-----------------------------------------\n")
print(anv4)

cat("
# -------------------------------------------------------------------
#  Part 7: Multinomial LR is a Generalisation of LR (can work with
#  multiclass)
# -------------------------------------------------------------------
")

library(nnet)
mlr.model = multinom(tag ~ ., data=fraud.train[,2:9])
print(mlr.model)
print(summary(mlr.model))

# Basically multinom() witk K=2 is the same as glm() except Z-statistic 
# and p-values are not provided.

