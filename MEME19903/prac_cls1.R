# -------------------------------------------------------------------
# Purpose: Practical for Working with Logistic Regression Model 
#          (and its extensions) for Classification Problems in R
# Author : Liew How Hui (2023)
# Reference & Data: 
#  1. https://www.statlearning.com/resources-second-edition
# License: BSD-3
# Software: R 4.x
# Duration: 1 hour
# -------------------------------------------------------------------

#install.packages("ISLR2")
library(ISLR2)

#
# Working with Example 2 from Lecture Slide s41_logreg.pdf
#
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

#
# Working with Qualitative Predictors (Example 3) from 
# Lecture Slide s41_logreg.pdf
#

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

#
# Relating categorical variables with dummy variables
#
# (a) Original Data with Original Variables
head(Default)
# (B) Original Categorical Variables are Transformed to Dummy Variables
head(model.matrix(default ~ ., Default))


# -------------------------------------------------------------------
# Performance Measures for Classification
# -------------------------------------------------------------------

# A more sophisticated implementation is caret::confusionMatrix
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

# -------------------------------------------------------------------
#  Logistic Regression Analysis of the ISLR2's `Smarket' Dataset
# -------------------------------------------------------------------

### Explore the dataset
#View(Smarket)       # From ISLR
names(Smarket)       # or colnames
summary(Smarket)     # Except for the 1st & last columns, the rests are numerics
#
# pairs = scatter plots of every pair of the columns
# Purpose: try to rule out correlation 
# => correlation is BAD for Logistic Regression
#
pairs(Smarket)       # Scatter plots of all columns
# Pair plot is only OK when we have less than 15 or so numeric columns
#cor(Smarket)        # Won't work, Direction is numeric
cor(Smarket[,-9])    # Remove variable Direction
#
# Correlation can be visualised using HeatMap.
#
# `Some' correlation between Year and Volume
plot(Smarket$Year,Smarket$Volume)

### Split data into train set and validation set
### For time series, we always split the data into `past' and `present'
### train set = data from Year 2001-2004
### validation set = data from Year 2005
train = (Smarket$Year < 2005)     # For Boolean selection
Smarket.2005 = Smarket[!train,]   # !TRUE == FALSE
dim(Smarket.2005)     # Check the dimension of the testing data table
Y.2005 = Smarket.2005$Direction

### Fit the train set into logistic regression (parametric predictive model)
# Output / Target / Response = Direction (Interested to see how price go)
# Inputs / Factors / Predictors / Independent Variables = various variations
#
# Why no 'Today'?  Because Up / Down is BASED on Today & they are correlated
#
#lr.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
#             data=Smarket, subset=train, family=binomial)
### Equivalent expression
lr.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
                  data=Smarket[train,], family=binomial)
summary(lr.fit)    # Need to understand for final exam

### Apply model into validation set and predict the probability to be Class 1
# Response = P(Y=1 | X=x) = 1/(1 + exp(-(DefaultT)))
# DefaultT = beta0 + beta1*x1 + ... + betap*xp
Y.probs = predict(lr.fit, newdata=Smarket.2005, type="response")
# contrasts is used for the construction of one-hot encoding
contrasts(Smarket$Direction)    # To show the value (1/0) for level (Up/Down)
### Make prediction based on the probability computed (>=0.5 is Up)
yhat = ifelse(Y.probs >= 0.5, "Up", "Down")

### Construct confusion matrix and performance measures
cfmat  = table(yhat,Y.2005)
performance(cfmat, "Performance of Logistic Regression Model on Smarket Data")

#
#  Model Comparison for Badly Fitted Data does not make sense --- Skip
#



# -------------------------------------------------------------------
#  Analysis of the original `Fraud' Dataset using Logistic Regression glm
# -------------------------------------------------------------------

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
logreg_model = glm(tag~.-id_person, data=fraud.train, family=binomial)
summary(logreg_model)    # Need to understand for final

## Remove those with p-value > 0.05
#logreg_model = glm(tag~.-id_person-base_value, data=fraud.train, family=binomial)
#summary(logreg_model)

#
# Perform binary classification using conditional probability
#
fraud.test.prob = predict(logreg_model, 
  newdata=fraud.test[ ,1:8], type='response')
#fraud.test.prob = predict(logreg_model,
#  newdata=subset(fraud.test,select=1:8), type='response')

yhat = ifelse(fraud.test.prob >= 0.5, "pred_1", "pred_0")
cfmat = table(yhat, fraud.test$tag)
performance(cfmat, "Performance of the Logistic Regression Model")



# -------------------------------------------------------------------
#  Model Comparison for Nicely Fitted Data makes sense
# -------------------------------------------------------------------

# Is removing the base_value and age better?
reduced.model = glm(tag~ gender + status + employment + account_link + supplement, data=fraud.train[,2:9], family=binomial)
summary(reduced.model)
# What about removing employment as well?
rm2 = glm(tag~ gender + status + account_link + supplement, data=fraud.train[,2:9], family=binomial)
summary(rm2)

anova(logreg_model, reduced.model, rm2, test="Cp")
anova(logreg_model, reduced.model, rm2, test="Chisq")
anova(logreg_model, reduced.model, rm2, test="Rao")

# The p-values of the coefficients suggest NO variable is
# able to "explain" the output nicely.
#
# Try the model with only one input Lag1 and compare it to the full model
#
lr.fit2 = glm(Direction~Lag1, data=Smarket[train,], family=binomial)
summary(lr.fit2)
#
# The residual deviance improves a bit but the model is still bad
#
anova(lr.fit, lr.fit2)
#
# Chisq, Rao, tests are OK for binary output:
# The result suggests bad model.
#
anova(lr.fit, lr.fit2, test="Cp")     # Cp == AIC
anova(lr.fit, lr.fit2, test="Chisq")  # Same thing as LRT for logistic regression 
anova(lr.fit, lr.fit2, test="LRT")    # LRT = Likelihood Ratio Test
anova(lr.fit, lr.fit2, test="Rao")


# -------------------------------------------------------------------
#  Multinomial LR is a Generalisation of LR
# -------------------------------------------------------------------

library(nnet)
mlr.model = multinom(tag ~ ., data=fraud.train[,2:9])
print(mlr.model)
print(summary(mlr.model))

# Basically multinom() witk K=2 is the same as glm() except Z-statistic 
# and p-values are not provided.


