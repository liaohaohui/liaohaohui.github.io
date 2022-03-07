# -------------------------------------------------------------------
# Purpose: Practical for Naive Bayes Predictive Models in R
# Author : Liew How Hui (2022)
# References: 
#  1. http://www.dbenson.co.uk/Rparts/subpages/spamR/
#  2. http://www.learnbymarketing.com/tutorials/naive-bayes-in-r/
# Data   : fraud.csv, fraud_new.csv
# License: BSD-3
# Software: R 3.6 & R 4.0
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
#    Dataset 1: Building Naive Bayes Model for Fraud Data
# -------------------------------------------------------------------

# If there is a column with categorical data, using stringsAsFactors=TRUE
# is more convenient.
#https://liaohaohui.github.io/UECM3993/fraud.csv
fraud = read.csv("fraud.csv")  # categorical data are encoded as integers

# change data type from integer to categorical (mentioned in Practical 3)
col_fac = c("gender", "status", "employment", "account_link", "supplement", "tag")
fraud[col_fac] = lapply(fraud[col_fac], factor)

### Stratified sampling (mentioned in Practical 2)
set.seed(123)

#
# Use the following if the company only has R and no other
# libraries available.
#
#fraud_tag0 = fraud[fraud$tag=="0", ]
#fraud_tag1 = fraud[fraud$tag=="1", ]
#tag0_idx = sample(1:nrow(fraud_tag0), size=0.7*nrow(fraud_tag0))
#tag1_idx = sample(1:nrow(fraud_tag1), size=0.7*nrow(fraud_tag1))
#fraud.train = rbind(fraud_tag0[tag0_idx,],fraud_tag1[tag1_idx,])
#fraud.test = rbind(fraud_tag0[-tag0_idx,],fraud_tag1[-tag1_idx,])

#
# If you can install extra libraries from CRAN (Internet), then
# you should use `caTools' or splitstackshape + dplyr
#
library(caTools)
train.row.index = sample.split(fraud, SplitRatio=0.7)
fraud.train = fraud[train.row.index, ]
fraud.test = fraud[-train.row.index, ]

#library(splitstackshape)
#fraud.train <- stratified(fraud,"tag",size=0.7)
#library(dplyr)  # It has a lot of dependencies
#fraud.test <- anti_join(fraud, fraud.train, by="id_person")

fraud.train$id_person = NULL
fraud.test$id_person = NULL

#
# Choices for Naive Bayes:
# (1) naivebayes library (used by the main reference book)
# (2) e1071 library
# (3) klaR library?
#
library(naivebayes)
cat("
Calculations without Laplace Smoothing
")
model.nb = naive_bayes(tag~., data = fraud.train)
#library(e1071)  # naiveBayes
#model.e1071 = naiveBayes(tag~., data=fraud.train, laplace=0)
p = ncol(fraud.train)-1
pred.nb = predict(model.nb, newdata = fraud.test[,1:p])  # columns 1:p for inputs
cfmat = table(pred.nb, actual.fraud=fraud.test$tag)
performance(cfmat, "Performance of Naive Bayes without Laplace Smoothing")

cat("
Calculations with Laplace Smoothing
")
model.nb.lp = naive_bayes(tag~., data=fraud.train, laplace=1)
pred.nb.lp = predict(model.nb.lp, fraud.test[,1:p])
cfmat = table(pred.nb.lp, actual.fraud=fraud.test$tag)
performance(cfmat, "Performance of Naive Bayes with Laplace Smoothing")

#
# CRISP-DM's Deployment:
# Score new fraud data using naive bayes model
#
#https://liaohaohui.github.io/UECM3993/fraud_new.csv
fraud_new = read.csv("fraud_new.csv")
col_fac2 = c("gender", "status", "employment", "account_link", "supplement")
# there is no `tag' column in fraud_new
fraud_new[col_fac2] <- lapply(fraud_new[col_fac2], factor)
fraud_new$pred <- predict(model.nb,fraud_new)
print(head(fraud_new))
# We can't calculate the confusion matrix because the fraud_new.csv
# does not contain actual response `tag'

# -------------------------------------------------------------------
#    Dataset 2: Spam Filtering with Naive Bayes Model
# -------------------------------------------------------------------

# Original Source: http://www.dbenson.co.uk/Rparts/subpages/spamR/sms_spam.csv
#https://liaohaohui.github.io/UECM3993/sms_spam.csv
sms = read.csv('sms_spam.csv')
# head, names
sms$type = factor(sms$type)  # not necessary for R 3.6 and below
table(sms$type)

#spam_messages = subset(sms,type=="spam")
#ham_messages  = subset(sms,type=="ham")

library(tm)  # Text Mining package
corpus = VCorpus(VectorSource(sms$text))
dtm = DocumentTermMatrix(corpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  removePunctuation = TRUE,
  stemming = TRUE
))   # Statistical model
### The features
# dtm$dimnames
train.idx = 1:4169
trainLabels = sms[ train.idx,]$type
testLabels  = sms[-train.idx,]$type
prop.table(table(trainLabels))    # To check that it works with
prop.table(table(testLabels))     # the stratification
dtmTrain = dtm[ train.idx,]       # Transform to STRUCTURED DATA -> Table
dtmTest  = dtm[-train.idx,]
freqWords = findFreqTerms(dtmTrain,5)  # Remove any words which does not appear 5 times
freqTrain = dtmTrain[,freqWords]  # STRUCTURED DATA with LESS COLUMNS
freqTest  = dtmTest [,freqWords]
# Transform the frequently used words table to BINARY DATA! -> Bernoulli NB
convert_counts = function(x) {x = ifelse(x > 0, "Yes", "No")}
train = apply(freqTrain, MARGIN=2, convert_counts)
test  = apply(freqTest,  MARGIN=2, convert_counts)

#
# Maybe very very slow ????
#

#
# For `text' classification with Naive Bayes, we always want to
# turn on the Laplace smoothing
#
# naive_bayes from `naivebayes' package has issue with the *PREDICTION*
#classifier = naive_bayes(train, trainLabels, laplace=1)
library(e1071)
classifier = naiveBayes(train, trainLabels)
#classifier$tables$call   # Probability table of seeing the world `call'
yhat = predict(classifier, test)

cfmat = table(yhat, testLabels)
performance(cfmat, "e1071 Naive Bayes with Laplace Smoothing")

# -------------------------------------------------------------------
#    Working with Simulated Data
# -------------------------------------------------------------------

no_resp = 500
resp = 100
set.seed(1)
response = factor(c(rep(0,no_resp),rep(1,resp)))
purchased_previously = factor(c(sample(0:1,no_resp,prob=c(0.6,0.4),replace=T),
                          sample(0:1,resp,prob=c(0.2,0.8),replace=T)))
opened_previously = factor(sample(0:1,(no_resp+resp),prob=c(0.8,0.2),replace=T))
sales_12mo = c(rnorm(n=no_resp,mean = 50, sd = 10),
               rnorm(n=resp,mean = 60, sd = 5))
none_open_buy = factor(c(sample(0:1, no_resp,prob=c(0.8,0.2),replace=T),
                          rep(1,resp)))
test_var = sample(LETTERS[1:2],(resp+no_resp),replace=T)
 
naive_data = data.frame(purchased_previously = purchased_previously,
                        opened_previously = opened_previously,
                        sales_12mo = sales_12mo,
                        none_open_buy = none_open_buy,
                        test_var = test_var,
                        response = response)

#
# Linear Sampling
#
# Shuffle all the rows
naive_data = naive_data[sample(1:nrow(naive_data),nrow(naive_data)),]
# Take first 70% for training and the remainder for testing
train = naive_data[1:(nrow(naive_data)*.7),]
test  = naive_data[(nrow(naive_data)*.7+1):nrow(naive_data),]

# Without Laplace Smoothing
#nb_default = naiveBayes(response~., data=train[,-4], laplace=0)
nb_default = naive_bayes(response~., data=train[,-4])  # laplace defaults to 0
default_pred = predict(nb_default, test, type="class")
# To extract information from Naive Bayes Network Model
#default_raw_pred <- predict(nb_default, test, type="raw")
table(default_pred, test$response,dnn=c("Prediction","Actual"))

# With Laplace Smoothing
#nb_laplace1 = naiveBayes(response~., data=train, laplace=1)
nb_laplace1 = naive_bayes(response~., data=train, laplace=1)
laplace1_pred = predict(nb_laplace1, test, type="class")
table(laplace1_pred, test$response,dnn=c("Prediction","Actual"))

