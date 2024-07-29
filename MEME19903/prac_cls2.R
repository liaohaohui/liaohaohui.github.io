# -------------------------------------------------------------------
# Purpose: Practical for Working with Naive Bayes Predictive Models
#          for Classification Problems
# Author : Liew How Hui (2024)
# References: 
#  1. https://www.statlearning.com/resources-second-edition
#  2. http://www.dbenson.co.uk/Rparts/subpages/spamR/
#  3. http://www.learnbymarketing.com/tutorials/naive-bayes-in-r/
# Data   : http://faculty.marshall.usc.edu/gareth-james/ISL/data.html
# License: BSD-3
# Software: R 4.x
# Duration: 1 hour
# -------------------------------------------------------------------

#install.packages("ISLR2")
library(ISLR2)

# -------------------------------------------------------------------
# Performance Measurements for Classification Problem
# A more sophisticated implementation is caret::confusionMatrix
# -------------------------------------------------------------------

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
# Working with Case Study 1 from Lecture Slide s42_nb.pdf
# -------------------------------------------------------------------

d.f = read.csv(text="
X1,X2,X3,Y
C, No,0,Positive
A,Yes,1,Positive
B,Yes,0,Negative
B,Yes,0,Negative
A, No,1,Positive
C, No,1,Negative
B,Yes,1,Positive", stringsAsFactors=TRUE)
d.f$X3 = factor(d.f$X3)
print(d.f)
#install.packages("naivebayes")
library(naivebayes)
m = naive_bayes(Y ~ X1+X2+X3, d.f)   # No Laplace smoothing
print(m)

# Predict response Y for X1=B, X2=Yes, X3=1
newX = data.frame(X1='B', X2='Yes', X3='1')
prob = predict(m, newX, type="prob")
# Try to compare to the `products' in Case Study 1:
# prop.table(c(3/7*2/3*2/3*1/3, 4/7*1/4*1/2*3/4))
yhat = predict(m, newX, type="class")

# -------------------------------------------------------------------
# Working with Case Study 2 from Lecture Slide s42_nb.pdf
# -------------------------------------------------------------------

d.f = read.csv(text="
Weather,Car,Y
sunny,working,go-out
rainy,broken ,go-out
sunny,working,go-out
sunny,working,go-out
sunny,working,go-out
rainy,broken ,stay-home
rainy,broken ,stay-home
sunny,working,stay-home
sunny,broken ,stay-home
rainy,broken ,stay-home",stringsAsFactors=TRUE)
m = naive_bayes(Y ~ ., d.f)
print(m)   # Compare them to the lecture slides


# -------------------------------------------------------------------
# Working with Case Studies 3 & 6 from Lecture Slide s42_nb.pdf
# -------------------------------------------------------------------

d.f = read.csv(text="
balance,student,Default
500 ,No ,N
1980,Yes,Y
60  ,No ,N
2810,Yes,Y
1400,No ,N
300 ,No ,N
2000,Yes,Y
940 ,No ,N
1630,No ,Y
2170,Yes,Y",stringsAsFactors=TRUE)
#
# Without Laplace smoothing: There is a zero prob.
#
m = naive_bayes(Default ~ ., d.f)
print(m)
#
# Using Laplace smoothing to remove zero prob warning
#
m2 = naive_bayes(Default ~ ., d.f, laplace=1)
print(m2)


# -------------------------------------------------------------------
#    Dataset 1: Building Naive Bayes Model for Fraud Data
# -------------------------------------------------------------------

# If there is a column with categorical data, using stringsAsFactors=TRUE
# is more convenient.
#https://liaohaohui.github.io/MEME19903/fraud.csv
fraud = read.csv("fraud.csv", row.names=1)
# change data type from integer to categorical
col_fac = c("gender", "status", "employment", "account_link", "supplement", "tag")
fraud[col_fac] = lapply(fraud[col_fac], factor)

#
# Manual stratified sampling for binary classes with R
#
set.seed(123)
fraud_tag0 = fraud[fraud$tag=="0", ]
fraud_tag1 = fraud[fraud$tag=="1", ]
tag0_idx = sample(1:nrow(fraud_tag0), size=0.7*nrow(fraud_tag0))
tag1_idx = sample(1:nrow(fraud_tag1), size=0.7*nrow(fraud_tag1))
fraud.train = rbind(fraud_tag0[tag0_idx,],fraud_tag1[tag1_idx,])
fraud.test = rbind(fraud_tag0[-tag0_idx,],fraud_tag1[-tag1_idx,])

#
# Alternative: caTools's sample.split
#
#library(caTools)   # for sample.split()
#train.row.index = sample.split(fraud, SplitRatio=0.7)
#fraud.train = fraud[train.row.index, ]
#fraud.test = fraud[-train.row.index, ]
#

#
# Various Choices for Naive Bayes:
# (1) naivebayes library (used by the main reference book)
# (2) e1071 library
# (3) klaR library
# (4) fastNaiveBayes library
#

cat("
Calculations without Laplace Smoothing
")
model.nb = naive_bayes(tag~., data = fraud.train)
#library(e1071)  # for naiveBayes()
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


# -------------------------------------------------------------------
#    Dataset 2: Spam Filtering with Naive Bayes Model
# -------------------------------------------------------------------

d.f = read.csv(text='
ham,1,"Hi sir, just want to ask you if the formula xxx is OK?"
spam,1,"Maxis great deal is here"
ham,2,"If I solve the problem the following way ... is it OK?"
ham,3,"Your solution is correct.  Great job"
spam,2,"Discount 20% from Maxis when dinning at ..."
ham,4,"The maximum value for ... is the coefficient for the model ..."
spam,3,"Win a phone when subscribing to Maxis new plan ..."
spam,4,"Upgrade to Digi new plan ..."
spam,5,"Subscribe to ASTRO  ... with only RM250 per month"
ham,5,"Why can\'t I get the right result?"
',header=F,col.names=c("Y","id", "content"))

#install.package("tm")
library(tm)  # Text Mining package.  For DocumentTermMatrix, VCorpus, ...
corpus = VCorpus(VectorSource(d.f$content))
# The DocumentTermMatrix can be slow for large data
# and the stemming is too primitive and brutal!
dtm = DocumentTermMatrix(corpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  removePunctuation = TRUE,
  stemming = TRUE      # This is bad, need to work on it
))   # Statistical model
### The features are encoded in
# dtm$dimnames$...
inspect(dtm)

#
# For `text' classification with Naive Bayes, we may want to
# turn on the Laplace smoothing!
#

library(naivebayes)    # for multinomial_naive_bayes()

idx.train = 1:6
train = as.matrix(dtm[idx.train,])    # not suitable for large matrix
Y.train = d.f$Y[idx.train]
idx.test = 7:10
test  = as.matrix(dtm[idx.test,])
Y.test  = d.f$Y[idx.test]

classifier = multinomial_naive_bayes(train, Y.train, laplace=1)
summary(classifier)
coef(classifier)
#
# Let's check the word 'you':
# p = length(dtm$dimnames$Terms) = 45
# P(word='you'|Y='ham') = (2+1)/(28+45)
# number of times the word 'you' occured in training data of class 'ham' = 2
# number of words in training data of class 'ham' = 28
# P(word='you'|Y='spam') = (0+1)/(9+45)
# number of times the word 'you' occured in training data of class 'spam' = 0
# number of words in training data of class 'spam' = 9
#
yhat = predict(classifier, test)
cfmat = table(yhat, Y.test)
print(cfmat)

### https://www.kaggle.com/code/abeperez/building-a-spam-filter-using-fastnaivebayes/notebook
#install.packages("fastNaiveBayes")
library(fastNaiveBayes)   # for fnb.multinomial()
mnnb = fnb.multinomial(x=train, y=Y.train, laplace=1)
# The fastNaiveBayes provides a nice summary of word counts with
# the list item 'present':
mnnb$present
yhat = predict(mnnb, test)
cfmat = table(yhat, Y.test)
print(cfmat)

### naivebayes::bernoulli_naive_bayes
convert2bin = function(x){ifelse(x>0,1,0)}
library(Matrix)   # for Matrix() to handle sparse matrix
train = Matrix(apply(dtm[idx.train,],2,convert2bin),sparse=T)
Y.train = d.f$Y[idx.train]
test = Matrix(apply(dtm[idx.test,],2,convert2bin),sparse=T)
Y.test  = d.f$Y[idx.test]

classifier = bernoulli_naive_bayes(train, Y.train, laplace=1)
yhat = predict(classifier, test)
cfmat = table(yhat, Y.test)
print(cfmat)

#
# Binary Categorical NB == Bernoulli NB ???
#
# naive_bayes from `naivebayes' package has issue with the *PREDICTION*
#classifier = naive_bayes(train, trainLabels, laplace=1)

convert = function(x){ifelse(x>0,"Yes","No")}
train = as.data.frame(apply(dtm[idx.train,],2,convert))
train = as.data.frame(lapply(train, function(c){factor(c,levels=c("No","Yes"))}))
Y.train = factor(d.f$Y[idx.train],levels=c("ham","spam"))
test = as.data.frame(apply(dtm[idx.test,],2,convert))
test = as.data.frame(lapply(test, function(c){factor(c,levels=c("No","Yes"))}))
Y.test  = factor(d.f$Y[idx.test],levels=c("ham","spam"))

library(e1071)    # for naiveBayes()
classifier = naiveBayes(train, Y.train, laplace=1)
#classifier$tables$call   # Probability table of seeing the world `call'
yhat = predict(classifier, test)

cfmat = table(yhat, Y.test)
performance(cfmat, "e1071 Naive Bayes with Laplace Smoothing")



