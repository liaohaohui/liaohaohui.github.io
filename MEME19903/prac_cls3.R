# -------------------------------------------------------------------
# Purpose: Practical for Working with Classification Problems
#          with Tree Based Predictive Models in R
# Author : Liew How Hui (2024)
# Reference & Data: 
#  1. https://hastie.su.domains/ISLR2/Labs/R_Labs/Ch8-baggboost-lab.R
#  2. https://daviddalpiaz.github.io/r4sl/trees.html
#  3. http://faculty.marshall.usc.edu/gareth-james/ISL/Chapter%208%20Lab.txt
#  4. http://www.di.fc.ul.pt/~jpn/r/tree/tree.html
#  5. https://cran.r-project.org/web/packages/C50/vignettes/C5.0.html
# License: BSD-3
# Software: R 4.x
# Duration: 1 hour
# -------------------------------------------------------------------

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
#  Working on Example 1 from Lecture Slide s43_tree.pdf with C5.0
#  Ref: https://www.rulequest.com/see5-info.html
# -------------------------------------------------------------------

d.f = read.table(text='
X1 X2  Y
 T  T  T
 T  T  F
 T  T  T
 T  F  F
 F  T  T
 F  F  T', header=T, colClasses=rep("factor",3))

library(C50)
tree.model = C5.0(d.f[,1:2], d.f[,3], control=C5.0Control(subset=FALSE,noGlobalPruning=TRUE,earlyStopping=FALSE,minCases=1,CF=1) )
plot(tree.model)   # smaller tree comparing to lecture slide's tree


# -------------------------------------------------------------------
#  Example 2: Analysis of the `Tennis' Dataset with C5.0
#  (Multiway split --- used in lecture)
# -------------------------------------------------------------------

#https://liaohaohui.github.io/MEME19903/playtennis.csv
d.f = read.csv("playtennis.csv",stringsAsFactors=TRUE,row.names=1)

#
# C5.0 supports both multi-way split and binary split
# It depends on stringr which has much more dependencies since version 1.5.
#
tree.model = C5.0(d.f[ , 1:4], d.f[ , 5])
plot(tree.model)
print(summary(tree.model))
#
# Print the importance of predictors
#
print(C5imp(tree.model))
#
# Source: https://www.rulequest.com/see5-unix.html#RULES
# An important feature of C5.0 is its ability to generate classifiers 
# called rulesets that consist of unordered collections of (relatively) 
# simple if-then rules. 
#
# Each rule consists of:
#
# * A rule number -- this is quite arbitrary and serves only to identify 
#   the rule.
# * Statistics (n, lift x) or (n/m, lift x) that summarize the performance 
#   of the rule. Similarly to a leaf, n is the number of training cases 
#   covered by the rule and m, if it appears, shows how many of them do not 
#   belong to the class predicted by the rule. The rule's accuracy is 
#   estimated by the Laplace ratio (n-m+1)/(n+2). The lift x is the result 
#   of dividing the rule's estimated accuracy by the relative frequency of 
#   the predicted class in the training set.
# * One or more conditions that must all be satisfied if the rule is 
#   to be applicable.
# * A class predicted by the rule.
# * A value between 0 and 1 that indicates the confidence with which 
#   this prediction is made.
#
rule.model = C5.0(d.f[ , 1:4], d.f[ , 5], rules=T)
print(summary(rule.model))


# -------------------------------------------------------------------
#  Analysis of the `Tennis' Dataset with CART Tree (binary splits)
# -------------------------------------------------------------------

#install.packages("tree")
library(tree)   # Requires R >= 3.6 (no dependencies)
### It is a decision tree package developed by Professor Brian D. Ripley
### for his book: http://www.stats.ox.ac.uk/~ripley/PRbook/
### Ref: https://stat.ethz.ch/pipermail/r-help/2005-May/070922.html
### It is generates binary tree and is a kind of CART

tree.model = tree(playtennis ~ ., d.f)
print(tree.model)
print(summary(tree.model))  # calc. deviance & misclassification

#
# Default tree.control: mincut = 5, minsize = 10, mindev = 0.01
#
tree.model2 = tree(playtennis ~ ., d.f, control=
  tree.control(nrow(d.f), minsize=1))
par(mfrow=c(1,2))
plot(tree.model,main="Default \"Pre-Pruned\" CART Tree")
text(tree.model)
plot(tree.model2,main="Fully-grown CART Tree")
text(tree.model2)


# -------------------------------------------------------------------
#  Analysis of the `Carseats' Dataset with CART Tree & Pruning
# -------------------------------------------------------------------

### Initial Exploration of the Dataset ``Carseats''
library(ISLR2)

## Fitting Classification Trees

### Sales is the ``response'' variable with respect to the rest
### To turn it to a ``classification problem'', we introduce
### a cut-off 8 to have a categorical output:
### "High" = "Yes" if "Sales" > 8 and "No" if "Sales"<= 8
Carseats$High  = factor(ifelse(Carseats$Sales>8,"Yes","No"))
#
# Removing the Y=Sales (regression)
#
Carseats$Sales = NULL
###
tree.carseats = tree(High ~ ., Carseats)
###
tree.carseats
###
summary(tree.carseats)
###
plot(tree.carseats)
text(tree.carseats, pretty = 0)

### Validation Set Approach: split data into training and testing sets
### using stratified sampling
set.seed(2)
cs_tag0 = Carseats[Carseats$High=="No", ]
cs_tag1 = Carseats[Carseats$High=="Yes", ]
tag0_idx = sample(nrow(cs_tag0), size=0.7*nrow(cs_tag0))
tag1_idx = sample(nrow(cs_tag1), size=0.7*nrow(cs_tag1))
Carseats.train = rbind(cs_tag0[ tag0_idx,],cs_tag1[ tag1_idx,])
Carseats.test  = rbind(cs_tag0[-tag0_idx,],cs_tag1[-tag1_idx,])

#
# Train the CART tree predictive model
#
tree.carseats = tree(High~.,Carseats.train)
# Text layout is suitable for programming
print(tree.carseats)
# Diagram representation
plot(tree.carseats)
text(tree.carseats,cex=0.8)

### Validate the trained CART tree model
tree.pred = predict(tree.carseats,Carseats.test,type="class")
cf.mat = table(tree.pred,Carseats.test$High)
performance(cf.mat, "\nCarseats with tree::tree")

### Prunning the ``trained'' tree model
### Rationale: Smaller trees are less `overfit'
### Strategy : Need to the correct number of nodes using cross-validation (CV).
###            We will use a 10-fold CV to find a tree by considering 
###            trees of different sizes which have been pruned from
###            our original tree.
set.seed(5)
#
# Decisions trees are usually prone to overfitting
#
# Growing the trees with different sizes:
# 1. The smaller the better!
# 2. Must make sure the misclassification errors are small as possible
#
seat_tree_cv = cv.tree(tree.carseats, FUN=prune.misclass) #K=10 by default
#
# Visualise how the misclassication error changes w.r.t. tree size
#
par(mfrow = c(1, 2))
plot(seat_tree_cv)
plot(seat_tree_cv$size, seat_tree_cv$dev / nrow(Carseats.train), 
     type="b", xlab="Tree Size", ylab="CV Misclassification Rate")
# num_terminal_nodes defined below.
#
# 1. Find the minimum: min(seat_tree_cv$dev)
# 2. Find the position of the minimum: which(seat_tree_cv$dev==min(seat_tree_cv$dev))
# 3. There may be multiple minimum, get the first one
#
min_idx = max(which(seat_tree_cv$dev==min(seat_tree_cv$dev)))
num_terminal_nodes = seat_tree_cv$size[min_idx]
#prune.carseats = prune.rpart(tree.carseats,cp=15) cp=best ???
prune.carseats = prune.misclass(tree.carseats,best=num_terminal_nodes)
# Replotting
plot(seat_tree_cv)
plot(seat_tree_cv$size, seat_tree_cv$dev / nrow(Carseats.train), 
     type="b", xlab=paste("Tree Size (minimum @", num_terminal_nodes, ")"),
     ylab="CV Misclassification Rate")
#
# Compare the pruned tree and the original tree
#
par(mfrow = c(1, 2))
plot(prune.carseats)   # One can see a ``cleaner'' tree
text(prune.carseats,cex=0.8)
title("Pruned Tree")
plot(tree.carseats)
text(tree.carseats,cex=0.8)
title("Original Tree")
#print(prune.carseats)
prune.pred = predict(prune.carseats,Carseats.test,type="class")
cf.mat.prune = table(prune.pred,Carseats.test$High)
performance(cf.mat.prune, "\nPrunned tree::tree to `minimal' nodes")

#
# Prune on our own decision on how many leaves to keep
#
prune.carseats = prune.misclass(tree.carseats,best=15)
par(mfrow = c(1, 2))
plot(prune.carseats)
text(prune.carseats,cex=0.8)
title("Pruned Tree with 15 leaves")
plot(tree.carseats)
text(tree.carseats,cex=0.8)
title("Original Tree")
tree.pred=predict(prune.carseats,Carseats.test,type="class")
cf.mat.prune2 = table(tree.pred,Carseats.test$High)
performance(cf.mat.prune2, "\nPrunned tree::tree to around 15 nodes")


# -------------------------------------------------------------------
#  Analysis of the `Iris' Dataset with Conditional Inference Tree 
#  Classification Tree (partykit) => Unbias (statistics)
# -------------------------------------------------------------------

# http://www.di.fc.ul.pt/~jpn/r/tree/tree.html
# Conditional inference trees estimate a regression relationship by binary
# recursive partitioning in a conditional inference framework. Roughly, 
# the algorithm works as follows: 
# 1) Test the global null hypothesis of independence between any of 
#    the input variables and the response (which may be multivariate 
#    as well). Stop if this hypothesis cannot be rejected.
#    Otherwise select the input variable with strongest association to 
#    the resonse. This association is measured by a p-value 
#    corresponding to a test for the partial null hypothesis of 
#    a single input variable and the response.
# 2) Implement a binary split in the selected input variable.
# 3) Recursively repeat steps 1) and 2)

set.seed(1)
idx = sample(nrow(iris), size=0.7*nrow(iris))
iris.train = iris[ idx,]
iris.test  = iris[-idx,]
# If we need perfect ratio, we need `stratified sampling'

# -------------------------------------------------------------------
# Conditional Inference Tree (theoretically the inputs need to be
# numeric): partykit
# -------------------------------------------------------------------
library(partykit)   # converts categorical data to numeric data
iris.ctree = ctree(Species ~ ., iris.train)   # Conditional Inference Tree
plot(iris.ctree)
#iris.ctree = ctree(Fraud ~ RearEnd, train)
#plot.party(cart.ctree, newpage=FALSE)
#
cat("\n*** Confusion Matrix for Training Data\n")
#
yhat = predict(iris.ctree, iris.train)
cf.mat.train = table(yhat, iris.train$Species)
print(cf.mat.train)
cat("Accuracy for train=", sum(diag(cf.mat.train))/nrow(iris.train), "\n")
#
cat("\n*** Confusion Matrix for Testing Data\n")
#
yhat = predict(iris.ctree, iris.test)
cf.mat.test = table(yhat, iris.test$Species)
cat("Accuracy for test=", sum(diag(cf.mat.test))/nrow(iris.test), "\n")
#performance(cf.mat.test)
# gmodels::CrossTable(yhat, iris.test$Species)



##------------------------------------------------------------------
## Fitting Regression Trees with MASS' Boston Data
##------------------------------------------------------------------

set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston) / 2)
tree.boston <- tree(medv ~ ., Boston, subset = train)
summary(tree.boston)
###
plot(tree.boston)
text(tree.boston, pretty = 0)
###
cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = "b")
###
prune.boston <- prune.tree(tree.boston, best = 5)
plot(prune.boston)
text(prune.boston, pretty = 0)
###
yhat <- predict(tree.boston, newdata = Boston[-train, ])
boston.test.Y <- Boston[-train, "medv"]
plot(yhat, boston.test.Y)
abline(0, 1)
mean((yhat - boston.test.Y)^2)

##------------------------------------------------------------------
## Bagging and Random Forests with MASS' Boston Data
## (Collection of trees)
##------------------------------------------------------------------

library(randomForest)
set.seed(1)
bag.boston <- randomForest(medv ~ ., data = Boston,
    subset = train, mtry = 12, importance = TRUE)
bag.boston
###
yhat.bag <- predict(bag.boston, newdata = Boston[-train, ])
plot(yhat.bag, boston.test.Y)
abline(0, 1)
mean((yhat.bag - boston.test.Y)^2)
###
bag.boston <- randomForest(medv ~ ., data = Boston,
    subset = train, mtry = 12, ntree = 25)
yhat.bag <- predict(bag.boston, newdata = Boston[-train, ])
mean((yhat.bag - boston.test.Y)^2)
###
set.seed(1)
rf.boston <- randomForest(medv ~ ., data = Boston,
    subset = train, mtry = 6, importance = TRUE)
yhat.rf <- predict(rf.boston, newdata = Boston[-train, ])
mean((yhat.rf - boston.test.Y)^2)
###
importance(rf.boston)
###
varImpPlot(rf.boston)

##------------------------------------------------------------------
## Boosting with MASS' Boston Data
##------------------------------------------------------------------

library(gbm)  # no longer developed in 2024. gbm3 is newer version
set.seed(1)
boost.boston <- gbm(medv ~ ., data = Boston[train, ],
    distribution = "gaussian", n.trees = 5000,
    interaction.depth = 4)
###
summary(boost.boston)
###
plot(boost.boston, i = "rm")
plot(boost.boston, i = "lstat")
###
yhat.boost <- predict(boost.boston,
    newdata = Boston[-train, ], n.trees = 5000)
mean((yhat.boost - boston.test.Y)^2)
###
boost.boston <- gbm(medv ~ ., data = Boston[train, ],
    distribution = "gaussian", n.trees = 5000,
    interaction.depth = 4, shrinkage = 0.2, verbose = F)
yhat.boost <- predict(boost.boston,
    newdata = Boston[-train, ], n.trees = 5000)
mean((yhat.boost - boston.test.Y)^2)


