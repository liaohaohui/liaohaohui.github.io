# -------------------------------------------------------------------
# Purpose: Supervised Learning - Classification in R (Part 4)
# Detail: Methods of classification --- Classification Trees
#         Models comparison: CART, ctree, C5.0
# Author : Liew How Hui (2026)
# References: 
#  1. https://daviddalpiaz.github.io/r4sl/trees.html
#  2. http://www.di.fc.ul.pt/~jpn/r/tree/tree.html
#  3. https://cran.r-project.org/web/packages/C50/vignettes/C5.0.html
#  4. https://hastie.su.domains/ISLR2/Labs/R_Labs/Ch4-classification-lab.R
#  5. https://www.statlearning.com/resources-second-edition
# Data: ISLR2, https://liaohaohui.github.io/UECM3993/fraud.csv,
#       https://liaohaohui.github.io/UECM3993/credit_data.csv
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
#  Practical : Analysis of the fraud Dataset with Classification 
#  Tree (tree)
#
#  tree package is developed by Professor Brian D. Ripley
#  for his book: http://www.stats.ox.ac.uk/~ripley/PRbook/
#  Ref: https://stat.ethz.ch/pipermail/r-help/2005-May/070922.html
#  It is generates binary tree and is a kind of CART.
#
#  Alternative: library(rpart), default each leaves has 5 items 
#  from the data table.
#  rpart's tree pruning is more complex than the tree library.
# -------------------------------------------------------------------

fraud = read.csv("fraud.csv",row.names=1)
fraud$tag = factor(fraud$tag)
col_fac = c("gender", "status", "employment", "account_link", "supplement")
fraud_fac = fraud    # create a copy
fraud_fac[col_fac] = lapply(fraud[col_fac], factor)

set.seed(123)
fraud_tag0 = fraud[fraud$tag=="0", ]
fraud_tag1 = fraud[fraud$tag=="1", ]
tag0_idx = sample(nrow(fraud_tag0), size=round(0.7*nrow(fraud_tag0)))
tag1_idx = sample(nrow(fraud_tag1), size=round(0.7*nrow(fraud_tag1)))
fraud.train = rbind(fraud_tag0[ tag0_idx,], fraud_tag1[ tag1_idx,])
fraud.test  = rbind(fraud_tag0[-tag0_idx,], fraud_tag1[-tag1_idx,])

set.seed(123)
fraud_fac_tag0 = fraud_fac[fraud_fac$tag=="0", ]
fraud_fac_tag1 = fraud_fac[fraud_fac$tag=="1", ]
tag0_idx = sample(nrow(fraud_fac_tag0), size=round(0.7*nrow(fraud_fac_tag0)))
tag1_idx = sample(nrow(fraud_fac_tag1), size=round(0.7*nrow(fraud_fac_tag1)))
fraud_fac.train = rbind(fraud_fac_tag0[ tag0_idx,], fraud_fac_tag1[ tag1_idx,])
fraud_fac.test  = rbind(fraud_fac_tag0[-tag0_idx,], fraud_fac_tag1[-tag1_idx,])

#install.packages("tree")
library(tree)
m.t1 = tree(tag ~ ., fraud.train)
m.t2 = tree(tag ~ ., fraud_fac.train)
par(mfrow=c(1,2))
plot(m.t1)
title("Numeric Features")
text(m.t1)
plot(m.t2)
title("Categorical Features")
text(m.t2)
par(mfrow=c(1,1))

cfmat1 = table(predict(m.t1, fraud.test, type="class"), fraud.test$tag)
performance(cfmat1, "Numeric Feature+Tree")
cfmat2 = table(predict(m.t2, fraud_fac.test, type="class"), fraud_fac.test$tag)
performance(cfmat2, "Categorical Feature+Tree")


# -------------------------------------------------------------------
#  Practical : Analysis of the `Carseats' Dataset (from ISLR2 or ISLR) 
#  by comparing CART, Conditional Inference Tree (ctree) and C5.0 models
# -------------------------------------------------------------------

library(ISLR2)
Orig.Carseats = Carseats
sapply(Orig.Carseats,class)
Carseats = Orig.Carseats

#
# Regression Tree for the Sales
#
reg.tree = tree(Sales ~ ., Carseats)
plot(reg.tree)
text(reg.tree)

### Sales is the ``response'' variable with respect to the rest
### To turn it to a ``classification problem'', we need to discretise
### it: "High" = "Yes" if "Sales" > 8 and "No" if "Sales"<= 8
#
# Adding categorical target "High" based on "Sales".
#
Carseats$High  = factor(ifelse(Carseats$Sales<=8,"No","Yes"))
#
# Removing the Y=Sales to prevent cheating
#
Carseats$Sales = NULL
# Turning the regression problem (Y=Sales) to binary classification
# problem with Y=High(Sales)
dim(Carseats)

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
dev.new()
plot(tree.carseats)
text(tree.carseats,cex=0.8)
title("Carseats tree model")
### Alternative choice: rpart
#library(rpart)
#rpart.carseats=rpart(High~.,Carseats.train)
#library(rpart.plot)  # A fancier tree plotting library
#rpart.plot(rpart.carseats)

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
#
# Decisions trees are usually prone to overfitting
#
# Growing the trees with different sizes:
# 1. The smaller the better!
# 2. Must make sure the misclassification errors are small as possible
#
#set.seed(6)
set.seed(5)
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
prune.carseats = prune.misclass(tree.carseats,best=num_terminal_nodes)
#
#Rpart's pruning is more complex, it does not count leaves:
#  printcp(tree.carseats)  # try to find a suitable cp
#  b = tree.carseats$cptable[which.min(tree.carseats$cptable[,"xerror"]), "CP"]
#  prune.carseats = prune.rpart(tree.carseats,cp=b)
#
# Replotting indicating the minimum pruned tree
#
par(mfrow = c(1, 2))
plot(seat_tree_cv)
plot(seat_tree_cv$size, seat_tree_cv$dev / nrow(Carseats.train), 
     type="b", xlab=paste("Tree Size (minimum @", num_terminal_nodes, ")"),
     ylab="CV Misclassification Rate")
#
# Compare the pruned tree and the original tree
#
dev.new()
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
NLeaves = 5
prune.carseats = prune.misclass(tree.carseats,best=NLeaves)
par(mfrow = c(1, 2))
plot(prune.carseats)
text(prune.carseats,cex=0.8)
title(paste0("Pruned Tree with around ", NLeaves, " leaves"))
plot(tree.carseats)
text(tree.carseats,cex=0.8)
title("Original Tree")
tree.pred=predict(prune.carseats,Carseats.test,type="class")
cf.mat.prune2 = table(tree.pred,Carseats.test$High)
performance(cf.mat.prune2, paste0("\nPrunned tree::tree to around ", NLeaves, " nodes"))

# -------------------------------------------------------------------
# Conditional Inference Tree (ctree.  Input needs to be numeric):
# -------------------------------------------------------------------
# party & partykit depend on 'modeltoolsa' 'zoo', 'sandwich', 
# 'strucchange', matrixStats, TH.data, multicomp, 'coina', etc.
#
# http://www.di.fc.ul.pt/~jpn/r/tree/tree.html
# Conditional inference trees estimate a regression relationship by binary
# recursive partitioning in a conditional inference framework. Roughly, the
# algorithm works as follows: 
#  1) Test the global null hypothesis of independence between any of 
#     the input variables and the response (which may be multivariate as well). 
#     Stop if this hypothesis cannot be rejected.
#     Otherwise select the input variable with strongest association to 
#     the resonse.  This association is measured by a p-value corresponding 
#     to a test for the partial null hypothesis of a single input variable 
#     and the response.
#  2) Implement a binary split in the selected input variable. 
#  3) Recursively repeat steps 1) and 2)
#

#install.packages("partykit")   # Less dependencies, required by C5.0
library(partykit)
carseats.ctree = ctree(High~., Carseats.train)
plot(carseats.ctree)
print(carseats.ctree)
yhat2 = predict(carseats.ctree,Carseats.test,type="response")
cf.mat = table(yhat2,Carseats.test$High)
performance(cf.mat, "\nCarseats with conditional inference tree")

#
# Comparing to C5.0 tree:
# C5.0 is the extension of C4.5 which extends ID3 (Quilan)
# C5.0 is patterned by https://www.rulequest.com/see5-info.html
#
#install.packages("C50")
# C50 depends on mvtnorm, libcoin, inum, Formula, partykit, Rcpp, plyr,
# stringi, glue, stringr, reshape2, Cubist
library(C50)
#
# If want to get C4.5/multiway tree, we can use 
#
#  C5.0Control(subset=FALSE)
#
# and more controls on the branching with multiway split:
#
#  C5.0Control(subset=FALSE,noGlobalPruning=TRUE,earlyStopping=FALSE,
#    minCases=1,CF=1))
#
# otherwise C5.0 will prefer binary split than multiway split.
#

# By default, C5.0 tend to construct binary trees
C50.tree = C5.0(High~.,Carseats.train)
plot(C50.tree)
summary(C50.tree)
cat(C50.tree$tree)
yhat = predict(C50.tree,Carseats.test,type="class")
cf.mat = table(yhat,Carseats.test$High)
performance(cf.mat, "\nCarseats with C5.0 tree")

# Configure C5.0 to be more like C4.5 / ID3 tree
C50.tree = C5.0(High~.,Carseats.train,control=C5.0Control(subset=FALSE, minCases=10))
#C50.tree = C5.0(High~.,Carseats.train,control=C5.0Control(minCases=5))
plot(C50.tree)
summary(C50.tree)
cat(C50.tree$tree)
yhat = predict(C50.tree,Carseats.test,type="class")
cf.mat = table(yhat,Carseats.test$High)
performance(cf.mat, "\nCarseats with C5.0 tree")


# -------------------------------------------------------------------
#  Practical : Analysis of the `Iris' Dataset by comparing CART tree, 
#  Conditional Inference Tree (partykit)
# -------------------------------------------------------------------

set.seed(1)
idx = sample(nrow(iris), size=0.7*nrow(iris))
iris.train = iris[ idx,]
iris.test  = iris[-idx,]
# If we need perfect ratio, we need `stratified sampling'

#
# CART tree
#
iris.tree = tree(Species ~ ., iris.train)
par(mfrow=c(1,1))
plot(iris.tree)
text(iris.tree)
title("CART tree")

cat("On the training data:\n")
yhat = predict(iris.tree, iris.train, type="class")
print(table(yhat, iris.train$Species))

cat("On the testing data:\n")
yhat = predict(iris.tree, iris.test, type="class")
print(table(yhat, iris.test$Species))

# We should prune the CART tree => Exercise
prune.iris = prune.misclass(iris.tree,best=3)
dev.new()
par(mfrow=c(1,2))
plot(iris.tree)
text(iris.tree)
title("CART tree")
plot(prune.iris)
text(prune.iris)
title("Prune CART tree for IRIS data")

cat("On the testing data:\n")
yhat = predict(prune.iris, iris.test, type="class")
print(table(yhat, iris.test$Species))

iris.ctree = ctree(Species ~ ., iris.train)   # Conditional Inference Tree
dev.new()
plot(iris.ctree, main="Conditional Inference Tree")
#iris.ctree = ctree(Fraud ~ RearEnd, train)
#plot.party(cart.ctree, newpage=FALSE)

#
cat("\n*** Confusion Matrix for Training Data\n")
#
yhat = predict(iris.ctree, iris.train)
cf.mat.train = table(yhat, iris.train$Species)
performance(cf.mat.train)

#
cat("\n*** Confusion Matrix for Testing Data\n")
#
yhat = predict(iris.ctree, iris.test)
cf.mat.test = table(yhat, iris.test$Species)
print(cf.mat.test)
performance(cf.mat.test)

iris.C50 = C5.0(Species ~ ., iris.train)
plot(iris.C50)
yhat = predict(iris.C50, iris.test)
cf.mat.test = table(yhat, iris.test$Species)
performance(cf.mat.test)


# -------------------------------------------------------------------
#  Practical : Analysis of the `credit_data' with C5.0
# -------------------------------------------------------------------

##install.packages("modeldata")
#library(modeldata)   # modeldata requires dplyr, no go
# https://github.com/gastonstat/CreditScoring
#data(credit_data)
#https://liaohaohui.github.io/UECM3993/credit_data.csv
credit_data = read.csv("credit_data.csv", stringsAsFactors=T)
#names(credit_data)
#str(credit_data)

#
# Linear Sampling
#
set.seed(2026)
in_train   = sample(nrow(credit_data), size = 3000)
train_data = credit_data[ in_train,]
test_data  = credit_data[-in_train,]

#
# Using 2 inputs Home & Seniority
#
C50tree_model = C5.0(x=train_data[, c("Home", "Seniority")], y=train_data$Status)
summary(C50tree_model)
plot(C50tree_model)  # depends on partykit for plotting
yhat = predict(C50tree_model, test_data)
cf.mat.test = table(yhat, test_data$Status)
performance(cf.mat.test)

#
# Using all inputs (not really better)
#
C50tree_model = C5.0(x=train_data[, -1], y=train_data$Status)
plot(C50tree_model)  # depends on partykit for plotting
yhat = predict(C50tree_model, test_data)
cf.mat.test = table(yhat, test_data$Status)
performance(cf.mat.test)

