# -------------------------------------------------------------------
# Purpose: Practical for Classification Tree Based Predictive Models in R
# Author : Liew How Hui (2021)
# References: 
#  1. https://daviddalpiaz.github.io/r4sl/trees.html
#  2. http://faculty.marshall.usc.edu/gareth-james/ISL/Chapter%208%20Lab.txt
#  3. http://www.di.fc.ul.pt/~jpn/r/tree/tree.html
#  4. https://cran.r-project.org/web/packages/C50/vignettes/C5.0.html
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
#  Analysis of a Toy Example with CART (partykit)
# -------------------------------------------------------------------
")
library(partykit)
train <- data.frame(
  ClaimID = c(1,2,3),
  RearEnd = c(TRUE, FALSE, TRUE),
  Fraud = c(TRUE, FALSE, TRUE)
)
train$RearEnd = as.factor(train$RearEnd)
train$Fraud = as.factor(train$Fraud)
cart = ctree(Fraud ~ RearEnd, train)
plot.party(cart)

cat("
# -------------------------------------------------------------------
#  Analysis of the `Carseats' Dataset with Classification Tree (tree)
# -------------------------------------------------------------------
")
library(tree)   # Requires R >= 3.6 (no dependencies)
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
tag0_idx = sample(nrow(cs_tag0), size=0.7*nrow(cs_tag0))
tag1_idx = sample(nrow(cs_tag1), size=0.7*nrow(cs_tag1))
Carseats.train = rbind(cs_tag0[ tag0_idx,],cs_tag1[ tag1_idx,])
Carseats.test  = rbind(cs_tag0[-tag0_idx,],cs_tag1[-tag1_idx,])
### Alternative way of splitting data:
#library(splitstackshape)
#Carseats.train <- stratified(Carseats,"High",size=0.7,keep.rownames=TRUE)
#Carseats.test  = Carseats[-as.integer(Carseats.train$rn),]
#Carseats.train = Carseats.train[,-c("rn")]

### Train the tree model
### A decision tree package developed by Professor Brian D. Ripley
### for his book: http://www.stats.ox.ac.uk/~ripley/PRbook/
### Ref: https://stat.ethz.ch/pipermail/r-help/2005-May/070922.html
### It is generates binary tree and is a kind of CART
tree.carseats = tree(High~.,Carseats.train)
### Alternative choice: rpart
#tree.carseats=rpart(High~.-Sales,Carseats.train)
#print(tree.carseats)
plot(tree.carseats)
text(tree.carseats,cex=0.8)
#library(rpart.plot)  # A fancier tree plotting library
#rpart.plot(cart)

### Validate the trained tree model
tree.pred = predict(tree.carseats,Carseats.test,type="class")
cf.mat = table(tree.pred,Carseats.test$High)
performance(cf.mat, "\nCarseats with tree::tree")

### Prunning the ``trained'' tree model
### Rationale: Smaller trees are less `overfit'
### Strategy : Need to the correct number of nodes using cross-validation (CV).
###            We will use a 10-fold CV to find a tree by considering 
###            trees of different sizes which have been pruned from
###            our original tree.
set.seed(3)
seat_tree_cv = cv.tree(tree.carseats, FUN=prune.misclass) #K=10 by default
min_idx = max(which(seat_tree_cv$dev==min(seat_tree_cv$dev)))
num_terminal_nodes = seat_tree_cv$size[min_idx]
par(mfrow = c(1, 2))
plot(seat_tree_cv)
plot(seat_tree_cv$size, seat_tree_cv$dev / nrow(Carseats.train), 
     type="b", xlab=paste("Tree Size (minimum @", num_terminal_nodes, ")"), 
     ylab="CV Misclassification Rate")
#prune.carseats = prune.rpart(tree.carseats,cp=15) cp=best ???
prune.carseats = prune.misclass(tree.carseats,best=num_terminal_nodes)
par(mfrow = c(1, 1))
plot(prune.carseats)   # One can see a ``cleaner'' tree
text(prune.carseats,cex=0.8)
#print(prune.carseats)
tree.pred = predict(prune.carseats,Carseats.test,type="class")
cf.mat.prune = table(tree.pred,Carseats.test$High)
performance(cf.mat.prune, "\nPrunned tree::tree to `minimal' nodes")

prune.carseats = prune.misclass(tree.carseats,best=15)
plot(prune.carseats)
text(prune.carseats,cex=0.8)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
cf.mat.prune2 = table(tree.pred,Carseats.test$High)
performance(cf.mat.prune2, "\nPrunned tree::tree to 15 nodes")

cat("
# -------------------------------------------------------------------
#  Analysis of the `Iris' Dataset with Conditional Inference 
#  Classification Tree (party)
# -------------------------------------------------------------------
")

# http://www.di.fc.ul.pt/~jpn/r/tree/tree.html
# Conditional inference trees estimate a regression relationship by binary
# recursive partitioning in a conditional inference framework. Roughly, the
# algorithm works as follows: 1) Test the global null hypothesis of
# independence between any of the input variables and the response (which may
# be multivariate as well). Stop if this hypothesis cannot be rejected.
# Otherwise select the input variable with strongest association to the
# resonse. This association is measured by a p-value corresponding to a test
# for the partial null hypothesis of a single input variable and the response.
# 2) Implement a binary split in the selected input variable. 3) Recursively
# repeat steps 1) and 2)

suppressMessages(library(party))  # Only supports numerical input
# Depends on ‘modeltools’, ‘zoo’, ‘sandwich’, ‘strucchange’, 
# matrixStats, TH.data, multicomp, ‘coin’
set.seed(1)
idx = sample(nrow(iris), size=0.7*nrow(iris))
iris.train = iris[ idx,]
iris.test  = iris[-idx,]

iris.ctree = ctree(Species ~ ., iris.train)
plot(iris.ctree)
cat("\n*** Confusion Matrix for Training Data\n")
yhat = predict(iris.ctree, iris.train)
cf.mat.train = table(yhat, iris.train$Species)
print(cf.mat.train)
cat("\n*** Confusion Matrix for Testing Data\n")
yhat = predict(iris.ctree, iris.test)
cf.mat.test = table(yhat, iris.test$Species)
performance(cf.mat.test)

#
# C5.0 is the extension of C4.5 which extends ID3
#
cat("
# -------------------------------------------------------------------
#  Analysis of the `credit_data' with C5.0 (patterned by https://www.rulequest.com/see5-info.html)
# -------------------------------------------------------------------
")
library(modeldata)
data(credit_data)
#str(credit_data)

set.seed(2411)
in_train   = sample(nrow(credit_data), size = 3000)
train_data = credit_data[ in_train,]
test_data  = credit_data[-in_train,]

library(C50)
# Depends on mvtnorm, libcoin, inum, Formula, partykit, Rcpp, plyr,
# stringi, glue, stringr, reshape2, Cubist.
#names(train_data)
C50tree_model = C5.0(x=train_data[, c("Home", "Seniority")], y=train_data$Status)
summary(C50tree_model)
plot(C50tree_model)  # depends on partykit for plotting
yhat = predict(C50tree_model, test_data)
cf.mat.test = table(yhat, test_data$Status)
performance(cf.mat.test)

# Try working with the exercises from lecture notes topic 5

