# -------------------------------------------------------------------
# Purpose: Basic Commands for Data Processing in R (Part 2)
# Author : Liew How Hui (2025)
# License: BSD-3
# Software: R 4.1+
# Duration: 1 hour
# -------------------------------------------------------------------

# Check current working directory
getwd()
# Change the working directory
#setwd("PathToYourFolder")


# -------------------------------------------------------------------
#  (A) Matrix is a kind of 2-D Numeric Array
#      We cannot construct a matrix using Practical 1's c()
#      We need matrix(), to get help for matrix type ?matrix
# -------------------------------------------------------------------

# A is a 4 by 4 matrix (data is filled in columns by default)

A = matrix(1:16,nrow=4,ncol=4)

# To fill in by rows, we need to set option "byrow"

B = matrix(1:16,4,4,byrow=TRUE)

# matrix transpose

t(B)    # same as A

#
# Stacking matrices
#
cbind(A,B)    # stack horizontally
rbind(A,B)    # stack vertically

#
# Matrix arithmetic operations
#
2*A     # scalar multiplication
A+2     # adding every element with a number
A+B     # matrix addition
A*B     # elementwise multiplication, NOT matrix multiplication
A%*%B   # matrix multiplication
# A*B == B*A BUT A%*%B != B%*%A

#
# Indexing using [ row index , column index ]
#
dim(A)             # Get the shape of the matrix A
A[2,3]             # Get an element from matrix
A[1,]              # Get first row
A[,2]              # Get second column
A[1:2,]            # Get first two rows
A[,1:2]            # Get first two columns
A[1:3,2:4]         # Get intersection between rows and columns
A[c(1,3),c(2,4)]   # Get intersection between rows and columns
A[-c(1,3),]        # Remove first and third rows
A[-c(1,3),-c(3,4)] # Remove some rows and some columns

#
# Statistical analysis using Reduction apply()
#
apply(A, 1, sum)    # sum along the rows
apply(A, 2, sum)    # sum along the columns
rowSums(A)
rowMeans(A)
colSums(A)
colMeans(A)
apply(A, 1, var)    # variance along the rows
apply(A, 2, var)    # variance along the columns



# -------------------------------------------------------------------
#  (B) Working with user-defined Data Frame 
#
#  R's Data Frame is a 2D Structured Data to handle the mix of 
#  numeric and categorical data just like Excel table.
#  Many of the matrix operations are applicable to Data Frame.
# -------------------------------------------------------------------

#
# Define Data Frame using data.frame() and vectors from Practical 1
#
X = data.frame(
  Progromme = c("MCCG11503-Kampar","MCCG11503-Kampar",
                "MECG11503-SL", "MECG11503-SL", "MEME19803", "MEME19803"),
  Name = c("Stud1", "Student2", "Student3", "Student4", "Student5", "Stud6"),
  LearnerType = c("Visual", "Read-Write", "Read-Write", "Visual", "Read-Write", "Auditory"),
  Quiz1Q1 = c(3.9, 4, 4.7, 5.9, 5.3, 3.4),
  Quiz1Q2 = c(0.9, 0.5, 2, 1.8, 1.5, 0),
  Quiz1Q3 = c(0.5, 1, 0.5, 2, 0, 0.5),
  Assign1 = c(16.9, 16, 14.2, 15.4, 11.1, 11.1))

# Univariate Analysis
summary(X)

#
# The $ sign is used to access the `Column' of the `Table': Table$Column
#

X$Quiz1 = X$Quiz1Q1 + X$Quiz1Q2 + X$Quiz1Q3

# apply(Data, 1 or 2, operation):  1 = ack along row, 2 = ack along column
X$Quiz1 = apply(X[,4:6],1,sum)     # Classical R syntax
# f(x,y) can be written as x |> f(y)
# h(g(f(x))) === x |> f |> g |> h
X$Quiz1 = X[,4:6] |> apply(1,sum)  # Functional R syntax (only for R 4.1 and above):

# -------------------------------------------------------------------
#  Alternative Technology --- https://dplyr.tidyverse.org/
#  * Intro:
#      https://cran.r-project.org/web/packages/dplyr/vignettes/dplyr.html
#      - Use select() to choose variables from a data frame.
#      - Use filter() to choose data based on values.
#      - Use mutate() to create new variables.
#      - Use group_by() and summarize() to work with subsets of data.
#  * Handling CSV, TSV, etc.:
#      https://readr.tidyverse.org/reference/read_delim.html
#  * Handling Excel:
#      https://readxl.tidyverse.org/
#  Not used due to the number of package dependencies
# -------------------------------------------------------------------

#library(dplyr) --- marks will be deducted if you use this library for
#                   unknow reasons in your assignment
#X = X %>% mutate(Quiz1 = Quiz1Q1 + Quiz1Q2 + Quiz1Q3)

X$Tot1  = X$Quiz1 + X$Assign1

# A vector can be sorted using the Shellsort or Quicksort algorithms:
# sort() returns sorted vector
# rank() returns the order of values in a numeric vector;
# order() returns a vector of indices that will sort a vector

sort(X$Tot1)    # Sort results

rank(X$Tot1)    # Ranking (small to large)
cbind(X[, c("Name", "Tot1")], Rank=rank(-X$Tot1))  # large to small

# Sort by column "Tot1" from small to large as in Excel
order(X$Tot1)   # Get the order
X[order(X$Tot1), c("Name", "Tot1")]    # Order by total

#
# Dealing with Categorical Data (Topic 1)
# 1. If the categorical data in string type, try changing them to 
#    categorical data in R using factor()
# 2. Check data type: sapply(X, class)
X$LearnerType = factor(X$LearnerType)
summary(X)

#
# Removing some irrelevant columns, e.g. Name
#
X$Name      = NULL

#
# Converting useful categorical data to numeric:
# 1. Create a copy of selected columns of the original data
# 2. Convert categorical data to numeric, treating it as ordinal
#
X.numeric = X[ , 2:8]
X.numeric$LearnerType = as.integer(X.numeric$LearnerType)

#
# Standardisation of X.numeric
#
X.numeric.std = scale(X.numeric)
#
# Min-Max Scaling of X.numeric : Need to do it manually or load library
#
minmax = function(x) { (x-min(x))/(max(x)-min(x)) }
X.numeric.mms = apply(X.numeric, 2, minmax)   # 1 = row, 2 = column


# -------------------------------------------------------------------
#  Practical: Working with IO on tabular data
# -------------------------------------------------------------------

#
# Reading data frame from string
# When column 1 is the row names, we can use row.names=1
#
d.f = read.csv(text=
"Day,Outlook,Temp,Humidity,Wind,Decision
1,Sunny,85,85,Weak,No
2,Sunny,80,90,Strong,No
3,Overcast,83,78,Weak,Yes
4,Rain,70,96,Weak,Yes
5,Rain,68,80,Weak,Yes
6,Rain,65,70,Strong,No
7,Overcast,64,65,Strong,Yes
8,Sunny,72,95,Weak,No
9,Sunny,69,70,Weak,Yes
10,Rain,75,80,Weak,Yes
11,Sunny,75,70,Strong,Yes
12,Overcast,72,90,Strong,Yes
13,Overcast,81,75,Weak,Yes
14,Rain,71,80,Strong,No", row.names=1)

#
# The Temp(erature) is in USA Fahrenheit scale.
# We want to convert it to Celcius, we can
#
d.f$Temp.C = 5/9 * (d.f$Temp-32)

#
# Saving the processed data in the order 
# Outlook, Temp.C, Humidity, Wind, Decision
# => Column 1,6,3,4,5
#
write.csv(d.f[,c(1,6,3:5)], "tennis_new.csv")
#
# Try openning tennis_new.csv in Excel to confirm that data is saved correctly.
#

#
#  Reading data frame from Arff (Attribute-Relation File Format), which is
#  similar to CSV with some declarations.
#  Miscellaneous Arff data:
#    https://storm.cis.fordham.edu/~gweiss/data-mining/datasets.html
#

# https://storm.cis.fordham.edu/~gweiss/data-mining/weka-data/cpu.arff
# https://liaohaohui.github.io/UECM3993/cpu.arff
CPU = read.csv("cpu.arff", skip=17, header=FALSE)

#
#  Reading data frame from tabular data with `spacing'.
#  E.g. https://liaohaohui.github.io/UECM3993/Auto.data
#
#  Need to set header to true when the data has a header
#
Auto=read.table("Auto.data", header=T)


# -------------------------------------------------------------------
#  Practical: Exploratory Data Analysis (EDA) on Tabular Data (e.g. Auto)
# -------------------------------------------------------------------

# Check dimension or data
dim(Auto)      # nrow(Auto)  ncol(Auto)

# Looking at the first few columns
head(Auto)

# Looking at the first few columns
tail(Auto)

# Getting the headers / column names.  Alternative: colnames(Auto)
names(Auto)

#
# EDA : Numeric Univariate Analysis
#
# 1. Check each column data type
sapply(Auto, class)   # horsepower is "character", something wrong

# 2. If the column data type is "character", then
#    summary() may not produce good results
summary(Auto)    # We cannot see the missing values which is `?'

# 3. For the column data type being "character", using table()
#    may give more useful information.
table(Auto$horsepower)

# 4. We find "?" showing up in the last part apart from all numeric values
#    We suspect it is a missing value and check the rows with missing value "?"
apply(Auto=="?",1,sum)

#
# 5. If we discover missing values which read.table() cannot recognise,
#    we need to re-read the data again with na.strings=c("?")
#
Auto=read.table("Auto.data",header=T,na.strings="?")

# 6. Don't forget to perform Univariate Analysis after loading the data
#    We can find NA's showing horsepower.
#    In R, NA = Not Available = Missing Value
#
summary(Auto)    # Now the summary is nice

#
# Some statisitcal commands in R will automatically omit NA's in 
# the calculations but some will not.
#
# Let us investigate the mean, var, sd on Columns 1 to 8
#
colMeans(Auto[,1:8])           # Handles NA by returning NA
colMeans(Auto[,1:8], na.rm=T)  # NA will be omitted
# Variances are usually large
apply(Auto[,1:8], 2, function(column) { var(column, na.rm=T) })
# We may prefer sample standard deviation
apply(Auto[,1:8], 2, function(column) { sd(column, na.rm=T) })

#
# Using na.rm=T all the time is tedious, we may consider removing
# rows with missing values when the missing values are less than
# 5% of the original data
#
# 1. showing rows with missing values (NA = Not Available)
# 2. create a clean data without missing values
# 3. compare the dimension of the original and clean data
#
Auto[apply(is.na(Auto),1,sum)>0,]
Auto.clean = na.omit(Auto)   # na.omit removes all rows with missing values

dim(Auto)
dim(Auto.clean)   # 5 rows removed

#
# EDA : Visual Univariate Analysis (Data Visualisation) => Histograms
# hist() seems to remove NA's automatically 
#
par(mfrow=c(2,4))
cn = names(Auto)[1:8]
for (i in 1:8) {
  hist(Auto[,i],col="blue",xlab="",main=cn[i])
}

#
# EDA : Numeric Bivariate Analysis (Data Visualisation)
#   (a) numeric input vs numeric output     => correlation coefficient
#       Many correlation coefficients => correlation matrix
#   (b) categorical input vs numeric output => ???
#   (c) categorical input vs categorical output => Chi square test?
#
round(cor(Auto[,1:8]),2)           # Using original data with missing values
round(cor(Auto.clean[,1:8]),2)     # Using clean data

#
# EDA : Visual Bivariate Analysis (Data Visualisation)
#   (a) numeric input vs numeric output     => scatter plot
#       Many scatter plots => pair plot
#   (b) categorical input vs numeric output => boxplot
#   (c) categorical input vs categorical output => barplot
#
plot(Auto[,1:8])     # or pairs(Auto[,1:8])

# If we want to select particular columns to focus, we need to use
# "formula".  An expression in R with a tilde (~) is a formula.
#
pairs(~ mpg + displacement + horsepower + weight + acceleration, Auto)

#
# Based on the Univariate Analysis, some numeric columns should be changed
# to categorical data, e.g. cylinders, origin
#
Auto$cylinders = as.factor(Auto$cylinders)
Auto$origin    = as.factor(Auto$origin)

#
# Making prettier boxplots with extra options
#
cylinders = Auto$cylinders
mpg = Auto$mpg
par(mfrow=c(2,2))   # create a 2x2 subplots
plot(cylinders, mpg, col="red")
plot(cylinders, mpg, col="red", varwidth=T)
plot(cylinders, mpg, col="red", varwidth=T, horizontal=T)
plot(cylinders, mpg, col="red", varwidth=T, xlab="cylinders", ylab="MPG")

#
#  Bar plot example  vs  Tabular presentation
#
par(mfrow=c(1,1))
plot(Auto$cylinders, Auto$origin)
table(Auto$cylinders, Auto$origin)
#
#  table is usually better for categorical data vs categorical data
#


# -------------------------------------------------------------------
#  (C) Resampling for estimating statistics (corresponding to Topic 2)
# -------------------------------------------------------------------

#
# Suppose the true model is rnorm(100, the.mean, the.stdv)
#
the.mean = 10
the.stdv =  2   # standard deviation is 2
set.seed(2025)  # Fix the starting value of random number generator
the.samples = rnorm(100, the.mean, the.stdv)
count = length(the.samples)
#
# For probability theory, we know the standard deviation of
# the true model rnorm(100, the.mean, the.stdv) is the.stdv
#
# From the samples, the unbiased estimate of the.stdv is
sd(the.samples)

# Holdout Resampling Method Estimate
# the 'sample' function depends on the seed
set.seed(2025)
idx = sample(count, 0.7*count)   # Linear Sampling of 70% of the Indices
# We are 'resampling' from the samples!
sd(the.samples[idx])

# LOOCV
loocv = 0    # create the variable 'loocv' for storing data
for(i in 1:count) {
  loocv[i] = sd(the.samples[-i])
}
mean(loocv)

d.f = data.frame(x=1:20, y = (1:20)^2)
loocv = 0
for(i in 1:nrow(d.f)) {
  m = lm(y ~ poly(x,2), d.f[-i,])
  yhat = predict(m, d.f[i, ])
  loocv[i] = (yhat-d.f[i,]$y)**2
}

# 10-fold CV
# 100 data cut into 10-fold, each fold has 10 items
range = 1:(count/10)
fold = 0
for(i in 1:10){
  fold[i] = sd(the.samples[-(range+(i-1)*10)])
}
mean(fold)

# Simple Boostrapping (Sample with Replacement)
set.seed(2025)
idx.boostrap = sample(count, 0.7*count, replace=TRUE)
sd(the.samples[idx.boostrap])


