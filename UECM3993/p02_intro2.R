# -------------------------------------------------------------------
# Purpose: Basic Commands for Data Processing in R (Part 2)
# Author : Liew How Hui (2022)
# Reference: http://faculty.marshall.usc.edu/gareth-james/ISL/Chapter%202%20Lab.txt
# Data   : https://www.statlearning.com/resources-second-edition
#          (Old: http://faculty.marshall.usc.edu/gareth-james/ISL/data.html)
# License: BSD-3
# Software: R 3.6 & R 4.x
# Duration: 1 hour
# -------------------------------------------------------------------

# -------------------------------------------------------------------
#  Practical: Data Frame (2D Structured Data)
# -------------------------------------------------------------------

## readr: Read Rectangular Text Data
## The goal of 'readr' is to provide a fast and friendly way to read rectangular data (like 'csv', 'tsv', and 'fwf'). It is designed to flexibly parse many types of data found in the wild, while still cleanly failing when data unexpectedly changes.
#install.packages("readr")
#library(readr)

# Check current working directory
getwd()
# Change the working directory
#setwd("PathToYourFolder")

#
# Data Frame = Generalisation of Matrix to handle the mix of 
# numeric and categorical data
#
# Reading data frame from string
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
14,Rain,71,80,Strong,No")

#
# We can delete the first column Day using
#
d.f$Day = NULL
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

### Loading ``structured'' table to R Data Frame
# https://liaohaohui.github.io/UECM3993/Auto.data
Auto=read.table("Auto.data")
head(Auto)
Auto=read.table("Auto.data",header=T,na.strings="?",stringsAsFactors=TRUE)
head(Auto)

# https://liaohaohui.github.io/UECM3993/College.csv
College=read.csv("College.csv",na.strings="?",stringsAsFactors=TRUE)
head(College)

#
# Commands to understand basic statistics of a data frame
# and commands to remove `missing values'
#
dim(Auto)      # nrow(Auto)  ncol(Auto)
names(Auto)    # column names.  Alternative: colnames(Auto)
Auto[1:4,]     # Assess particular rows / columns
# Inside R, NA = Not Available = Missing Value
Auto=na.omit(Auto)   # na.omit removes all rows with missing values
dim(Auto)      # check the dimension again to see if some rows are removed

### Additional Graphical Summaries
# On the $ sign: Table$Column = the `Column' from `Table'
plot(Auto$cylinders, Auto$mpg)   # scatter plot = numeric vs numeric
cylinders=as.factor(Auto$cylinders)   # In R, factor = categorical data
mpg = Auto$mpg
plot(cylinders, mpg)   # boxplot = categorical vs numeric

par(mfrow=c(2,2))   # create a 2x2 subplots
plot(cylinders, mpg, col="red")
plot(cylinders, mpg, col="red", varwidth=T)
plot(cylinders, mpg, col="red", varwidth=T, horizontal=T)
plot(cylinders, mpg, col="red", varwidth=T, xlab="cylinders", ylab="MPG")

# Histogram => univariate numeric data
par(mfrow=c(1,2))
hist(mpg)
hist(mpg,col=2)  # 2 = "red" (k, rgb, cmy, gray)

par(mfrow=c(1,1))
pairs(Auto)         # Pair plots = Scatter plots for all numeric columns
pairs(~ mpg + displacement + horsepower + weight + acceleration, Auto)
plot(Auto$horsepower,mpg)

### Numerical Summaries (less nice for numeric data?)
# EDA = Exploratory Data Analysis
summary(Auto)
summary(mpg)

#
# pairs is for numeric vs numeric
# boxplot is for numeric vs categorical
# barplot is for categorical vs categorical
# Ref: https://ademos.people.uic.edu/Chapter11.html#5_bar_graphs
#


# -------------------------------------------------------------------
#  Practical: IO, if-else, for, and defining functions
# -------------------------------------------------------------------

# IO = Input (read.csv, read.table) & Output (cat, print)
x = c(1,2,1,2,3,2)
y = c(1,2,1,2,1,2)
cat("similarity(",x,",",y,")=", sum(x==y)/length(x), "\n", sep="")

# if-else (similar to C, C++, Java) & defining function
thegrade = function(val) {
    if (val<0 || val>100) {
		return("????")
    } else if (val >= 90) {
		return("A+")
    } else if (val >= 80) {
		return("A")
    } else if (val >= 75) {
		return("A-")
    } else if (val >= 70) {
		return("B+")
    } else if (val >= 65) {
		return("B")
    } else if (val >= 60) {
		return("B-")
    } else if (val >= 55) {
		return("C+")
    } else if (val >= 50) {
		return("C")
    } else                {
		return("F")
    }
}

f = function(x) {   if(x<0)  {return(-1)}   else  {return(1)}   }
sapply(c(1,2,3,-1),f)

# for loop (is a bit rarely used in R)
x = c(-1,0,1,-3,-4,1,3,3,-1)
for ( i in 1:length(x) ) {
    if (x[i] < 0) {
        cat(x[i], "is negative\n")
	} else {
        cat(x[i], "is nonnegative\n")
	}
}
# Working on ``array'' is simpler than working with for loop
y1 = sin(x)
num_to_class = function(x) {
	if (x < 0) { "Negative" } else { "Nonneg" }
}
#num_to_class = function(x) {
#	if (x < 0) { return("Negative") } else { return("Nonneg") }
#}
v_num_to_class = Vectorize(num_to_class)
y2 = v_num_to_class(x)   # Simpler: y2 = ifelse(x<0,"Negative", "Nonneg")


# -------------------------------------------------------------------
#  https://astrostatistics.psu.edu/datasets/2006tutorial/2006desc.html
# -------------------------------------------------------------------

# https://astrostatistics.psu.edu/datasets/HIP_star.dat
hip = read.table("https://liaohaohui.github.io/UECM3993/HIP_star.dat", header=T,fill=T)

# learning about the `astrophysics data' hip
dim(hip)
names(hip)
# Go through each column
for(i in 1:ncol(hip)) {
    print(c(max(hip[,i]), min(hip[,i]), median(hip[,i]), mad(hip[,i])))
}
# Apply is `shorter' than using for loop
# 1 = Row
# 2 = Column
apply(hip, 2, max)
apply(hip, 2, min)
apply(hip, 2, median)
apply(hip, 2, mad)  # Median Absolute Deviation
# summary(hip) gives max, min, median, mean

# Find out how many missing values (NA = Not Available) and `where' missing values are
sum(is.na(hip[,9]))
which(is.na(hip[,9]))
# To skip missing values, some statistics functions in R allow
# us to skip `missing values' using na.rm=T
for(i in 1:ncol(hip)) {
    print(c(max(hip[,i],na.rm=T), min(hip[,i],na.rm=T), median(hip[,i],na.rm=T), mad(hip[,i],na.rm=T)))
}

# A vector can be sorted using the Shellsort or Quicksort algorithms; rank returns the order of values in a numeric vector; and order returns a vector of indices that will sort a vector. The last of these functions, order, is often the most useful of the three, because it allows one to reorder all of the rows of a matrix according to one of the columns:

# Sort column number 4:
sort(hip[1:10,4])
hip[order(hip[1:10,4]),]

###  Arff (Attribute-Relation File Format), similar to CSV with some declarations
# Example: https://storm.cis.fordham.edu/~gweiss/data-mining/weka-data/cpu.arff
# https://storm.cis.fordham.edu/~gweiss/data-mining/datasets.html

CPU = read.csv("https://storm.cis.fordham.edu/~gweiss/data-mining/weka-data/cpu.arff", skip=17, header=FALSE)

# -------------------------------------------------------------------
#  Resampling
# -------------------------------------------------------------------

#
# Suppose the true model is rnorm(100, the.mean, the.stdv)
#
the.mean = 10
the.stdv =  2   # standard deviation is 2
set.seed(2022)  # Fix the starting value of random number generator
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
set.seed(2022)
idx = sample(count, 0.7*count)   # Linear Sampling of 70% of the Indices
# We are 'resampling' from the samples!
sd(the.samples[idx])

# LOOCV
loocv = 0    # create the variable 'loocv' for storing data
for(i in 1:count) {
  loocv[i] = sd(the.samples[-i])
}
mean(loocv)

# 10-fold CV
# 100 data cut into 10-fold, each fold has 10 items
range = 1:(count/10)
fold = 0
for(i in 1:10){
  fold[i] = sd(the.samples[-(range+(i-1)*10)])
}
mean(fold)

# Simple Boostrapping (Sample with Replacement)
set.seed(2022)
idx.boostrap = sample(count, 0.7*count, replace=TRUE)
sd(the.samples[idx.boostrap])

