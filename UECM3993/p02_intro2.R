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

#setwd("PathToYourFolder")

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
dim(Auto)
names(Auto)    # column names.  Alternative: colnames(Auto)
Auto[1:4,]
Auto=na.omit(Auto)   # na.omit removes all rows with missing values
dim(Auto)      # check the dimension again to see if some rows are removed

### Additional Graphical and Numerical Summaries
plot(Auto$cylinders, Auto$mpg)
cylinders=as.factor(Auto$cylinders)
mpg = Auto$mpg
plot(cylinders, mpg)

par(mfrow=c(2,2))   # create a 2x2 subplots


plot(cylinders, mpg, col="red")
plot(cylinders, mpg, col="red", varwidth=T)
plot(cylinders, mpg, col="red", varwidth=T,horizontal=T)
plot(cylinders, mpg, col="red", varwidth=T, xlab="cylinders", ylab="MPG")

par(mfrow=c(1,2))
hist(mpg)
hist(mpg,col=2)
par(mfrow=c(1,1))
pairs(Auto)         # Pair plots = Scatter plots for all numeric columns
pairs(~ mpg + displacement + horsepower + weight + acceleration, Auto)
plot(Auto$horsepower,mpg)
summary(Auto)
summary(mpg)

# -------------------------------------------------------------------
#  Practical: IO, if-else, for, and defining functions
# -------------------------------------------------------------------

# IO = Input (read.csv, read.table) & Output (cat, print)
x = c(1,2,1,2,3,2)
y = c(1,2,1,2,1,2)
cat("similarity(",x,",",y,")=", sum(x==y)/length(x), "\n", sep="")

# if-else & defining function
thegrade = function(val) {
    if (val<0 || val>100) { return("????")
    } else if (val >= 90) { return("A+")
    } else if (val >= 80) { return("A")
    } else if (val >= 75) { return("A-")
    } else if (val >= 70) { return("B+")
    } else if (val >= 65) { return("B")
    } else if (val >= 60) { return("B-")
    } else if (val >= 55) { return("C+")
    } else if (val >= 50) { return("C")
    } else                { return("F")
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
apply(hip, 2, max)
apply(hip, 2, min)
apply(hip, 2, median)
apply(hip, 2, mad)

# Find out how many missing values and `where' missing values are
sum(is.na(hip[,9]))
which(is.na(hip[,9]))
# To skip missing values, some statistics functions in R allow
# us to skip `missing values' using na.rm=T
for(i in 1:ncol(hip)) {
    print(c(max(hip[,i],na.rm=T), min(hip[,i],na.rm=T), median(hip[,i],na.rm=T), mad(hip[,i],na.rm=T)))
}

# A vector can be sorted using the Shellsort or Quicksort algorithms; rank returns the order of values in a numeric vector; and order returns a vector of indices that will sort a vector. The last of these functions, order, is often the most useful of the three, because it allows one to reorder all of the rows of a matrix according to one of the columns:

sort(hip[1:10,3])
hip[order(hip[1:10,3]),]


