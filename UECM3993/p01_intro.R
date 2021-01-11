# -------------------------------------------------------------------
# Purpose: Basic Commands for Data Processing in R
# Author : Liew How Hui (2021)
# Reference: http://faculty.marshall.usc.edu/gareth-james/ISL/Chapter%202%20Lab.txt
# Data   : http://faculty.marshall.usc.edu/gareth-james/ISL/data.html
# License: BSD-3
# Software: R 3.6 & R 4.0
# -------------------------------------------------------------------

# -------------------------------------------------------------------
#  Practical: `Basic Structures' and Operations in R
# -------------------------------------------------------------------

# Line comment starts with the `#' (sharp) symbol
# In C, C++, JavaScript, the line comment is usually double slash `//'

### Letters & Strings / Characters

letters
LETTERS
"A String in double quotes"
'A String in single quotes'
"Escaping quotes using backslash \""
"A string\n with multiple lines\n using `backslash n'"

# Joining strings with paste
paste("A string", "and another string")
# by default they will be `joined' by a `space' between them

# A variable is something to the left of 
# the ``assignment'' operation  <- or =
a = paste("a", "b", "c", "d")
A = paste("A", "B", "C", "D")
ls()
rm(a,A)
ls()
rm(list=ls())  # Don't do this

# Find the length of a string
nchar("Hello World")


### (Real) Numbers
1 + 2
2 - 3
3 * 4
4 / 5

### Integers
1L + 2L
2L - 3L
3L * 4L
4L / 5L

### Booleans / Logicals
TRUE  & TRUE
TRUE  | FALSE
!FALSE


# -------------------------------------------------------------------
#  Practical: Basics of 1D & 2D `Arrays' and Operations in R
# -------------------------------------------------------------------

# 1D Arrays & Operations
x <- c(1,3,2,5)
y <- c(1,4,3)
length(x)
length(y)
x+y
x*y
sequence1 = c(1:10)
sequence2 = c(20:1)
sequence3 = seq(1,30,2)
x^2
sqrt(x)

# 2D Arrays & Operations
?matrix     # Question mark is for getting help
x = matrix(data=c(1,2,3,4),nrow=2,ncol=2)
y = matrix(c(3,2,1,5),2,2)
z = matrix(c(5,4,3,2),2,2,byrow=TRUE)
x*y
y*x
x%*%y
y%*%x
z=matrix(1:16,4,4)
z[2,3]
z[c(1,3),c(2,4)]
z[1:3,2:4]
z[1:2,]
z[,1:2]
z[1,]
z[-c(1,3),]
z[-c(1,3),-c(1,3,4)]
dim(z)
# Comparing two vectors
c(1,2,3) == c(3,2,1)
table(c(1,0,1,0,1), c("Y","N","N","Y","Y"))

# 2D data is useful in the understanding of 2D Predictive Models
x1 = c(2,1.5)
x2 = c(2,1)
y = c(1,1)
x1 = c(x1,-x1,x1,-x1)
x2 = c(x2,x2,-x2,-x2)
y = c(y,y,-y,-y)
plot(x1,x2,col=2+y, pch=15+y, cex=1.5)
tmp = matrix(c(x1,x2),ncol=2)
tt = pi/4
rot.matrix = matrix(c(cos(tt),-sin(tt),sin(tt),cos(tt)),2,2)
data1 = tmp %*% rot.matrix
a = 1
data1 = data1 + c(rep(0,nrow(tmp)), rep(a,nrow(tmp)))
plot(data1, col=2+y, pch=15+y, cex=2)
abline(a, tan(tt))

# Random data & Basic Statistics
x=rnorm(50)
y=x+rnorm(50,mean=50,sd=.1)
cor(x,y)
set.seed(1303)
rnorm(50)
set.seed(3)
y=rnorm(100)
mean(y)
var(y)
sqrt(var(y))
sd(y)

# -------------------------------------------------------------------
#  Practical: Graphics
# -------------------------------------------------------------------

x=rnorm(100)
y=rnorm(100)

plot(x,y)
plot(x,y,xlab="this is the x-axis",ylab="this is the y-axis",main="Plot of X vs Y")
plot(x,y,col="green")

x=seq(1,10)
x=1:10
x=seq(-pi,pi,length=50)

plot(
  1:25,
  cex=3,
  lwd=3,
  pch=1:25,
  col=rainbow(25),
  bg=terrain.colors(5)  # only for pch 21:25
)


# -------------------------------------------------------------------
#  Practical: Data Frame (2D Structured Data)
# -------------------------------------------------------------------

## readr: Read Rectangular Text Data
## The goal of 'readr' is to provide a fast and friendly way to read rectangular data (like 'csv', 'tsv', and 'fwf'). It is designed to flexibly parse many types of data found in the wild, while still cleanly failing when data unexpectedly changes.
#install.packages("readr")
#library(readr)

#setwd("PathToYourFolder")

### Loading ``structured'' table to R Data Frame
Auto=read.table("DataLab/Auto.data")
head(Auto)
Auto=read.table("DataLab/Auto.data",header=T,na.strings="?",stringsAsFactors=TRUE)
head(Auto)

College=read.csv("DataLab/College.csv",header=T,na.strings="?",stringsAsFactors=TRUE)
head(College)

dim(Auto)
Auto[1:4,]
Auto=na.omit(Auto)
dim(Auto)
names(Auto)

### Additional Graphical and Numerical Summaries
plot(Auto$cylinders, Auto$mpg)
cylinders=as.factor(Auto$cylinders)
mpg = Auto$mpg
plot(cylinders, mpg)

par(mfrow=c(2,2))


plot(cylinders, mpg, col="red")
plot(cylinders, mpg, col="red", varwidth=T)
plot(cylinders, mpg, col="red", varwidth=T,horizontal=T)
plot(cylinders, mpg, col="red", varwidth=T, xlab="cylinders", ylab="MPG")

par(mfrow=c(1,2))
hist(mpg)
hist(mpg,col=2)
par(mfrow=c(1,1))
pairs(Auto)
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

# Def    : https://en.wikipedia.org/wiki/Gini_coefficient
# Math   : http://www.statsdirect.com/help/generatedimages/equations/equation154.svg
# Formula: http://www.statsdirect.com/help/default.htm#nonparametric_methods/gini.htm
# All values are treated equally, arrays must be 1d:
gini = function(arr) {
    # Calculate the Gini coefficient of a numpy array.
    arr = unlist(arr, recursive=TRUE)
    # Shift the data to non-negative
    if (min(arr) < 0) { arr = arr - min(arr) }
    arr = sort(arr)
    if (sum(arr) < 1e-16) { arr = arr + 1e-8 }  # Prevent zero sum arr
    n = length(arr)
    # Gini coefficient:
    return ((sum((2*c(1:n)-n-1)*arr)) / (n*sum(arr)))
}
gini.trapz = function(arr) {  # My derivation using Trapzoidal rule
    arr = unlist(arr, recursive=TRUE)
	if (min(arr) < 0) { arr = arr - min(arr) }
    arr = sort(arr)
    n = length(arr)
    return ((sum(c(2*(1:(n-1))-n-1)*arr[1:(n-1)] + arr[n]))/n/sum(arr))
}
marks_clsA = rep(60, 10)
marks_clsB = c(rep(0, 9), 100)
marks_clsC = seq(10,100,10)
cat("Gini coefficient of class A =", gini(marks_clsA), "\n")
cat("Gini coefficient of class B =", gini(marks_clsB), "\n")
cat("Gini coefficient of class C =", gini(marks_clsC), "\n")
cat("Gini coefficient of class A =", gini.trapz(marks_clsA), "\n")
cat("Gini coefficient of class B =", gini.trapz(marks_clsB), "\n")
cat("Gini coefficient of class C =", gini.trapz(marks_clsC), "\n")

# https://www.r-bloggers.com/gini-index-and-lorenz-curve-with-r/
# http://exzuberant.blogspot.com/2012/12/exploring-inequality-entry-point-to.html
# http://www.stat.ucla.edu/~handcock/RelDist/
# library: ineq, reldist

