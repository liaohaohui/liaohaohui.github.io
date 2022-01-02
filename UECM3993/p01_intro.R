# -------------------------------------------------------------------
# Purpose: Basic Commands for Data Processing in R (Part 1)
# Author : Liew How Hui (2022)
# Reference: http://faculty.marshall.usc.edu/gareth-james/ISL/Chapter%202%20Lab.txt
# Data   : http://faculty.marshall.usc.edu/gareth-james/ISL/data.html
# License: BSD-3
# Software: R 3.6 & R 4.x
# Duration: 1 hour
# -------------------------------------------------------------------

# -------------------------------------------------------------------
#  Practical: Very Basic Programming Notations
# -------------------------------------------------------------------

# Line comment starts with the `#' (sharp) symbol
# In C, C++, JavaScript, the line comment is usually double slash `//'

# Variables: `names' which are used to reference data
# The `<-' or `=' is used to denote `assignment'.  Most books use
# the notation `<-' but we will be using `=' in the practicals.
a <- 1  # a is refering to the floating point number 1
a = 1L  # a is now refering to the integer 1

# Try not to use `c' for variable name, it is used to create an array.

# Variables can have dot `.' which is usually not allowed in other 
# programing languages

# Commands to list and remove variables
ls()
rm(a,A)
ls()
rm(list=ls())  # Not recommended in general

# -------------------------------------------------------------------
#  Practical: `Basic Structures' and Operations in R
# -------------------------------------------------------------------

### Letters & Strings / Characters

"A String in double quotes"
'A String in single quotes'
"Escaping quotes using backslash \""
"A string\n with multiple lines\n using `backslash n'"
letters  # Array/Vector of small letter alphabet characters
LETTERS  # Array/Vector of capital letter alphabet characters

# If we don't assign the variables to a variable, R will print them out

# Joining strings with paste
# by default they will be `joined' by a `space' between them
paste("A string", "and another string")

# If want to change the `gap' between strings, we need to change `sep'

A = paste("A", "B", "C", "D", sep=",")

# If we pass in an array/vector of string, we need to use `collapse'

a = c("a", "b", "c", "d")
a = paste(a, collapse="+")

# Find the length of a string
nchar("Hello World")


### (Real) Floating Numbers and Arithmetic Operations
a = 1 + 2
b = 2 - 3
d = 3 * 4
e = 4 / 5
f = 5 ** 6    # 5 to the power of 6
g = 5 ^ 6     # Also 5 to the power of 6

### Integers and Arithmetic Operations
a = 1L + 2L
b = 2L - 3L
d = 3L * 4L
e = 4L / 5L
f = 5 ** 6    # 5 to the power of 6
g = 5 ^ 6     # Also 5 to the power of 6

### Booleans / Logicals
TRUE  & TRUE
TRUE  | FALSE
!FALSE
((a < b) & (d < e)) | (f > g)

# -------------------------------------------------------------------
#  Practical: Basics of 1D & 2D `Arrays' and Operations in R
# -------------------------------------------------------------------

# 1D Arrays & Operations
x <- c(1,3,2,5)   # Assignment operation in R: `<-', `='
y <- c(1,4,3)
length(x)
length(y)
x+y
x*y
sequence1 = 1:10      # or seq(1,10)
sequence2 = c(20:1)
sequence3 = seq(1,30,2)           # arithmetic progression sequence:
                                  #  a_n = a + (n-1)d
sequence4 = seq(-pi,pi,length=50) # seq(a,b,length=N):
                                  #  a_n = a + (n-1)*(b-a)/(N-1), n=0,1,...,N-1
x^2
sqrt(x)

# 2D Arrays & Operations
?matrix     # Question mark is for getting help
x = matrix(data=c(1,2,3,4),nrow=2,ncol=2)
y = matrix(c(3,2,1,5),2,2)
z = matrix(c(5,4,3,2),2,2,byrow=TRUE)
x*y    # elementwise multiplication
y*x
x%*%y  # matrix multiplication
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
# Comparing two vectors & Contingency table / Confusion matrix
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
z = runif(50)   # for future use
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
#  Practical: Visualising the Data
# -------------------------------------------------------------------

x=rnorm(100)  # A vector
y=rnorm(100)  # A vector

plot(x,y)     # Scatter plot !!!
plot(x,y,xlab="this is the x-axis",ylab="this is the y-axis",main="Plot of X vs Y")
plot(x,y,col="green")

plot(
  1:25,
  cex=3,
  lwd=3,
  pch=1:25,
  col=rainbow(25),
  bg=terrain.colors(5)  # only for pch 21:25
)


