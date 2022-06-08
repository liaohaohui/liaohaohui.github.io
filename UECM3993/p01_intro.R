# -------------------------------------------------------------------
# Purpose: Practical: Basic R Commands for Data Processing (Part 1)
# Author : Liew How Hui (2022)
# Reference: http://faculty.marshall.usc.edu/gareth-james/ISL/Chapter%202%20Lab.txt
# License: BSD-3
# Software: R 3.6 & R 4.x
# Duration: 1 hour
# -------------------------------------------------------------------

# -------------------------------------------------------------------
#  Getting familiar with the Basic R and R Studio
# -------------------------------------------------------------------

# 1. Basic R (for fast startup and lower memory comsumption)
#
#    Download link: https://cran.r-project.org/
#
#    It supports Windows, MacOS and Linux, download the software
#    related to your computer.

# 2. RStudio (for a computer with more than 8 Gig RAM and fast CPU
#
#    Donwload link: https://www.rstudio.com/products/rstudio/download/
#
#    Get the "RStudio Desktop"

# -------------------------------------------------------------------
#  Practical: Very Basic Programming Notations
# -------------------------------------------------------------------

# Line comment starts with the `#' (sharp, e.g. C# / hash, twitter hash tag) symbol
# In C, C++, JavaScript, the line comment is usually double slash `//'

# -------------------------------------------------------------------
#  Practical: Variables and Assignment in R
# -------------------------------------------------------------------

# Variables: `names' which are used to reference data in memory
#
# 1. ls() is used to list the environment variables
#
ls()
#
#    In RStudio, the variables will be listed on the top right hand
#    window.
#
# 2. Variables are created using assignment operation <- or =
#    Most books use the notation `<-' but we will be using `=' 
#    in the practicals since it is easier to type.
#
# 3. Variables can be 'a'-'z', 'A'-'Z', '0'-'9' and '.' (dot)
#    Try to avoid using `c' for variable name, it is used to create 
#    an array.
#
a <- 1                  # the floating point number 1
a.second.Variable = 1L  # L is used to create an integer
#
# 4. Try ls() to see the variables created (or for RStudio users
#    look at the top right hand window to watch the variables
#
ls()
#
# 5. We can check the data type/class of a variable using the command 'class'
#
class(a)
class(a.second.Variable)
#
# 6. Variables that are no longer needed can be deleted:
#
rm(a)
ls()
#
# 7. We can move all variable but this is usually not recommended
# rm(list=ls())  # Not recommended in general
#

# -------------------------------------------------------------------
#  Practical: Basic and Array Data Structures and Operations in R
# -------------------------------------------------------------------

##
## (A) Letters & Strings / Characters
##

"A String in double quotes"
'A String in single quotes'
"Escaping quotes using backslash \""
"A string\n with multiple lines\n using `backslash n'"
# cat = concatenate to output
# use ?cat to get help for `cat'
cat("A string\n with multiple lines\n", " using `backslash n'")

# If we don't assign the variables to a variable, R will print them out

# Joining strings with paste
# by default they will be `joined' by a `space' between them
paste("A string", "and another string")

# If want to change the `gap' between strings, we need to change `sep'

A = paste("A", "B", "C", "D", sep=",")

# Find the length of a string
nchar("Hello World")

##
## (B) (Real) Floating Numbers and Arithmetic Operations
##
a = 1 + 2
b = 2 - 3
d = 3 * 4
e = 4 / 5
f = 5 ** 6    # 5 to the power of 6 (** is also used in Python)
g = 5 ^ 6     # Also 5 to the power of 6 (^ has different meaning in Python)

##
## (C) Integers and Arithmetic Operations
##
a = 1L + 2L
b = 2L - 3L
d = 3L * 4L
e = 4L / 5L   # Usual number division, NOT integer division
f = 5 ** 6    # 5 to the power of 6
g = 5 ^ 6     # Also 5 to the power of 6
##
## Non-basic data structure => Dates (internally integer)
##
mydates = as.Date("2022-06-22")
today = Sys.Date()

##
## (D) Booleans / Logicals => Basic Maths / Discrete Maths
##
TRUE  & TRUE
TRUE  | FALSE
!FALSE
((a < b) & (d < e)) | (f > g)

##
## (E) 1-D Numeric Arrays, Operations and Statistics in R
##

#
# c() is used to create a general 1D array
#
x = c(5,4,5,3,5,5,5,2)
# We can use length to check the array length
length(x)
# For numeric arrays, we can add, subtract, multiply etc.
y1 = 2*x-1
y2 = x^2
# Elementwise function operation to 1D array
y3 = sqrt(x)
y4 = 2*sin(x) + 3*cos(x)
#
# Statistical functions
#
length(x) # n
sum(x)    # x[1] + ... + x[n]
mean(x)   # x.bar = (x[1] + ... + x[n])/n
var(x)    # sample variance: sum((x-x.bar))^2)/(n-1)
sd(x) == sqrt(sum((x-mean(x))^2)/(length(x)-1))     # sample standard deviation

#
# Special Statistical Feature in R
#
# If we want to add repeating 1D data like
#    5,4,5,3,5,5,5,2 +
#    1,2,1,2,1,2,1,2
# => 6 6 6 5 6 7 6 4
# In R, we can just perform 
x + c(1,2)
#
# R will perform cyclic repeat for us.
#
# Are you able to tell y5 below without using R?
y5 = c(2,3)*x + c(5,4,-2,3)

#
# Regular 1D arithmetic sequences in R
#
x.sequence1 = 1:10    # or seq(1,10)
x.sequence2 = 20:1    # or seq(20,1)
#
# Arithmetic progression sequence:
#    a_n = a_0 + (n-1)*d
#    seq(a_0, num, d),   a_n <= num < a_n + d
#
x.sequence3 = seq(1,30,2)
#
# Arithmetic progression sequence used in plotting
#    a_0, a_1, ..., a_(N-1)
#    a_n = a + (n-1)*(b-a)/(N-1), n=0,1,...,N-1
#
x.sequence4 = seq(-pi,pi,length=50)
y.sequence4 = exp(x.sequence4)
#
# Default plot of y vs x (scatter plot)
#
plot(x.sequence4, y.sequence4)        # Default: points
plot(x.sequence4, y.sequence4, 'l')   # Change to line
#
# 2D plotting of numeric data with labels and title
#
plot(x.sequence4, y.sequence4, xlab="x",ylab="y",main="Scatter Plot of X vs Y")
# If we no longer need the variables, we should remove them
# to prevent from misusing them below.
rm(x.sequence4, y.sequence4)
#
# Various 2D plotting options: col (colours), pch (point symbols), 
#   cex, (point size), lwd (line width).
#
plot(
  1:25,
  cex=3,
  lwd=3,
  pch=1:25,
  col=rainbow(25),
  bg=terrain.colors(5)  # only for pch 21:25
)

#
# Regular 1-D random sequences in R
#
# Seed is used to make random number generation less random, i.e.
# we can repeat the "random number generation".  The theory of
# seed number is probably mentioned in SSIF but you don't need to
# take SSIF just like anyone driving a car does not need to know
# how to design and construct a car.
set.seed(2022)
# r = random numbers, 50 = 50 elements, unif = uniform dist.
x.unif = runif(50)
# r = random numbers, 50 = 50 elements, norm = normal dist.
x.norm = rnorm(50)
y.norm = x.norm + rnorm(50,mean=50,sd=.1)
# Correlation between two 1-D arrays
cor(x.norm,y.norm)

##
## (F) 1-D String Arrays
##

#
# Categorical data are usually represented by an array of characters or strings
#
gender = c("M", "F", "F", "M", "M", "F", "F")
# By checking the class, we can see that they are characters (i.e. strings)
class(gender)
# We can use table to summarise the `string' categorical data
table(gender)
# We can use summary() to summarise the categorical data by converting
# it from characters to factors
summary(factor(gender))

#
# Special Array of Strings
#
letters  # Array/Vector of small letter alphabet characters
LETTERS  # Array/Vector of capital letter alphabet characters

letters[5:10]    # Assessing elements in an array

# If we pass in an array/vector of string, we need to use `collapse'

a = c("ab", "bc", "cd", "de")  # c() constructs an array of something
a = paste(a, collapse="+")

#
# Comparing two 1-D arrays
#
c(1,2,3) == c(3,2,1)
any(c(1,2,3) == c(3,2,1))    # Any single true element?
all(c(1,2,3) == c(3,2,1))    # Are all elements true?
# Contingency table / Confusion matrix
table(c(1,0,1,0,1), c("Y","N","N","Y","Y"))

##
## (H) 2-D Numeric Arrays & Operations
##     Note: A 2-D Numeric Array is just a matrix
##     Getting help/documentation: ?matrix

# A is a 4 by 4 matrix (data is filled in columns by default)
A = matrix(1:16,nrow=4,ncol=4)
# To fill in by rows, we need to set option
B = matrix(1:16,4,4,byrow=TRUE)

#
# Note that A and B are in 'transpose relation', i.e.
# A = t(B)
#

#
# Matrix operations
#
2*A     # scalar multiplication
A+2     # adding every element with a number
A+B     # matrix addition
A*B     # elementwise multiplication, NOT matrix multiplication
A%*%B   # matrix multiplication
# A*B == B*A BUT A%*%B != B%*%A

#
# Indexing
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

# -------------------------------------------------------------------
# Optional: 2-D array rotation in 2-D graphics
# Prerequisites: Linear Algebra
# -------------------------------------------------------------------

x1 = c(2,1.5)
x2 = c(2,1)
y = c(1,1)
x1 = c(x1,-x1,x1,-x1)
x2 = c(x2,x2,-x2,-x2)
y = c(y,y,-y,-y)
plot(x1,x2,col=2+y, pch=15+y, cex=1.5)
tmp = matrix(c(x1,x2),ncol=2)
tt = pi/4   # 180 / 4 = 45 degree
rot.matrix = matrix(c(cos(tt),-sin(tt),sin(tt),cos(tt)),2,2)
data1 = tmp %*% rot.matrix
a = 1
data1 = data1 + c(rep(0,nrow(tmp)), rep(a,nrow(tmp)))
plot(data1, col=2+y, pch=15+y, cex=2)
abline(a, tan(tt))

