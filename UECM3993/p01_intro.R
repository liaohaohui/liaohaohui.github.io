# -------------------------------------------------------------------
# Purpose: Practical: Basic R Commands for Data Processing (Part 1)
# Author : Liew How Hui (2022)
# Reference: http://faculty.marshall.usc.edu/gareth-james/ISL/Chapter%202%20Lab.txt
# License: BSD-3
# Software: R 3.6 & R 4.x
# Duration: 1 hour
# -------------------------------------------------------------------

# -------------------------------------------------------------------
#  Practical: Very Basic Programming Notations
# -------------------------------------------------------------------

# Line comment starts with the `#' (sharp, e.g. C# / hash, twitter hash tag) symbol
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
#  Practical: `Basic Data Structures' and Operations in R
# -------------------------------------------------------------------

### Letters & Strings / Characters

"A String in double quotes"
'A String in single quotes'
"Escaping quotes using backslash \""
"A string\n with multiple lines\n using `backslash n'"
# cat = concatenate to output
# use ?cat to get help for `cat'
cat("A string\n with multiple lines\n using `backslash n'")

# If we don't assign the variables to a variable, R will print them out

# Joining strings with paste
# by default they will be `joined' by a `space' between them
paste("A string", "and another string")

# If want to change the `gap' between strings, we need to change `sep'

A = paste("A", "B", "C", "D", sep=",")

# Find the length of a string
nchar("Hello World")


### (Real) Floating Numbers and Arithmetic Operations
a = 1 + 2
b = 2 - 3
d = 3 * 4
e = 4 / 5
f = 5 ** 6    # 5 to the power of 6 (** is also used in Python)
g = 5 ^ 6     # Also 5 to the power of 6 (^ has different meaning in Python)

### Integers and Arithmetic Operations
a = 1L + 2L
b = 2L - 3L
d = 3L * 4L
e = 4L / 5L   # Usual number division, NOT integer division
f = 5 ** 6    # 5 to the power of 6
g = 5 ^ 6     # Also 5 to the power of 6

### Booleans / Logicals => Basic Maths / Discrete Maths
TRUE  & TRUE
TRUE  | FALSE
!FALSE
((a < b) & (d < e)) | (f > g)

### Non-basic data structure => Dates <-> integers


# -------------------------------------------------------------------
#  Practical: Basics of 1D & 2D `Arrays' and Operations in R
# -------------------------------------------------------------------

# 1D Arrays & Operations
x <- c(1,3,2,5)   # Assignment operation in R: `<-', `='
y <- c(1,4,3)     # c() is used to create array
length(x)
length(y)
x+y               # Beware!
x*y
# if(length(x) != length(y)) { .... }
sequence1 = 1:10      # or seq(1,10)
sequence2 = c(20:1)
sequence3 = seq(1,30,2)           # arithmetic progression sequence:
                                  #  a_n = a + (n-1)d
          # seq(1,30,by=2)  -> increase by 2
sequence4 = seq(-pi,pi,length=50) # seq(a,b,length=N):
                                  #  a_n = a + (n-1)*(b-a)/(N-1), n=0,1,...,N-1
# Elementwise function operation to 1D array
x^2
sqrt(x)
# Statistical functions (will be introduced below again)
mean(x)
sum(x)
var(x)     # sample statistics
sd(x)      # sample statistics

#
# 1D array of `letters' and strings
#
letters  # Array/Vector of small letter alphabet characters
LETTERS  # Array/Vector of capital letter alphabet characters

letters[5:10]    # Assessing elements in an array

# If we pass in an array/vector of string, we need to use `collapse'

a = c("ab", "bc", "cd", "de")  # c() constructs an array of something
a = paste(a, collapse="+")

#
# 2D Arrays & Operations
#
#?matrix     # Question mark is for getting help
x = matrix(data=c(1,2,3,4),nrow=2,ncol=2)  # Default: Fill by column
y = matrix(c(3,2,1,5),2,2)
z = matrix(c(5,4,3,2),2,2,byrow=TRUE)      # C/C++/Python: Fill by row
x*y    # elementwise multiplication, NOT matrix multiplication
y*x
x%*%y  # matrix multiplication
y%*%x
z=matrix(1:20,4,5)
z[2,3]
z[c(1,3),c(2,4)]
z[1:3,2:4]      # Same as z[c(1,2,3), c(2,3,4)]
z[1:2,]
z[1,]           # Pick the first row
z[,1:2]         # Pick the first two columns
z[-c(1,3),]     # Remove the first & third row
z[-c(1,3),-c(1,3,4)]
dim(z)          # vs length(z) gives the numbers of elements in the matrix z
# Comparing two vectors & Contingency table / Confusion matrix
c(1,2,3) == c(3,2,1)
table(c(1,0,1,0,1), c("Y","N","N","Y","Y"))

# 2D data is useful in the understanding of 2D graphics
# Requires you to recall Linear Algebra
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

# Random data & Basic Statistics
z = runif(50)   # r = random numbers, 50 = 50 elements, unif = uniform dist.
x = rnorm(50)   # r = random numbers, 50 = 50 elements, norm = normal dist.
y = x+rnorm(50,mean=50,sd=.1)
cor(x,y)
set.seed(1303)  # SSIF, seed is the `initial' value for PRNG
rnorm(50)
set.seed(3)
y=rnorm(100)
mean(y)
var(y)          # In R, `sample' statistics => Sum((x_i-xbar)^2)/(n-1)
sqrt(var(y))
sd(y)

# -------------------------------------------------------------------
#  Practical: Visualising the Data
# -------------------------------------------------------------------

x=rnorm(100)  # A vector/array of random numbers ~ N(0,1)
y=rnorm(100)  # A vector/array of random numbers ~ N(0,1)

plot(x,y)     # Scatter plot !!!  Default colour is black
plot(x,y,col="green",xlab="this is the x-axis",ylab="this is the y-axis",main="Plot of X vs Y")

plot(
  1:25,                 # y.  x is not given, so 1:length(y) will be used.
  cex=3,                # Change symbol size
  lwd=3,                # Change line width
  pch=1:25,             # Change symbols
  col=rainbow(25),
  bg=terrain.colors(5)  # only for pch 21:25
)

# For usual function y = f(x), a <= x <= b
#plot(x, y, 'l')

