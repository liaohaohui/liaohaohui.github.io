# -------------------------------------------------------------------
# Purpose: Practical: Basic R Commands for Data Processing (Part 1)
# Author : Liew How Hui (2025)
# Reference: http://faculty.marshall.usc.edu/gareth-james/ISL/Chapter%202%20Lab.txt
# License: BSD-3
# Software: R 4.1+
# Duration: 1 hour
# -------------------------------------------------------------------

# -------------------------------------------------------------------
#  (A) Getting familiar with the Basic R and R Studio
# -------------------------------------------------------------------

# 1. Basic R (Good for fast startup and a computer with lower memory)
#
#    Download link: https://cran.r-project.org/
#
#    It supports Windows, MacOS and Linux, download the software
#    related to your computer.

# 2. RStudio (Good for a computer with 8 Gig RAM or more and fast CPU)
#
#    Donwload link: https://www.rstudio.com/products/rstudio/download/
#
#    Get the "RStudio Desktop"

# -------------------------------------------------------------------
#  (B) Very Basic Programming Notations
#      * Comments
#      * Variables, Assignment, ...
#      * Data class/type
# -------------------------------------------------------------------

# Line comment starts with the `#' (sharp, e.g. C# / hash, twitter hash tag) symbol
# In C, C++, JavaScript, the line comment is usually double slash `//'
#
# Comments are used to put remarks on programming statements, e.g. 
# the purpose / theory of for loop, if-else statement, etc.

# Variables: `names' which are used to reference data in memory
#
# 1. ls() is used to show the variable name list (when the list
#    is empty, character(0) whill be shown)

ls()

#    In RStudio, the variable name list will be listed on the top right 
#    hand window.
#
# 2. Variables are created using assignment operation <- or =
#    Most books use the notation `<-' but we will be using `=' 
#    in the practicals since it is easier to type.

a <- 1                  # the floating point number 1

# 3. Variables can be 'a'-'z', 'A'-'Z', '0'-'9' and '.' (dot)
#    Try to avoid using `c' for variable name, it is used to create 
#    an array.
#

a.second.Variable = 1L  # L is used to create an integer

#    Unlike C/C++/Java programming language, dot (.) is used in 
#    variable names.

# 4. Try ls() to see the variables created (or for RStudio users
#    look at the top right hand window to watch the variables

ls()

#
# 5. We can check the data type/class of a variable using the command 'class'
#

class(a)
class(a.second.Variable)

#
# 6. Variables that are no longer needed can be deleted to free
#    some memory
#

rm(a)
ls()

#
# 7. We can remove all variables but this is usually not recommended
# rm(list=ls())  # Not recommended in general
#

# -------------------------------------------------------------------
#  (C) 1-D Floating Number Array Data Structures and Operations
#
#  Since R is primarily used for statistical analysis, the fundamental
#  data structure in R is vector/array of numbers, booleans & strings.
# -------------------------------------------------------------------

a = 1 + 2
b = 2 - 3
d = 3 * 4
e = 4 / 5
f = 5 ** 6    # 5 to the power of 6 (** is also used in Python)
g = 5 ^ 6     # Also 5 to the power of 6 (^ has different meaning in Python)

#
#  When you show the variables, there will be [1] indicating the 
#  index of the first element in the 1-D array.  For example,
#

a

#
#  When we assign a number to a variable, a 1-D array is created
#  with only one number.  The way to key in more numbers is to
#  used c().  For example,
#

x = c(5,4,5,3,5,5,5,2,1,4,5,3,5,5,5,2,1,4,5,3,5,5,5,2,1,4,5,3,5,5,5,2,1,
      1,2,5,5,5,3,5,4,1,2,5,5,5,3,5,4,1,2,5,5,5,3,5,4,1,2,5,5,5,3,5,4,5)

#  Now type x again and observe that [1] and [39] are shown

x

#  We can use length to check the array length

length(x)

#
# Generating regular floating number array with regular patterns
# using seq() and its short form
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
# Floating point sequences can be used in plotting graphs when
# we combine array, arithmetic and functions (e.g. sin, cos, etc.)
# For example, plotting sin(), exp(), etc. for the range [-pi, pi]
#

x  = seq(-pi,pi,length=50)
y1 = sin(x)
y2 = exp(x)
y3 = x^2+2*x-1
y4 = sqrt(abs(x))

#
# Special Array Arithmetic Feature in R
#
# If we want to add repeating 1D data like
#    5,4,5,3,5,5,5,2 +
#    1,2,1,2,1,2,1,2
# => 6 6 6 5 6 7 6 4
#
# In R, we can just write

x + c(1,2)

# R will perform cyclic repeat for us.

#
# Are you able to tell y5 below without using R?
#
y5 = c(2,3)*x + c(5,4,-2,3)

#
# Plotting of y vs x (scatter plot) is not as convenient as in
# other system (e.g. Python, MATLAB).  A plot() only takes in
# two arrays x and y and not more.
#
# We need to use lines() to add more data into a plot.
#
plot(x, y1)           # Points only
lines(x, y2)          # Points only
lines(x, y3, 'l')     # 'l' for lines
lines(x, y4, 'l')

#
# Since y1 (sin()) has a range of [-1,1].  A plot based on
# y1 will have y limit of -1 to 1 leading to a rather
# ugly plot.
#
# To have prettier plot, we nned various plotting options: 
#   col (colours), pch (point symbols), 
#   cex, (point size), lwd (line width), etc.
#

plot(
  1:25,
  1:25,
  cex=3,
  lwd=3,
  pch=1:25,
  col=rainbow(25),
  bg=terrain.colors(5),  # only for pch 21:25
  xlab="x label",ylab="y label",
  main="Title of the Plot"
)


# -------------------------------------------------------------------
# (D) Integers (are internally different from floating point numbers) 
#     and Integer-related Arithmetic Operations
# -------------------------------------------------------------------

a = 1L + 2L
b = 2L - 3L
d = 3L * 4L
e = 7L %/% 5L   # Integer division
f = 7L %% 5L    # Modulo

##
## Dates are internally represented as integer
##

mydates = as.Date("2025-01-01")
today = Sys.Date()

#
# If I don't put a proper comment, do you know what the following 
# is about?
#

1970 + as.integer(mydates)/365.25

# -------------------------------------------------------------------
# (E) Booleans / Logicals => Basic Maths / Discrete Maths
#     and logical operations (and, or, not) as well as
#     relational operations (==, !=, <, >, <=, >=) 
# -------------------------------------------------------------------

TRUE  & TRUE
TRUE  | FALSE
!FALSE
((a < b) & (d < e)) | (f > g)

#
# Application of Booleans: Comparing two 1-D arrays
#
c(1,2,3) == c(3,2,1)
any(c(1,2,3) == c(3,2,1))    # Any single true element?
all(c(1,2,3) == c(3,2,1))    # Are all elements true?


# -------------------------------------------------------------------
# (F) Array of Letters and Array of Strings and operations
# -------------------------------------------------------------------

"A String in double quotes"
'A String in single quotes'
"Escaping quotes using backslash \""
"A string\n with multiple lines\n using `backslash n'"

# We use nchar() to find the length of a string.  len() will only
# give us the length of 1-D array!

nchar("Hello World")

# We use cat(), i.e. concatenate to output, to print strings
# use ?cat to get help for `cat'
cat("A string\n with multiple lines\n", " using `backslash n'")

#
# Array of letters
#

letters  # Array/Vector of small letter alphabet characters
LETTERS  # Array/Vector of capital letter alphabet characters
letters[5:10]    # Assessing elements in an array

#
# Joining strings with paste().  By default strings will be `joined' by 
# a `space' between them.  To use different seperator, use sep
# When an array of strings is encountered, we use the collapse for
# seperator.
#

paste("A string", "and another string")

paste("A", "B", "C", "D", sep=",")

paste(LETTERS[1:10], collapse=",")

#
# Application of strings: Categorical data are usually represented by 
# an array of strings
#
gender = c("M", "F", "F", "M", "M", "F", "F")
# By checking the class, we can see that they are characters (i.e. strings)
class(gender)
# We can use table to summarise the `string' categorical data
table(gender)
# We can use summary() to summarise the categorical data by converting
# it from characters to factors
summary(factor(gender))


# -------------------------------------------------------------------
# (G) Statistical Analysis of Floating Point Array and 
#     Probability Functions
# -------------------------------------------------------------------

#
# Statistical functions for Descriptive Statistics (simple EDA)
#
length(x) # n
min(x)
max(x)
range(x)  # finds min and max
diff(range(x))   # max(x) - min(x)
quantile(x)      # 4 quantiles
median(x)
sum(x)    # x[1] + ... + x[n]
mean(x)   # x.bar = (x[1] + ... + x[n])/n
var(x)    # sample variance: sum((x-x.bar))^2)/(n-1)
sd(x) == sqrt(sum((x-mean(x))^2)/(length(x)-1))     # sample standard deviation
summary(x)

#
# Regular 1-D random sequences in R
#

# Seed is used to make random number generation less random, i.e.
# we can repeat the "random number generation".  The theory of
# seed number is probably mentioned in SSIF but you don't need to
# take SSIF just like anyone driving a car does not need to know
# how to design and construct a car.
set.seed(2025)

# r = random numbers, 50 = 50 elements, unif = uniform dist.
x.unif = runif(50)
# r = random numbers, 50 = 50 elements, norm = normal dist.
x.norm = rnorm(50)
y.norm = x.norm + rnorm(50,mean=50,sd=.1)
# Correlation between two 1-D arrays
cor(x.norm,y.norm)

# Contingency table / Confusion matrix
table(c(1,0,1,0,1), c("Y","N","N","Y","Y"))


# -------------------------------------------------------------------
# Random Variable        samples   probability  quantile  density   
# ---------------        -------   -----------  --------  -------   
# Beta                   rbeta     pbeta        qbeta     dbeta     
# Binomial               rbinom    pbinom       qbinom    dbinom    
# Cauchy                 rcauchy   pcauchy      qcauchy   dcauchy   
# Chi-Square             rchisq    pchisq       qchisq    dchisq    
# Exponential            rexp      pexp         qexp      dexp      
# F                      rf        pf           qf        df        
# Gamma                  rgamma    pgamma       qgamma    dgamma    
# Geometric              rgeom     pgeom        qgeom     dgeom     
# Hypergeometric         rhyper    phyper       qhyper    dhyper    
# Logistic               rlogis    plogis       qlogis    dlogis    
# Log Normal             rlnorm    plnorm       qlnorm    dlnorm    
# -ve Binomial           rnbinom   pnbinom      qnbinom   dnbinom   
# Normal                 rnorm     pnorm        qnorm     dnorm     
# Poisson                rpois     ppois        qpois     dpois     
# Student t              rt        pt           qt        dt        
# Uniform                runif     punif        qunif     dunif     
# Weibull                rweibull  pweibull     qweibull  dweibull  
#
# Ref: https://www.stat.umn.edu/geyer/old/5101/rlook.html
# -------------------------------------------------------------------

set.seed(2025)
x1 = rnorm(200, mean=60, sd=15)   # X ~ Normal(60,15^2)
x2 = rbinom(200, size=100, prob=0.5)   # X ~ Binom(100, 0.5) ≈ Normal?
x3 = rpois(200, lambda=2.5)       # X ~ Poisson(2.5) ≈ Exponential?
# Gamma distribution is applied in cancer rates, insurance claims & rainfall. 
x4 = rgamma(200, shape=2, scale=2.5)
par(mfrow=c(2,2))

hist(x1, freq=FALSE, main="Normal(60,15^2)")
x = seq(min(x1),max(x1),length.out=500)
lines(x,dnorm(x, mean=60, sd=15))

hist(x2, freq=FALSE, main="Binomial(100,0.5)")
x = seq(min(x2),max(x2),length.out=500)
lines(x,dnorm(x, mean=mean(x2), sd=sd(x2)))
# Is it possible to work with dbinom???
# Probably Not.  Because dnorm is not a density plot

hist(x3, freq=FALSE, main="Poisson(2.5)")
x = seq(min(x3),max(x3),length.out=500)
lines(x,dexp(x, rate=1/mean(x3)))
# Is it possible to work with dbinom???
# Probably Not.  Because dpois is not a density plot

# https://en.wikipedia.org/wiki/Gamma_distribution
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/GammaDist.html
hist(x4, freq=FALSE, main="Gamma(shape=2, scale=2.5)")
x = seq(min(x4),max(x4),length.out=500)
lines(x,dgamma(x, shape=mean(x4)^2/var(x4), scale=var(x4)/mean(x4)))


