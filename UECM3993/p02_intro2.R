# -------------------------------------------------------------------
# Purpose: Basic Commands for Data Processing in R (Part 2)
# Author : Liew How Hui (2024)
# License: BSD-3
# Software: R 4.1+
# Duration: 1 hour
# -------------------------------------------------------------------

# Check current working directory
getwd()
# Change the working directory
#setwd("PathToYourFolder")

# -------------------------------------------------------------------
#  Practical: Data Frame (2D Structured Data to handle the mix of 
#  numeric and categorical data just like Excel table) & Sorting
#  Many of the matrix operations are applicable to Data Frame.
# -------------------------------------------------------------------

#
# Define Data Frame using data.frame() and vectors from Practical 1
#
X = data.frame(
  Progromme = c("MCCG11503-Kampar","MCCG11503-Kampar",
                "MECG11503-SL", "MECG11503-SL", "MEME19803", "MEME19803"),
  Name = c("Stud1", "Student2", "Student3", "Student4", "Student5", "Stud6"),
  Quiz1Q1 = c(3.9, 4, 4.7, 5.9, 5.3, 3.4),
  Quiz1Q2 = c(0.9, 0.5, 2, 1.8, 1.5, 0),
  Quiz1Q3 = c(0.5, 1, 0.5, 2, 0, 0.5),
  Assign1 = c(16.9, 16, 14.2, 15.4, 11.1, 11.1))

#
# The $ sign is used to access the `Column' of the `Table': Table$Column
#

X$Quiz1 = X$Quiz1Q1 + X$Quiz1Q2 + X$Quiz1Q3

# apply(Data, 1 or 2, operation):  1 = ack along row, 2 = ack along column
X$Quiz1 = apply(X[,3:5],1,sum)     # Classical R syntax
# f(x,y) can be written as x |> f(y)
# h(g(f(x))) === x |> f |> g |> h
X$Quiz1 = X[,3:5] |> apply(1,sum)  # Functional R syntax (only for R 4.1 and above):

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

rank(X$Tot1)    # Ranking

order(X$Tot1)   # Get the order
X[order(X$Tot1), c("Name", "Tot1")]
cbind(X[order(X$Tot1), c("Name", "Tot1")], Rank=rank(X$Tot1))


# -------------------------------------------------------------------
#  Practical: IO on tabular data
# -------------------------------------------------------------------

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
#

# https://liaohaohui.github.io/UECM3993/Auto.data
Auto=read.table("Auto.data")          # Default to no header
head(Auto)
dim(Auto)      # nrow(Auto)  ncol(Auto)
Auto=read.table("Auto.data",header=T) # Use first row as header
head(Auto)
names(Auto)    # column names.  Alternative: colnames(Auto)
dim(Auto)      # nrow(Auto)  ncol(Auto)
summary(Auto)    # We cannot see the missing values which is `?'
sapply(Auto, class)   # horsepower is "character", something wrong
table(Auto$horsepower)    # We can find something interesting: `?'
apply(Auto=="?",1,sum)    # the `?' is missing value

#
# If we want to set column strings to `categorical data' format,
# we need to set stringsAsFactors=TRUE
#
Auto=read.table("Auto.data",header=T,na.strings="?",stringsAsFactors=TRUE)
summary(Auto)    # Now the summary is nice

#
# Commands to understand basic statistics of a data frame
# and commands to remove `missing values'
#
# Inside R, NA = Not Available = Missing Value
Auto=na.omit(Auto)   # na.omit removes all rows with missing values
dim(Auto)      # check the dimension again to see if some rows are removed

#
# Exploratory Data Analysis (EDA) with Data Visualisation
#
#  (a) Histogram => univariate numeric data
par(mfrow=c(1,2))
hist(Auto$mpg)
hist(Auto$mpg,col=2)  # 2 = "red" (k, rgb, cmy, gray)

#  (b) scatter plot => numeric input vs numeric output
plot(Auto$cylinders, Auto$mpg)
#  (c) boxplot => categorical input vs numeric output
cylinders=as.factor(Auto$cylinders)   # In R, factor = categorical data
mpg = Auto$mpg
plot(cylinders, mpg)
#  Decorations: Making prettier plots
par(mfrow=c(2,2))   # create a 2x2 subplots
plot(cylinders, mpg, col="red")
plot(cylinders, mpg, col="red", varwidth=T)
plot(cylinders, mpg, col="red", varwidth=T, horizontal=T)
plot(cylinders, mpg, col="red", varwidth=T, xlab="cylinders", ylab="MPG")

#  (d) matrix of scatter plots => pair plot
par(mfrow=c(1,1))
pairs(Auto)         # Pair plots = Scatter plots for all numeric columns
pairs(~ mpg + displacement + horsepower + weight + acceleration, Auto)
plot(Auto$horsepower,mpg)

#
#  (e) barplot is for categorical vs categorical (difficult to interpret)
#
plot(as.factor(Auto$cylinders), as.factor(Auto$origin))
#
#  table is better for categorical data vs categorical data
#
table(Auto$cylinders, Auto$origin)


# -------------------------------------------------------------------
#  Practical:  More advanced programming techniques with Penguins Data,
#  which were collected and made available by Dr. Kristen Gorman and 
#  the Palmer Station, Antarctica LTER, a member of the Long Term 
#  Ecological Research Network.
#  install.packages("palmerpenguins")
#  Ref: https://allisonhorst.github.io/palmerpenguins/
# -------------------------------------------------------------------
library(palmerpenguins)   # for `penguins' data
summary(penguins)
cor(penguins[,3:6])       # The missing values will give useless values

# Show rows with missing values (NA = Not Available)
penguins[apply(is.na(penguins),1,sum)>0,]

cor(na.omit(penguins[,3:6]))
#
# Let us investgate column 3 w.r.t. the output using histograms
# and for loops
#
Y = penguins$species
sps = levels(penguins$species)
par(mfrow=c(1,length(sps)))
for(i in 1:length(sps)){
	hist(penguins[Y==sps[i], ]$bill_length_mm, xlab=sps[i], main="")
}
#
# The histograms are not good for comparison.  We want to put them
# together according to 
# https://stackoverflow.com/questions/3541713/how-to-plot-two-histograms-together-in-r
#
p = list()
for(i in 1:length(sps)){
	p[[i]] = hist(penguins[Y==sps[i], ]$bill_length_mm, xlab=sps[i], main="")
}
par(mfrow=c(1,1))
plot(p[[1]], col=rgb(1,0,0,1/4), xlim=range(na.omit(penguins[,3])), ylim=c(0,40))
plot(p[[2]], col=rgb(0,1,0,1/4), add=TRUE)
plot(p[[3]], col=rgb(0,0,1,1/4), add=TRUE)


# -------------------------------------------------------------------
#  Practical: Resampling
# -------------------------------------------------------------------

#
# Suppose the true model is rnorm(100, the.mean, the.stdv)
#
the.mean = 10
the.stdv =  2   # standard deviation is 2
set.seed(2024)  # Fix the starting value of random number generator
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
set.seed(2024)
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
set.seed(2024)
idx.boostrap = sample(count, 0.7*count, replace=TRUE)
sd(the.samples[idx.boostrap])




