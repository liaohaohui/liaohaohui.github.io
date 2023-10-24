# ===================================================================
# Purpose:
#   Practical 1: Software Environment (I/O, Scripting, Numbers, Strings)
#   Practical 3: Script Programming with Python
# Author : Liew How Hui (2023)
# License: BSD-3
# Software: Python 3.6+
# Duration: 3 hours (Week 1)
# ===================================================================

# ===================================================================
#   Practical 1: Software Environment (I/O, Scripting, Numbers, Strings)
#   1 Hour
# ===================================================================

# -------------------------------------------------------------------
#   Variables and General Commands for Working Environments
# -------------------------------------------------------------------

a = 1
type(a)

b = 1.0
type(b)

c = "1"
type(c)

import platform
print("Machine Architecture", " ".join(platform.architecture()))
print("Operating System:", platform.platform())
print("Python version:", platform.python_version())

import os
os.getcwd()
os.listdir()
#os.chdir("d:/")

dir()

# Getting local help
help(dir)

#
# Exercise: How can you get help for trigonometric functions in Python?
#

# -------------------------------------------------------------------
#   Basic Data Types
# -------------------------------------------------------------------

#
# Try and see if you can see if you can calculate the following
# results using basic data types
#
# (a) "1 + 2 = " + "3"
# (b) a = 1 + 2 + ... + 1000
# (c) (1 < 2) & (2 >= 1)
# (d) b = 1/(5!)
# (e) type(a)
# (f) type(b)
# (g) dir()   # Check the variables in the current environment
# (h) del a
# (i) dir()   # What do you observe when a variable is deleted?
# (j) Write the command to print pi to 6 decimal places occupying 
#     10-char position
# (k) Write the command to print pi to 6 decimal places in
#     scientific form.
# (l) l1 = [1,3,2,3,2,2,1]
# (m) l1.append("A string")
# (n) t1 = (1,2,3)    # Cannot append
# (o) t2 = (3,2,1)
# (p) len(t1 + t2)    # Can concatenate, can check 'size'


# -------------------------------------------------------------------
#   Scripting
# -------------------------------------------------------------------

#
# Solving quadratic equation
# 1. Application complex functions: sqrt(-1) = 1j (imaginary number)
# 2. Application of if-else statement
#
print("""
The purpose of this program is to solve the quadratic equation

      2
    ax + bx + c = 0.

""")
a = float(input("Please enter the value of a: "))
b = float(input("Please enter the value of b: "))
c = float(input("Please enter the value of c: "))
from cmath import sqrt
x1 = (-b+sqrt(b**2-4*a*c))/2/a
x2 = (-b-sqrt(b**2-4*a*c))/2/a

print("""
The solutions of the quadratic equation

    {a}x^2 + {b}x + {c} = 0.

are

    x1 = {x1},
    x2 = {x2}
""".format(a=a,b=b,c=c,x1=x1,x2=x2))


# -------------------------------------------------------------------
#   Functions
# -------------------------------------------------------------------

#
# Single-variable equation in standard form
#
# Practical Test with SOA-Exam-FM-Q3 ....................... [1 mark]
# Submit hardcopy answer on a piece of paper before end of practical
# class
#
#Consider a case study from theory of interest below.
#
#Eric deposits 100 into a savings account at time 0, which pays interest at 
#an annual nominal rate of i, compounded semiannually.
#
#Mike deposits 200 into a different savings account at time 0, which pays 
#simple interest at an annual rate of i.
#
#Eric and Mike earn the same amount of interest during the last 6 months of 
#the 8th year.
#
#Write a Python Script to find a solution to the above problem using
#scipy.optimize.fsolve using an appropriate function and 1.0 as initial
#guess and try to see if you can get i=[0.09458825]
#


# -------------------------------------------------------------------
#   Selective Statements
# -------------------------------------------------------------------

#
# We use selective statement for selection, for example, to
# check if something has certain property.
#
# even is a property where it takes a number and check if it is 
# divisible by two; odd is the opposite property where it takes
# a number and check if it is not divisible by two.
#

def even(n):
    if n % 2 == 0:
        return True
    else:
        return False

even(3)
even(6)

def odd(n):
    if n % 2 != 0:
        return True
    else:
        return False

odd(3)
odd(6)


# -------------------------------------------------------------------
#   Loops with For Loop
# -------------------------------------------------------------------

#
# 1. Work with the simplest for loop to know how it works:
#
for i in range(10):
    print("Value of i =", i)

#
# 2. Work with the simplest for loop for maths:
#
for i in range(10):
    x = 0.1 * i
    print("i=", i, "x=", x)

#
# 3. Work with the points in different ranges [a,b] on real line
#
N = 10
from math import pi
for i in range(N):
    a = -pi
    b =  pi
    h = (b-a)/N
    x = a + h*i
    print("i=", i, "x=", x)    # b is not included

N = 10
from math import pi
for i in range(N+1):
    a = -pi
    b =  pi
    h = (b-a)/N
    x = a + h*i
    print("i=", i, "x=", x)    # b is *included*

#
# 4. Combine for loop with selective statements:
#    Find the sum of all integers between 120 to 7000 inclusive which are
#    divisible by 3 or 7.
#

total = 0
for i in range(120,7000+1):
    if i%3==0 or i%7==0:
        total+=i

print("The sum is", total)

#
# 5. Try to work on the SOA-Exam-FM-Q3 example from lecture notes to calculate 
#    the difference between Mike and Eric's interest difference for 
#    the range i from 0 to 0.3 with an increment of 0.01
#
#

