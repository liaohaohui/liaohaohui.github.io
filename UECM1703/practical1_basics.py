# ===================================================================
# Purpose:
#   Practical 1: Software Environment (I/O, Scripting, Numbers, Strings)
#   Practical 3: Script Programming with Python
# Author : Liew How Hui (2022)
# License: BSD-3
# Software: Python 3.6+
# Duration: 3 hours (Week 1)
# ===================================================================

# ===================================================================
#   Practical 1: Software Environment (I/O, Scripting, Numbers, Strings)
#   1 Hour
# ===================================================================

# -------------------------------------------------------------------
#   Variables and Working Environments
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
# (g) dir()   # What do you observe?
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
#   Functions
# -------------------------------------------------------------------

#
# 1. For the mathematical expression 
#  
#    cos 15^\circ |cos 225^\circ| - sin 315^circ |cos 105^\circ|.
#   
#    Note: x^\circ indicates that then angle x has the unit `degree'.
#   
#    Write down the Python instruction to evaluate the expression.
#    Try and see if you can get the value 0.8660254037844389
#

#
# 2. Determine which of the following statement is true:
#    
#    (a) $\cot 1 < \cos 1 < \sin 1 < \tan 1$
#    (b) $\cot 1 < \sin 1 < \cos 1 < \tan 1$
#    (c) $\cos 1 < \sin 1 < \cot 1 < \tan 1$
#    (d) $\cos 1 < \cot 1 < \sin 1 < \tan 1$
#

#
# 3. In a triangle ABC, cos A = 2sqrt(5)/5, cos B = 3sqrt(10)/10, 
#    show that the angle C is 135^\circ
#

#
# 4. Solve the nonlinear equation x = e^{x/2}/2 + x using the solver
#    scipy.optimize.fsolve.
#


# -------------------------------------------------------------------
#   Input-Output and File Types
# -------------------------------------------------------------------

#
# Write a script to read data and calculate the mean, standard
# deviation, ... of the data
#
# Steps involve:
# (a) Input a string of numbers
# (b) Convert strings to list of numbers
# (c) Calculate the mean, standard deviation, etc.
# (d) Print out the results nicely.
#


# ===================================================================
#   Practical 3: Scripting
#   2 Hours
# ===================================================================

# -------------------------------------------------------------------
#   Simple Scripting
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

#
# If time permits, try out quadratic equation, cubic equation, etc.
#


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
# 5. Write a function with a for loop in it which allows us to 
#    generate the sequence numerically
#
#     2/(1^2+2), 3/(2^2+2), 4/(3^2+2), 5/(4^2+2), ...
#

#
# 6. Write a for loop to calculate the following series numerically
#
#     1/7^4 - 1/9^4 + 1/11^4 - 1/13^4 + ... - 1/77^4
#



#
# Write a Python script to generate the truth table for the following
# statement
# \[
#   q \wedge \sim(\sim p \to r).
# \]
#

for p in [True, False]:
    for q in [True, False]:
        for r in [True, False]:
            statement = q and not (r if not p else True)
            print("{} {} {} | {}".format(str(p)[0],
               str(q)[0],str(r)[0],str(statement)[0]))



# -------------------------------------------------------------------
#   Loops with While Loop
# -------------------------------------------------------------------

#
# Practise with the translation of for loop to while loop?
#





# -------------------------------------------------------------------
#   Advanced Scripting
# -------------------------------------------------------------------

#
# 1. Approximating Differentiation at a Point Numerically
#
#    (a) Practise with f(x) = x^2 at x = 2
#    (b) Practise with f(x) = ln x at x = 2
#    (c) Practise with f(x) = cos x at x = 2
#


#
# 2. Approximating Definite Integral Numerically
#
#    `Signed Area' under the curve y = f(x) to the x-axis is given
#    by 
#
#       / b
#       |    f(x) dx
#       / a
#
#    If we know the 'anti-derivative' F(x) of f(x), then we can
#    calculate the above definite integral by
#
#      F(b) - F(a)
#
#    using the Fundamental Theorem of Calculus
#
#    However, F(x) is difficult to find in general, we can approximate
#    the signed area using 'trapezoidal approximation':
#
#    Area of Trapezoid = (base length) * (average of vertical height 1 and height 2)
#
#    Try to write a script to calculate the signed areas belows
#
#           / 2
#    (a)    |    x^2 dx
#           / 0
#    
#           / 2
#    (b)    |    cos(x) dx
#           / 0
#
#           / 2
#    (c)    |    exp(-x^2) dx
#           / 0
#


