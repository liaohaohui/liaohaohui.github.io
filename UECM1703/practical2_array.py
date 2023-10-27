# ===================================================================
# Purpose:
#   Practical 2: Programming with Arrays
# Author : Liew How Hui (2023)
# License: BSD-3
# Software: Python 3.6+
# Duration: 3 hours (Week 2)
# ===================================================================

# -------------------------------------------------------------------
#   Numpy Array Data Types
# -------------------------------------------------------------------

import numpy as np

# -------------------------------------------------------------------
#   Array Construction
# -------------------------------------------------------------------

#
# 1D arrays
#
# Write down the commands to generate the following 1-D arrays
# 
# (a) 1, 3, 5, 7, 9, 11, 13, 15
# (b) tan(1.5), tan(3.0), tan(4.5), cos(3)
# (c) 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0
# (d) 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.4, 0.3, 0.2, 0.1, 0
# (e) 35 random numbers following standard normal distribution
#

#
# 2D arrays
#

# (a) Create
#
#         3   0   0   0   0   0   0
#         0   4   0   0   0   0   0
#         0   0   3   0   0   0   0
#  A =    0   0   0   4   0   0   0
#         0   0   0   0   3   0   0
#         0   0   0   0   0   4   0
#         0   0   0   0   0   0   3
#


print(A.shape)
print(A.size)
print(A.ravel())
print(A.flatten())

# (b) Construct B1 and B2 below from A
#
#         3   0   0   0   0   0   0   0   0   0   0   0   0   3
#         0   4   0   0   0   0   0   0   0   0   0   0   4   0
#         0   0   3   0   0   0   0   0   0   0   0   3   0   0
#  B1 =   0   0   0   4   0   0   0   0   0   0   4   0   0   0
#         0   0   0   0   3   0   0   0   0   3   0   0   0   0
#         0   0   0   0   0   4   0   0   4   0   0   0   0   0
#         0   0   0   0   0   0   3   3   0   0   0   0   0   0
#
#         3   0   0   0   0   0   0
#         0   4   0   0   0   0   0
#         0   0   3   0   0   0   0
#  B2 =   0   0   0   4   0   0   0
#         0   0   0   0   3   0   0
#         0   0   0   0   0   4   0
#         0   0   0   0   0   0   3
#         0   0   0   0   0   0   3
#         0   0   0   0   0   4   0
#         0   0   0   0   3   0   0
#         0   0   0   4   0   0   0
#         0   0   3   0   0   0   0
#         0   4   0   0   0   0   0
#         3   0   0   0   0   0   0
#

# -------------------------------------------------------------------
#   Array Indexing: Usual Indexing and Negative Indexing
# -------------------------------------------------------------------

#
#  Getting familiar with array indexing:
#
#  Each index can be a number, :, start:stop:step form or a list
#  E.g. C[1:], C[:4], C[1:4], C[1:9:2], C[9:-1:-1], C[:,2], ..
#

#
#  Construct the following matrix:
# 
#        [ 1,  3,  5,  7,  9, 11, 13]
#        [15, 17, 19, 21, 23, 25, 27]
#        [29, 31, 33, 35, 37, 39, 41]
#   C =  [43, 45, 47, 49, 51, 53, 55]
#        [57, 59, 61, 63, 65, 67, 69]
#        [71, 73, 75, 77, 79, 81, 83]
#        [85, 87, 89, 91, 93, 95, 97]
#
#  Work through various indexing on C and make sure you understand
#  how array indexing works in Python
#

print(C[1,4])
print(C[[1,2,3],[4,5,6]])
print(C[:, [3,2,3,4,3,5]])
print(C[:,-1])
print(C[:,[-1,-2]])
print(C[:,-1:-2])      # What's wrong about this???
print(C[::3,::3])
print(C[1::2,1::2])
print(C[::-1,::-1])
print(C[:5,:5])


# -------------------------------------------------------------------
#  Ufuncs: Array Mathematical Functions
# -------------------------------------------------------------------

x = np.array([0, 0.1, 0.2, 0.3])
from math import sin
#sin(x)     # This command will fail
np.sin(x) == np.array([sin(x[0]), sin(x[1]), sin(x[2]), sin(x[3])])

#
# Application of Ufuncs --- Draw simple function (more in Topic 4)
#
import matplotlib.pylab as plt
x = np.linspace(0, 2*np.pi, 1000)
y = np.sin(x)
plt.plot(x,y)


# -------------------------------------------------------------------
#   Array Arithmetic, Logical and Relational Operations
# -------------------------------------------------------------------

# Scalar addition
np.zeros((4,3))+10

# Scalar multiplication
np.ones((4,3))*10

#
# Geometric sequence: $a, ar, ar^2, ..., ar^{n-1}$
#
n = 20
a = 10
r = 0.98
S4 = a*r**np.r_[:n]


#
# Exercise: Generate the following matrix
#
#
#         3  -2   0   0   0   0   0
#        -2   4  -2   0   0   0   0
#         0  -2   3  -2   0   0   0
#  E =    0   0  -2   4  -2   0   0
#         0   0   0  -2   3  -2   0
#         0   0   0   0  -2   4  -2
#         0   0   0   0   0  -2   3
#
# which works for any size matrix.  This is 7x7.  You method
# also need to work for 1001x1001.                  (1 mark)
# Method 1: Use three np.diag() and add together
# Method 2: use one np.diag(), array indexing and assignment
# Method 3: use one np.diag(), array indexing and for loop
#

#
# Consider the matrix
#

A = np.array([[0, 1, 0, 0, 1], [0, 0, 1, 1, 1], [1, 1, 0, 1, 0]])

#
# There two basic `relations' between each row:
#
# 1. Pearson correlation (`scaled' covariance matrix):
#
#      cor(x,y) = (x-mu_x) . (y-mu_y) / (n Sum x Sum y)
#
#    Related: np.corrcoef(A)

i = 0
j = 1
n = len(A[i])  # compare row i & row j
np.dot(A[i]-A[i].mean(),A[j]-A[j].mean())/n/A[i].std()/A[j].std()

#
#  Express the Cosine similarity in Python:
#
#   cos.sim(x,y) = 1 - (x . y ) / (|x| |y|)}
#
#  It is available in Scipy as
#
#    scipy.spatial.distance.cdist(A,A,'cosine')
#

i, j = 0,1  # compare row i & row j
1 - np.dot(A[i], A[j])/np.linalg.norm(A[i])/np.linalg.norm(A[j])


#
# Boolean Array Arithmetic: Truth Table as a 2-D Array
#

p = np.array([True]*4 + [False]*4)
q = np.array(([True]*2 + [False]*2)*2)
r = np.array(([True]*1 + [False]*1)*4)
#
# We need to logical equivalence P -> Q = ~P \/ Q
#
bool_expr = q & ~ ( p | r)    # <--- Boolean Arithmetic
truthtable = np.vstack((p, q, r, bool_expr)).T
print(truthtable)
#
# Constructing a prettier table
#
import pandas as pd
truthtable = pd.DataFrame(truthtable, columns=['p','q','r','q /\ ~(~p->r)'])


## -------------------------------------------------------------------
## Final Exam Oct 2020, Q2(a) --- Complicated Array Arithmetic
## -------------------------------------------------------------------

#
#  Suppose that the Taylor series of a cosine function at $x=0$ with
#  the first 11 terms:
# 
#    cos x ~ 1 - x^2/2! + x^4/4! - x^6/6! + ... + (-1)^{10} x^{20}/(20)!
#
#  is going to be used in an embedded system.
#
#  Write a script containing the function mycosfor(x,n=10)
#  which implements the cosine series using for loops without
#  importing anything from Python modules.  If you import anything from
#  any Python module (e.g. math, numpy), marks will be deducted.
#

#
# A sample implementation of the script using for loop looks like this:
#
def mycosfor(x,n=10):
    val = 1.0
    for k in range(1,n+1):
        fac = 1.0
        for i in range(2,2*k+1):
            fac *= float(i)
        val += (-1)**k*x**(2*k)/fac
    return val

#  The Taylor series of a cosine function at x=0 can be rewritten 
#  into a recursive form:
#
#    cos x = 1 - x^2/(2!) [1 - x^2/(4*3) [
#                          1 - x^2/(6*5) [
#                          1 - ... ]]]
#
# A sample implementation of the script using recursion:
#
def mycosrec(x,n=10):
    def cos_aux(n0, x):
        if n0 > n: return 1.0
        return 1-x*x/(2*n0)/(2*n0-1) * cos_aux(n0+1, x)
    return cos_aux(1,x)

#
# Write a script which imports the functions mycosfor(x,n=10),
# mycosrec(x,n=10) to calculate their values at $x=-1,0,1,2,...,10$ 
# and their absolute difference  errors with the standard numpy 
# implementation np.cos.
#
# Your script should output the following text:
#
#      x   mycosfor    abs.err   mycosrec    abs.err
#   -1.0  0.5403023 1.1102e-16  0.5403023          0
#    0.0  1.0000000          0  1.0000000          0
#    1.0  0.5403023 1.1102e-16  0.5403023          0
#    2.0 -0.4161468 3.6637e-15 -0.4161468 3.6082e-15
#    3.0 -0.9899925  2.747e-11 -0.9899925  2.747e-11
#    4.0 -0.6536436 1.5209e-08 -0.6536436 1.5209e-08
#    5.0  0.2836642 2.0287e-06  0.2836642 2.0287e-06
#    6.0  0.9602802 0.00010987  0.9602802 0.00010987
#    7.0  0.7570938  0.0031916  0.7570938  0.0031916
#    8.0 -0.0867742   0.058726 -0.0867742   0.058726
#    9.0 -0.1491107    0.76202 -0.1491107    0.76202
#   10.0  6.6645643     7.5036  6.6645643     7.5036
#
#                                                       (1 mark)


# -------------------------------------------------------------------
#   Array Reduction Operations
# -------------------------------------------------------------------

np.random.seed(12345)
A = np.random.randn(7,6)
A.sum()
A.sum(axis=0)   # column
A.sum(axis=1)   # row

# Theoretically, the means should be close to zero:
A.mean()
A.mean(axis=0)   # column
A.mean(axis=1)   # row

# Theoretically, the standard deviation should be close to zero:
A.std()
A.std(axis=0)   # column
A.std(axis=1)   # row


## -------------------------------------------------------------------
##  Working with Coloured Image: Reading and Processing An Image
##  (Skip this if there is not enough time)
## -------------------------------------------------------------------

#
# An image array can be 2-D or 3-D depending on whether it is
# ``grey-scale'' or ``coloured''.  In the case of a coloured image, red,
# green, blue (and alpha) are required an this means that
# an (m x n x 3)-array (or an (m x n x 4)-array) is required.  
# Python supports the loading of images using functions
# from imageio or matplotlib.  By using plt.imread (requires pillow)
# typical image types such as Jpeg, Png and Bmp can be read and
# imshow and imsave can be used to view and save the image.
#
# The module scipy.ndimage provides many image array processing
# functions for measuring, filtering, interpolating
# and morphing a given image.  To test and add more functions,
# newer Scipy package includes two images ascent (gray) and
# face (colour).
#

Try loading your favourite image using the code from Topic 2 and
(i)   do histogram analysis on the red, green and blue components
(ii)  create an elliptic frame
(iii) convert the coloured image to gray image
(iv)  flip the gray image using the usual indexing and the negative indexing.



# -------------------------------------------------------------------
#   Linear Algebra Operations
# -------------------------------------------------------------------

from scipy import linalg

A = np.array([[3,-2], [2,3], [1,5]])
B = A[:2,:2]

# Matrix Multiplication
A @ B

# The product with conjugate matrix is used a lot in statistics
A @ A.T
A.T @ A
(A-A.mean(1,keepdims=True)).T @ (A-A.mean(1,keepdims=True))
np.cov(A)

linalg.matrix_power(B, 5)   # Related to eigenvalue problem

#
# Kronecker product is noncommutative
#
np.kron(A, B)
np.kron(B, A)

#
#  Toeplitz Matrix
#
#  It arises from the the `convolution' of 1D arrays h[n] and x[n]:
#
#  y[n] = (h*x)[n] = \sum_{i=-\infty}^{\infty} h[n-i]x[i]
#
#  in the response y[n] of the Linear Time Invariant system with
#  an impulse response h[n] and an input sequence x[n] (0 when n<0).
#
#
#  For example, if h[0] = 3, h[1]=2, h[2]=5, h[3]=7, h[n]=0 for
#  n != 0,1,2,3.  Then
#
# 
#  (a) y[0] = \sum_{i=-\infty}^{\infty} h[-i]x[i] = h[0]x[0]
#  (b) y[1] = \sum_{i=-\infty}^{\infty} h[1-i]x[i] = h[1]x[0]+h[0]x[1]
#  (c) y[2] = h[2]x[0]+h[1]x[1]+h[0]x[2]
#  (d) y[3] = h[3]x[0]+h[2]x[1]+h[1]x[2]+h[0]x[3]
#  (e) y[4] = h[4]x[0]+h[3]x[1]+h[2]x[2]+h[1]x[3] +h[0]x[4]
#
#   Note that h[4]=0.
# 
#   This leads to the following matrix representation:
#
#   y[0]    [ h[0]     0     0     0     0 ] [ x[0] ]
#   y[1]    [ h[1]  h[0]     0     0     0 ] [ x[1] ]
#   y[2] =  [ h[2]  h[1]  h[0]     0     0 ] [ x[2] ]
#   y[3]    [ h[3]  h[2]  h[1]  h[0]     0 ] [ x[3] ]
#   y[4]    [    0  h[3]  h[2]  h[1]  h[0] ] [ x[4] ]
#

h0=3
h1=2
h2=5
h3=7
H = linalg.toeplitz([h0,h1,h2,h3,0],[h0,0,0,0,0])
x = np.array([1,2,3,4,5])
y = H @ x


# -------------------------------------------------------------------
#   Linear Algebra Solvers
# -------------------------------------------------------------------

#
#  Application 1 of 2-D Array: Oct 2020 Test Question (Vandermonde matrix)
#
#  To use a polynomial 
#
#    y  = a_0 + a_1x + a_2x^2 + a_3x^3 + a_4x^4
#
#  to fit the following 2D data points:
#
#     (145, 7), (155, 17), (165, 32), (175, 51), (180, 60)
#
#  will lead to the following system of linear equations:
#
#     7 = a_0 + 145a_1 + 145^2a_2 + 145^3a_3 + 145^4a_4
#    17 = a_0 + 155a_1 + 155^2a_2 + 155^3a_3 + 155^4a_4
#    32 = a_0 + 165a_1 + 165^2a_2 + 165^3a_3 + 165^4a_4
#    51 = a_0 + 175a_1 + 175^2a_2 + 175^3a_3 + 175^4a_4
#    60 = a_0 + 185a_1 + 185^2a_2 + 185^3a_3 + 185^4a_4
#
#  In matrix form:
#
#   [  7 ]   [1   145   145^2   145^3   145^4]  [ a_0 ] 
#   [ 17 ]   [1   155   155^2   155^3   155^4]  [ a_1 ]
#   [ 32 ] = [1   165   165^2   165^3   165^4]  [ a_2 ]
#   [ 51 ]   [1   175   175^2   175^3   175^4]  [ a_3 ]
#   [ 60 ]   [1   185   185^2   185^3   185^4]  [ a_4 ]
#

#
# An Implementation using For Loop is shown below:
#   
N = 5
A = np.ones((N,N))
cs = [145., 155., 165., 175., 185.]
b  = [7,17,32,51,60]
for i in range(N):
    for j in range(1,N):
        A[i,j] = cs[i]**j
print(A)

#
# Are you able to write a more compare command to construct A?
#

#
# Determinant is good in theory but not good in practical calculation
# because they are either too large or too small.
#
np.linalg.det(A)

#
# Oct 2020 Test Question
#
# Identify the problem of using the polynomial $y = a_0 + a_1x
# + a_2x^2 + a_3x^3 + a_4x^4$ to fit the 2D data points and
# propose a solution to solve the problem you state.
# [Hint: The answer provided must be relevant to scientific computing.]
#

#
# Answer:
#   The problem: The determinant of the matrix is too large.
#
#   A possible solution: Use $y = a_0 + a_1(x-\bar{x})
#   + a_2(x-\bar{x})^2 + a_3(x-\bar{x})^3 + a_4(x-\bar{x})^4$
#   and it is possible to have a matrix with smaller determinant.
#

#
# Exercise: Write down the command to find the coefficients 
# a_0, a_1, a_2, a_3, a_4.
#


## -------------------------------------------------------------------
## Matrix Sine Fn vs Elementwise Sine Fn
## -------------------------------------------------------------------

np.sin(1)
np.sin(np.pi/2)
np.sin(np.pi)

# vs

linalg.sinm(np.array([[1]]))
linalg.sinm(np.array([[np.pi/2]]))
linalg.sinm(np.array([[np.pi]]))

#
# Matrix Sine Fn vs Elementwise Sine Fn
#
A1 = np.eye(2)
A2 = np.ones((2,2))
A3 = np.array([[1,np.pi/2], [np.pi/2,np.pi]])

np.sin(A1)
np.sin(A2)
np.sin(A3)

#
# vs
#

linalg.sinm(A1)
linalg.sinm(A2)
linalg.sinm(A3)

#
#  What did you observe?
#
