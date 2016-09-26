from __future__ import division

import math
import numpy as np

def f(x):
    return x**2 - x - 1

def fp(x):
    return 2*x - 1

def y(x):
    return x**3 + x**2 -2

def yp(x):
    return 3*x**2 + 2*x

def g(x):
    return x**3 - 3*x**2 + 2

def gp(x):
    return 3*x**2 - 6*x

def Newton_solve(f, fp, x0, tol=0.001, maxiter=100):
    x = x0
    n = 0
    while n <= maxiter:
        n += 1
        x1 = x - (f(x) / fp(x))
        t = abs(x1 - x)
        if t < tol:
            break
        x = x1
    return x, n

print Newton_solve(f, fp, 1.0, 10**-9)
print Newton_solve(y, yp, 1.5, 10**-10)
print Newton_solve(g, gp, 1.77, 10**-10)
