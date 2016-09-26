from __future__ import division
import numpy as np

def f(x):
    return x**3 + 2*x - 1

def secant(f, a, b, tol, maxiter=100):
    iterations = 0
    while iterations < maxiter:
        c = b - f(b) * ((b - a) / (f(b) - f(a)))
        if abs(c - b) < tol:
            return c, iterations
        else:
            a = b
            b = c
        iterations +=1
    return c, iterations

tmp = secant(f, 0, 1, 0.001)
print tmp
