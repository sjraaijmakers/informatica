# source: https://github.com/apauley/numerical-analysis/blob/master/Chapter1/Python/regula-falsi.py

from __future__ import division
import math

def f(x):
    return x ** 3 + 2*x - 1

def regula_falsi_solve(f, a, b, tol=0.001, maxiter=100):
    p = 0
    for i in range(maxiter):
        p = b - (f(b) * ((a-b)/(f(a)-f(b))))
        if math.copysign(f(a), f(b)) != f(a):
            b = p
        else:
            a = p
        if abs(f(p)) < tol:
            return (p, i)
    return (p, i)

for i in range(1, 16):
    tol = math.pow(10, -i)
    tmp = regula_falsi_solve(f, 0, tol)
    print "Tol: " + str(tol) + ", Iterations:" + str(tmp[1]) + ". " + str( i / tmp[1])
