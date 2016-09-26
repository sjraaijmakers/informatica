# source: http://bit.ly/1RgA7Z8

from __future__ import division
import numpy as np
import math

def f(x):
    return x**3 + 2*x - 1

def bisection_solve_a(f, a, b, tol=0.001, maxiter=100):
    iterations = 0
    c = (a + b) / 2
    while (b - a) / 2 > tol and iterations <= maxiter:
        if f(c) == 0:
            return (c, iterations)
        elif f(a) * f(c) < 0:
            b = c
        else:
            a = c
        c = (a + b) / 2
        iterations += 1
    return (c, iterations)

def bisection_solve_b(f, a, b, tol=0.001, maxiter=100):
    iterations = 0
    c = (a + b) / 2
    while (b - a) / (b + a) > tol or iterations <= maxiter:
        if f(c) == 0:
            return c, iterations
        elif f(a) * f(c) < 0:
            b = c
        else:
            a = c
        c = (a + b) / 2
        iterations += 1
    return c, iterations

def regula_falsi_solve(f, a, b, tol=0.001, maxiter=100):
    p = 0.0
    iterations = 0
    while  iterations < maxiter:
        iterations += 1
        p = b - (f(b) * ((a-b)/(f(a)-f(b))))
        if math.copysign(f(a), f(b)) != f(a):
            b = p
        else:
            a = p
        if abs(f(p)) < tol:
            return p, iterations
    return p, iterations

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

print "Normal bisection: "
for i in range(1, 16):
    tol = math.pow(10, -i)
    print "Tolerance: " + str(tol),
    tmp = bisection_solve_a(f, 0, 1, tol)
    print "C: " + str(tmp[0]),
    print "Iterations: " + str(tmp[1]),
    print "Lineair: " + str(i / tmp[1])

print "\n2nd bisection: "
for i in range(1, 16):
    tol = math.pow(10, -i)
    print "Tolerance: " + str(tol),
    tmp = bisection_solve_b(f, 0, 1, tol)
    print "C: " + str(tmp[0]),
    print "Iterations: " + str(tmp[1]),
    print "Lineair: " + str(i / tmp[1])

print "\nFalsi bisection: "
for i in range(1, 16):
    tol = math.pow(10, -i)
    print "Tolerance: " + str(tol),
    tmp = regula_falsi_solve(f, 0, 1, tol)
    print "C: " + str(tmp[0]),
    print "Iterations: " + str(tmp[1]),
    print "Lineair: " + str(i / tmp[1])

print "\nSecant bisection: "
for i in range(1, 16):
    tol = math.pow(10, -i)
    print "Tolerance: " + str(tol),
    tmp = secant(f, 0, 1, tol)
    print "C: " + str(tmp[0]),
    print "Iterations: " + str(tmp[1]),
    print "Lineair: " + str(i / tmp[1])
