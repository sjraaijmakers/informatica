from __future__ import division

import math
import numpy as np

def iteratie(f, x, tol):
    n = 0
    last = round(x, 5)
    try:
        while True:
            current = round(f(last), 5)
            if current == last:
                return current, n
            last = current
            n += 1
    except:
        return "overflow"

def f1(x):
    return (4 - x**3) / (3*x)

def f2(x):
    return (4 - 3*x) / (x**2)

def f3(x):
    return x**3 + 4*x - 4

def f4(x):
    return x - 1/6 * (x**3 + 3 * x - 4)

def f5(x):
    return (2*x**3 + 4) / (3*x**2 + 3)

def g(x):
    return x - 1/3 * (x**2 - 2)

functions = [f1, f2, f3, f4, f5]

for f in functions:
    print f.__name__

    for i in range(0, 21):
        n = i * 0.1
        tmp = iteratie(f, n, 6)
        if tmp != "overflow":
            print str(n) + " " + str(tmp),
            if tmp[0] == n:
                print "(dekpunt)"
            else:
                print ""
    print ""
