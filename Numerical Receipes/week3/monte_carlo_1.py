from __future__ import division

import numpy as np
from scipy.integrate import quad
import random, math
import matplotlib.pyplot as plt
from time import time

# source: code.activestate.com/recipes/577263-numerical-integration-using-monte-carlo-method/

def f(x):
    return np.sin(x)

# Voor de afbreekfout via main
def normal(f, x_min, x_max, n=100):
    result, _ = quad(f, x_min, x_max)
    print result
    return result

# Finds min & max y of function f over interval x_min -> x_max
def find_y_range(f, x_min, x_max, n=100):
    y_min = f(x_min)
    y_max = y_min
    for i in range(n):
        x = x_min + (x_max - x_min) * i / n
    	y = f(x)
    	if y < y_min: y_min = y
    	if y > y_max: y_max = y
    return y_min, y_max

def monte_carlo(f, x_min, x_max, n=10):
    precision = 1 / n
    result, _ = quad(f, x_min, x_max)

    # Get corresponding y's
    y_min, y_max = find_y_range(f, x_min, x_max)

    # Init
    on, off = 0, 0

    # Surfaces
    s = (x_max - x_min) * (y_max - y_min)
    e = (x_max - x_min) * y_min

    i = 0
    while True:
        i += 1
        x = random.uniform(x_min, x_max)
        y = random.uniform(y_min, y_max)

        if y <= f(x):
            on += 1
        else:
            off += 1

        mc = (on / (on + off)) * s + e

        if abs(result - mc) <= precision:
            return mc
            # return k voor de iteraties die nodig waren

def fast_monte_carlo(f, x_min, x_max, n=10):
    y = f(np.linspace(x_min, x_max, n))
    y_min, y_max = np.amin(y), np.amax(y)

    precision = 1 / n
    result, _ = quad(f, x_min, x_max)

    # Init
    on, off = 0, 0

    x, y = np.random.uniform(x_min, x_max, n), np.random.uniform(y_min, y_max, n)

    # Surfaces
    s = (x_max - x_min) * (y_max - y_min)
    e = (x_max - x_min) * y_min

    for y, t in enumerate(y):
        if y <= f(x[t]):
            on += 1
        mc = (on / n) * s + e
        if abs(result - mc) <= precision:
            return t

#on = sum((y <= f(x[i])) for i, y in enumerate(y))

# Test if fmc is faster
if __name__ == '__main__':
    x_min = 0
    x_max = np.pi
    n = 1000

    print monte_carlo(f, x_min, x_max, n)
