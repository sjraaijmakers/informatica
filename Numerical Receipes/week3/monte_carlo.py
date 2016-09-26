from __future__ import division

import numpy as np
from scipy.integrate import quad
import random, math
from time import time

def find_y_range(f, x_min, x_max, n=100):
    # set ys
    y_min = f(x_min)
    y_max = y_min
    # get some points and return biggest found value and lowest
    for i in range(n):
        x = x_min + (x_max - x_min) * i / n
    	y = f(x)
    	if y < y_min: y_min = y
    	if y > y_max: y_max = y
    return y_min, y_max

# Monte Carlo approach of computing surface beneath f
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

    i = 1
    while True:
        x = random.uniform(x_min, x_max)
        y = random.uniform(y_min, y_max)

        if y <= f(x):
            on += 1
        else:
            off += 1

        mc = (on / (on + off)) * s + e

        if abs(result - mc) <= precision:
            return i

        i += 1

def fast_monte_carlo(f, x_min, x_max, n=10):
    pass

# Uniform method to find surface beneath f
def uniform_integration(f, x_min, x_max, n=100):
    precision = 1 / n
    result, _ = quad(f, x_min, x_max)

    u = np.array([])

    i = 1
    while True:
        x = random.uniform(x_min, x_max)
        y = f(x)

        u = np.append(u, y)
        uni = np.average(y) * (x_max - x_min)
        if abs(result - uni) <= precision:
            return i
        i += 1
