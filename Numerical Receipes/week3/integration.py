from __future__ import division

import numpy as np

def riemann_sum(f, x_min, x_max, n=100, method='left'):
    h = (x_max - x_min) / n
    som = 0

    for k in range(1, n + 1):
        if method == 'left':
            som += h * f(x_min + (k - 1) * h)

        elif method == 'right':
            som += h * f(x_min + k * h)

        elif method == 'middle':
            som += h * f(x_min + (k - 0.5) * h)

    return som

def trapezoidal_rule(f, x_min, x_max, n=100):
    h = (x_max - x_min) / n
    som = f(x_min) + f(x_max)

    for k in range(1, n):
        som += 2 * f(x_min + k * h)

    return (h / 2) * som

def simpson_rule(f, x_min, x_max, n=100):
    h = (x_max - x_min) / n
    som = f(x_min) + f(x_max)

    for k in range(1, n):
        som += 2 * f(x_min + k * h)

    for k in range(1, n+1):
        som += 4 * f(x_min + (k - 0.5) * h)

    return (h / 6) * som
