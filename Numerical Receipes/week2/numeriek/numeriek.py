import numpy as np
import math

def f(t):
    return 3*t**2 - 2*t - 1

def differentie(t, dt):
    return (f(t + dt) - f(t)) / dt

for i in range(1, 17):
    n = math.pow(10, -i)
    print str(i) + ":   " + str(differentie(1, n))
