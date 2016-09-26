from __future__ import division
import math
import numpy as np
import matplotlib.pyplot as plt

def mypi_1(e):
    x = 0
    k = 0
    mypi = 0
    while True:
        term = 4 * ((-1)**k) / (2*k+1)
        mypi += term
        k += 1
        error = math.fabs(math.fabs(mypi) - math.fabs(np.pi))
        if error < e:
            return k

def mypi_2(n):
    x = 0
    k = 0

    mypi = 0
    while k <= (2 * n - 1):
        term = (1 / (2*n) + 4) * ((-1)**k) / (2*k+1)
        mypi += term
        k += 1
    error = math.fabs(math.fabs(mypi) - math.fabs(np.pi))
    return mypi, error

x = np.linspace(-np.pi, np.pi, 10)

for i in range(1, 101):
    tmp = mypi_2(i)
    print str(i) + "    " + str(tmp[0]) + "   " + str(tmp[1])

print ""

for i in range(1, 6):
    n = 10 ** -i
    tmp = mypi_1(n)
    print tmp
