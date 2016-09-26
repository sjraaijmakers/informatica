from __future__ import division

import numpy as np
import matplotlib.pylab as plt

import random, math

def f(x):
	return x**2
x = np.linspace(-1, 1, 31)
rps = []
w = np.array([1, -8, 0, 8,-1]) / (12 * (2 / 31))
y = f(x)

for i in range(0, 31):
	rps.append(random.uniform(-0.001, 0.001))

y += rps
g = np.correlate(y, w)

#plots
plt.plot(x[2: -2], g)
plt.plot(x[2: -2], y[2:-2])
plt.plot(x[4: -4], np.correlate(g, w))

plt.show()
