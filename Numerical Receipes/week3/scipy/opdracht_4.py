from __future__ import division

import numpy as np
import scipy as sp
from scipy.optimize import curve_fit
from scipy import integrate
from scipy.optimize import minimize_scalar

import matplotlib.pyplot as plt

def f(t, a, b, c, d):
    return a * sp.exp(-b * t) - c * sp.exp(-d * t)

x = np.array([0.25, 0.50, 0.75, 1.00, 1.50, 2.00, 3.00, 4.00, 6.00, 8.00, 10.00, 24.00])
y = np.array([0.0, 19.2, 74.6, 125.0, 200.0, 223.1, 215.4, 192.3, 157.7, 130.8, 115.4, 38.5])

a = 300
b = 0.1
c = 500
d = 1.0

ct = lambda o: f(o, a, b, c, d)

t = np.linspace(0.25, 25.00, 100)
y2 = ct(t)

maxt = minimize_scalar(lambda x: -ct(x), method='bounded', bounds=(0,5)).x

# plt.plot(x, y, 'rx')
# plt.plot(t, y2)
plt.legend(['gemeten', 'gefit'])

print "max t:", maxt

t1 = maxt
while(ct(t1) > 1.0):
    t1 += 0.01
print "t1 : ", t1

surf, _ = integrate.quad(ct, 0.25, t1)
print "surface:", surf

# plt.grid()
# plt.show()
