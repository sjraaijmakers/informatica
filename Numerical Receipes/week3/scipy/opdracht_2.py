import numpy as np
import matplotlib.pyplot as plt
import scipy.interpolate as ip

def f(x):
    return x**3 + 2

def fa(x):
    return 3*x**2

def faa(x):
    return 6*x

# A
x = np.arange(0, 1.2, 0.2)
y = f(x)

S = ip.InterpolatedUnivariateSpline(x, y)
xs = np.linspace(0, 1.1, 100)
ys = S(xs)

# B
xb = np.arange(0.1, 1.0, 0.2)
yb = S(xb)

print "Splines 1"
for x in xb:
     print str(x) + ": " + str(f(x)) + " " + str(S(x)) + " Verschil: " + str(abs(f(x) - S(x)))

# C

y1 = S.derivative()(xb)
y2 = S.derivative(2)(xb)

print "\n1ste afgeleide"
for t, x in enumerate(xb):
    print str(x) + ": " + str(fa(x)) + " " + str(y1[t]) + " Verschil: " + str(abs(fa(x) - y1[t]))

print "\n2e afgeleide"

for t, x in enumerate(xb):
    print str(x) + ": " + str(faa(x)) + " " + str(y2[t]) + " Verschil: " + str(abs(faa(x) - y2[t]))
