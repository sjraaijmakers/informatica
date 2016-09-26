import scipy as sp
import numpy as np
from scipy.optimize import minimize_scalar
from scipy.optimize import fsolve
from scipy.integrate import quad
import matplotlib.pyplot as plt

def f(x):
    return x**3 + 4*x**2 + 2*x - 1

x = np.linspace(-4, 1, 100)
y = f(x)

# B
nulpunten = np.array([])

nulpunten = np.append(nulpunten, fsolve(f, -4.0))
nulpunten = np.append(nulpunten, fsolve(f, -2.0))
nulpunten = np.append(nulpunten, fsolve(f, 0.0))

yn = f(nulpunten)

# C
minx = minimize_scalar(f).x
maxx = minimize_scalar(lambda x: -f(x), method='bounded', bounds=(-3,-2)).x
print minx, maxx

#D
result, _ = quad(f, maxx, minx)
print "Oppervlakte f tussen " + str(maxx) + " en " + str(minx) + ": " +  str(result)

plt.plot(x, y)
plt.plot(nulpunten, yn, 'o')
plt.grid()
plt.show()
