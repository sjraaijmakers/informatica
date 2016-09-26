# NAME: Mees Kalf
# STD nr: 10462074
# Opdracht: Cholesky en QR decompositie
# DATE: 10-1-2016


import numpy as np
import scipy as sp
from scipy.interpolate import interp1d
import matplotlib.pyplot as plt
from scipy.optimize import fsolve
from scipy.optimize import fmin
from scipy import integrate

if __name__ == "__main__":
    # A
    a = 300.0
    b = 0.1
    c = 500.0
    d = 1.0

    t = [0.25, 0.5, 0.75, 1.0, 1.5, 2.0, 3.0, 4.0, 6.0, 8.0, 10.0, 24.0]
    C = [0.0, 19.2, 74.6, 125.0, 200.0, 223.1, 215.4, 291.3, 157.7, 130.8, 115.4, 39.5]

    plt.plot(t,C, "bs")


    co = []
    concentration = lambda t: a * np.e**(-b * t) - c * np.e**(-d * t)
    x =  np.linspace(0.25,24, 200)
    y = concentration(x)
    plt.plot(x, concentration(x))

    # B
    x_max = x[y.tolist().index(y.max())]
    plt.plot(x_max, concentration(x_max), "ro")

    # C
    t1 = x_max
    while(concentration(t1) > 1.0):
        t1 += 0.01
    print "t1 : ", t1

    # D

    print integrate.quad(concentration, 0.25, t1)

    #plt.show()
