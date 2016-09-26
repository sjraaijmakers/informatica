import numpy as np
import matplotlib.pyplot as plt
import scipy.interpolate as ip

def f(t):
    return (t-1)*(t-2)*(t-3)

def t(n):
    i = np.linspace(0, n, n + 1)
    x = - 5 + (10 * i) / n
    return x, 1 / (x**2 + 1)

def lagrange(Lx, Ly):
    y = np.poly1d([0])
    for i in range(len(Lx)):
        a = []
        b = 1
        t = np.poly1d([Ly[i]])
        for j in range(len(Lx)):
            if j != i:
                a.append(Lx[j])
                b *= (Lx[i] - Lx[j])
        temp = (1 / b) * np.poly1d(a, True)
        t *= temp
        y += t
    return y

x = np.linspace(-5, 5, 1000)
Lx_6, Ly_6 = t(6)
Lx_14, Ly_14 = t(14)

#lagranges
lg_6 = lagrange(Lx_6,Ly_6)
lg_14 = lagrange(Lx_14, Ly_14)

plt.plot(x, lg_6(x))
plt.plot(x, lg_14(x))
plt.plot(Lx_6, Ly_6, "bo")
plt.plot(Lx_14, Ly_14, "go")
plt.show()
