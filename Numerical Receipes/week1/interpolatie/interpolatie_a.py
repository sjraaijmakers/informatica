import numpy as np
import matplotlib.pyplot as plt
import scipy.interpolate as ip

def f(t):
    return (t-1)*(t-2)*(t-3)

def skippy(Lx, Ly):
    return ip.interp1d(Lx, Ly)

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

x = np.array([0,1,4], float)
y = f(x)

# lagrange
lg = lagrange(x, y)
# skippy
sk = ip.interp1d(x, y, kind='quadratic')

t = np.linspace(0, 4, 10)
y_lg = lg(t)
y_sk = sk(t)

plt.plot(x, y, "g^")
plt.plot(t, y_lg, "r--") # la grange == red
plt.plot(t, y_sk, "bs")
plt.show()
