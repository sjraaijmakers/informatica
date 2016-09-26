import numpy as np
import matplotlib.pyplot as plt

def S(t):
    return 100 / (100 + (t - 0.5*np.pi)**8)

def R(t):
    return S(t) * (2 - np.sin(7*t) - 0.5*np.cos(30*t))

def X(t):
    return R(t) * np.cos(t)

def Y(t):
    return R(t) * np.sin(t)

t_min = np.pi*-1/2
t_max = np.pi*3/2

t = np.linspace(-0.5*np.pi, 1.5*np.pi, 1000)
x = X(t)
y = Y(t)

plt.plot(x, y)
plt.show()
