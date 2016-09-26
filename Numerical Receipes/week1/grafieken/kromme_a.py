import numpy as np
import matplotlib.pyplot as plt

def fx(t):
    return np.cos(t) / (1 + np.sin(t)**2)

def fy(t):
    return np.cos(t) * np.sin(t) / (1 + np.sin(t)**2)

t = np.linspace(-np.pi, np.pi, 100)
x = fx(t)
y = fy(t)

plt.plot(x, y)
plt.show()
