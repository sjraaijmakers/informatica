import numpy as np
import math
import matplotlib.pyplot as plt

# (sin(x))' = cos(x)
def sin(x):
    return np.sin(x)

part = (2*np.pi) / 25

x = np.linspace(0, 2*np.pi, 25)
y = sin(x)

# A: derivative via numpy.diff
x2 = np.linspace(0, 2*np.pi, 24) # diff returns n-1 points
y1 = np.diff(y) / part

# B: derivative via numpy.convolve
y2 = np.convolve(y, np.array([1, 0, -1]) / (2 * part), "same")

# C: derivative via numpy.roll
y3 = -(np.roll(y, 1) - np.roll(y, -1)) / (2*part)

# D: derivative like B, only more points
x4_1 = np.linspace(0, 2*np.pi, 101)
x4_2 = np.linspace(0, 2*np.pi, 1001)
x4_3 = np.linspace(0, 2*np.pi, 1000001)

y4_1 = np.convolve(sin(x4_1), np.array([1, 0, -1]) / (2 * ((2*np.pi)/101)), "same")
y4_2 = np.convolve(sin(x4_2), np.array([1, 0, -1]) / (2 * ((2*np.pi)/1001)), "same")
y4_3 = np.convolve(sin(x4_3), np.array([1, 0, -1]) / (2 * ((2*np.pi)/1000001)), "same")

plt.subplot(511)
plt.plot(x, y)

plt.subplot(512)
plt.plot(x2, y1)

plt.subplot(513)
plt.plot(x, y2)

plt.subplot(514)
plt.plot(x, y3)

plt.subplot(515)
plt.plot(x4_1, y4_1)
plt.plot(x4_2, y4_2)
plt.plot(x4_3, y4_3)

plt.show()
