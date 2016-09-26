import numpy as np
import matplotlib.pyplot as plt

def f(x, t):
    return np.exp(-(x-3*t)**2) * np.sin(3*np.pi*(x-t))

t = np.linspace(-4, 4, 1000)

y1 = f(t, -0.5)
y2 = f(t, -0.25)
y3 = f(t, 0)
y4 = f(t, 0.25)
y5 = f(t, 0.5)

plt.figure()
plt.subplot(511)
plt.plot(t, y1)

plt.subplot(512)
plt.plot(t, y2)

plt.subplot(513)
plt.plot(t, y3)

plt.subplot(514)
plt.plot(t, y4)

plt.subplot(515)
plt.plot(t, y5)

# # labels
# plt.legend(['t^4*exp(-t^2)'])
# plt.xlabel('t')
# plt.ylabel('y(t)')
# plt.axis([-4.0, 4.0, -4.0, 4.0])

plt.show()
