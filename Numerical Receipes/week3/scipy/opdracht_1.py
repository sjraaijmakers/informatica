import numpy as np
import matplotlib.pyplot as plt
import scipy.interpolate as ip

def f(t):
    return (t - 1) * (t - 2) * (t - 3)

t = np.array([0, 1, 4], float)
tnew = np.arange(0, 4.1, 0.1)
y = f(t)

g1 = ip.interp1d(t, y, kind='linear')
ynew1 = g1(tnew)

g2 = ip.interp1d(t, y, kind='quadratic')
ynew2 = g2(tnew)

ynew = f(tnew)

fig = plt.figure()

plt.plot(t, y, 'o')
plt.plot(tnew, ynew1, 'r--')
plt.plot(tnew, ynew2, 'g-.')
plt.plot(tnew, ynew, 'b-')

plt.axis([-.1, 4.1, -6.5, 6.5])
plt.xlabel('t')
plt.ylabel('y')

plt.legend(['punten', 'lineair', 'kwadratisch','kubisch'], loc=4)

plt.title('interpolatie')
plt.show()
