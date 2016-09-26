import numpy as np
import matplotlib.pyplot as plt
import math

LAMBDA = 1

def f(x, lmb):
    return lmb * np.exp((-lmb*x))

def e(u, lmb):
    return -1 / lmb * np.log(u)

x = np.linspace(0, 5, 10000)
y = f(x, LAMBDA)
z = e(np.random.uniform(0, 1, 1000), LAMBDA)

plt.plot(x, y)
plt.hist(z, 50, normed=True)

mu = np.mean(z)
print "Lambda: " + str(1 / mu)

plt.show()
