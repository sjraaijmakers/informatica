import numpy as np
import matplotlib.pyplot as plt
import random

def f(x):
    return 2 * x

def g(x):
    return f(x) - np.random.uniform(-0.5, 0.5, size=x.shape)

x = np.linspace(0, 5, 51)
y = f(x)
y2 = g(x)

plt.plot(x, y, 'b-')
plt.plot(x, y2, 'ro')

# labels
plt.legend(['2x'])
plt.xlabel('x')
plt.ylabel('y')

plt.show()
