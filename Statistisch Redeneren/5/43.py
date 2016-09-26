import matplotlib.pyplot as plt
import numpy as np
import matplotlib.mlab as mlab
import math

# 1
x = np.linspace(-5, 15, 100)
y_1 = mlab.normpdf(x, 4, 1) * 0.3
y_2 = mlab.normpdf(x, 7, 1.5) * 0.7

y = y_1 + y_2

plt.plot(x, y_1, 'b') # c = 1
plt.plot(x, y_2, 'r') # c = 2

plt.plot(x, y_1 / y, 'r--') # c = 2
plt.plot(x, y_2 / y, 'b--') # c = 2

plt.show()
