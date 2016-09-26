from __future__ import division

import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d.axes3d import Axes3D
from matplotlib import cm


def f(x,y):
    return np.log(np.sqrt((x+1)**2 + y**2)) - np.log(np.sqrt((x-1)**2 + y**2))

x = np.linspace(-2.0, 2.0, 100)
y = np.linspace(-1.0, 1.0, 100)
x, y = np.meshgrid(x, y)
z = f(x,y)
fig = plt.figure(figsize=(14,3))
ax1 = fig.add_subplot(1,2,1)
levels = np.arange(-1.5, 1.7, 0.3)
ax1.contour(x, y, z, levels=levels, cmap=cm.coolwarm)
ax1.set_xlabel('$x$')
ax1.set_ylabel('$y$')
ax2 = fig.add_subplot(1,2,2)

p2 = ax2.contourf(x, y, z, levels=levels, cmap=cm.coolwarm)
fig.colorbar(p2)
ax2.set_xlabel('$x$')
ax2.set_ylabel('$y$')
plt.show()
