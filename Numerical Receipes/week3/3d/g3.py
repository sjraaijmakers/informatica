import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d.axes3d import Axes3D
from matplotlib import cm

def f(t):
    return (t * np.cos(t), t * np.sin(t), 0.2*t)

fig = plt.figure()
ax = fig.gca(projection='3d')

t = np.linspace(0, 50, 500)

x, y, z = f(t)
ax.plot(x, y, z)

plt.show()
