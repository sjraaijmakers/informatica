from __future__ import division
import inspect
# Vak: Numerical Recipes
# Auteurs: Steven Raaijmakers, Daan Meijers
try:
    import matplotlib.pyplot as plt
except ImportError:
    print "Warning: could not import matplotlib.pyplot"
try:
    import numpy as np
    assert np
except ImportError:
    print "Warning: could not import numpy"
try:
    from scipy import ndimage, misc
    assert ndimage, misc
except ImportError:
    print "Warning: could not import scipy.ndimage"

from matplotlib.lines import Line2D

# Plots lines of df with direction based on slope
def lines(df, length, x_min, y_min, x_max, y_max):
    length = length / 2

    # Choose 2000 random points with 0 < x < 2
    #xp = np.random.uniform(x_min, x_max, 2000)

    xp = np.random.uniform(0, 2, 2000)
    yp = np.linspace(x_min, x_max, 20)

    lines = np.array([])
    for i in xp:
        j = np.random.uniform(0, 2)
        # Get a random y value

        # Compute slope of i, j in function
        slope = df(i, j)

        # Get scale (y = sqrt(a^2 + b^2))
        scaled = np.sqrt(1 + slope ** 2)

        # Get vectors
        h = 1 / scaled
        v = slope / scaled

        # X: old_x, new_x Y: old_y, new_y
        x = (i - length * h, i + length * h)
        y = (j - length * v, j + length * v)

        # Add lines
        lines = np.append(lines, Line2D(x, y))

    return lines

if __name__ == "__main__":
    dy = lambda t, y: 1 - 2 * t * y

    x_min = 0
    y_min = 0
    x_max = 2
    y_max = 2

    delta = 0.001

    fig = plt.figure()
    # setup
    ax = fig.add_subplot(111)
    #ax.set_title(inspect.getsource(dy))
    ax.set_xlim(x_min - 0.1, x_max + 0.1)
    ax.set_ylim(y_min - 0.1, y_max + 0.1)

    # get lines + print
    lines_array = lines(dy, 0.1, x_min, y_min, x_max, y_max)
    for l in lines_array:
        ax.add_line(l)

    plt.grid()
    plt.show()
