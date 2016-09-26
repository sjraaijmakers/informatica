# Vak: Numerical Recipes
# Auteurs: Steven Raaijmakers, Daan Meijers
try:
    import matplotlib.pyplot as plt
    assert plt
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

# Voorwaartse Euler methode: berekend y1 op basis van y0 en phi
def euler(phi, t0, y0, t1, n):
    t = np.array([t0])
    y = np.array([y0])

    # delta t
    stepsize = t1 - t0

    # nieuwe coordinaten berekenen
    for i in range(0, n):
        new_t = t[-1] + stepsize
        t = np.append(t, new_t)
        new_y = y[-1] + phi(new_t, y[-1]) * stepsize
        y = np.append(y, new_y)

    return t, y


if __name__ == "__main__":
    dy = lambda t, y: 1 - 2 * t * y

    t, y = euler(dy, 0, 1, 0.25, 10)
    t2, y2 = euler(dy, 0, 1.5, 0.25, 10)

    plt.plot(t, y)
    plt.plot(t2, y2)

    plt.show()
