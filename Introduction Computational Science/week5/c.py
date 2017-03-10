import matplotlib.pyplot as plt
import numpy as np

def f1(x, t):
	return 1

def f2(x, t):
    return 2 * t

def f3(x, t):
    return -x

def euler(f, y0, delta_t, a=0, b=3):
    t,y = a,y0
    while t <= b:
        print "%6.3f %6.3f" % (t,y)
        t += delta_t
        y += delta_t * f(t,y)

def euler(f, x, y0, delta_t):
    tmp = []

    t,y = np.amin(x), y0
    for i in x:
        tmp.append(y)
        t += delta_t
        y += delta_t * f(t,y)
    return tmp

if __name__ == '__main__':
    t_min = 0
    t_max = 3
    delta_t = 1

    x = np.arange(t_min, t_max + 1, delta_t)
    y = euler(f3, x, 4, delta_t)

    plt.plot(x, y)
    plt.show()
