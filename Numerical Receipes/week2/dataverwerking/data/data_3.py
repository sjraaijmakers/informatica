from __future__ import division
import numpy as np
import matplotlib.pyplot as plt

def fwc(data):
    tmp = np.zeros(len(data))
    tmp[0] = sum([data[x] for x in range(10)]) / 10
    for i in range(1, len(tmp)):
        tmp[i] = 0.5 * data[i] + 0.5 * tmp[i - 1]
    return tmp

def bwc(data):
    tmp = np.zeros(len(data))
    tmp[len(data)-1] = sum([data[len(data) - 1 - x] for x in range(10)]) / 10
    for i in range(1, len(tmp)):
        tmp[len(tmp) - i - 1] = 0.5 * data[len(data) - i - 1] + 0.5 * tmp[len(data) - i]
    return tmp

data = np.loadtxt('bierkraag.csv', delimiter=',')
x = data[:,0]
y = data[:,-1]

# D
#x = np.linspace(0, np.pi, 50)
#y = [np.sin(x + random.uniform(-0.5, 0.5)) for x in xs]

yf = fwc(y)
yb = bwc(y)

yc = [(i + j) / 2 for i, j in zip(yb, yf)]

plot1 = plt.subplot(3, 1, 1)
plot2 = plt.subplot(3, 1, 2)
plot3 = plt.subplot(3, 1, 3)

plot1.set_title("Voorwaarts")
plot2.set_title("Achterwaarts")
plot3.set_title("Verschil")

plt.show()
