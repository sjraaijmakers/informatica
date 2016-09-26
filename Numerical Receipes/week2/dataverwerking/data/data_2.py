from __future__ import division
import numpy as np
import matplotlib.pyplot as plt

def full_wave(data, width):
    tmp = np.zeros(len(data))
    absolute = np.absolute(data)
    np.lib.pad(absolute, (width, width), 'constant', constant_values=(0, 0))
    for i, t in enumerate(tmp):
        tmp[i] = np.sum(absolute[i - width: i + width + 1]) * (1 / (2*width+1))
    return tmp

def root_mean_square(data, width):
    tmp = np.zeros(len(data))
    absolute = np.absolute(data)
    np.lib.pad(absolute, (width, width), 'constant', constant_values=(0, 0))
    absolute = [x**2 for x in absolute]
    for i, t in enumerate(tmp):
        tmp[i] = np.sqrt(np.sum(absolute[i-width:i+width+1]) * (1 / (2 * width + 1)))
    return tmp

def draw(data, arv, rms):
    plt.plot(data[:,0], data[:,1], color='black')
    plt.plot(data[:,0], arv, color='red')
    plt.plot(data[:,0], rms, color='blue')
    plt.legend()
    plt.show()

data = np.loadtxt('emg.csv', delimiter=';')
interval = data[(1.0 > data[:,0]) & (data[:,0] > 0.3)]
interval_mean = np.mean(interval[:,1])
interval[:,1] -= interval_mean

arv = full_wave(interval[:,1], 50)
rms = root_mean_square(interval[:,1], 50)

x = interval[:,0]
y = interval[:,1]

plt.plot(x, y)
plt.plot(x, arv)
plt.plot(x, rms)

plt.show()
