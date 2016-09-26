from __future__ import division
import numpy as np
import matplotlib.pyplot as plt

def full_wave_rectificatie(data, filterbreedte):
    arv = np.zeros(len(data))
    absolute_data = np.absolute(data)
    np.lib.pad(absolute_data, (filterbreedte, filterbreedte), 'constant', constant_values=(0, 0))

    for i in range(len(arv)):
        arv[i] = np.sum(absolute_data[i-filterbreedte:i+filterbreedte+1]) * (1 / (2 * filterbreedte + 1))

    return arv

def root_mean_square(data, filterbreedte):
    rms = np.zeros(len(data))
    absolute_data = np.absolute(data)
    np.lib.pad(absolute_data, (filterbreedte, filterbreedte), 'constant', constant_values=(0, 0))
    absolute_data = [x**2 for x in absolute_data]

    for i in range(len(rms)):
        rms[i] = np.sqrt(np.sum(absolute_data[i-filterbreedte:i+filterbreedte+1]) * (1 / (2 * filterbreedte + 1)))
        
    return rms


def draw(data, arv, rms):
    plt.plot(data[:,0], data[:,1], color='black')
    plt.plot(data[:,0], arv, color='red')
    plt.plot(data[:,0], rms, color='blue')
    plt.legend()
    plt.show()

if __name__ == "__main__":
    emg_data = np.loadtxt('emg.csv', delimiter=';')
    emg_data_interval = emg_data[(1.0 > emg_data[:,0]) & (emg_data[:,0] > 0.3)]
    emg_data_interval_gemiddeld = np.mean(emg_data_interval[:,1])
    emg_data_interval[:,1] -= emg_data_interval_gemiddeld
    
    arv = full_wave_rectificatie(emg_data_interval[:,1], 50)
    rms = root_mean_square(emg_data_interval[:,1], 50)

    draw(emg_data_interval, arv, rms)

