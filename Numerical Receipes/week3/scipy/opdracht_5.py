from __future__ import division

import numpy as np
import matplotlib.pyplot as plt
import scipy as sp
from scipy.signal import savgol_filter

def v(t, h):
    return h / t**2

tijd, hoek, hoek2, a_analoog = np.loadtxt('Pezzack.txt', skiprows=6, unpack=True)

w = np.diff(hoek) / 0.0201

w = np.insert(w, 0, 0)
print w

print w

plt.xlabel('tijd (s)')
plt.ylabel('versnelling (rad/s$^2$)', fontsize=12)
