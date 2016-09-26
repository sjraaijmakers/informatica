import numpy as np
from pylab import *
import matplotlib.pyplot as plt

mu = array([[5],[6],[0],[1]])
Sigma=array(
 [[3.01602775,  1.02746769, -3.60224613, -2.08792829],
 [ 1.02746769,  5.65146472, -3.98616664,  0.48723704],
 [-3.60224613, -3.98616664, 13.04508284, -1.59255406],
 [-2.08792829,  0.48723704, -1.59255406,  8.28742469]] )

def points(N, M):
    # n should be same n as mu and sigma!
    d, U = eig(Sigma)
    L = diagflat(d)
    A = dot(U, sqrt(L))
    X = randn(N, M)               # 4*4
    Y = dot(A, X) + tile(mu, M)
    return Y

ys = points(4, 1000)

for i in range(0, 4):
    for j in range(0, 4):
        n = i*4 + j+1
        plt.subplot(4, 4, n)
        if i != j:
            plt.plot(ys[i], ys[j], "+g")
        # plt.title(str(i) + " vs " + str(j))

plt.show()
