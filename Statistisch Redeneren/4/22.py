import numpy as np
from pylab import *
import matplotlib.pyplot as plt

mu = array([[5],[6],[0],[1]])
Sigma = array(
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
    return X, Y

def test(sample, n):
    means = []
    covs = []
    for i in range(n):
        means.append(sample.mean(1))
        covs.append(np.cov(sample))
    means = np.asarray(means)
    covs = np.asarray(covs)
    return means.mean(0), covs.mean(0)

if __name__ == '__main__':
    _, sample10     = points(4, 10)
    _, sample100    = points(4, 100)
    _, sample1000   = points(4, 1000)
    _, sample10000  = points(4, 10000)
    _, sample100000 = points(4, 100000)
    mean10,     covs10     = test(sample10,     10000)
    mean100,    covs100    = test(sample100,    10000)
    mean1000,   covs1000   = test(sample1000,   10000)
    mean10000,  covs10000  = test(sample10000,  10000)
    print "n = 10"
    print "mean:", mean10
    print "cov:", covs10
    print "n = 100"
    print "mean:", mean100
    print "cov:", covs100
    print "n = 1000"
    print "mean:", mean1000
    print "cov:", covs1000
    print "n = 10000"
    print "mean:", mean10000
    print "cov:", covs10000
    print "Actual"
    print "mean:", mu.flatten()
    print "cov:", Sigma
