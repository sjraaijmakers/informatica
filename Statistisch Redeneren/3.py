import math
import numpy as np
import scipy as sp
from scipy.stats import norm
import matplotlib.pyplot as plt

#pdf
x = np.linspace(norm.ppf(0.1), norm.ppf(0.99), 100)
pdf = norm.pdf(x)
cdf = norm.cdf(x)

k = norm.rvs(size=1000)

plt.plot(x, pdf)
plt.plot(x, cdf)
plt.hist(k, 30, normed=True)

plt.show()
