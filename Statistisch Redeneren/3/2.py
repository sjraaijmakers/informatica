import numpy as np
import matplotlib.pyplot as plt

## a
n = 20

x = []
y = []

for i in range(0, n+1):
    x.append(np.random.uniform(0, 1))
    y.append(np.random.uniform(0, 1))
#
plt.plot(x, y, '.')

## b

def randu(seed, size):
    l = [seed]
    a = 65539
    m = 2 ** 31
    c = 0

    while size > 0:
        k = (a * l[-1] + c) % m
        l.append(k)
        size -= 1
    return [x / float(m - 1) for x in l[1:]]

a = randu(2, 20)
b = randu(8, 20)

plt.plot(a, b, '.')

k = randu(1, 3)
r = (6 * k[1] - 9 * k[0]) % (2**31)
print str(r) + " = " + str(k[2])
b = randu(8, 20)

plt.show()
