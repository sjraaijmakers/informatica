import numpy as np
import numpy.linalg as la

A = np.array([[-3, -1, 0], [4, 7, -10], [4, 3, -3]])
B = np.array([[-2, 2, -5], [12, -7, 20], [6, -4, 11]])
C = np.array([[-2, 14, 8], [-1, 10, 6], [1, -13, -8]])

matrices = [A, B]

def mpower(m, exp):
    tmp = m
    for i in range(1, exp):
        tmp = tmp.dot(m)
    return tmp

for m in matrices:
    print "M:\n" + str(m)
    for i in range(2, 6):
        tmp = mpower(m, i)
        if np.array_equal(m, tmp):
            print "M = M^" + str(i)
    print ""

print "M:\n" + str(C)
for i in range(2, 6):
    i = i + 2
    tmp = mpower(C, i)
    if np.all(tmp == 0):
        print "M^" + str(i) + " = 0"
