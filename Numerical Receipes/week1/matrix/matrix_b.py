import math
import numpy as np

# Cholesky
A = np.array([[5, -2, -2], [-2, 5, 1], [2, 1, 2]])
L = np.linalg.cholesky(A)
Lt = np.transpose(L)
A_cholesky = np.dot(L, Lt)

print str(A) + "\n = "
print str(L) + "\n X "
print str(Lt) + "\n"

# QR decomposition
Q, R = np.linalg.qr(A)
A_qr = np.dot(Q, R)

print str(A_qr) + "\n = "
print str(Q) + "\n X "
print str(R) + "\n"
