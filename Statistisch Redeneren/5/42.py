from pylab import tile, argmin, loadtxt, arange, loadtxt, permutation, transpose,\
    zeros, sum, plot, subplot, array, scatter, logical_and, figure,\
    savefig
import sys
import numpy as np
import matplotlib.pyplot as plt
sys.path.append("./python")

class NNb:
    def __init__(self, X, c):
        self.n, self.N = X.shape
        self.X = X
        self.c = c

    def classify(self, x):
        d = self.X - tile(x.reshape(self.n, 1), self.N)
        dsq = sum(d*d, 0)
        minindex = argmin(dsq)
        return self.c[minindex]

    def kclassify(self, k, x):
        d = self.X - tile(x.reshape(self.n, 1), self.N)
        dsq = sum(d*d, 0)
        lowest = np.argsort(dsq)[:k] # find index of k lowest distance
        classes = [self.c[l] for l in lowest] # get corresponding class
        return np.argmax(np.bincount(classes)) # return most common class

def cnvt(s):
    tab = {'Iris-setosa':1.0, 'Iris-versicolor':2.0, 'Iris-virginica':3.0}
    if tab.has_key(s):
        return tab[s]
    else:
        return -1.0

XC = loadtxt('iris.data', delimiter=',', dtype=float, converters={4: cnvt})
ind = arange(150) # indices into the dataset
ind = permutation(ind) # random permutation
L = ind[0:90] # learning set indices
T = ind[90:] # test set indices

# Learning Set
X = transpose(XC[L,0:4])

# Original
nnc = NNb(X, XC[L,-1])
c = zeros(len(T))
for i in arange(len(T)):
    c[i] = nnc.classify(XC[T[i],0:4])
# Confusion Matrix
CM = zeros((3,3))
for i in range(3):
    for j in range(3):
        CM[i,j] = sum( logical_and(XC[T,4]==(i+1),c==(j+1)) )
print "Original: "
print(CM)

# Ks
def create_k_cm(k):
    nnc = NNb(X, XC[L,-1])
    c = zeros(len(T))
    for i in arange(len(T)):
        c[i] = nnc.kclassify(k, XC[T[i],0:4])
    # Confusion Matrix
    CM = zeros((3,3))
    for i in range(3):
        for j in range(3):
            CM[i,j] = sum( logical_and(XC[T,4]==(i+1),c==(j+1)) )
    return (CM)

para = 20

ks = [1, 3, 5, 7, 9]

for k in ks:
    conf = zeros((3,3))
    for i in range(para):
        tmp = create_k_cm(k)
        conf += tmp
    conf = conf / para
    print str(k) + ": "
    print conf




# Plot Test Set
# figure(1)
# color = array( [[1,0,0],[0,1,0],[0,0,1]] )
# for i in range(4):
#     for j in range(4):
#         subplot(4,4,4*i+j+1)
#         if i==j:
#             continue
#         scatter(XC[T,i], XC[T,j], s=100, marker='s',
#             edgecolor=color[XC[T,4].astype(int)-1],
#             facecolor=[1,1,1]*len(T))
#         scatter( XC[T,i], XC[T,j], s=30, marker='+',
#             edgecolor=color[c.astype(int)-1])

# plt.show()
