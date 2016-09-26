from __future__ import division

import numpy as np
from sklearn import svm, grid_search

# the colors found in the dataset
Colors = ['black', 'blue', 'brown', 'gray', 'green', 'orange', 'pink', 'red', 'violet', 'white', 'yellow']
# does a line of text contains a color name?
def containsColor( line ):
    for c in Colors:
        if line.find(c)>=0:
            return Colors.index(c), c
    return None, None

# read the file and store spectra in matrix D (rows are the spectra) # and the classes in vector y
fp = open("natural400_700_5.asc")
lines = fp.readlines()

D = np.zeros((0, 61))
y = np.array([])
for i in range(0, len(lines), 2):
    ind, c = containsColor(lines[i])
    if ind is not None:
        d = np.fromstring(lines[i + 1], dtype=int, sep=" ")
        D = np.append(D, np.array([d]), axis=0)
        y = np.append(y, ind)

size_1 = len(D) * 0.5
size_2 = len(D) * 0.5

# Learning set
D_1 = D[:size_1]
y_1 = y[:size_1]

# Test set
D_2 = D[size_2:]
y_2 = y[size_2:] # real classes

parameters = {'kernel':('linear', 'rbf'), 'C':[1, 10]}
svr = svm.SVC()
clf = grid_search.GridSearchCV(svr, parameters)
clf.fit(D_1, y_1)

good = 0
for i, t in enumerate(D_2):
    real = y_2[i]
    prd = clf.predict([t])
    if real == prd:
        good += 1

print good / len(D_2) * 100
