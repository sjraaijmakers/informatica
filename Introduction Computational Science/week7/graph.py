from __future__ import division
import networkx as nx
import matplotlib.pyplot as plt
from collections import Counter
from random import choice
import random
import numpy as np
import sys
import math

if __name__ == '__main__':
    usage = "give args"

    if len(sys.argv) != 2:
        print(usage)
    else:
        argument = sys.argv[1]

        if argument == "A":
            G = nx.Graph()
            G.add_nodes_from([1,2,3,4,5])
            G.add_edges_from([(1,2), (2,3), (3,4), (3,5), (4, 5)])

            degrees = [G.degree(n) for n in G.nodes()]
            frequences = dict(Counter(degrees))

            k = [key for key, _ in frequences.items()]
            pk = np.array([value for _, value in frequences.items()])
            pk = pk / sum(pk)

            plt.bar(k, pk)
            axes = plt.gca()
            axes.set_ylim([0, 1])
            plt.title("Percentage of degrees in network")
            plt.xlabel("k")
            plt.ylabel("p(k)")
            plt.show()
