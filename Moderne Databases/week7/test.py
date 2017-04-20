# Steven Raaijmakers, Baer Halberkamp
# Run mapreduce and show plots

import matplotlib.pyplot as plt
from collections import Counter
from time import time

from btree import Tree
from parser import parse
from mapreduce import Script

start_time = time()
store = parse('nvdcve-2.0-2016.xml')
print("Parsing dataset: {}s".format(time() - start_time))

start_time = time()
tree = Tree('./storage.boefdb')

for i in range(len(store)):
        tree[i] = store[i]
print("Parsing B+ tree: {}s".format(time() - start_time))

mapred_files = ['./test/vendors.py', './test/products.py']

for f in mapred_files:
    wrapper = Script()
    wrapper.add_file(f)

    start_time = time()
    results = Counter(wrapper.run_mapreduce(store))
    top10 = results.most_common(10)
    elapsed_time = time() - start_time
    
    plt.title("Map/Reduce query {}. Elapsed time: {}s".format(f, elapsed_time))
    plt.bar(range(10), [a[1] for a in top10])
    plt.xticks(range(10), [a[0] for a in top10])
    plt.show()

