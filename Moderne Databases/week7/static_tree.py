from parser import parse
from btree import Tree

store = parse("databases/nvdcve-2.0-2015.xml")
tree = Tree('test')

for i in range(len(store)):
    tree[i] = store[i]
