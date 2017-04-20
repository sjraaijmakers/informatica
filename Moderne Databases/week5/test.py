#!env/bin/python

from checksum import add_integrity, check_integrity
from encode import encode, decode
from btree import Tree
from storage import DocumentStore

# store.write_footer(420, 0)
# print(store.read_footer())

# test b+ tree
t = Tree(max_size=5)
for i in range(0,7):
    t[i] = i ** 2

def test(id):
    print(t[id])

if __name__ == '__main__':
    test(3)
