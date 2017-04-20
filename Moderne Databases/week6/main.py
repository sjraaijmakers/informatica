#!env/bin/python

# Steven Raaijmakers, 10804242
# Bar Harbelkamp,

# Testing

from btree import Tree

def main():
    t = Tree(max_size=3)

    for i in range(0,50):
        t[i] = i ** 2

    for k in t.root.bucket.items():
        print(k)

if __name__ == '__main__':
    main()
