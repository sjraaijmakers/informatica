from __future__ import division
from collections import Mapping, MutableMapping
from sortedcontainers import SortedDict
from storage import DocumentStore

store = DocumentStore()

class Tree(MutableMapping):
    def __init__(self, max_size=1024):
        self.root = self._create_leaf(tree=self)
        self.max_size = max_size

    @staticmethod
    def _create_leaf(*args, **kwargs):
        return Leaf(*args, **kwargs)

    @staticmethod
    def _create_node(*args, **kwargs):
        return Node(*args, **kwargs)

    def _create_root(self, lhs, rhs):
        root = self._create_node(tree=self)
        root.rest = lhs
        root.bucket[min(rhs.bucket)] = rhs
        return root

    def _commit(self):
        offset = self.root._commit()

    def __getitem__(self, key):
        s = self.root._select(key)
        while(not isinstance(s, Leaf)):
            s = s._select(key)
        return s[key]

    def __setitem__(self, key, value):
        """
        Inserts the key and value into the root node. If the node has been
        split, creates a new root node with pointers to this node and the new
        node that resulted from splitting.
        """
        # inserts new item
        n = self.root._insert(key, value)
        # check if split happened
        if n:
            self.root = self._create_root(self.root, n)

    # hoeft niet deze
    def __delitem__(self, key):
        pass

    def __iter__(self):
        yield from self.root

    def __len__(self):
        return len(self.root)

    def __repr__(self):
        return str(self.root)

# Basenode
class BaseNode(object):
    def __init__(self, tree, changed=False):
        self.tree = tree
        self.bucket = SortedDict()
        self.changed = changed

    def _split(self):
        """
        Creates a new node of the same type and splits the contents of the
        bucket into two parts of an equal size. The lower keys are being stored
        in the bucket of the current node. The higher keys are being stored in
        the bucket of the new node. Afterwards, the new node is being returned.
        """
        other = self.__class__(self.tree)

        # fill "other" with first part of original bucket
        # + remove first part from original bucket
        for k, v in self.bucket.items()[len(self.bucket) // 2:]:
            other.bucket[k] = v
            del self.bucket[k]

        return other

    def _insert(self, key, value):
        """
        Inserts the key and value into the bucket. If the bucket has become too
        large, the node will be split into two nodes.
        """
        self.bucket[key] = value
        self.changed = True

        # determine wheter bucket should be split
        if len(self.bucket) > self.tree.max_size:
            return self._split()

        # using None to indicate no split has happened
        return None

# Node
class Node(BaseNode):
    def __init__(self, *args, **kwargs):
        self.rest = None
        super(Node, self).__init__(*args, **kwargs)

    def _select(self, key):
        """
        Selects the bucket the key should belong to.
        """
        # determine where key should be put
        for i, (k, v) in enumerate(self.bucket.items()):
            # if first item has a bigger key then the key-2b-placed
            if i == 0 and key < k:
                return self.rest
            # if current key is bigger then k2p, return PREV
            elif key < k:
                return self.bucket[self.bucket.iloc[i - 1]]

        # nothing found; return last
        return self.bucket[self.bucket.iloc[i]]

    def _insert(self, key, value):
        """
        Recursively inserts the key and value by selecting the bucket the key
        should belong to, and inserting the key and value into that back. If the
        node has been split, it inserts the key of the newly created node into
        the bucket of this node.
        """
        n = self._select(key)._insert(key, value)

        # upon split: current buckets lowest val should point to split
        if n:
            self.bucket[min(n.bucket)] = n

        # split if neccessary
        if len(self.bucket) > self.tree.max_size:
            return self._split()

        return None

    # printing for debugging
    def __repr__(self):
        t = []
        for i in self.bucket:
            t.append(i)
        return "Node: " + str(t)

# wht
class Leaf(Mapping, BaseNode):
    def __getitem__(self, key):
        return self.bucket[key]

    def __iter__(self):
        for key in self.bucket.keys():
            yield key, self[key]

    def __len__(self):
        return len(self.bucket)

    def __repr__(self):
        return "Leaf: " + str(dict(self.bucket))

class LazyNode(object):
    _init = False

    def __init__(self, tree=None, offset=None, node=None):
        self.offset = offset
        self.node = node
        self.tree = tree
        self._init = True

    @property
    def changed(self):
        if self.node is None:
            return False

        return self.node.changed

    def _commit(self):
        if not self.changed:
            return

        self.node._commit()
        self.changed = False

    def _load(self):
        if not self.offset:
            self.offset = 0

        data = decode(store.get(self.offset))
        self.node = Node(tree=self.tree, id=self.offset)
        if 'children' in data:
            for child in data.children:
                self.node._insert(child, LazyNode(tree=self.tree, offset=child, node=None))

    def __getattr__(self, name):
        if not self.node:
            self.node = self._load()

        return getattr(self.node, name)

    def __setattr__(self, name, value):
        if not self._init or hasattr(self, name):
            return super().__setattr__(name, value)

        setattr(self.node, name, value)
