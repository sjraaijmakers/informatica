#!env/bin/python

"""
Moderne Databases Project

Authors:    Nicky Kessels   (nicky_kessels@hotmail.com)
            Huck Nuchelmans (me@hucknuchelmans.nl)

Course:     Moderne Databases
            Bachelor Informatica
            Universiteit van Amsterdam

File:       btree.py

This file implements a B+ tree.
"""

from os import stat, rename
from os.path import isfile
from struct import Struct

from collections import Mapping, MutableMapping
from sortedcontainers import SortedDict
from encode2 import encode, decode
from checksum import add_integrity, check_integrity

pack_uint32 = Struct('>L').pack
unpack_uint32 = lambda x: Struct('>L').unpack(x)[0]


class Tree(MutableMapping):
    """ Tree wrapper class. """
    def __init__(self, db_file, max_size=1024):
        self.db_file = db_file
        self.max_size = max_size
        if isfile(self.db_file) and stat(self.db_file).st_size != 0:
            with open(self.db_file, 'rb') as f:
                # First find the offset of the footer, then the root
                f.seek(0, 2)
                footer_offset = f.tell() - Struct('>L').size
                f.seek(footer_offset, 0)
                root_offset = unpack_uint32(f.read(Struct('>L').size))
            self.root = LazyNode(offset=root_offset, ltree=self)
            self.root._load()
        else:
            self.root = self._create_leaf(tree=self, changed=True)

    @staticmethod
    def _create_leaf(*args, **kwargs):
        return LazyNode(node=Leaf(*args, **kwargs))

    @staticmethod
    def _create_node(*args, **kwargs):
        return LazyNode(node=Node(*args, **kwargs))

    def _create_root(self, lhs, rhs):
        """ Create a root node with two children. """
        root = self._create_node(tree=self, changed=True)
        root.rest = LazyNode(node=lhs)
        root.bucket[min(rhs.bucket)] = rhs

        return root

    def _commit(self, **kwargs):
        """ Write changes to disk. """
        self.root._commit(**kwargs)

        f = open(self.db_file, 'ab')
        footer = pack_uint32(self.root.offset)
        f.write(footer)
        f.close()

    def compact(self):
        """
        Compact the database by only keeping the newest version of each node.
        """
        old_size = stat(self.db_file).st_size

        # Dirty hack: len loads the entire database.
        len(self)

        # Point the tree to a temporary file.
        original_file = self.db_file
        self.db_file = original_file + '.tmp'

        self._commit(force=True)

        # Rename the temporary file to the original, overwriting the original
        # database in the process.
        rename(self.db_file, original_file)
        self.db_file = original_file

        # Compare the old and new sizes of the database.
        new_size = stat(self.db_file).st_size
        print("New database size: %d bytes (%f%%)" %
              (new_size, new_size/old_size * 100))

    def __getitem__(self, key):
        value = self.root[key]
        if value is not None:
            return value
        else:
            raise KeyError(key)

    def __setitem__(self, key, value):
        """
        Inserts the key and value into the root node. If the node has been
        split, creates a new root node with pointers to this node and the new
        node that resulted from splitting.
        """
        self.root._insert(key, value)

    def __delitem__(self, key):
        del self.root[key]

        if len(self.root.bucket) == 0:
            self.root = self.root.rest

    def __iter__(self):
        return iter(self.root)

    def __len__(self):
        return len(self.root)

class BaseNode(object):
    """ Base class both Leaf and Node inherit from. """
    def __init__(self, tree, changed):
        self.tree = tree
        self.changed = changed
        self.bucket = SortedDict()

    def _split(self):
        """
        Creates a new node of the same type and splits the contents of the
        bucket into two parts of an equal size. The lower keys are being stored
        in the bucket of the current node. The higher keys are being stored in
        the bucket of the new node. Afterwards, the new node is being returned.
        """
        if isinstance(self, Node):
            new_node = self.tree._create_node(self.tree, changed=True)
        elif isinstance(self, Leaf):
            new_node = self.tree._create_leaf(self.tree, changed=True)
        new_bucket = SortedDict()

        for key in self.bucket.keys()[len(self.bucket)//2:]:
            new_node.bucket[key] = self.bucket[key]

        for key in self.bucket.keys()[:len(self.bucket)//2]:
            new_bucket[key] = self.bucket[key]

        self.bucket = new_bucket

        if self.tree.root.node == self:
            self.tree.root = self.tree._create_root(self, new_node)
            if isinstance(new_node.node, Node):
                min_key = min(new_node.bucket)
                new_node.rest = new_node.bucket[min_key]
                del new_node.bucket[min_key]
        else:
            return new_node

    def _insert(self, key, value):
        """
        Inserts the key and value into the bucket. If the bucket has become too
        large, the node will be split into two nodes.
        """
        self.bucket[key] = value
        self.changed = True

        if len(self.bucket) > self.tree.max_size - 1:
            return self._split()


class Node(BaseNode):
    """ A node containing references to other nodes or leaves. """
    def __init__(self, *args, **kwargs):
        self.rest = None

        super(Node, self).__init__(*args, **kwargs)

    def _get_data(self):
        data = [(key, value.offset) for key, value in self.bucket.items()]

        return (False, [self.rest.offset] + data)

    def _select(self, key):
        """
        Selects the bucket the key should belong to.
        """
        for word in reversed(self.bucket):
            if key < word:
                continue
            return self.bucket[word]

        return self.rest

    def _insert(self, key, value):
        """
        Recursively inserts the key and value by selecting the bucket the key
        should belong to, and inserting the key and value into that back. If
        the node has been split, it inserts the key of the newly created node
        into the bucket of this node.
        """
        new_node = self._select(key)._insert(key, value)
        self.changed = True

        if new_node is not None:
            min_key = min(new_node.bucket)
            # If the rest is empty, move the lowest key to the rest.
            if isinstance(new_node.node, Node) and new_node.rest is None:
                new_node.rest = new_node.bucket[min_key]
                del new_node.bucket[min_key]
            # Insert the new node and return possibly another new node.
            return super()._insert(min_key, new_node)

    def __getitem__(self, key):
        return self._select(key)[key]

    def __delitem__(self, key):
        target = self._select(key)
        del target[key]
        self.changed = True

        if len(target.bucket) < (self.tree.max_size - 1) / 2:
            target_bucket_key = None
            left_bucket = None
            right_bucket = None
            right_bucket_key = None
            for word in reversed(self.bucket):
                if key < word:
                    continue
                target_bucket_key = word
                break

            # Determines if the node is an edge case or very central node
            if target_bucket_key is None:
                right_bucket_key = self.bucket.keys()[0]
                right_bucket = self.bucket[right_bucket_key]
            elif target_bucket_key == self.bucket.keys()[0]:
                left_bucket = self.rest
                if len(self.bucket) > 1:
                    right_bucket_key = self.bucket.keys()[1]
                    right_bucket = self.bucket[right_bucket_key]
            elif target_bucket_key == self.bucket.keys()[-1]:
                left_bucket = self.bucket[self.bucket.keys()[-2]]
            else:
                index = self.bucket.index(target_bucket_key)
                left_bucket = self.bucket[self.bucket.keys()[index - 1]]
                right_bucket_key = self.bucket.keys()[index + 1]
                right_bucket = self.bucket[right_bucket_key]

            if right_bucket is not None and \
                    len(right_bucket.bucket) > self.tree.max_size / 2:
                # The node receives a key from the right neighbour
                if isinstance(target.node, Leaf):
                    (new_key, new_value) = \
                        right_bucket.bucket.popitem(last=False)
                    target.bucket[new_key] = new_value
                    self.bucket[right_bucket.bucket.keys()[0]] = \
                        self.bucket.pop(new_key)
                else:
                    rest_key = right_bucket.rest.bucket.keys()[0]
                    target.bucket[rest_key] = right_bucket.rest
                    (new_key, new_value) = \
                        right_bucket.bucket.popitem(last=False)
                    right_bucket.rest = new_value
            elif left_bucket is not None and \
                    len(left_bucket.bucket) > self.tree.max_size / 2:
                # The node receives a key from the left neighbour
                (new_key, new_value) = left_bucket.bucket.popitem(last=True)
                if isinstance(target.node, Leaf):
                    target.bucket[new_key] = new_value
                else:
                    rest_key = target.rest.bucket.keys()[0]
                    target.bucket[rest_key] = target.rest
                    target.rest = new_value
                self.bucket[new_key] = self.bucket.pop(target_bucket_key)
            elif right_bucket is not None:
                # Merge with the right neighbour
                if isinstance(right_bucket.node, Node):
                    rest_key = right_bucket.rest.bucket.keys()[0]
                    right_bucket.bucket[rest_key] = right_bucket.rest
                target.bucket.update(right_bucket.bucket)
                del self.bucket[right_bucket_key]
            else:
                # Merge with the left neighbour
                if isinstance(target.node, Node):
                    rest_key = target.rest.bucket.keys()[0]
                    target.bucket[rest_key] = target.rest
                left_bucket.bucket.update(target.bucket)
                del self.bucket[target_bucket_key]

    def __iter__(self):
        if self.rest is not None:
            yield from self.rest
        for key in self.bucket:
            yield from self.bucket[key]

    def __len__(self):
        size = sum([len(value) for value in self.bucket.values()])
        if self.rest is not None:
            size += len(self.rest)
        return size


class Leaf(Mapping, BaseNode):
    """ A leaf containing key value pairs. """
    def _get_data(self):
        data = [(key, value) for key, value in self.bucket.items()]

        return (True, data)

    def __getitem__(self, key):
        if key in self.bucket:
            return self.bucket[key]

    def __delitem__(self, key):
        del self.bucket[key]
        self.changed = True

    def __iter__(self):
        yield from self.bucket

    def __len__(self):
        return len(self.bucket)


class LazyNode(object):
    """
    Implements a node wrapper allowing nodes and leaves to be loaded
    dynamically.
    """
    _init = False

    def __init__(self, offset=None, node=None, ltree=None):
        """
        Sets up a proxy wrapper for a node at a certain disk offset.
        """
        self.offset = offset
        self.node = node
        self.ltree = ltree
        self._init = True

    @property
    def changed(self):
        """
        Checks if the node has been changed.
        """
        if self.node is None:
            return False

        return self.node.changed

    def _commit(self, force=False):
        """
        Commit the changes if the node has been changed. If force is True, the
        node is commited even if no changes were made.
        """
        if not force and not self.changed:
            return

        if isinstance(self.node, Node):
            self.node.rest._commit(force)
            for key in self.node.bucket:
                self.node.bucket[key]._commit(force)

        data = self.node._get_data()
        chunk = encode(data)

        with open(self.tree.db_file, 'ab') as f:
            self.offset = f.tell()
            f.write(add_integrity(chunk))

        self.changed = False

    def _load(self):
        """
        Load the node from disk.
        """
        if self.offset is None:
            raise ValueError('Can not read from offset \'None\'')

        with open(self.ltree.db_file, 'rb') as f:
            f.seek(self.offset, 0)
            chunklen = f.read(Struct('>L').size)
            rest = f.read(unpack_uint32(chunklen))

        data = decode(check_integrity(chunklen + rest))

        if data[0]:
            # The case where the loaded node is a leaf
            self.node = Leaf(tree=self.ltree, changed=False)

            for child in data[1]:
                self.node.bucket[str(child[0], 'utf-8')] = \
                    str(child[1], 'utf-8')
        else:
            # The case where the loaded node is an inner node
            self.node = Node(tree=self.ltree, changed=False)
            self.node.rest = LazyNode(offset=data[1].pop(0), ltree=self.ltree)

            for child in data[1]:
                self.node.bucket[str(child[0], 'utf-8')] = LazyNode(
                    offset=child[1], ltree=self.ltree)

    def __getitem__(self, name):
        if self.node is None:
            self._load()

        return self.node[name]

    def __getattr__(self, name):
        """
        Loads the node if it hasn't been loaded yet, and dispatches the request
        to the node.
        """
        if self.node is None:
            self._load()

        return getattr(self.node, name)

    def __setattr__(self, name, value):
        """
        Dispatches the request to the node, if the proxy wrapper has been fully
        set up.
        """
        if not self._init or name in self.__dict__:
            return super().__setattr__(name, value)

        setattr(self.node, name, value)

    def __delitem__(self, key):
        if self.node is None:
            self._load()

        del self.node[key]

    def __iter__(self):
        if self.node is None:
            self._load()

        return iter(self.node)

    def __len__(self):
        if self.node is None:
            self._load()

        return len(self.node)
