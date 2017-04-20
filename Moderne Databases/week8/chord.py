from time import time

import math

# Code based on pseudocode found on wikipedia, as well as the pseudocode found in the paper about chord provided on blackboard

class Node:
    ring_size = 16
    m = int(math.log(ring_size, 2))

    def __init__(self, node_id):
        self.predecessor = None
        self.id = node_id
        self.fingers = [self for _ in range(self.m)]

    def distance(self, lhs, rhs):
        return (self.ring_size + rhs - lhs) % self.ring_size

    def in_range(self, value, lower, upper):
        if lower is upper:
            return True

        return self.distance(lower, value) < self.distance(lower, upper)

    def find_successor(self, node_id):
<<<<<<< HEAD:moda/week8/chord.py
        if node_id > self.id and node_id <= self.fingers[0].id:
            return self.fingers[0]
=======
        n = self.find_predecessor(node_id)
        return n.fingers[0]

    def find_predecessor(self, node_id):
        n = self
        while not self.in_range(node_id, n.id+1, n.fingers[0].id+1):
            n = n.closest_preceding_finger(node_id)
        return n
>>>>>>> 6b9b714771b0d484abdf8536398becef70c1db67:week8/chord.py

    def closest_preceding_finger(self, node_id):
        for i in reversed(range(self.m)):
            if self.in_range(self.fingers[i].id, self.id+1, node_id):
                return self.fingers[i]
        return self

    def join(self, node):
        if node:
            self.init_finger_table(node)
            self.update_others()
        else:
            for i in range(self.m):
                self.fingers[i] = self
            self.predecessor = self
    
    def init_finger_table(self, node):
        self.fingers[0] = node.find_successor(self.id + 1)
        self.predecessor = self.fingers[0].predecessor
        self.fingers[0].predecessor = self
        for i in range(self.m-1):
            start = (self.id + 2 ** i) % (2 ** self.m)
            if self.in_range(start, self.id, self.fingers[i].id):
                self.fingers[i+1] = self.fingers[i]
            else:
                self.fingers[i+1] = node.find_successor((self.id + 2 ** (i + 1)) % (2 ** self.m))

    def update_others(self):
        for i in range(self.m):
            p = self.find_predecessor(self.id - 2 ** i)
            p.update_finger_table(self, i)

    def update_finger_table(self, node, index):
        if self.in_range(node.id, self.id, self.fingers[index].id):
            self.fingers[index] = node
            p = self.predecessor
            if p:
                p.update_finger_table(node, index)

    def stabilize(self):
        x = self.fingers[0].predecessor
        if self.in_range(x.id, self.id+1, self.fingers[0].id):
            self.fingers[0] = x
        self.fingers[0].notify(self)

    def notify(self, node):
        if self.predecessor is None or self.in_range(node.id, self.predecessor.id+1, self.id):
            self.predecessor = node

    def fix_fingers(self):
        for i in range(self.m):
            self.fingers[i] = self.find_successor(self.id + 2 ** i)

    def __repr__(self):
<<<<<<< HEAD:moda/week8/chord.py
        return "({}: {})".format(self.id, self.fingers)
=======
        return str(self.id)

    def print_fingers(self):
        print('Finger table for node #{}: {}'.format(self, self.fingers))
        print('Predecessor: {}'.format(self.predecessor))
>>>>>>> 6b9b714771b0d484abdf8536398becef70c1db67:week8/chord.py

if __name__ == '__main__':
    print("- Settings up nodes")
    timer = time()
    nodes = [Node(i) for i in range(Node.ring_size)]
    print("-- Set up nodes: {}s".format(time() - timer))
    timer = time()
    for i in range(1, len(nodes)):
        nodes[i].join(nodes[0])
    print("-- Joined root node: {}s".format(time() - timer))
    timer = time()
    for node in nodes:
        node.stabilize()
        node.fix_fingers()
    print("-- Stabilized ring: {}s".format(time() - timer))
<<<<<<< HEAD:moda/week8/chord.py
    print("Fingers of node 0: {}".format(nodes[0]))
=======
    for n in nodes:
        n.print_fingers()
  
>>>>>>> 6b9b714771b0d484abdf8536398becef70c1db67:week8/chord.py
