# Steven Raaijmakers, 10804242
# script does some testing with cellular automaton

from __future__ import division
import matplotlib.pyplot as plt

import numpy as np
import sys

from pyics import Model

# Converts decimal to base-k
def decimal_to_base_k(n, k):
    return list(np.base_repr(n, base=k))

# checks if list contains only the same items
def is_homogene(list):
    for i in list:
        for j in list:
            if i != j:
                return False
    return True


def contains_only_1_2(list):
    for i in list:
        if i != 1 and i != 2:
            return False
    return True

# CA model
class CASim:
    def __init__(self, rule, width, height):
        self.t = 0
        self.rule_set = []
        self.config = None

        self.r = 1
        self.k = 2
        self.width = width
        self.height = height
        self.rule = rule
        self.all = []

        self.cycles = []

    def setter_rule(self, val):
        rule_set_size = self.k ** (2 * self.r + 1)
        max_rule_number = self.k ** rule_set_size
        return max(0, min(val, max_rule_number - 1))

    # return array filled with zero's with the rule on the end of it
    def build_rule_set(self):
        rule = decimal_to_base_k(self.rule, self.k)
        rule_set = [0] * (self.k ** (2 * self.r + 1) - len(rule))
        rule_set.extend(rule)
        self.rule_set = rule_set

    # return state based on neighboorhood
    def check_rule(self, inp):
        inp_string = ''.join([str(int(i)) for i in inp])
        index = int(inp_string, base=self.k)
        return list(reversed(self.rule_set))[index]

    # initial row
    def setup_initial_row(self, flag):
        if flag == False:
            init_row = [0] * self.width
            init_row[int(self.width / 2)] = 1
        elif flag == True:
            init_row = np.random.randint(self.k, size=self.width)
        return init_row

    def reset(self):
        self.t = 0
        self.config = np.zeros([self.height, self.width])
        self.config[0, :] = self.setup_initial_row(True)
        self.build_rule_set()
        self.all = []

    def draw(self):
        """Draws the current state of the grid."""

        import matplotlib
        import matplotlib.pyplot as plt

        plt.cla()
        if not plt.gca().yaxis_inverted():
            plt.gca().invert_yaxis()
        plt.imshow(self.config, interpolation='none', vmin=0, vmax=self.k - 1,
                cmap=matplotlib.cm.binary)
        plt.axis('image')
        plt.title('t = %d' % self.t)

    def step(self):
        row = []
        """Performs a single step of the simulation by advancing time (and thus
        row) and applying the rule to determine the state of the cells."""
        self.t += 1
        if self.t >= self.height:
            return True

        for patch in range(self.width):
            indices = [i % self.width
                    for i in range(patch - self.r, patch + self.r + 1)]
            values = self.config[self.t - 1, indices]
            self.config[self.t, patch] = self.check_rule(values)
            row.append(self.check_rule(values))
        return row

    # finds a cycle within a ca
    def find_cycle(self):
        for i in range(0, self.height):
            r = self.step()
            if r not in self.all:
                self.all.append(r)
            else:
                k = self.all.index(r)
                return i - k
        return False

    # repeats finding one
    def find_cycles(self, repetition):
        for i in range(0, repetition):
            self.reset()
            r = self.find_cycle()
            if r:
                self.cycles.append(r)
            else:
                break

    def get_cycles(self):
        return self.cycles

    # determine class
    def get_class(self):
        c = self.get_cycles()

        avg = sum(c) / len(c)
        print c
        print avg

        # class 1 is homogene
        if (avg < 2 and avg >= 1.0) and is_homogene(self.all[-1]):
            return 1
        # class 2 = not(class 1) + only 1's or 2's in cycle
        elif contains_only_1_2(self.get_cycles()):
            return 2

if __name__ == '__main__':
    width = 10
    height = 1000

    r = 1
    k = 2

    repetition = 10

    if len(sys.argv) > 1:
        RULE = int(sys.argv[1])

        cycle_lengths = []
        sim = CASim(RULE, width, height + 2)
        sim.find_cycles(repetition)

        print sim.get_class()
    else:
        ca_rules = []
        avg_cycles = []

        for i in range(0, 256):
            ca_rules.append(i)

            sim = CASim(i, width, height)
            sim.find_cycles(repetition)

            print str(i) + ": " + str(sim.get_cycles())
            if len(sim.get_cycles()) > 0:
                avg = sum(sim.get_cycles()) / len(sim.get_cycles())
            else:
                avg = 0
            avg_cycles.append(avg)

        plt.bar(ca_rules, avg_cycles, color='r', edgecolor='none')
        plt.title('Average Cycle Length / rule')
        plt.suptitle('Width: ' + str(width) + ", Height: " +str(height))
        plt.ylabel('Cycle Length')
        plt.xlabel('Wolfram Rule')
        plt.show()
