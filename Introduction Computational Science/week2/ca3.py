# Steven Raaijmakers, 10804242

from __future__ import division
import matplotlib.pyplot as plt
from pyics import Model

import numpy as np
import math
import sys
import random
import itertools

# converst decimal (base 10) to base k
def decimal_to_base_k(n, k):
    return list(np.base_repr(n, base=k))

# Builds a rule set based on lambda
def lambda_rule_set(k, r, l, random_table=True):
    rule_length = k ** (2 * r + 1)

    # states
    states = list(range(k))
    sq = random.choice(states)
    states.remove(sq)

    lambda_rule_set = []
    # random table method:
    if random_table:
        for i in range(rule_length):
            g = random.uniform(0, 1)
            if g > l:
                val = sq
            else:
                val = random.choice(states)
            lambda_rule_set.append(val)
    # table walk through
    else:
        # init array with sq
        tmp = [sq] * rule_length

        # cells to be not(sq) to approx lambda
        approx = int(math.ceil(rule_length * l))

        # pick random "approx"s items from list
        indices = list(range(rule_length))
        random.shuffle(indices)

        for i in indices[:approx]:
            tmp[i] = random.choice(states)

        lambda_rule_set = tmp

    return lambda_rule_set

# checks occurences of lista in listb
def occurences(lista, listb, walls=False):
    parts = []

    # make cycle of listb
    if not walls:
        listb = listb[-(len(lista) - 1):] + listb

    return [tuple(listb[i:i+len(lista)]) for i in range(len(listb) - len(lista) + 1)].count(lista)

def get_entropy(k, r, inp):
    # all possible states
    k = list(itertools.product(list(range(0, k)), repeat=(2 * r + 1)))
    a_dict = dict(zip(k, ([0] * len(k))))

    # occurence of each possible state
    for key, value in a_dict.items():
        a_dict[key] = occurences(key, inp)

    # entropy calculation
    entropy = 0
    for key, value in a_dict.items():
        pi = value / sum(a_dict.values())
        if pi != 0:
            entropy += pi * np.log2(pi)

    return -entropy

# CA model simulator
class CASim(Model):
    def __init__(self):
        Model.__init__(self)

        self.t = 0
        self.rule_set = []
        self.config = None

        self.rows = []

        self.make_param('r', 2)
        self.make_param('k', 4)
        self.make_param('width', 50)
        self.make_param('height', 4)
        self.make_param('l', 0.0)
        self.make_param('random_table', False)
        # self.make_param('rule', 32, setter=self.setter_rule)

    def setter_rule(self, val):
        rule_set_size = self.k ** (2 * self.r + 1)
        max_rule_number = self.k ** rule_set_size
        return max(0, min(val, max_rule_number - 1))

    def build_rule_set(self):
        # rule = decimal_to_base_k(self.rule, self.k)
        # rule_set = ['0'] * (self.k ** (2 * self.r + 1) - len(rule))
        # rule_set.extend(rule)
        # self.rule_set = map(int, rule_set)
        if self.random_table:
            k = lambda_rule_set(self.k, self.r, self.l, True)
        else:
            k = lambda_rule_set(self.k, self.r, self.l, False)
        self.rule_set = k

    def check_rule(self, inp):
        inp_string = ''.join([str(int(i)) for i in inp])
        index = int(inp_string, base=self.k)
        return list(reversed(self.rule_set))[index]

    def set_lambda(self, l):
        # zeros = 0
        # for i in self.rule_set:
        #     if i == 0:
        #         zeros += 1
        # self.l = (len(self.rule_set) - zeros) / len(self.rule_set)
        self.l = l

    # flag gives a random init row
    def setup_initial_row(self, flag=True):
        if flag == False:
            init_row = [0] * self.width
            init_row[int(self.width / 2)] = 1
        elif flag == True:
            init_row = np.random.randint(self.k, size=self.width)
        return init_row

    # inits configuration / resets it
    def reset(self):
        self.rows = []
        self.t = 0
        self.config = np.zeros([self.height, self.width])
        self.config[0, :] = self.setup_initial_row()
        self.build_rule_set()
        # self.set_lambda()

    def draw(self):
        import matplotlib
        import matplotlib.pyplot as plt

        plt.cla()
        if not plt.gca().yaxis_inverted():
            plt.gca().invert_yaxis()
        plt.imshow(self.config, interpolation='none', vmin=0, vmax=self.k - 1,
                cmap=matplotlib.cm.binary)
        plt.axis('image')
        plt.title('t = %d' % self.t)

    def get_avg_se(self):
        e = 0
        for row in self.rows:
            e += get_entropy(self.k, self.r, row)
        return e / (len(self.rows))

    def step(self):
        row = []

        """Performs a single step of the simulation by advancing time (and thus
        row) and applying the rule to determine the state of the cells."""
        self.t += 1
        if self.t >= self.height:
            return True

        for patch in range(self.width):
            # We want the items r to the left and to the right of this patch,
            # while wrapping around (e.g. index -1 is the last item on the row).
            # Since slices do not support this, we create an array with the
            # indices we want and use that to index our grid.
            indices = [i % self.width
                    for i in range(patch - self.r, patch + self.r + 1)]
            values = self.config[self.t - 1, indices]
            self.config[self.t, patch] = self.check_rule(values)
            row.append(self.check_rule(values))
        self.rows.append(row)
        return row

    # run "step" till height

    def run(self):
        self.reset()
        tot = 0
        for i in range(0, self.height-1):
            row = self.step()
            k = get_entropy(self.k, self.r, row)
            tot += k
        return tot / self.height

if __name__ == '__main__':
    sim = CASim()
    # run GUI
    if len(sys.argv) > 1:
        from pyics import GUI
        cx = GUI(sim)
        cx.start()

        # print get_entropy(2, 1, [0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0])
    # show SE plot
    else:
        x = np.linspace(0.0, 1.0, num=1000)
        y = []
        #
        for i in x:
            sim.l = i

            tmp = 0

            it = 1
            for i in range(0, it):
                tmp += sim.run()
            e = tmp / it
            print sim.l, e
            y.append(e)

        plt.scatter(x, y, s=1, color='black', edgecolor='none')
        plt.title('Average Shannon Entropy / lambda CA (table walkthrough)')
        plt.suptitle('Height = ' + str(sim.height) + '. k = ' + str(sim.k) + ', r = ' + str(sim.r))
        plt.ylabel('Average H')
        plt.xlabel('Lambda')
        plt.show()
