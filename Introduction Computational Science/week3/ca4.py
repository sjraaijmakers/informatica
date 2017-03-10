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

# setup initial row by density
def density_init_row(density, width):
    # init array
    tmp = [0] * width

    # ceil density to appropiate amount of cells
    approx = int(math.ceil(width * density))

    indices = list(range(width))
    random.shuffle(indices)

    for i in indices[:approx]:
        tmp[i] = 1

    return tmp

# CA model simulator
class CASim(Model):
    def __init__(self):
        Model.__init__(self)

        self.t = 0
        self.rule_set = []
        self.config = None

        self.carflow = 0

        self.make_param('r', 1)
        self.make_param('k', 2)
        self.make_param('width', 50)
        self.make_param('height', 50)
        self.make_param('rule', 102)
        self.make_param('density', 0.4)

    def setter_rule(self, val):
        rule_set_size = self.k ** (2 * self.r + 1)
        max_rule_number = self.k ** rule_set_size
        return max(0, min(val, max_rule_number - 1))

    def build_rule_set(self):
        rule = decimal_to_base_k(self.rule, self.k)
        rule_set = [0] * (self.k ** (2 * self.r + 1) - len(rule))
        rule_set.extend(rule)
        self.rule_set = rule_set

    def check_rule(self, inp):
        inp_string = ''.join([str(int(i)) for i in inp])
        index = int(inp_string, base=self.k)
        return list(reversed(self.rule_set))[index]

    def find_carflow(self, row):
        if row[0] == '0' and row[self.width - 1] == '1':
            return 1
        return 0

    # flag gives a random init row
    def setup_initial_row(self):
        return density_init_row(self.density, self.width)

    # inits configuration / resets it
    def reset(self):
        self.carflow = 0
        self.t = 0
        self.config = np.zeros([self.height, self.width])
        self.config[0, :] = self.setup_initial_row()
        self.build_rule_set()

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

    def set_density(self, density):
        self.density = density

    def set_height(self, height):
        self.height = height

    def step(self):
        row = []
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

    def run(self):
        self.reset()
        for i in range(0, self.height-1):
            row = self.step()
            self.carflow += self.find_carflow(row)
        return self.carflow / self.height

def find_max(x, y, n_samples=100):
    # take n_samples between min(x) and max(x)
    samples = np.linspace(np.amin(x), np.amax(x), num=n_samples)
    # return max found value for sample in interpolate(x, y)
    return np.amax(np.interp(samples, x, y))

def get_cp(xs, sim, r):
    ys = []
    for x in xs:
        sim.set_density(x)

        # compute average of r tries
        all_cf = []
        for _ in range(0, r):
            all_cf.append(sim.run())

        ys.append(np.average(all_cf))

    return find_max(xs, ys)

def correct(i, real, error=0.05):
    if i >= real - error and i <= real + error:
        return True
    return False

if __name__ == '__main__':
    r = 10
    r2 = 10
    sim = CASim()

    from pyics import GUI
    cx = GUI(sim)
    cx.start()

    # print sim.rule
    # sim.run()
    #
    # xs = np.linspace(0.0, 1.0, num=20)
    #
    # ts = np.arange(30, 40)
    # cd = []
    #
    # real = 0.478
    #
    # # for t
    # for t in ts:
    #     sim.set_height(t)
    #     print "T: " + str(sim.height)
    #
    #     all_cp = []
    #
    #     # find critical point r2 times
    #     for i in range(0, r2):
    #         cp = get_cp(xs, sim, r)
    #         print "\t i: " + str(i) + ", cp: " + str(cp)
    #         all_cp.append(cp)
    #
    #     # get percentage of "correct" found cps
    #     o = 100 * (len([cp for cp in all_cp if correct(cp, real)]) / r2)
    #
    #     print "\t %: " + str(o)
    #     cd.append(o)
    #
    # line = np.array([90] * len(ts))
    # #
    # plt.plot(ts, line, color="red")
    # plt.bar(ts, cd)
    # plt.title('Percentage of correct critical point / T')
    # plt.ylabel('Correct critical point %')
    # plt.xlabel('T')
    # axes = plt.gca()
    # axes.set_ylim([40, 100])
    #
    # plt.show()
