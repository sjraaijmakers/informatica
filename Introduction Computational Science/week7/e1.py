# Steven Raaijmakers, 10804242
# Epidemics 1

from __future__ import division
import matplotlib.pyplot as plt
import sys
from funcs import run_rnd_simulation, get_r0

if __name__ == '__main__':
    usage = "give args: B or H"
    if len(sys.argv) != 2:
        print(usage)
    else:
        argument = sys.argv[1]
        if argument == "B":
            xs, ys = run_rnd_simulation(10**5, 5.0, 0.01, 0.001, t=100, repeat=10)
            ys = ys / 10**5
            plt.plot(xs, ys, label="Case I")

            xs, ys = run_rnd_simulation(10**5, 0.8, 0.1, 0.001, t=100, repeat=10)
            ys = ys / 10**5
            plt.plot(xs, ys, label="Case II")

            # set plot things
            plt.legend()
            plt.title("Prevalance over time")
            plt.suptitle("n=10**5, i_start=0.001, repetition=10")
            plt.xlabel("t")
            plt.ylabel("percentage of infected people")
            plt.show()
        elif argument == "H":
            xs, ys = get_r0(10**5, 5.0, 0.01, 0.001, t=100)
            plt.plot(xs, ys, label="case I")

            xs2, ys2 = get_r0(10**5, 0.8, 0.1, 0.001, t=100)
            plt.plot(xs2, ys2, label="case II")

            plt.legend()
            plt.xlabel("t")
            plt.ylabel("R0")
            plt.title("R0 over time")
            plt.show()
