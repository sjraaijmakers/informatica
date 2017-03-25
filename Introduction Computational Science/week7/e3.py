import matplotlib.pyplot as plt
import sys
import numpy as np
from funcs import run_rnd_simulation, run_euler

if __name__ == '__main__':
    usage = "give args: B"
    if len(sys.argv) != 2:
        print(usage)
    else:
        argument = sys.argv[1]
        if argument == "B":
            xs, ys = run_euler(10**5, 5.0, 0.01, 0.001, t=40)
            plt.plot(xs, ys, label="Case I (euler)")

            xs, ys = run_rnd_simulation(10**5, 5.0, 0.01, 0.001, t=40, repeat=1)
            ys = ys / 10**5
            plt.plot(xs, ys, label="Case I (nx)")

            xs, ys = run_euler(10**5, 0.8, 0.1, 0.001, t=40)
            plt.plot(xs, ys, label="Case II (euler)")

            xs, ys = run_rnd_simulation(10**5, 0.8, 0.1, 0.001, t=40, repeat=1)
            ys = ys / 10**5
            plt.plot(xs, ys, label="Case II (nx)")

            plt.xlabel("t")
            plt.ylabel("percentage of infectd people")
            plt.title("Euler method vs Networkx modules")

            plt.legend()
            plt.show()
            # ys2 = ys2 / 10**5
            #
            # plt.plot(xs2, ys2, label="scalefree network")
            # run_sf_simulation(10**5, 5.0, 2.5, 0.01, 0.001)
