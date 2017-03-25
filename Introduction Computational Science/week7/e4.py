import matplotlib.pyplot as plt
import sys
import numpy as np
from funcs import run_sf_simulation, run_rnd_simulation, run_sf_simulation_2

if __name__ == '__main__':
    usage = "give args: A"
    if len(sys.argv) != 2:
        print(usage)
    else:
        argument = sys.argv[1]
        if argument == "A":
            xs, ys = run_sf_simulation(10**5, 5, 2.5, 0.01, 0.001, t=30, repeat=5)
            ys = ys / 10**5
            plt.plot(xs, ys, label="Case I (sf)")

            xs, ys = run_rnd_simulation(10**5, 5, 0.01, 0.001, t=30, repeat=5)
            ys = ys / 10**5
            plt.plot(xs, ys, label="Case I (rnd)")

            plt.title("Sample free vs random")
            plt.legend()
            plt.xlabel("t")
            plt.ylabel("prevalence")
            plt.show()
        if argument == "C":
            xs, ys = run_sf_simulation_2(10**5, 5, 2.5, 0.01, 0.001, t=50, repeat=5)
            plt.plot(xs, ys, label="Case I")
            plt.title("Avg degree of new infected nodes")
            plt.xlabel("t")
            plt.ylabel("degree")
            plt.show()
