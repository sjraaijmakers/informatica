# Daan Meijers 10727167 & Steven Raaijmakers 10804242

# This code shows the plots from the report

from ca import CASim
import sys
import numpy as np
import matplotlib.pyplot as plt
from scipy.interpolate import spline

if __name__ == '__main__':
    usage = "args should be: infected, probability or prevention"
    if len(sys.argv) != 2:
        print usage
    else:
        argument = sys.argv[1]
        if argument not in ["infected", "probability", "prevention"]:
            print usage
            sys.exit()

        # init dict
        parameters = {
            "humans": 0,
            "mosquitos": 0,
            "m_infected": 0,
            "has_mosquito_net": 0,
            "p_mosquito_human": 0,
            "p_human_mosquito": 0,
        }
        sim = CASim()
        x = np.arange(0, 300, 10)

        parameters["height"] = 100
        parameters["width"] = 100
        parameters["humans"] = 0.7
        parameters["mosquitos"] = 0.7
        parameters["m_infected"] = 0.6
        parameters["p_mosquito_human"] = 0.3
        parameters["p_human_mosquito"] = 0.9
        parameters["has_mosquito_net"] = 0.0
        sim.set_params(parameters)

        # normal run
        v1_h, v1_s = sim.run()
        v1_h = np.delete(v1_h, 0)
        v1_s = np.delete(v1_s, 0)
        v1_p = v1_s / (v1_h + v1_s) * 100

        # smoother
        xnew = np.linspace(x.min(), x.max(), 50)
        h1 = spline(x, v1_p, xnew)

        if argument == 'infected':
            parameters["m_infected"] = 0.2
            sim.set_params(parameters)

            v2_h, v2_s = sim.run()
            label_1 = "Mosquitos infected 60%"
            label_2 = "Mosquitos infected 20%"
        elif argument == 'probability':
            parameters["p_mosquito_human"] = 0.0
            sim.set_params(parameters)

            v2_h, v2_s = sim.run()
            label_1 = "Mosquito infected by human 30%"
            label_2 = "Mosquito infected by human 0%"
        elif argument == 'prevention':
            parameters["has_mosquito_net"] = 0.5
            sim.set_params(parameters)

            v2_h, v2_s = sim.run()
            label_1 = "Has net 0%"
            label_2 = "Has net 50%"

        v2_h = np.delete(v2_h, 0)
        v2_s = np.delete(v2_s, 0)
        v2_p = v2_s / (v2_h + v2_s) * 100
        h2 = spline(x, v2_p, xnew)

        plt.plot(xnew, h1, label=label_1, color="orange")
        plt.plot(xnew, h2, label=label_2, color="purple")
        # plt.plot(xnew, h2, label="prevention 50%", color="orange")
        axes = plt.gca()
        axes.set_ylim([0,100])
        plt.title('Prevalence of malaria among humans')
        plt.ylabel('Percentage of people with malaria')
        plt.xlabel('Days')
        plt.legend()
        plt.show()
