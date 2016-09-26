from __future__ import division
import numpy as np
import random
import matplotlib.pyplot as plt

def forward_convolute(data):
    a = 0.5
    new_data = np.zeros(len(data))
    new_data[0] = sum([data[x] for x in range(10)]) / 10

    for i in range(1, len(new_data)):
        new_data[i] = a * data[i] + (1-a)*new_data[i-1]

    return new_data

def backward_convolute(data):
    a = 0.5
    new_data = np.zeros(len(data))
    new_data[len(data)-1] = sum([data[len(data) - 1 - x] for x in range(10)]) / 10

    for i in range(1, len(new_data)):
        new_data[len(new_data) -i - 1] = a * data[len(data)-i-1] + (1-a)* new_data[len(data) - i]

    return new_data

if __name__ == "__main__":
    random.seed()
    xs = np.linspace(0, np.pi, 100)
    ys = [np.sin(x+random.uniform(-0.2, 0.2)) for x in xs]
    for_conv = forward_convolute(ys)
    back_conv = backward_convolute(ys)
    conv = [(i + j) / 2 for i, j in zip(back_conv, for_conv)]
                
    fig = plt.figure(figsize = (12, 12))

    plot1 = plt.subplot(3, 1, 1)
    plt.plot(xs, ys, "x", label = "gemeten")
    plt.plot(xs, for_conv, label = "gefilterd")
    plot2 = plt.subplot(3, 1, 2)
    plt.plot(xs, ys, "x", label="gemeten")
    plt.plot(xs, back_conv, label="gefilterd")
    plot3 = plt.subplot(3, 1, 3) 
    plt.plot(xs, ys, "x", label = "gemeten")
    plt.plot(xs, conv, label = "gefilterd")
    
    plot1.set_title("Voorwaartse convolutie")
    plot2.set_title("Achterwaartse convolutie")
    plot3.set_title("Verschil convolutie")
    
    plot1.legend()
    plot2.legend()
    plot3.legend() 
    
    plt.xlabel("x")
    plt.ylabel("sinus(x)")
    plt.show()
