import numpy as np
import random
import scipy.stats as stats
import math

def import_data(file):
    data = []
    f = open(file, 'r')
    for line in f:
        data.append(float(line))
    f.close()
    return data

def take_sample(data, size):
    data_length = len(data)
    res = []
    for _ in range(0, size):
        index = random.randint(0, data_length - 1)
        res.append(data[index])
    return res

def confidence_interval(sample, confidence):
    size = len(sample)
    std = np.std(sample)
    mean = np.mean(sample)
    a = 1 - confidence
    c = stats.norm.ppf(1 - a / 2)
    delta = c * (std / math.sqrt(size))
    bounds = [mean - delta, mean + delta]
    return bounds

def test(data, confidence, repeat=100):
    actual_mean = np.mean(data)
    successes = 0
    intervals = []
    for _ in range(0, repeat):
        sample = take_sample(data, 50)
        interval = confidence_interval(sample, 0.95)
        if actual_mean >= interval[0] and actual_mean <= interval[1]:
            successes += 1

    return successes / float(repeat)

if __name__ == '__main__':
    data = import_data("tijden-medium.log")
    confidence = test(data, 0.95, 100)
    print confidence
