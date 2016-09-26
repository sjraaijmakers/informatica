import numpy as np

def import_data(file, headers):
    raw_data = np.array(np.recfromcsv(file, delimiter=","))
    data = np.delete(raw_data, range(0,headers))
    return data

def process_data(data):
    weights = {
        "M": [],
        "F": []
    }
    heights = {
        "M": [],
        "F": []
    }
    feet = {
        "M": [],
        "F": []
    }
    classifiers = {
        "M": [],
        "F": []
    }
    for entry in data:
        entry[0] = entry[0][0]
        weights[entry[0]].append(float(entry[1]))
        heights[entry[0]].append(float(entry[2]))
        feet   [entry[0]].append(float(entry[3]))
    for gender in ["M", "F"]:
        classifiers[gender].append(np.mean(weights[gender]))
        classifiers[gender].append(np.mean(heights[gender]))
        classifiers[gender].append(np.mean(feet   [gender]))
        classifiers[gender].append(np. var(weights[gender]))
        classifiers[gender].append(np. var(heights[gender]))
        classifiers[gender].append(np. var(feet   [gender]))
    return classifiers

def naive_bayes(classifiers, test, verbose=False):
    probabilities = {
        "M": 0.5,
        "F": 0.5
    }
    for gender in ["M", "F"]:
        cls = classifiers[gender]
        for i in (0, 1, 2):
            mean = cls[i]
            var  = cls[i + 3]
            term1 = 1 / (np.sqrt(2 * np.pi * var))
            term2 = (-1 * pow(test[i] - mean, 2)) / (2 * var)
            prob = term1 * pow(np.e, term2)
            probabilities[gender] *= prob
    evidence = probabilities["M"] + probabilities["F"]
    for gender in ["M", "F"]:
        probabilities[gender] /= evidence / 100
    gender = "male" if probabilities["M"] > probabilities["F"] else "female"
    if verbose:
        print("Naive Bayes classification has determined that this person is a " +
              str(gender) + " with a certainty of " +
              str((max(probabilities["M"], probabilities["F"]))) + "%% ")
    return "M" if gender == "male" else "F"

def test(classifiers, test_data):
    results = {
        "FF": 0,
        "FM": 0,
        "MM": 0,
        "MF": 0
    }
    for test in test_data:
        test = test.tolist()
        real = test[0]
        test = tuple(x for x in test if test.index(x) > 0)
        found = naive_bayes(classifiers, test)
        results[real + found] += 1
    return results

if __name__ == '__main__':
    data = import_data("biometrie2014.csv", 4)
    test_data = import_data("test.csv", 0)
    classifiers = process_data(data)
    print(test(classifiers, test_data))
