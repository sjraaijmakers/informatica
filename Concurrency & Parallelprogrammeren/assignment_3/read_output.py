# First: read results from experiment 1, to find top 10 hashtags
file_1 = open("results_1", "r+")

results_1 = {}
for line in file_1:
    k = line.split()
    results_1[k[0]] = int(k[1])

top_10 = sorted(results_1, key=results_1.get, reverse=True)[:10]

# Combine results:
file_2 = open("results_2", "r+")

results_2 = {}
for line in file_2:
    k = line.split()
    results_2[k[0]] = {'occurence': k[1], 'mean': k[2], 'std': k[3]}

# print:
for top in top_10:
    print top, results_1[top], results_2[top]['occurence'], results_2[top]['mean'], results_2[top]['std']
