# Steven Raaijmakers, 10804242
# Task 2

from Graph import Graph
import networkx as nx

if __name__ == "__main__":
    nx_file = nx.read_gml('Aarnet.gml')
    G = Graph(nx_file)

    # Create mapping: sorted alphabeticly and change to int
    mapping = {}
    for i, t in enumerate(sorted(nx_file.nodes())):
        mapping[t] = i

    # Find shortest path between Adelaide1 and Brisbane1
    path, cost = G.distance("Adelaide1", "Brisbane1")
    print "Shortest path between Adelaide1 & Brisbane1: \n" + str(path) + \
          " (Cost: " + str(cost) + ")"

    print [mapping[k] for k in path]
    G.color_path(path)
    G.change_labels(mapping)
    G.output('Steven-Raaijmakers-shortestpath.png')
