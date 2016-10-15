# Steven Raaijmakers, 10804242
# Task 3

from Graph import Graph
import networkx as nx

if __name__ == "__main__":
    # Read file and make Graph Class
    nx_file = nx.read_gml('Aarnet.gml')
    G = Graph(nx_file)

    # Create mapping: sorted alphabeticly and change to int
    mapping = {}
    for i, t in enumerate(sorted(nx_file.nodes())):
        mapping[t] = i

    reverse_mapping = inv_map = {v: k for k, v in mapping.items()}

    # Calculate diameter of Graph (taking path with highest cost)

    diameter_path, diameter_cost = G.get_diameter()
    print "Diameter: " + str(diameter_cost)
    print diameter_path

    # Find all paths with bigger costs than smallest path between 1 and 6
    _, cost = G.distance(reverse_mapping[1], reverse_mapping[6])

    pairs_bigger = []
    # Double for loop!
    for i in G.graph.nodes():
        for j in G.graph.nodes():
            # if i is not itself
            if i != j:
                # calculate distance
                path, tmp = G.distance(i, j)
                # check wheter distance is bigger than previously known
                if tmp > cost:
                    pair = (path[0], path[-1])
                    # if so, append pair and cost
                    pairs_bigger.append((pair, tmp))
    # Print length of all nodes with a bigger patht than (1, 6)
    print len(pairs_bigger)
