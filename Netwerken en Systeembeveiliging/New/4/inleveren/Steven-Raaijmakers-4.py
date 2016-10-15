# Steven Raaijmakers, 10804242
# Task 4

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

    # Create reversed map
    reverse_mapping = inv_map = {v: k for k, v in mapping.items()}

    # Remove edge from 12 to 17
    G.graph.remove_edge(reverse_mapping[12], reverse_mapping[17])

    # Print new graph
    G.change_labels(mapping)
    # G.output('Steven-Raaijmakers-4.png')

    diameter_path, diameter_cost = G.get_diameter()
    print "Diameter: " + str(diameter_cost)
    print diameter_path
