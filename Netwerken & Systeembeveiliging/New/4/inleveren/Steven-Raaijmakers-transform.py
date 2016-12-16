# Steven Raaijmakers, 10804242
# Task 1

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

    # Reverse map
    reverse_mapping = {v: k for k, v in mapping.items()}

    G.change_labels(mapping)

    # Output graph
    # G.output('Steven-Raaijmakers-transform.png')
