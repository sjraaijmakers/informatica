# Steven Raaijmakers, 10804242

from geopy.distance import vincenty
import networkx as nx
import pydot
import heapq

toPdot = nx.drawing.nx_pydot.to_pydot

# Shortest path implemented by: http://code.activestate.com/recipes/119466-dijkstras-algorithm-for-shortest-paths/

def shortest_path(G, start, end):
    def flatten(L):       # Flatten linked list of form [0,[1,[2,[]]]]
        while len(L) > 0:
            yield L[0]
            L = L[1]

    q = [(0, start, ())]  # Heap of (cost, path_head, path_rest).
    visited = set()       # Visited vertices.
    while True:
        (cost, v1, path) = heapq.heappop(q)
        if v1 not in visited:
            visited.add(v1)
            if v1 == end:
                return (list(flatten(path))[::-1] + [v1], cost)
            path = (v1, path)
            for (v2, cost2) in G[v1].iteritems():
                if v2 not in visited:
                    heapq.heappush(q, (cost + cost2, v2, path))


# Graph Class
class Graph:
    # Init
    def __init__(self, graph):
        self.graph = graph
        self.adjency_matrix = None

    # Get node info by labelname
    def get_node(self, node_label):
        return self.graph.nodes(data=True)[node_label]

    # distance between two nodes
    def distance(self, source, target):
        return shortest_path(G.adjency_matrix, source, target)

    # Get distance between two nodes
    def nodes_to_vicenty(self, source, target):
        _, source = self.get_node(source)
        _, target = self.get_node(target)

        pos_source = (source["Latitude"], source["Longitude"])
        pos_target = (target["Latitude"], target["Longitude"])

        return vincenty(pos_source, pos_target).kilometers

    def set_adjency_matrix(self):
        adjency = {}

        list1 = nx.to_dict_of_lists(self.graph)
        for k in list1:
            sub = {}
            for i in list1[k]:
                sub[i] = self.nodes_to_vicenty(k, i)
            adjency[k] = sub

        self.adjency_matrix = adjency

        return adjency

    def change_labels(self, mapping):
        self.graph = nx.relabel_nodes(self.graph, mapping)

    def shortest(self, source, target):
        return shortest_path(G.adjency_matrix, source, target)

    def output(self, name):
        graph = toPdot(self.graph)

        graph.write_png(name)

    def get_diameter(self):
        self.set_adjency_matrix()
        longest_cost = 0
        longest_path = []

        for i in self.graph.nodes():
            for j in self.graph.nodes():
                if i != j:
                    path, cost = self.shortest(i, j)
                    if cost > longest_cost:
                        longest_cost = cost
                        longest_path = path

        return longest_path, longest_cost

    def color_path(self, list2):
        # color nodes
        for node in list2:
            self.graph.node[node].update({"color": "red"})

        # color edges
        for i in range(0, len(list2) - 1):
            for edge in self.graph.edge:
                city = list2[i]
                next = list2[i+1]

                if edge == city:
                    for dic in self.graph.edge[edge]:
                        if dic == next:
                            self.graph.edge[edge][dic].update({"color": "red"})

if __name__ == "__main__":
    # 1
    # Read file and make Graph Class
    nx_file = nx.read_gml('Aarnet.gml')
    G = Graph(nx_file)

    # Create mapping: sorted alphabeticly and change to int
    mapping = {}
    for i, t in enumerate(sorted(nx_file.nodes())):
        mapping[t] = i

    # Reverse map
    reverse_mapping = inv_map = {v: k for k, v in mapping.items()}

    G.change_labels(mapping)

    # Output graph
    G.output('Steven-Raaijmakers-transform.png')

    # 2

    # Set adjency matrix
    G.set_adjency_matrix()

    # Find shortest path between Adelaide1 and Brisbane1
    path, cost = G.shortest(mapping["Adelaide1"], mapping["Brisbane1"])
    print "Shortest path between Adelaide1 & Brisbane1: \n" + str(path) + \
          " (Cost: " + str(cost) + ")"

    # Print node names
    print [reverse_mapping[k] for k in path]

    print " "

    G.color_path(path)
    G.output('Steven-Raaijmakers-shortestpath.png')

    #3
    # Calculate diameter of Graph (taking path with highest cost)

    diameter_path, diameter_cost = G.get_diameter()
    print "Diameter: " + str(diameter_cost)
    print diameter_path
    print [reverse_mapping[k] for k in diameter_path]

    # Find all paths with bigger costs than smallest path between 1 and 6
    _, cost = G.distance(1, 6)

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

    #4
    print ""
    G.graph.remove_edge(12, 17)
    G.output('test.png')
    diameter_path, diameter_cost = G.get_diameter()
    print "Diameter: " + str(diameter_cost)
    print diameter_path
