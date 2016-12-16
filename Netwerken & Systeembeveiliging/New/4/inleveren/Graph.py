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
        self.adjacency_matrix = None

    # Create adjacency from graph
    def set_adjacency_matrix(self):
        adjacency = {}
        list1 = nx.to_dict_of_lists(self.graph)
        for k in list1:
            sub = {}
            for i in list1[k]:
                sub[i] = self.nodes_to_vicenty(k, i)
            adjacency[k] = sub
        self.adjacency_matrix = adjacency

    # Get node info by labelname
    def get_node(self, node_label):
        for key, value in self.graph.nodes_iter(data=True):
            if key == node_label:
                return value

    # distance between two nodes
    def distance(self, source, target):
        self.set_adjacency_matrix()
        return shortest_path(self.adjacency_matrix, source, target)

    # Get distance between two nodes
    def nodes_to_vicenty(self, source, target):
        source = self.get_node(source)
        target = self.get_node(target)

        pos_source = (source["Latitude"], source["Longitude"])
        pos_target = (target["Latitude"], target["Longitude"])

        return vincenty(pos_source, pos_target).kilometers

    # Change node labels to mapping
    def change_labels(self, mapping):
        self.set_adjacency_matrix()
        self.graph = nx.relabel_nodes(self.graph, mapping)

    # Print graph
    def output(self, name):
        graph = toPdot(self.graph)
        graph.write_png(name)

    # Calculate diameter by finding the shortest longest path
    def get_diameter(self):
        longest_cost = 0
        longest_path = []

        for i in self.graph.nodes():
            for j in self.graph.nodes():
                if i != j:
                    path, cost = self.distance(i, j)
                    if cost > longest_cost:
                        longest_cost = cost
                        longest_path = path

        return longest_path, longest_cost

    # Color path within nodes
    def color_path(self, nodes):
        # color nodes
        for node in nodes:
            self.get_node(node).update({"color": "red"})

        for i, current in enumerate(nodes):
            if i == len(nodes) - 1:
                return
            next = nodes[i + 1]
            for edge in self.graph.edge:
                if edge == current:
                    for a in self.graph.edge[edge]:
                        if a == next:
                            self.graph.edge[edge][a].update({"color": "red"})
