# Steven Raaijmakers
# Bereken korste pad in adjencymatrix op 2 manieren: dijksta & naive

import timeit, heapq

start = timeit.default_timer()

# File uitlezen
graph = open("inputgraph", "r").readlines()
for i, t in enumerate(graph):
    graph[i] = map(int, t.split())

# Dijkstra algoritme via:
# http://code.activestate.com/recipes/119466-dijkstras-algorithm-for-shortest-paths/)
# Het dijkstra algoritme heeft een complexiteit van O((|e| + |v|) * log(|v|))
def dijkstra(graph, start, end):
    queue = [(0, start, [])]
    seen = set()
    # do while loop, python style
    while 1:
        # sla som, huidige knoop en het path op uit de heap
        (sum, v, path) = heapq.heappop(queue)
        # als knoop nog niet eerder bezocht is:
        if v not in seen:
            # v wordt toegevoegd aan path & seen
            path = path + [v]
            seen.add(v)
            # als eind knoop bereikt is kan er geturned worden
            if v == end:
                return path, sum
            for i, t in enumerate(graph[v]):
                # waarde groter dan -1 kan worden toegevoegd aan de heap
                if t > 0:
                    heapq.heappush(queue, (sum + t, i, path))

# Locale variabele
class Locals:
    min_path = []
    min_sum = 1000

# Naive:
# Het naive algoritme heeft een complexiteit van O(knopen!). Dit doordat
# bij worst-case telkens verwezen wordt naar elke andere knoop. De knoop waar net
# vandaan gekomen is wordt niet meegerekent wat neerkomt op knopen!
def naive(graph, begin, end):
    sum = 0
    path = [begin]
    short(graph, begin, end, sum, path)
    return Locals.min_path, Locals.min_sum

# Naive algoritme recursieve helper-functie
def short(graph, row_num, end, sum, path):
    # Als "end" in path voorkomt, is het einde bereikt
    if end in path:
        # Kijk dan meteen of de gevonden som lager is dan de huidige min_sum
        if sum < Locals.min_sum:
            Locals.min_sum = sum
            Locals.min_path = path
        return
    # Recursief gedeelte
    row = graph[row_num]
    for i, t in enumerate(row):
        # Kijk of waarde niet -1 is en niet al in het path voorkomt
        if t > 0 and i not in path:
            new_path = list(path) # deepcopy
            new_path.append(i)
            # re-run functie op de zojuist gevonden knoop
            short(graph, i, end, sum + t, new_path)

# OUTPUT

path, sum = naive(graph, 0, 14)
print "Naive:       ", path, sum

# path, sum = dijkstra(graph, 0, 14)
# print "Dijkstra:  ", path, sum

stop = timeit.default_timer()
print stop - start, "sec."
