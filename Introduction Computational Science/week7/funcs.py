import networkx as nx
import numpy as np

def infect(Graph, percentage):
    to_infect = int(len(Graph.nodes()) * percentage)
    infected_nodes = np.random.choice(Graph.nodes(), to_infect, replace=False)
    return infected_nodes

def time_step(Graph, infected_nodes, i):
    tmp = np.asarray(infected_nodes)

    for node in infected_nodes:
        for neighbor in Graph.neighbors(node):
            if np.random.random() < i and neighbor not in tmp:
                tmp = np.append(tmp, neighbor)
    return tmp

def get_avg_degree(Graph, nodes):
    degrees = 0
    for k in nodes:
        degrees += Graph.degree(k)
    try:
        print(len(nodes))
        return degrees / len(nodes)
    except:
        return 0

def run_rnd_simulation(N, k, i, i_start, t=100, repeat=10):
    xs = np.arange(0, t, 1)
    avg_ys = []
    for _ in range(repeat):
        # create graph
        graph_rnd = nx.fast_gnp_random_graph(N, k/N)
        # do stuff with graph
        infected_nodes = infect(graph_rnd, i_start)

        ys = [N*i_start]
        for x in xs[1:]:
            infected_nodes = time_step(graph_rnd, infected_nodes, i)
            ys.append(len(infected_nodes))
            print(x, len(infected_nodes))
        avg_ys.append(np.asarray(ys))
    return xs, (sum(avg_ys) / repeat)

def run_sf_simulation(N, k, y, i, i_start, t=100, repeat=10):
    xs = np.arange(0, t, 1)
    avg_ys = []
    for _ in range(repeat):
        # create graph
        s = nx.utils.powerlaw_sequence(N, 2.5)
        s = s / np.mean(s) * k
        graph_sf = nx.expected_degree_graph(s, selfloops=False)
        # do stuff with graph
        infected_nodes = infect(graph_sf, i_start)

        ys = [N*i_start]
        for x in xs[1:]:
            infected_nodes = time_step(graph_sf, infected_nodes, i)
            ys.append(len(infected_nodes))
            print(x, len(infected_nodes))
        avg_ys.append(np.asarray(ys))
    return xs, (sum(avg_ys) / repeat)

def run_sf_simulation_2(N, k, y, i, i_start, t=100, repeat=10):
    xs = np.arange(0, t, 1)
    avg_ys = []
    for _ in range(repeat):
        # create graph
        s = nx.utils.powerlaw_sequence(N, 2.5)
        s = s / np.mean(s) * k
        graph_sf = nx.expected_degree_graph(s, selfloops=False)
        # do stuff with graph
        infected_nodes = infect(graph_sf, i_start)

        ys = [get_avg_degree(graph_sf, infected_nodes)]
        for x in xs[1:]:
            infected_nodes_2 = time_step(graph_sf, infected_nodes, i)
            new = np.setdiff1d(infected_nodes_2, infected_nodes)
            avgd = get_avg_degree(graph_sf, new)
            ys.append(avgd)
            print(x, avgd)
        avg_ys.append(np.asarray(ys))
    return xs, (sum(avg_ys) / repeat)

def get_r0(N, k, i, i_start, t=100):
    graph_r0 = nx.fast_gnp_random_graph(N, k/N)
    infected_nodes = infect(graph_r0, i_start)

    xs = np.arange(0, t, 1)
    infections = [N*i_start]
    r0s = [0]
    for x in xs[1:]:
        infected_nodes = time_step(graph_r0, infected_nodes, i)
        infections.append(len(infected_nodes))
        r0s.append((infections[-1] - infections[-2]) / infections[-2])
        infections.pop(0)
        print(x, r0s[-1])
    return xs, np.asarray(r0s)

def euler(I, i, k, N):
    S = N - I
    p = k / N
    b = 1 - (1 - i)**p
    tmp = (1 - (1 - b)** I) * S
    return (I + tmp)

def run_euler(N, k, i, i_start, t=100):
    xs = np.arange(0, t, 1)
    ys = []
    I = N*i_start
    for x in xs:
        print(x, I)
        ys.append(I)
        I = euler(I,  i, k, N)
    ys = np.asarray(ys) / N
    return xs, ys
