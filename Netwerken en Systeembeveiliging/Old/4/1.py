# Steven Raaijmakers, 10804242

from geopy.distance import vincenty
import networkx as nx
import matplotlib.pyplot as plt


def main():
    # Read file
    G = nx.read_gml('Aarnet.gml')

    # create dictionary (always alphabetic)
    # sort alfabeticly
    labels = {}
    for i, t in enumerate(sorted(G.nodes())):
        labels[t] = i

    # 2

    tmp_place = None

    total_cost = 0

    check = False

    for path in nx.all_simple_paths(G, source="Adelaide1", target="Brisbane1"):
        if check:
            return
        check = True
        for place in path:
            k = 0
            name, info = sorted(G.nodes(data=True))[labels[place]]
            pos = (info["Latitude"], info["Longitude"])
            if tmp_place:
                k = vincenty(tmp_place, pos).kilometers
                print name, pos, k
            else:
                print name, pos
                tmp_place = pos
            total_cost += k

        tmp_place = None
        print "Total cost: " + str(total_cost) + " km. \n"

    # draw
    # nx.draw(G)
    #
    # nx.draw_networkx_labels(G, labels=labels, pos=nx.spring_layout(G))

    # plt.show()

if __name__ == "__main__":
    main()
