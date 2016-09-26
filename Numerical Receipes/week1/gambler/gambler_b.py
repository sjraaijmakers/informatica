from __future__ import division
import random

def spel(playerA, playerB):
    turns = 0
    while playerA > 0 and playerB > 0:
        flip = random.choice(['H', 'C'])
        if flip == "H":
            playerA -= 1
            playerB += 1
        elif flip == "C":
            playerA += 1
            playerB -= 1
        turns += 1

    if playerA == 0:
        winner = "Bob"
    else:
        winner = "Anna"
    return (turns, winner)

def gambler(playerA, playerB, games):
    playerA_wins = []
    playerB_wins = []

    for i in range(0, games):
        tmp = spel(playerA, playerB)
        if tmp[1] == "Anna":
            playerA_wins.append(tmp[0])
        elif tmp[1] == "Bob":
            playerB_wins.append(tmp[0])

    print "Anna: " + str(len(playerA_wins))
    print "Bob: " + str(len(playerB_wins))

gambler(10, 5, 10000)
