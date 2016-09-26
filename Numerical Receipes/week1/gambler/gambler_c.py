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

    avg_turns = (sum(playerA_wins) + sum(playerB_wins)) / games
    perc_anna = len(playerA_wins) / games * 100

    # stats
    print "Average turns: " + str(avg_turns)
    print "Games won by Anna (%): " + str(perc_anna)
    print "Average turns of games won by Anna: " + str(sum(playerA_wins) / len(playerA_wins))
    print "Average turns of games won by Bob: " + str(sum(playerB_wins) / len(playerB_wins))

gambler(10, 5, 100000)
