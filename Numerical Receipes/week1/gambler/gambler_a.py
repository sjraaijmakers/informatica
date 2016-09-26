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

print spel(10, 5)
