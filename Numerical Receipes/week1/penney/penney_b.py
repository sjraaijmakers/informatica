from __future__ import division
import random

# B
def spel():
    state = 0
    automaton = [{'K': 1, 'M': 4}, {'K': 2, 'M': 4}, {'K': 2, 'M': 3}, {}, {'K': 5, 'M': 4}, {'K': 6, 'M': 4}, {}]

    turns = 0

    while True:
        letter = random.choice(['K', 'M'])
        turns += 1

        state = automaton[state][letter]
        if state == 3:
            return (turns, "Anna")
        elif state == 6:
            return (turns, "Bob")

def penney(games):
    games_anna = []
    games_bob = []

    for i in range(0, games):
        tmp = spel()
        if tmp[1] == "Anna":
            games_anna.append(tmp[0])
        elif tmp[1] == "Bob":
            games_bob.append(tmp[0])

    tot_games = len(games_anna) + len(games_bob)

    print "Percentage of games won by Bob: " + str((len(games_bob) / tot_games) * 100)

penney(10000)
