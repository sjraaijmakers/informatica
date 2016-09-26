from __future__ import division
import random

def spel():
    letters = []
    turns = 0

    while True:
        letter = random.choice(['K', 'M'])
        turns += 1

        if len(letters) > 2:
            letters.pop(0)
        letters.append(letter)

        string = ''.join(letters)
        if string == "KKM":
            return (turns, "Anna")
        elif string == "KMM":
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

    print "Avg turns: " + str((sum(games_anna) + sum(games_bob)) / (len(games_anna) + len(games_bob)))
    print "Avg turns from games won by Anna: " + str(sum(games_anna) / len(games_anna))
    print "Avg turns from games won by Bob: " + str(sum(games_bob) / len(games_bob))

penney(1000)
