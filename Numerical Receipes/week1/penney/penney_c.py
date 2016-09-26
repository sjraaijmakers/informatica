from __future__ import division
import random, itertools, copy

# C
def spel(anna, bob):
    anna = ''.join(anna)
    bob = ''.join(bob)
    letters = []

    while True:
        letter = random.choice(['K', 'M'])
        if len(letters) > 2:
            letters.pop(0)
        letters.append(letter)
        string = ''.join(letters)
        if string == anna:
            return "Anna"
        elif string == bob:
            return "Bob"

def helper(anna, bob, games):
    anna_wins = 0
    bob_wins = 0

    for i in range(0, games):
        tmp = spel(anna, bob)
        if tmp == "Anna":
            anna_wins = anna_wins + 1
        if tmp == "Bob":
            bob_wins = bob_wins + 1

    return bob_wins / games

def penney(games):
    # all perms
    anna = list(itertools.product("KM", repeat = 3))
    bob = list(itertools.product("KM", repeat = 3))

    for a in anna:
        best = (0, 0)
        print ''.join(a) + " >",
        for b in bob:
            if a != b:
                tmp = helper(a, b, games)
                if tmp > best[0]:
                    best = (tmp, b)
        print ''.join(best[1]) + " with " + str(best[0]*100)

penney(10000)
