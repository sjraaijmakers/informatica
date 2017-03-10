import random
import numpy as np

MAX_AGE = 65

# Class to contain all information about humans
class Humans():
    # ttl = countdown after infected
    state, ttl, age = None, None, None
    width, height = 0, 0
    death, births = 0, 0

    def __init__(self, width, height):
        self.width = width
        self.height = height
        self.state = np.zeros((width, height))
        self.ttl = np.zeros((width, height))
        self.age = np.zeros((width, height))

    def printClass(self):
        print "State:\n" + str(self.state)
        print "TTL:\n" + str(self.ttl)

    # Sets the cell to the base values
    def die(self, x, y):
        # Empties a cell to "kill" it
        self.state[x, y] = 0
        self.ttl[x, y] = 0
        self.age[x, y] = 0

        # Selects a random empty cell to place a human
        xnew, ynew = np.where(self.state == 0)
        i = random.randint(0, xnew.size-1)
        self.birth(xnew[i], ynew[i])

    # Places a new human on the grid
    def birth(self, x, y):
        self.state[x, y] = 1
        self.ttl[x, y] = 14
        if random.random() < 0.4:
            self.age[x, y] = random.randint(0, 365 * 15)
        else:
            self.age[x, y] = random.randint(365 * 15, 365 * MAX_AGE)

    # Fill the grid with humans in state 1
    def build(self, humans):
        for x in range(self.width):
            for y in range(self.height):
                if random.random() < humans:
                    self.state[x, y] = 1
                    self.ttl[x, y] = 14

                    if random.random() < 1.0:
                        self.age[x, y] = random.randint(0, 365 * 15)
                    else:
                        self.age[x, y] = random.randint(365 * 15, 365 * MAX_AGE)

    def update(self):
        for x in range(self.width):
            for y in range(self.height):
                if self.state[x, y] > 0:
                    self.age[x, y] -= 1

                    # If infected the incubation timer counts down
                    if self.state[x, y] == 2:
                        self.ttl[x, y] -= 1

                    # Incubation period of malaria is over
                    if self.ttl[x, y] <= 0:
                        # Chance on surviving the disease
                        if random.random() < 0.01:
                            self.ttl[x, y] = 14
                            self.state[x, y] = 3
                        # Human dies of malaria
                        else:
                            self.die(x, y)

                    # Human dies of old age
                    if self.age[x, y] <= 0 and self.state[x, y] > 0:
                        self.die(x, y)

class Mosquito():
    # Locatie
    coordinate = []
    # Honger-timer
    hunger = 0
    # Infected boolean
    infected = 0
    width = 0
    height = 0

    def __init__(self, width, height, infected):
        self.width = width
        self.height = height
        self.infected = infected
        x = random.randint(0, width-1)
        y = random.randint(0, height-1)
        self.coordinate = (x, y)
        self.hunger = random.randint(0, 7)

    def walk(self):
        if self.hunger > 0:
            self.hunger -= 1
        x, y = self.coordinate
        while(True):
            nx = x + random.randint(-1, 1)
            ny = y + random.randint(-1, 1)
            if nx >= 0 and ny >= 0 and nx < self.width and ny < self.height:
                self.coordinate = [nx, ny]
                return
