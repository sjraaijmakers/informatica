# -*- coding: utf-8 -*-
#   Student:        Steven Raaijmakers
#   Number:         10804242
#   Desc:           Programma tekent figuur op basis van het Lindenmayer systeem
#

from graphics import *
from math import sin, cos, pi

class LS:
    def __init__(self, defstep, defangle):
        self.rules = {}
        self.axiom = 0
        self.defstep = defstep
        self.defangle = defangle

    def setAxiom(self, ax):
        self.axiom = ax

    def addRule(self, letter, word):
        self.rules[letter] = word

    # Replace each letter out of axiom if there exists a rule for it
    def generate(self, n):
        axiom = self.axiom
        for x in range(0, n):
            newax = ""
            for y in axiom:
                if y in self.rules:
                    newax = newax + self.rules[y]
                else:
                    newax = newax + y
            axiom = newax
        return axiom

    def __repr__(self):
        toPrint = "LS(" + str(self.defstep) + ", " + str(self.defangle) + ", " + "\"" +  self.axiom + "\"" + ", " + str(self.rules) + ")"
        return toPrint

class TurtleState:
    def __init__(self, pos, step, angle, width):
        self.pos = pos
        self.step = step
        self.angle = angle
        self.width = width

    # Clone (?)
    def clone(self):
        clone = TurtleState(self.pos, self.step, self.angle, self.width)
        return clone

    def __repr__(self):
        toPrint = str(self.pos) + ", " + str(self.step) + ", " + str(self.angle) + ", " + str(self.width)
        return toPrint

# Stack list, used to draw banches
class Stack:
    def __init__(self):
        self.stack = []

    def push(self, item):
        self.stack.append(item)

    def pop(self):
        self.stack.pop()

# Filter axiom, return letters with their associated paramaters
def parseWord(word, startIndex):
    c = word[startIndex]
    if startIndex < len(word) - 1: # if is not latest letter
        if word[startIndex + 1] == "(":
            for i in range(startIndex + 2, len(word)):
                if word[i] == ")":
                    par = float(word[startIndex + 2:i])
                    return c, par, i + 1
        else:
            par = None
    else:
        par = None
    pastIndex = startIndex + 1
    return c, par, pastIndex

# Draw function
class Turtle:
    def __init__(self, win, defwidth):
        self.window = win
        self.width = defwidth
        self.s = Stack()
        # declare values (so they can be configured)
        self.lsys = None
        self.angle = 0
        self.length = 0
        self.startx = 0
        self.starty = 0

    def stepPenUp(self):
        self.step(False)

    def stepPenDown(self):
        self.step(True)

    def step(self, isPenDown):
        # Calculate end point, based on pythagoras' algorithm
        self.endx = self.startx + (sin(self.angle) * self.length) #sos
        self.endy = self.starty + (cos(self.angle) * self.length) #cas
        # Make new line based on 2 points
        self.p1 = Point(self.startx, self.starty)
        self.p2 = Point(self.endx, self.endy)
        # Update startpoint to old endpoint
        self.startx = self.endx
        self.starty = self.endy
        # Draw line
        if isPenDown == True:
            line = Line(self.p1, self.p2)
            line.setWidth(self.width)
            line.draw(self.window)

    def left(self):
        self.angle = self.angle - self.lsys.defangle

    def right(self):
        self.angle = self.angle + self.lsys.defangle

    def scale(self, scale):
        if scale != None:
            x = scale
        elif scale == None: # thicker line
            x = 2
        elif scale == -1:
            x = 0.5 # smaller line
        self.width = self.width * x
        self.length = self.length * x

    def push(self):
        # Put position in Point
        pos = Point(self.startx, self.starty)
        # Put state into stack
        t = TurtleState(pos, self.length, self.angle, self.width)
        self.s.push(t.clone())

    def pop(self):
        # Return lasts item in stack
        clone = self.s.stack.pop()
        # Put returned values in Turtle
        self.startx = clone.pos.x
        self.starty = clone.pos.y
        self.length = clone.step
        self.angle = clone.angle
        self.width = clone.width

    def drawLS(self, lsys, n, startx, starty, startangle):
        # Default values
        self.lsys = lsys
        self.length = lsys.defstep
        # Start & Draw first line
        self.startx = startx
        self.starty = starty
        self.angle = - startangle
        self.right()
        # Execute associated action for each letter in generated word
        i = 0
        while i < len(lsys.generate(n)):
            c, par, i = parseWord(lsys.generate(n), i)
            if c == "F":
                self.stepPenDown()
            elif c == "f":
                self.stepPenUp()
            elif c == "+":
                self.left()
            elif c == "-":
                self.right()
            elif c == "[":
                self.push()
            elif c == "]":
                self.pop()
            elif c == "\"":
                self.scale(par)
            elif c == "\'":
                self.scale(-1)

# Execution
if __name__=='__main__':
    win = GraphWin('Lindenmayer System', 400, 400)
    win.yUp()

    ls = LS(3,pi/2)
    ls.setAxiom('F-F-F-F')
    ls.addRule('F','F-F+F+FF-F-F+F')

    print ls
    print ls.generate(1)

    t = Turtle(win, 1)
    t.drawLS(ls, 3, 100, 100, pi/2)

    tree = LS(80,pi/2)
    tree.setAxiom('"(1.5)FFA')
    tree.addRule('A', '"(0.687)[+FA][-FA]')

    t2 = Turtle(win, 12)
    t2.drawLS(tree, 10, 200, 30, pi/2)

    win.promptClose(win.getWidth()/2,20)
