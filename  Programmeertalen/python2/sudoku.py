# -*- coding: utf-8 -*-
#   Student:        Steven Raaijmakers
#   Number:         10804242
#   Source:         see.stanford.edu/materials/icspacs106b/H19-RecBacktrackExamples.pdf

from math import sqrt
import numpy as np

# Sudoku Klasse
class Sudoku():
    # Instantiatie variabelen:
    def __init__(self, file):
        self.file = file
        self.field = np.loadtxt(str(self.file), int)
        self.length = len(self.field)
        self.halflength = int(sqrt(self.length))

    # Controleer sudoku op leegheid
    def isEmpty(self):
        for x in range(self.length): #rows
            for y in range(self.length): #cols
                if self.field[x][y] == 0:
                    return True
        return False

    def sudokuIsValid(self):
        if len(self.field) == len(self.field[0]):
            return True
        return False

    # Controleer of opgegeven nummer in opgegeven rij, kolom én blok voorkomt
    def numberIsValid(self, row, col, num):
        for i in range(self.length):
            # Check rij
            if self.field[row][i] == num:
                return False
            # Check kolom
            if self.field[i][col] == num:
                return False
        # Check blok (0 t/m helft + rij - (rest rij / (0,5 * rij))
        for x in range(self.halflength):
            for y in range(self.halflength):
                if self.field[x + row - row % self.halflength][y + col - col % self.halflength] == num:
                    return False
        return True

    # Eerst voorkomende nul in sudoku
    def firstZero(self):
        for row in range(self.length):
            for col in range(self.length):
                if self.field[row][col] == 0:
                    return row, col
        return None

    # Deepcopy
    def copy(self):
        copy = Sudoku(self.file)
        return copy

    # Print
    def __repr__(self):
        return "\n\"" + self.file + "\" \nUnsolved: \n" + str(self.field) + "\n \nSolved: \n" + str(solve(self).field) + "\n"


# Solve (copy + solve + return solved sudoku)
def solve(sudoku):
    toSolve = sudoku.copy()
    if toSolve.isEmpty() == True and toSolve.sudokuIsValid() == True:
        solveSudoku(toSolve)
        return solvedSudoku(toSolve)
    else:
        return toSolve

# Solve sudoku
def solveSudoku(sudoku):
    # Check sudoku op nullen
    if sudoku.firstZero() == None:
        solvedSudoku(sudoku)
        return True
    else:
        row, col = sudoku.firstZero()

        for num in range(1, sudoku.length + 1):
            # Eerst voorkomende valide nummer plaatsen
            if sudoku.numberIsValid(row, col, num) == True:
                sudoku.field[row][col] = num
                # functie recursief
                if solveSudoku(sudoku) == True:
                    return True
                else:
                    sudoku.field[row][col] = 0
        # eerder toegewezen nummer aanpassen
        return False

# Nette manier om solved te returnen
def solvedSudoku(sudoku):
    return sudoku

# Uitvoer:
if __name__=='__main__':
    print Sudoku('Grids/21_open_spots_9_grid.txt')
