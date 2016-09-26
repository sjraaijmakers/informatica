# Vak: Numerical Recipes
# Auteurs: Steven Raaijmakers 10804242, Daan Meijers NUMMER
import numpy as np

# Return X, Y of Prewitt filter
def prewitt():
    Px = np.array([[1, 0, -1], [1, 0, -1], [1, 0, -1]])
    Py = np.array([[1, 1, 1], [0, 0, 0], [-1, -1, -1]])
    return Px, Py

# Returns Laplace filter
def laplace():
    return np.array([[0, 1, 0], [1, -4, 1], [0, 1, 0]])

# Returns X, Y of Sobel filter
def sobel():
    Sx = np.array([[1, 0, -1], [2, 0, -2], [1, 0, -1]])
    Sy = np.array([[1, 2, 1], [0, 0, 0], [-1, -2, -1]])
    return Sx, Sy
