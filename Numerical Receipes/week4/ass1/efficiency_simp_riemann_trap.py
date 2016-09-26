# Vak: Numerical Recipes
# Auteurs: <Naam> <Collegakaart>, <Naam> <Collekaart>
from common import func
from simpson_rule import simpson_rule
from riemannsom import riemannsom
from trapezoidal_rule import trapezoidal_rule

if __name__ == "__main__":
    # start implemenation
    # example function calls.
    riemannsom(func, 0, 1, 100)
    simpson_rule(func, 0, 1, 100)
    trapezoidal_rule(func, 0, 1, 100)
