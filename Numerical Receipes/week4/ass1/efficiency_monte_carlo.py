# Vak: Numerical Recipes
# Auteurs: <Naam> <Collegakaart>, <Naam> <Collekaart>
from common import func
from monte_carlo import monte_carlo_integral

if __name__ == "__main__":
    # example
    monte_carlo_integral(func, 0, 1, samples_size=10000)

    # start implemenation
