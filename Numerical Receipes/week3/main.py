#!env/bin/python

from functools import partial
import numpy as np

from integration import riemann_sum, trapezoidal_rule, simpson_rule
from monte_carlo import monte_carlo, fast_monte_carlo, uniform_integration

functions = [
        ('f(x) = 4 / (x ^ 2 + 1)', lambda x: 4 / (x ** 2 + 1), 0, 1),
        ('f(x) = sin(x)', np.sin, 0, np.pi),
    ]

integration_methods = [
        ('left riemann sum', riemann_sum),
        ('right riemann sum', partial(riemann_sum, method='right')),
        ('middle riemann sum', partial(riemann_sum, method='middle')),
        ('trapezoidal rule', trapezoidal_rule),
        ('simpson rule', simpson_rule),
    ]

monte_carlo_methods = [
        ('monte carlo', monte_carlo),
        ('fast monte carlo', fast_monte_carlo),
        ('uniform', uniform_integration),
    ]

def main():
    print('numerical integration:')

    for n in [10, 100, 1000, 10000]:
        print('\tprecision: {}'.format(1.0 / n))

        for display, f, x_min, x_max, in functions:
            print('\t\t{}:'.format(display))

            for name, solver in integration_methods:
                print('\t\t\t{}: {}'.format(name, solver(f, x_min, x_max, n)))

    for n in [10, 100, 1000, 10000, 100000]:
        print('\tprecision: {}'.format(1.0 / n))

        display, f, x_min, x_max = functions[0]

        for name, solver in monte_carlo_methods:
            print('\t\t\t{}: {}'.format(name, solver(f, x_min, x_max, n)))

if __name__ == '__main__':
    main()
