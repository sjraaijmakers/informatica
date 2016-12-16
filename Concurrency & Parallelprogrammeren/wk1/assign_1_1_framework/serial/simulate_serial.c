/* Wave simulations in serial
 * Steven Raaijmakers & Marcus van Bergen
 * 10824242 & 10817993
 *
 * Vak: CPP
 * About: simulates a SIN-Wave in serial fashion and one thread
 */
#include <stdio.h>
#include <stdlib.h>

const double c = 0.15;
/* Main simulation function */
double *simulate(const int i_max, const int t_max, const int num_threads,
        double *old_array, double *current_array, double *next_array){

    /* serial calculate the wave for each t */
    for(int t = 0; t < t_max; t++){
        /* Calc points */
        for(int i = 0; i < i_max; i++){
            next_array[i] = (2.0 * current_array[i]) - old_array[i] + (c * (current_array[i - 1] - (2.0 * current_array[i] - current_array[i + 1])));
        }

        /* Swap array */
        double *tmp = old_array;
        old_array = current_array;
        current_array = next_array;
        next_array = tmp;

    }
    return current_array;
}
