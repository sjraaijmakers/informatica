/* Wave simulations with OpenMP.
 * Steven Raaijmakers & Marcus van Bergen
 * 10824242 & 10817993
 *
 * Vak: CPP
 * About: simulates a SIN-Wave using OpenMP threads.
 */

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <omp.h>
#include "simulate.h"

const double c = 0.15;

typedef struct {
    int begin_i;
    int end_i;
} i_val;

/* Calc function for finding a segment for i-range in t*/
void calc(int begin_i, int end_i, double *old_array, double *current_array, double *next_array, int I_MAX){
    for(int i = begin_i; i <= end_i; i++){
        /* if i => 0 or the I_max then we set the value to 0 */
        if(i == 0 || i == I_MAX){
            next_array[i] = 0.0;
        }
        /* Function from the pdf */
        else {
            next_array[i] = 2.0 * current_array[i] - old_array[i] + c * (current_array[i-1] - (2.0 * current_array[i] - current_array[i + 1]));
        }
    }
}

/* simulate function */
double *simulate(const int i_max, const int t_max, const int num_threads,
        double *old_array, double *current_array, double *next_array)
{
    int begin_i, end_i;
    int step = i_max / num_threads;

    /* Set num threads equal to input number */
    omp_set_num_threads(num_threads);

    /* setting up the segments for the number of threads */
    i_val *i_values[num_threads];
    for(int i = 0; i < num_threads; i++){
        i_val *kev = malloc(sizeof(i_val));
        begin_i = i * step;
        if(i == num_threads - 1){
            end_i = i_max;
        }
        else {
            end_i = ((i + 1) * step) - 1;
        }
        kev->begin_i = begin_i;
        kev->end_i = end_i;
        i_values[i] = kev;
    }

    /* Time Complexity */
    for(int t = 0; t < t_max; t++){
        /* For each T the old, current (and next) - array are the same */
        #pragma omp parallel for
        for(int i = 0; i < num_threads; i++){
            // printf("%s %i %s %i\n", "thread: ", tid, "i: ", i);
            calc(i_values[i]->begin_i, i_values[i]->end_i, old_array, current_array, next_array, i_max);
        }

        /*Swap arrays
         *Swap arrays after threads have unlocked
        */
        #pragma omp barrier
        #pragma omp single
        {
            double *tmp = old_array;
            old_array = current_array;
            current_array = next_array;
            next_array = tmp;
        }
    }
    return current_array;
}
