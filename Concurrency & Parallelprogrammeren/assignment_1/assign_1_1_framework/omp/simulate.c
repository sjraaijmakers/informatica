/* Wave simulations with POSIX Threads.
 * Steven Raaijmakers & Marcus van Bergen
 * 10824242 & 10817993
 *
 * Vak: CPP
 * About: simulates a SIN-Wave using P-threads.
 */

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

#include "simulate.h"

const double c = 0.15;
int I_MAX;

/* Struct with information for each thread */
typedef struct {
    int begin_i;
    int end_i;

    double *old_array;
    double *current_array;
    double *next_array;
} p_args;

/* Calc function for the wave */
void * calc(void *arguments){
    p_args *cargs = (p_args*)(arguments);
    /* Get the regarding vars for the thread */
    for(int i = cargs->begin_i; i <= cargs->end_i; i++){
        /* handling i is 0 or i_max  will be 0 */
        if(i == 0 || i == I_MAX){
            cargs->next_array[i] = 0.0;
        }
        /* Formula from the PDF */
        else {
            cargs->next_array[i] = 2.0 * cargs->current_array[i] -
                                   cargs->old_array[i] + c *
                                   (cargs->current_array[i-1] -
                                   (2.0 * cargs->current_array[i] -
                                    cargs->current_array[i + 1]));
        }
    }

    return NULL;
}

/* Simulate function  */
double *simulate(const int i_max, const int t_max, const int num_threads,
        double *old_array, double *current_array, double *next_array){

    pthread_t threads_ids[num_threads];
    I_MAX = i_max;
    int step = i_max / num_threads;

    p_args* arguments[num_threads];

    for(int i = 0; i < num_threads; i++){
        arguments[i] = malloc(sizeof(p_args));
    }

    // Time complexity
    for(int t = 0; t < t_max; t++){
        // For each T the old, current (and next) - array are the same
        for(int i = 0; i < num_threads; i++){
            arguments[i]->old_array = old_array;
            arguments[i]->current_array = current_array;
            arguments[i]->next_array = next_array;

            arguments[i]->begin_i = i * step;
            if(i == num_threads - 1){
                arguments[i]->end_i = i_max;
            }
            else {
                arguments[i]->end_i = ((i + 1) * step) - 1;
            }
            //Start threads
            pthread_create(&threads_ids[i], NULL, &calc, (void*)arguments[i]);
        }

        // Wait for all threads to be finished
        for (int id = 0; id < num_threads; id++) {
            pthread_join(threads_ids[id], NULL);
        }

        // Swap arrays
        double *tmp = old_array;
        old_array = current_array;
        current_array = next_array;
        next_array = tmp;
    }

    //Free at the end
    for(int i = 0; i < num_threads; i++){
        free(arguments[i]);
    }

    return current_array;
}
