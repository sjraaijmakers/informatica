/*
 * simulate.c
 *
 * Implement your (parallel) simulation here!
 */

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

#include "simulate.h"

const int c = 0.15;

/* Add any global variables you may need. */


/* Add any functions you may need (like a worker) here. */

typedef struct {
    int begin_i;
    int end_i;
    int i_max;

    double *old_array;
    double *current_array;
    double *next_array;
} p_args;


void * calc(void *arguments){

    p_args *cargs = (p_args*)(arguments);

    for(int i = cargs->begin_i; i <= cargs->end_i; i++){
        if(i == 0 || i == cargs->i_max){
            cargs->next_array[i] = 0.0;
        }
        else {
            cargs->next_array[i] = 2.0 * cargs->current_array[i] - cargs->old_array[i] + c * (cargs->old_array[i - 1] - (2.0 * cargs->current_array[i] + cargs->current_array[i + 1]));
        }
    }

    return NULL;
}

/*
 * Executes the entire simulation.
 *
 * Implement your code here.
 *
 * i_max: how many data points are on a single wave
 * t_max: how many iterations the simulation should run
 * num_threads: how many threads to use (excluding the main threads)
 * old_array: array of size i_max filled with data for t-1
 * current_array: array of size i_max filled with data for t
 * next_array: array of size i_max. You should fill this with t+1
 */
double *simulate(const int i_max, const int t_max, const int num_threads,
        double *old_array, double *current_array, double *next_array)
{
    pthread_t threads_ids[num_threads];

    // TODO: !?!
    int step = i_max / num_threads;

    // Arguments TODO: Dit klopt niet
    p_args* arguments[num_threads];

    for(int i = 0; i < num_threads; i++){
        arguments[i] = malloc(sizeof(p_args));
    }

    // Time complexity
    for(int t = 0; t < t_max; t++){
        // For each T the old, current (and next) - array are the same
        for(int i = 0; i < num_threads; i++){
            arguments[i]->i_max = i_max;
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

    for(int i = 0; i < num_threads; i++){
        free(arguments[i]);
    }

    return current_array;
}
