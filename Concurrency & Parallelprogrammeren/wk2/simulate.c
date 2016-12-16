
/*
 * simulate.c
 *
 * Implement your (parallel) simulation here!
 */

#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

#include "simulate.h"

#define SIM_TAG 0


/* Add any global variables you may need. */
double C = 0.2;


/* Add any functions you may need (like a worker) here. */


/*
 * Executes the entire simulation.
 *
 * Implement your code here.
 *
 * i_max: how many data points are on a single wave
 * t_max: how many iterations the simulation should run
 * old_array: array of size i_max filled with data for t-1
 * current_array: array of size i_max filled with data for t
 * next_array: array of size i_max. You should fill this with t+1
 */
double *simulate(const int i_max, const int t_max)
{
    int my_rank, processes;
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &processes);

    MPI_Status s;
    int size;

    // first receive the array size
    MPI_Recv(&size, 1, MPI_INT, 0, SIM_TAG, MPI_COMM_WORLD, &s);

    double *old     = (double *) malloc(sizeof(double) * size),
           *current = (double *) malloc(sizeof(double) * size),
           *new     = (double *) malloc(sizeof(double) * size);

    // now receive the old and current arrays
    MPI_Recv(old, size, MPI_DOUBLE, 0, SIM_TAG, MPI_COMM_WORLD, &s);
    MPI_Recv(current, size, MPI_DOUBLE, 0, SIM_TAG, MPI_COMM_WORLD, &s);

    // send the edge values to the neighbours
    MPI_Send(&current[1], 1, MPI_DOUBLE, my_rank - 1, SIM_TAG, MPI_COMM_WORLD);
    MPI_Send(&current[i_max], 1, MPI_DOUBLE, my_rank + 1, SIM_TAG, MPI_COMM_WORLD);

    for (int t = 0; t < t_max; ++t) {
        for (int i = 1; i < i_max - 2; ++i) {
            new[i] = 2 * current[i] - old[i] +
                C * (current[i-1] - (2 * current[i] - current[i + 1]));
        }

        if (my_rank > 2) {
            printf("%d sending to %d\n", my_rank, my_rank - 1);
            MPI_Recv(&current[0], 1, MPI_DOUBLE, my_rank - 1, SIM_TAG,
                     MPI_COMM_WORLD, MPI_STATUS_IGNORE);

            new[1] = 2 * current[1] - old[1] +
                C * (current[0] - (2 * current[1] - current[2]));

            MPI_Send(&new[1], 1, MPI_DOUBLE, my_rank - 1, SIM_TAG, MPI_COMM_WORLD);
        }

        else if (my_rank < processes - 1) {
            printf("%d sending to %d\n", my_rank, my_rank + 1);
            MPI_Recv(&current[i_max - 1], 1, MPI_DOUBLE, my_rank + 1, SIM_TAG,
                     MPI_COMM_WORLD, MPI_STATUS_IGNORE);

            new[i_max - 2] = 2 * current[i_max - 2] - old[i_max - 2] +
                C * (current[i_max - 3] - (2 * current[i_max - 2] - current[i_max - 1]));

            MPI_Send(&new[i_max - 2], 1, MPI_DOUBLE, my_rank + 1, SIM_TAG, MPI_COMM_WORLD);
        }

        old = current;
        current = new;
        new = old;
    }


    /* You should return a pointer to the array with the final results. */
    free(old);
    free(new);
    return current;
}
