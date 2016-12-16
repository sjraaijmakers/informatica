/*
 * assign1_1.c
 *
 * Contains code for setting up and finishing the simulation.
 * NOTE: YOU SHOULD IMPLEMENT NOT HAVE TO LOOK HERE, IMPLEMENT YOUR CODE IN
 *       simulate.c.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "file.h"
#include "timer.h"
#include "simulate.h"
#include <iostream>

double c = 0.15;

using namespace std;

static void checkCudaCall(cudaError_t result) {
    if (result != cudaSuccess) {
        cerr << "cuda error: " << cudaGetErrorString(result) << endl;
        exit(1);
    }
}

__global__ void calc(int step, int max, double *old, double *cur, double *next){
    unsigned index = blockIdx.x * blockDim.x + threadIdx.x;

    int i_min = step * index;
    int i_max = ((index + 1) * step) - 1;

    // TODO: laatste moet I_MAX == i_max hebben

    for(int i = i_min; i < i_max; i++){
        next[i] = 2.0 * current[i] - old[i] + c * (cur[i - 1] - (2.0 * cur[i] - cur[i + 1]));
    }
}

double *simulate(const int i_max, const int t_max, const int num_threads,
        double *old, double *cur, double *next){

    double *old_c, *cur_c, *next_c;

    // allocate
    checkCudaCall(cudaMalloc(&old_c, i_max * sizeof(double)));
    checkCudaCall(cudaMalloc(&cur_c, i_max * sizeof(double)));
    checkCudaCall(cudaMalloc(&new_c, i_max * sizeof(double)));

    // fill
    // copy data to the vectors
    checkCudaCall(cudaMemcpy(old_c, old, i_max * sizeof(double), cudaMemcpyHostToDevice));
    checkCudaCall(cudaMemcpy(cur_c, cur, i_max * sizeof(double), cudaMemcpyHostToDevice));
    checkCudaCall(cudaMemcpy(next, next, i_max * sizeof(double), cudaMemcpyHostToDevice));

    int step = i_max / num_threads;

    for(int t = 0; t < t_max; t++){
        calc<<<step, num_threads>>>(step, i_max - 1, old, cur, next);

        double *tmp = old;
        old = cur;
        cur = next;
        next = tmp;
    }

    // copy cuda array to "normal" array
    checkCudaCall(cudaMemcpy(cur, cur_c, i_max * sizeof(double), cudaMemcpyDeviceToHost));

    // free cuda mallocs
    checkCudaCall(cudaFree(old));
    checkCudaCall(cudaFree(cur));
    checkCudaCall(cudaFree(next));

    return current_array;
}


// ASSIGN COPY: (nothing changed yet) 

typedef double (*func_t)(double x);

double gauss(double x){
    return exp((-1 * x * x) / 2);
}

void fill(double *array, int offset, int range, double sample_start,
        double sample_end, func_t f)
{
    int i;
    float dx;

    dx = (sample_end - sample_start) / range;
    for (i = 0; i < range; i++) {
        array[i + offset] = f(sample_start + i * dx);
    }
}


int main(int argc, char *argv[])
{
    double *old, *current, *next;
    int t_max, i_max, num_threads;
    double time;

    /* Parse commandline args: i_max t_max num_threads */
    if (argc < 4) {
        printf("Usage: %s i_max t_max num_threads [initial_data]\n", argv[0]);
        printf(" - i_max: number of discrete amplitude points, should be >2\n");
        printf(" - t_max: number of discrete timesteps, should be >=1\n");
        printf(" - num_threads: number of threads to use for simulation, "
                "should be >=1\n");
        printf(" - initial_data: select what data should be used for the first "
                "two generation.\n");
        printf("   Available options are:\n");
        printf("    * sin: one period of the sinus function at the start.\n");
        printf("    * sinfull: entire data is filled with the sinus.\n");
        printf("    * gauss: a single gauss-function at the start.\n");
        printf("    * file <2 filenames>: allows you to specify a file with on "
                "each line a float for both generations.\n");

        return EXIT_FAILURE;
    }

    i_max = atoi(argv[1]);
    t_max = atoi(argv[2]);
    num_threads = atoi(argv[3]);

    if (i_max < 3) {
        printf("argument error: i_max should be >2.\n");
        return EXIT_FAILURE;
    }
    if (t_max < 1) {
        printf("argument error: t_max should be >=1.\n");
        return EXIT_FAILURE;
    }
    if (num_threads < 1) {
        printf("argument error: num_threads should be >=1.\n");
        return EXIT_FAILURE;
    }

    /* Allocate and initialize buffers. */
    old = malloc(i_max * sizeof(double));
    current = malloc(i_max * sizeof(double));
    next = malloc(i_max * sizeof(double));

    if (old == NULL || current == NULL || next == NULL) {
        fprintf(stderr, "Could not allocate enough memory, aborting.\n");
        return EXIT_FAILURE;
    }

    memset(old, 0, i_max * sizeof(double));
    memset(current, 0, i_max * sizeof(double));
    memset(next, 0, i_max * sizeof(double));

    /* How should we will our first two generations? */
    if (argc > 4) {
        if (strcmp(argv[4], "sin") == 0) {
            fill(old, 1, i_max/4, 0, 2*3.14, sin);
            fill(current, 2, i_max/4, 0, 2*3.14, sin);
        } else if (strcmp(argv[4], "sinfull") == 0) {
            fill(old, 1, i_max-2, 0, 10*3.14, sin);
            fill(current, 2, i_max-3, 0, 10*3.14, sin);
        } else if (strcmp(argv[4], "gauss") == 0) {
            fill(old, 1, i_max/4, -3, 3, gauss);
            fill(current, 2, i_max/4, -3, 3, gauss);
        } else if (strcmp(argv[4], "file") == 0) {
            if (argc < 7) {
                printf("No files specified!\n");
                return EXIT_FAILURE;
            }
            file_read_double_array(argv[5], old, i_max);
            file_read_double_array(argv[6], current, i_max);
        } else {
            printf("Unknown initial mode: %s.\n", argv[4]);
            return EXIT_FAILURE;
        }
    } else {
        /* Default to sinus. */
        fill(old, 1, i_max/4, 0, 2*3.14, sin);
        fill(current, 2, i_max/4, 0, 2*3.14, sin);
    }

    timer_start();

    /* Call the actual simulation that should be implemented in simulate.c. */
    simulate(i_max, t_max, num_threads, old, current, next);

    time = timer_end();
    printf("Took %g seconds\n", time);
    printf("Normalized: %g seconds\n", time / (i_max * t_max));

    file_write_double_array("result.txt", current, i_max);

    free(old);
    free(current);
    free(next);

    return EXIT_SUCCESS;
}
