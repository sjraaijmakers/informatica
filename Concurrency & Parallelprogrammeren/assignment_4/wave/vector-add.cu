/* Steven Raaijmakers (10804242) & Marcus van Bergen (10871993) */
/* Program uses cuda to simulate wave equation. */
/* Sources: http://bit.ly/2dGeceE, http://bit.ly/2h0nViZ */
/* http://bit.ly/1hRMNnA */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "file.h"
#include "timer.h"
#include "simulate.h"
#include <iostream>

using namespace std;

static void checkCudaCall(cudaError_t result) {
    if (result != cudaSuccess) {
        cerr << "cuda error: " << cudaGetErrorString(result) << endl;
        exit(1);
    }
}

// Calc function
__global__ void calc(double *old, double *cur, double *next, int MAX){
    unsigned index = blockIdx.x * blockDim.x + threadIdx.x;

    double c = 0.15;

    // Borders must be 0
    if((index > 0) && (index < MAX)){
        next[index] = 2.0 * cur[index] - old[index] +
                      c * (cur[index - 1] - (2 * cur[index] - cur[index + 1]));
    }
    else if(index == 0 || index == MAX){
        next[index] = 0.0;
    }
    // If index is not within i_max range
    return;
}

double *simulate(const int i_max, const int t_max, double *old,
                 double *cur, double *next){
    int threadBlockSize = 512;
    // old array
    double* old_c = NULL;
    checkCudaCall(cudaMalloc((void **)&old_c, i_max * sizeof(double)));
    if(old_c == NULL){
        cout << "could not allocate memory!" << endl;
        exit(1);
    }

    // current array
    double* cur_c = NULL;
    checkCudaCall(cudaMalloc((void **)&cur_c, i_max * sizeof(double)));
    if(cur_c == NULL){
        checkCudaCall(cudaFree(old_c));
        cout << "could not allocate memory!" << endl;
        exit(1);
    }

    // next array
    double* next_c = NULL;
    checkCudaCall(cudaMalloc((void **)&next_c, i_max * sizeof(double)));
    if(next_c == NULL){
        checkCudaCall(cudaFree(old_c));
        checkCudaCall(cudaFree(cur_c));
        cout << "could not allocate memory!" << endl;
        exit(1);
    }

    cudaEvent_t start, stop;
    cudaEventCreate(&start);
    cudaEventCreate(&stop);

    // fill ca
    checkCudaCall(cudaMemcpy(old_c, old, i_max * sizeof(double),
                             cudaMemcpyHostToDevice));
    checkCudaCall(cudaMemcpy(cur_c, cur, i_max * sizeof(double),
                             cudaMemcpyHostToDevice));
    checkCudaCall(cudaMemcpy(next_c, next, i_max * sizeof(double),
                             cudaMemcpyHostToDevice));

    cudaEventRecord(start, 0);

    // T complexity
    for(int t = 0; t < t_max; t++){
        // Call cuda
        calc<<<i_max/threadBlockSize, threadBlockSize>>>(old_c, cur_c, next_c,
                                                         i_max - 1);
        // Error check
        checkCudaCall(cudaGetLastError());
        // Swap arrays
        double *tmp = old_c;
        old_c = cur_c;
        cur_c = next_c;
        next_c = tmp;
    }
    cudaEventRecord(stop, 0);

    // copy cuda array to "normal" array
    checkCudaCall(cudaMemcpy(cur, cur_c, i_max * sizeof(double),
                             cudaMemcpyDeviceToHost));

    // free cuda mallocs
    checkCudaCall(cudaFree(old_c));
    checkCudaCall(cudaFree(cur_c));
    checkCudaCall(cudaFree(next_c));

    // time
    float elapsedTime;
    cudaEventElapsedTime(&elapsedTime, start, stop);

    cout << "kernel invocation took " << elapsedTime << " milliseconds" << endl;

    return cur;
}


// COPY: (nothing changed yet)

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

// checks if x is power of two
int powerOfTwo(int x){
    return !(x == 0) && !(x & (x - 1));
}

int main(int argc, char *argv[])
{
    double *old, *current, *next;
    int t_max, i_max;

    /* Parse commandline args: i_max t_max */
    if (argc < 3) {
        printf("Usage: %s i_max t_max \n", argv[0]);
        printf(" - i_max: number of discrete amplitude points, should be >2\n");
        printf(" - t_max: number of discrete timesteps, should be >=1\n");
        return EXIT_FAILURE;
    }

    i_max = atoi(argv[1]);
    // i_max should be a power of 2
    if(!powerOfTwo(i_max)){
        printf("argument error: i_max should be 2^X.\n");
        exit(0);
    }
    t_max = atoi(argv[2]);

    if (i_max < 3) {
        printf("argument error: i_max should be >2.\n");
        return EXIT_FAILURE;
    }
    if (t_max < 1) {
        printf("argument error: t_max should be >=1.\n");
        return EXIT_FAILURE;
    }

    /* Allocate and initialize buffers. */
    old = (double*)malloc(i_max * sizeof(double));
    current = (double*)malloc(i_max * sizeof(double));
    next = (double*)malloc(i_max * sizeof(double));

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

    // double time;
    // timer_start();

    timer vectorAddTimer("vector add timer");

    vectorAddTimer.start();

    simulate(i_max, t_max, old, current, next);

    vectorAddTimer.stop();

    cout << vectorAddTimer;

    file_write_double_array("result.txt", current, i_max);

    free(old);
    free(current);
    free(next);

    return EXIT_SUCCESS;
}
