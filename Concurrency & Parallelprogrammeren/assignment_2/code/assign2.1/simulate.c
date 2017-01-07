/*
 * assign1_1.c
 *
 * Naam: Steven Raaijmakers & Marcus van Bergen
 *
 * Contains code for setting up and finishing the simulation.
 *
 * Programma simuleert een wave equation, geparalelliseerd via MPI. De stukken
 * van de wave worden berekent in simulate.c en gereturned naar deze functie
 * en samengevoegd in een array. De randcellen worden naar elkaar
 * gecommuniceerd via zogeheten halo cels
 *
 * Bron: http://bit.ly/2g1bK5x & http://bit.ly/2gn3PAN & http://fla.st/2flncFj
 */

 #include <stdio.h>
#include <stdlib.h>

#include "simulate.h"
#include "mpi.h"

double c = 0.15;

/* De berekening functie die wave_part gebruikt */
double calc(double *old, double *current, double *next, int i){
    return 2 * current[i] - old[i] + c * (current[i - 1] - \
           (2 * current[i] - current[i + 1]));
}

/* Berekent (gedeelte) van de wave */
double *wave_part(int i_max, int t_max, double *old_array,
                  double *current_array, double *next_array){
    MPI_Status status;

    int num_tasks, my_rank;
    MPI_Comm_size(MPI_COMM_WORLD, &num_tasks);
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

    /* Voor alle t-stappen: */
    for(int t = 0; t < t_max; t++){
        /* Als huidige process een linker neighbor heeft */
        if(my_rank > 0){
            /* Stuur linker neighbor onze current, en ontvang de zijne */
            MPI_Send(&current_array[1], 1, MPI_DOUBLE, my_rank - 1,
                     t, MPI_COMM_WORLD);
            MPI_Recv(&current_array[0], 1, MPI_DOUBLE, my_rank - 1,
                     t, MPI_COMM_WORLD, &status);
        }
        /* Als process een rechter neighbor heeft */
        if (my_rank < num_tasks - 1) {
            /* Stuur buurman laatste waarde+1 van onze current, en ontvang */
            MPI_Send(&current_array[i_max], 1, MPI_DOUBLE, my_rank + 1,
                     t, MPI_COMM_WORLD);
            MPI_Recv(&current_array[i_max + 1], 1, MPI_DOUBLE, my_rank + 1,
                     t, MPI_COMM_WORLD, &status);
        }

        /* Berekening */
        for (int i = 1; i < i_max + 1; i++) {
            next_array[i] = calc(old_array, current_array, next_array, i);
        }

        /* Array swap */
        double *temp = old_array;
        old_array = current_array;
        current_array = next_array;
        next_array = temp;
    }

    /* Return bewerkte array */
    return current_array;
}

/* Simulate function */
double *simulate(int argc, char *argv[], const int i_max,
                 const int t_max, double *old, double *current, double *next){
    double *res;

    /* MPI init */
    int rc, num_tasks, my_rank;
    rc = MPI_Init(&argc, &argv);
    if (rc != MPI_SUCCESS){
        fprintf(stderr, "Unable to set up MPI");
        MPI_Abort(MPI_COMM_WORLD, rc);
    }

    MPI_Status status;

    MPI_Comm_size(MPI_COMM_WORLD, &num_tasks);
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

    int size = i_max / num_tasks;

    /* Malloc stukjes van gehele array + plek voor halo cels */
    double *old_slice = malloc(sizeof(double) * (size + 2));
    double *cur_slice = malloc(sizeof(double) * (size + 2));
    double *next_slice = malloc(sizeof(double) * (size + 2));

    /* Head Proces doet de init */
    if(my_rank == 0){
        // Stuur alle child-processen het juiste gedeelte van de arrays
        for (int i = 1; i < num_tasks; i++) {
            MPI_Send(&old[size * i], size, MPI_DOUBLE, i, 0, MPI_COMM_WORLD);
            MPI_Send(&current[size * i], size, MPI_DOUBLE, i, 1,
                     MPI_COMM_WORLD);
        }
        res = wave_part(size, t_max, &old[1], &current[1], &next[1]);

        MPI_Send(&res[1], size, MPI_DOUBLE, 0, 0, MPI_COMM_WORLD);

        // Combineer slices door ze in de juiste positie te plakken van current
        for (int i = 0; i < num_tasks; i++) {
            MPI_Recv(&current[i * size], size, MPI_DOUBLE, i, 0,
                    MPI_COMM_WORLD, &status);
        }
        /* Hierna zijn we klaar */
        MPI_Finalize();
    }
    /* Alle niet-head processen */
    else {
        /* Recv informatie verzonden door master over arrays */
        MPI_Recv(&old_slice[1], size, MPI_DOUBLE,
                 0, 0, MPI_COMM_WORLD, &status);
        MPI_Recv(&cur_slice[1], size, MPI_DOUBLE,
                 0, 1, MPI_COMM_WORLD, &status);

        /* Run functie */
        res = wave_part(size, t_max, old_slice, cur_slice, next_slice);

        /* Stuur master resultaat van functie terug */
        MPI_Send(&res[1], size, MPI_DOUBLE, 0, 0, MPI_COMM_WORLD);
        MPI_Finalize();
        exit(0);
    }

    /* Free slices */
    free(old_slice);
    free(cur_slice);
    free(next_slice);

    return current;
}
