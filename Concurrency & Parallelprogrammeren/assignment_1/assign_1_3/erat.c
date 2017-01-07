/* Erasthenos implementation with POSIX threads.
 * Steven Raaijmakers & Marcus van Bergen
 * 10824242 & 10817993
 *
 * Vak: CPP
 * About: simulates Sieve of Erasthenos using pthreads in a
 * consumer - producer model.
 */

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <signal.h>
#include "timer.h"

#include "erat.h"
#include <unistd.h>

#define BUFSIZE 50

/* Define thread amount for the array TID */
#define THREADS 5000

pthread_t threads_ids[THREADS];
int num = 0;

double timer;
int time_set = 0;

int MAX_P;
int found_primes = 0;

/* Initialize a buffer, by assign space for data-array, and setting vars */
void init_buffer(buffer *c){
    /* Number data buffer */
    c->data = malloc(sizeof(int) * BUFSIZE);

    /* Meta data */
    c->occupied = 0;
    c->nextin = 0;
    c->nextout = 0;

    /* Var's and locks */
    c->buflock = (pthread_mutex_t)PTHREAD_MUTEX_INITIALIZER;
    c->items = (pthread_cond_t)PTHREAD_COND_INITIALIZER;
    c->space = (pthread_cond_t)PTHREAD_COND_INITIALIZER;
}

/* Check if x is divisible by y */
int is_divisble(int x, int y){
    if(x % y == 0){
        return 1;
    }
    return 0;
}

/* Producer: puts val in current (if there's room) */
void producer(buffer *current, int val){
    pthread_mutex_lock(&current->buflock);
    while(!(current->occupied < BUFSIZE)){
        pthread_cond_wait(&current->space, &current->buflock);
    }
    /* occupied < BUFSIZE */
    current->data[current->nextin] = val;
    current->nextin = (current->nextin + 1) % BUFSIZE;
    current->occupied = current->occupied + 1;
    pthread_cond_signal(&current->items);
    pthread_mutex_unlock(&current->buflock);
}

/* Consumer: takes val from current and returns it */
int consumer(buffer *current){
    pthread_mutex_lock(&current->buflock);
    while(!(current->occupied > 0)){
        pthread_cond_wait(&current->items, &current->buflock);
    }
    /* When incomming buffer has values */
    int val = current->data[current->nextout];
    current->nextout = (current->nextout + 1) % BUFSIZE;
    current->occupied = current->occupied - 1;
    pthread_cond_signal(&current->space);
    pthread_mutex_unlock(&current->buflock);
    return val;
}

/* Create thread; setup args and spawn pthread*/
void create_filter_thread(int filter_number, buffer *incomming){
    /* Allocate piece of memory */
    filter_args *new_thread = malloc(sizeof(filter_args));
    new_thread->incoming_buffer = incomming;
    new_thread->filter_number = filter_number;

    new_thread->outgoing_buffer = malloc(sizeof(buffer));
    init_buffer(new_thread->outgoing_buffer);
    for(int j = 0; j < BUFSIZE; j++){
        new_thread->outgoing_buffer->data[j] = 0;
    }

    /* Spawn new thread */
    pthread_create(&threads_ids[num++], NULL, &filter_thread, \
                   (void*)new_thread);
}

/* THREADS: */
/* Generates all natural numbers [2, 3, ... INF], and puts them in a buffer */
void * generator(void *arguments){
    buffer *all_numbers = (buffer*)(arguments);
    for(int i = 2; i < INFINITY; i++){
        producer(all_numbers, i);
    }
    return NULL;
}

/* Filter Thread */
void * filter_thread(void *arguments){
    /* Main operations for a running thread */
    found_primes++;
    if(found_primes > MAX_P && !time_set){
        /* Print time and set it to not print anymore*/
        timer = timer_end();
        printf("Took %g seconds\n", timer);
        sleep(5);
        time_set = 1;
    }

    /* Setting buffers up correctly of the thread */
    filter_args *current = (filter_args*)(arguments);
    buffer *in = current->incoming_buffer;
    buffer *out = current->outgoing_buffer;

    int FILTER = current->filter_number;
    printf("%d. %d\n", found_primes, FILTER);

    /* Consume from incomming buffer */
    for(int i = 0; i < INFINITY; i++){
        int val = consumer(in);

        if(!is_divisble(val, FILTER)){
            producer(out, val);

            /* Create new thread if current has no succesor */
            if(out->data[1] == 0){
                create_filter_thread(val, out);
            }
        }
    }
    return NULL;
}

void erat(int argc, const char* argv[]){
    /* Set MAX_P to user specified arg */
    if(argc > 1){
        MAX_P = atoi(argv[1]);
    }
    else {
        MAX_P = 999999;
    }

    /* Generator thread */
    buffer *all_natural = malloc(sizeof(buffer));
    init_buffer(all_natural);
    pthread_create(&threads_ids[num++], NULL, &generator, (void*)all_natural);

    /* Initialize first thread */
    create_filter_thread(2, all_natural);
    pthread_exit(0);

}

/* Main */
int main(int argc, char const *argv[]) {

    timer_start();
    erat(argc, argv);

    return 0;
}
