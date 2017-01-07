#include "timer.h"
#include "erat.h"
#include <pthread.h>
#include <signal.h>
#include <stdio.h>

int main(int argc, char const *argv[]) {

    double time;
    timer_start();

    erat(argc,  argv);

    time = timer_end();
    printf("Took %g seconds\n", time);


    return 0;
}
