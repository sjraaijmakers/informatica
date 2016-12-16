/*
 * simulate.h
 */
struct p_args;

void * calc(void *arguments);

double *simulate_pthreads(const int i_max, const int t_max, const int num_threads,
        double *old_array, double *current_array, double *next_array);
