/*
 * simulate.h
 */
struct Args;

double *simulate_serial(const int i_max, const int t_max, const int num_cpus,
        double *old_array, double *current_array, double *next_array);
