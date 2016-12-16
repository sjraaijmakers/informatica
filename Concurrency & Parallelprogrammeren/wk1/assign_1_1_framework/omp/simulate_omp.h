#pragma once

struct i_val;

double *simulate(const int i_max, const int t_max, const int num_cpus,
        double *old_array, double *current_array, double *next_array);
