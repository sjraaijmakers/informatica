#ifndef SYS_H
#define SYS_H

#include <stdio.h>
#include <stddef.h>
#include <stdbool.h>

// Only used by test programs or the simulation kernel.

void sim_init(size_t, size_t, size_t, size_t, bool, FILE*);
void sim_check(void);
void sim_report(void);
void sim_exit(void);
void sim_count_alloc(void*, size_t);
void sim_count_free(void*, size_t);

#endif
