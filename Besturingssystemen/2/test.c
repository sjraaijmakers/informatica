#include "myalloc.h"
#include "xcheck.h"
#include "sys.h"
#include <stdlib.h>
#include <stdio.h>
#include <getopt.h>
#include <string.h>
#include <ctype.h>

static mm_state_t* test_start(size_t, size_t, size_t, size_t, bool, FILE*);
static void run_trace(mm_state_t* st, const char *fname);

int main(int argc, char **argv)
{
    int opt;
    size_t s = 1024, m = 1, b = 42, v = 0;
    bool t = false;
    char *k = 0;
    FILE *logfile = NULL;

    while ((opt = getopt(argc, argv, "hv:tk:s:m:b:l:")) != -1)
    {
        switch(opt) {
        case '?':
        case 'h':
            printf("usage: %s [OPTS]\n"
                   "options:\n"
                   " -h       print this help.\n"
                   " -v N     display a visualization every N simulation steps.\n"
                   " -t       display a trace of the mm_alloc/mm_free calls.\n"
                   " -k TEST  process the trace TEST (default: simple.t).\n"
                   " -s N     set the bank size to N (default 1024).\n"
                   " -m N     set the number of modules to N (default 1).\n"
                   " -b N     set the number of banks per module to N (default 42).\n"
                   " -l FILE  set logfile name (default is to print to stdout).\n"
                   "\n"
                   "The visualization displays the (virtual) time since the beginning of\n"
                   "the simulation, then a textual content of the module(s) contents, then\n"
                   "the current number of activated banks, then the external overhead\n"
                   "(number of activated bytes - number of allocated bytes).\n"
                   "The module contents is displayed as:\n"
                   "  .  to indicate an inactive bank\n"
                   "  X  to indicate an inactive bank that was incorrectly written to\n"
                   "  o  to indicate an active bank, that was not yet written to\n"
                   "  O  to indicate an active bank that was written to.\n",
                   argv[0]);
            return EXIT_SUCCESS;
        case 's':
            s = strtoul(optarg, 0, 0);
            break;
        case 'm':
            m = strtoul(optarg, 0, 0);
            break;
        case 'b':
            b = strtoul(optarg, 0, 0);
            break;
        case 't':
            t = true;
            break;
        case 'v':
            v = strtoul(optarg, 0, 0);
            break;
        case 'k':
            k = optarg;
            break;
        case 'l':
            if (logfile)
                fclose(logfile);
            logfile = fopen(optarg, "w");
            if (!logfile) {
                perror("fopen");
                return EXIT_FAILURE;
            }
            break;
        }
    }

    if (!logfile)
        logfile = stdout;

    if (!k)
        k = "simple.t";
    fprintf(logfile, "starting trace file: %s\n", k);

    mm_state_t* st = test_start(m, b, s, v, t, logfile);

    run_trace(st, k);

    sim_report();
    sim_exit();
    return EXIT_SUCCESS;
}

static
mm_state_t* test_start(size_t m, size_t b, size_t s,
                       size_t v, bool t, FILE *logfile)
{
    sim_init(m, b, s, v, t, logfile);
    sim_check();

    mm_state_t* st;
    st = mm_initialize();
    xcheck(st != NULL);

    sim_check();

    return st;
}

static
void run_trace(mm_state_t* st, const char *fname)
{
    FILE *f;
    size_t lineno;

    void **vector = 0;
    size_t vsz = 0;
    bool done = 0;

    f = fopen(fname, "r");
    xcheck(f != 0);
    lineno = 1;
    while (!done)
    {
        char cmd;
        size_t index, arg;
        if (3 != fscanf(f, "%c %zu %zu\n", &cmd, &index, &arg))
        {
            fprintf(stderr, "%s:%zu: unrecognized command\n", fname, lineno);
            exit(1);
        }
        if (cmd != 'v' && cmd != 'e' && index >= vsz)
        {
            fprintf(stderr, "%s:%zu: index %zu out of vector (%zu)\n", fname, lineno, index, vsz);
            exit(1);
        }

        switch(cmd)
        {
        case 'e': // end
            done = 1;
            break;

        case 'p': // poke
        {
            if (!vector[index])
            {
                fprintf(stderr, "%s:%zu: pointer is zero, ignoring poke\n", fname, lineno);
                break;
            }
            size_t sz = *(size_t*)vector[index];
            memset((char*)vector[index] + sizeof(size_t), 42, sz - sizeof(size_t));

            sim_check();
            break;
        }
        case 'v': // vector
        {
            void **new_vector = (void**)malloc(arg * sizeof(void*));
            if (!new_vector)
            {
                fprintf(stderr, "%s:%zu: cannot allocate vector\n", fname, lineno);
                exit(1);
            }

            size_t i;
            for (i = 0; i < arg && i < vsz; ++i)
                new_vector[i] = vector[i];
            for ( ; i < arg; ++i)
                new_vector[i] = 0;

            if (vector)
                free(vector);

            vector = new_vector;
            vsz = arg;
            break;
        }
        case 'A': // alloc, force
        case 'a': // alloc
        case 'r': // alloc, random
        case 'R': // alloc, random, force
        {
            if (cmd == 'r' || cmd == 'R')
                arg = (rand()*rand()) % arg;
            size_t sz = arg > sizeof(size_t) ? arg : sizeof(size_t);

            void *p = mm_alloc(st, sz);
            sim_count_alloc(p, sz);
            sim_check();

            if (p == 0)
            {
                fprintf(stderr, "%s:%zu: allocation failed (%zu bytes)\n", fname, lineno, sz);
                if (isupper(cmd))
                    exit(1);
            }

            if (p)
            {
                memset(p, 42, sz);
                sim_check();

                *(size_t*)p = sz;

                if (vector[index])
                {
                    size_t osz = *(size_t*)vector[index];
                    memset(vector[index], 42, osz);
                    sim_check();

                    sim_count_free(vector[index], osz);
                    mm_free(st, vector[index]);
                    sim_check();
                }
            }

            vector[index] = p;
            break;
        }
        case 'f':
        {
            if (vector[index])
            {
                size_t sz = *(size_t*)vector[index];

                memset(vector[index], 42, sz);
                sim_check();

                sim_count_free(vector[index], sz);
                mm_free(st, vector[index]);
                sim_check();

                vector[index] = 0;
            }
            break;
        }
        default:
            fprintf(stderr, "%s:%zu: unrecognized command\n", fname, lineno);
            exit(1);
        }

        ++lineno;
    }
    if (vector)
        free(vector);
    fclose(f);
}
