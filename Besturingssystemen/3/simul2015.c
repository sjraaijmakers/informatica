#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <stdbool.h>
#include <limits.h>

#include "schedule.h"
#include "mem_alloc.h"
#include "mt19937.h"
#include "float.h"

/* The procedures in this file together implement a simple discrete-event
   simulator for process scheduling - except of course for the scheduler
   itself.

   In order to judge performance, some statistics must be collected.
   Which should be use-full?

   Per process: wait-time to memory allocation
			  to first service
		execution time
		turn-around time
	Look for average, maximum.
	For this we need per process:
		creation time
		t_mem
		t_run
		t_end
   Per device:  utilization of CPU
			    of memory
			    of IO-channels
	Time-averages only.
*/

typedef enum
{
    init, ready, io, defunct
}
proc_state;

#define IO_COUNT	(3)

#define N_SAMPLES	(40960)

#define MAX_MES		(150)

#define MEM_min		(512)

#define MEM_range	((int) (5 * (MEM_load * MEM_SIZE / (1.25 * LOAD_FACTOR + 1.75 * IO_timefactor) - MEM_min)))

static float T_mem_alloc[N_SAMPLES];
static float T_first_CPU[N_SAMPLES];
static float T_execution[N_SAMPLES];
static float T_turn_ar[N_SAMPLES];

static long get_stats = 0;
static long n_count;
static long n_samples = 0, n_msgs = 0;
static long next_request = 0, proc_num = 0;
static long len_new_q = 0, len_cpu_q = 0, len_io_q[IO_COUNT] = { 0, 0, 0 },
	len_def_q = 0;
static long max_new_q = 0, max_cpu_q = 0, max_io_q[IO_COUNT] = { 0, 0, 0 },
	max_def_q = 0;

static double IO_timefactor = 1.0;	/* added 10-09-2003 */
static double MEM_load = 1.0;		/* added 10-09-2003 */
static double av_new_q = 0,
              av_cpu_q = 0,
              av_io_q[IO_COUNT] = { 0, 0, 0 },
	      av_def_q = 0;

typedef struct MY_PCB
{
    double  CPU_need, IO_need[IO_COUNT];
    double  CPU_used, IO_used[IO_COUNT];
    double  CPU_burst, IO_burst[IO_COUNT];
    double  T_create, T_mem_alloc, T_CPU, T_IO, T_end;
    struct MY_PCB *prev, *next, *prev_q, *next_q;
    pcb    *stud_pcb, *in_queue;
    long    MEM_need, MEM_base, proc_num, io_q, IO_cycles;
    proc_state state;
} my_pcb;

static double CPU_util = 0, IO_util[IO_COUNT] = { 0, 0, 0 },
	MEM_util = 0, MEM_in_use = 0;
static double eps = 1.0e-12;

static my_pcb *first = NULL,
	 *last = NULL,
	 *CPU = NULL,
	 *(IO[IO_COUNT]) = {NULL, NULL, NULL};

static double TIME = 0.0, T_inc = 0.0;
static double T_start;
static double T_slice = 9.9e12;

static event_type cur_event;

#define N_requests	(4)

/***********************************************************************
   The execution of a process is as follows.
   It starts with a CPU burst, next an IO burst on device 0, CPU, IO on device 1
   CPU, IO on device 2, CPU, IO on 0, etc, ending in a CPU burst.
   In this way NC CPU bursts occur, N0 IO burst on device 0, N1 on 1, N2 on 2.
   NC = 1 + N0 + N1 + N2.
   N0 + N1 + N2 is called IO_cycles in this program.
   In the version of the code that stood over from Nov. 98, the calculation
   of the burst lengths was performed in the following manner.
   The remaining CPU time for each was calculated and divided by IO_cycles + 1
   to obtain the CPU burst length.
   IO_cycles was reduced by 3 after an IO_burst on device 2. (But forced to be at
	 least 0).
   The length of an IO_burst was obtained by dividing the remaining IO_time per
	device by IO_cycles.
   The result was a slightly oscilatory behaviour of the CPU burst length with
	an increasing amplitude towards the end of the execution. The IO burst
	lengths increase rather drastically towards the end of the execution.

   The CPU load should be the arrival rate * average CPU need per job
   The IO load should be the arrival rate * average IO time per device per job
   The memory load is more complex: it depends on the resident time (which
   	is at least the sum of the CPU and IO needs), the amount of memory
	per job, and the arrival rate, divided by the memory capacity.
	Only a lower bound can easily be given.

   The initialisation of the job parameters needs more care than we used
   originally.
   1. The average length of a CPU burst should be greater than the
      minimum timeslice (which was arbitrarily set to 1).
   2. The job execution times should be drawn from a reasonable random
      distribution.
   3. The length of the CPU burst should be modulated by a random amount.
   4. Same for the length of the IO burst.
   5. CPU load, memory load and IO load should be separately adjustable.

   Let's assume:
   CPU need is drawn from one of four randomly selected uniform
   	distributions:
   2 to 6 units (avg 4)
   6 to 18 units (avg 12)
   18 to 54 units (avg 36)
   54 to 162 units (avg 108)

   Grand avg 40 units. (Actually, just draw from a uniform distribution and
   select one of four multipliers). Arrival rate of 0.025 leads to unity
   CPU load.

   IO_cycles - uniform random number from 3 (3) 21 (avg 12, or 4 per device).

   IO_burst[0] 3 always
   IO_burst[1] 1 -- 5 uniform, avg 3.
   IO_burst[2] 4 -- 16 uniform, avg (10),

   i.e. only device 2 can lead to IO-saturation. multiply by a time factor to
   allow scaling w.r.t. CPU load.
   Avg minimum execution time becomes 40 + 16 * 4 * IO_timefactor

   For an IO_timefactor of 1, that just happens to be 104, i.e. 2.6 times
   the CPU time. I.e. in that case, we should have 2.6 jobs in memory,
   on average. Make average memory request MEM_SIZE / 3 (= 10920) words.

**************************************************************************/



static my_pcb request[N_requests] = { /* 1 */ {10, {5, 11, 15},
					       0, {0, 0, 0},
					       3, {3, 5, 3},
					       0, 0, 0, 0, 0,
					       NULL, NULL, NULL, NULL,
					       NULL, NULL,
					       4096, -1, 0, 0, 10,
					       init},
                                      /* 2 */ {20, {35, 41, 55},
                                               0, {0, 0, 0},
                                               3, {3, 5, 3},
                                               0, 0, 0, 0, 0,
                                               NULL, NULL, NULL, NULL,
                                               NULL, NULL,
                                               1024, -1, 0, 0, 13,
                                               init},
                                      /* 3 */ {70, {15, 21, 15},
                                               0, {0, 0, 0},
                                               3, {3, 5, 3},
                                               0, 0, 0, 0, 0,
                                               NULL, NULL, NULL, NULL,
                                               NULL, NULL,
                                               2048, -1, 0, 0, 2,
                                               init},
                                      /* 4 */ {10, {5, 51, 15},
                                               0, {0, 0, 0},
                                               3, {3, 5, 3},
                                               0, 0, 0, 0, 0,
                                               NULL, NULL, NULL, NULL,
                                               NULL, NULL,
                                               8192, -1, 0, 0, 4,
                                               init}
                                         };

static double T_next_new,
 	LOAD_FACTOR = 1.0,
 	T_delay[N_requests] = { 4, 27, 112, 17 } /* avg = 40; strongly clustered arrivals */;

double
sim_time ()
{
    return (TIME);
}

void
set_slice (double slice)
{
    slice = (slice < 1.0) ? 1.0 : slice;
    T_slice = TIME + slice;
}

static void
sluit_af ()
{
    printf ("Einde programma\n");
}

static void
my_init_sim ()
{
}

static void
my_reset_stats ()
{
}


static void
histogram (float *data, char *text)
{
    long    N, i, histo[66], j;
    double  av = 0, sigma = 0, scaleh, scale, lim;
    float   max, min, hi;
    char    c;

    N = (n_samples < N_SAMPLES) ? n_samples : N_SAMPLES;

    printf ("\nHistogram en statistieken van %s\n", text);
    printf ("over de laatste %ld beeindigde processen\n", N);

    if (N < 2)
    {
	printf ("Geen gegevens ...\n");
	return;
    }
    max = min = data[0];

    for (i = 0; i < N; i++)
    {
	av += data[i];
	sigma += data[i] * data[i];
	min = (data[i] < min) ? data[i] : min;
	max = (data[i] > max) ? data[i] : max;
    }
    av = av / (double) N;
    sigma = sigma / (double) N;
    sigma = sigma - av * av;
    sigma = N * sigma / ((double) N - 1);
    sigma = (sigma > 0) ? sqrt (sigma) : 0.0;

    scaleh = 65.9 / (max - min);

    for (i = 0; i < 65; i++)
    {
	histo[i] = 0;
    }
    for (i = 0; i < N; i++)
    {
	j = (data[i] - min) * scaleh;
	histo[j]++;
    }
    hi = 0.0;
    for (i = 0; i < 65; i++)
    {
	if (histo[i] > hi)
	    hi = histo[i];
    }
    /*
       make histogram 18 lines high
     */
    scale = hi / 17.9;
    for (i = 17; i >= 0; i--)
    {
	lim = scale * i;
	if (0 == (i % 5))
	{
	    printf ("%6.1f |", lim);
	} else
	{
	    printf ("       |");
	}
	for (j = 0; j < 65; j++)
	{
	    c = (lim >= histo[j]) ? ' ' : '*';
	    putchar (c);
	}
	printf ("\n");
    }
    printf ("%s\n%s\n  ",
	    "       |----|----|----|----|----|----|----|----|----|----|----|----|----|",
	    "       |         |         |         |         |         |         |     ");
    for (i = 0; i < 7; i++)
    {
	printf ("%6.0f    ", min + 10.0 * i / scaleh);
    }
    printf ("\n                                           tijdseenheden\n");
    printf ("\nGemiddelde waarde: %6.1f, spreiding: %6.2f\n", av, sigma);
    printf ("Minimum waarde: %6.1f, maximum waarde: %6.1f\n", min, max);
    printf ("-----------------------------------------------------------------\n");
}

void
print_statistics ()
{
    /*
       This routine will print the statistics gathered to this time
     */

    long    MEM_wait = 0, CPU_wait = 0, IO_wait = 0, DEF_wait = 0, i;
    pcb    *stud;

    printf ("Statistieken op tijdstip = %6.0f\n", TIME);
    printf ("Opnemen statistieken gestart na 100 aangemaakte processen\n");
    printf ("\top tijdstip %f\n", T_start);

    printf ("Aantal gevolgde processen: %ld, aantal gereed: %ld\n",
            proc_num - 100, n_samples);
    stud = new_proc;
    while (stud)
    {
        MEM_wait++;
        stud = stud->next;
    }
    stud = ready_proc;
    while (stud)
    {
        CPU_wait++;
        stud = stud->next;
    }
    stud = io_proc;
    while (stud)
    {
        IO_wait++;
        stud = stud->next;
    }
    stud = defunct_proc;
    while (stud)
    {
        DEF_wait++;
        stud = stud->next;
    }

    printf ("Aantal processen wachtend op geheugen: %ld\n", MEM_wait);
    printf ("Maximum was: %ld, gemiddelde was %f\n", max_new_q,
	    av_new_q / (TIME - T_start));
    printf ("Gemiddeld gebruik geheugen: %6.0f woorden, utilisatie %6.4f\n",
            MEM_util / (TIME - T_start),
            MEM_util / ((TIME - T_start) * MEM_SIZE));
    printf ("Aantal in de ready queue:              %ld\n", CPU_wait);
    printf ("Maximum was: %ld, gemiddelde was %f\n", max_cpu_q,
            av_cpu_q / (TIME - T_start));
    printf ("\nGebruikte CPU-tijd: %6.0f, CPU utilisatie: %6.4f\n",
            CPU_util, CPU_util / (TIME - T_start));
    printf ("Aantal in de I/O queue:                %ld\n", IO_wait);
    for (i = 0; i < IO_COUNT; i++)
    {
	printf ("Maximum voor kanaal %ld was: %ld, gemiddelde %f\n", i,
		max_io_q[i], av_io_q[i] / (TIME - T_start));
	printf ("Gebruikte tijd op IO-kanaal %ld: %6.0f, utilisatie: %6.4f\n",
		i, IO_util[i], IO_util[i] / (TIME - T_start));
    }
    printf ("Aantal wachtend op opruimen:           %ld\n", DEF_wait);
    printf ("Maximum was: %ld, gemiddelde was %f\n", max_def_q,
	    av_def_q / (TIME - T_start));

    if (n_count + MEM_wait + CPU_wait + IO_wait + DEF_wait != proc_num)
    {
	printf ("Er klopt iets niet met het totaal aantal processen,\n"
		"is een van de rijen misschien verstoord?\n");
	printf ("Geteld: %ld, verwacht: %ld \n",
		n_count + MEM_wait + CPU_wait + IO_wait + DEF_wait, proc_num);
    }
    histogram (T_mem_alloc, "wachttijd op geheugentoewijzing");
    histogram (T_first_CPU, "wachttijd op eerste CPU cycle");
    histogram (T_execution, "executie-tijd vanaf geheugentoewijzing");
    histogram (T_turn_ar, "totale verwerkingstijd");

    printf ("\nEinde statistieken ----------\n\n");
    finale ();
}

static void
NewProcess (pcb ** new_proc)
{
    /*
       Select the next new process from the list of creatable processes
     */

    pcb    *new_pcb, *tmp1, *tmp2;
    my_pcb *new_my;
    double CPU_factor;
    int    i;

    tmp1 = tmp2 = *new_proc;

    /*
       Create and initialize pcb structures
     */

    new_pcb = (pcb *) malloc (sizeof (pcb));
    new_my = (my_pcb *) malloc (sizeof (my_pcb));
    *new_my = request[next_request];

    /* Generate CPU need */
    CPU_factor = 4.0;
    i = genrand_int31() % 32;
    i >>= 3;
    while (i--) CPU_factor *= 3.0;
    new_my->CPU_need = CPU_factor * (0.5 + genrand_real1());

    new_my->IO_cycles = 2 * (1 + genrand_int31() % 10);
    new_my->CPU_burst = (new_my->CPU_need) / (1 + new_my->IO_cycles);
    new_my->CPU_burst *= (0.8 + 0.4 * genrand_real1());

    new_pcb->SIM_pcb = (void *) new_my;
    i = 1 + (genrand_int31() % MEM_range);
    new_pcb->MEM_need = new_my->MEM_need = MEM_min + (genrand_int31 () % i);
    if (new_pcb->MEM_need > (3 * MEM_SIZE) / 4)
    {
	new_pcb->MEM_need = new_my->MEM_need = (3 * MEM_SIZE) / 4;
    }
    new_pcb->MEM_base = -1;
    new_pcb->proc_num = new_my->proc_num = proc_num++;
    new_pcb->your_admin = NULL;
    new_my->T_create = TIME;
    new_my->stud_pcb = new_pcb;

    /*
       Tie pcb structures into various queues
     */

    while (tmp1)
    {
	tmp2 = tmp1;
	tmp1 = tmp1->next;
    }
    if (tmp2)
    {
	tmp2->next = new_pcb;
	new_pcb->prev = tmp2;
    } else
    {
	*new_proc = new_pcb;
	new_pcb->prev = NULL;
    }
    new_pcb->next = NULL;

    if (first)
    {
	last->next = new_my;
	new_my->prev = last;
	last = new_my;
    } else
    {
	first = last = new_my;
	new_my->prev = NULL;
    }
    new_my->next = NULL;

    /*
       Determine when next new process is to arrive
     */

    next_request = genrand_int31 () % N_requests;
    T_next_new = TIME + T_delay[next_request] / LOAD_FACTOR;
}

static void
CheckAll ()
{
    my_pcb *my;
    pcb    *stud;
    long    i;

    my = first;
    while (my)
    {
	stud = my->stud_pcb;
	if (my->MEM_base != stud->MEM_base)
	{
	    if (my->MEM_base <= 0)
	    {
		/*
		   Memory appears to have been allocated...
		 */
		my->MEM_base = stud->MEM_base;
		my->T_mem_alloc = TIME;
		MEM_in_use += my->MEM_need;
	    }
	}
	my->in_queue = NULL;
	my = my->next;
    }
    stud = new_proc;
    len_new_q = 0;
    while (stud)
    {
	len_new_q++;
	my = (my_pcb *) stud->SIM_pcb;
	my->in_queue = new_proc;
	stud = stud->next;
    }
    stud = ready_proc;
    len_cpu_q = 0;
    while (stud)
    {
	len_cpu_q++;
	my = (my_pcb *) stud->SIM_pcb;
	my->in_queue = ready_proc;
	if (my->state == init)
	{
	    my->state = ready;
	}
	stud = stud->next;
    }
    stud = io_proc;
    for (i = 0; i < IO_COUNT; i++)
    {
	len_io_q[i] = 0;
    }
    while (stud)
    {
	my = (my_pcb *) stud->SIM_pcb;
	my->in_queue = io_proc;
	len_io_q[my->io_q]++;
	stud = stud->next;
    }
    stud = defunct_proc;
    len_def_q = 0;
    while (stud)
    {
	len_def_q++;
	my = (my_pcb *) stud->SIM_pcb;
	my->in_queue = defunct_proc;
    }
    my = first;
    while (my)
    {
	if (!(my->in_queue))
	{
	    n_msgs++;
	    printf ("Proces no. %ld bestaat nog, maar zit in geen enkele",
		    my->proc_num);
	    printf (" queue\n");
	    stud = my->stud_pcb;
	    switch (my->state)
	    {
	    case init:
		printf ("Het betreft een nieuw proces\n");
		stud->next = new_proc;
		if (new_proc)
		{
		    new_proc->prev = stud;
		}
		new_proc = stud;
		stud->prev = NULL;
		break;
	    case ready:
		printf ("Het betreft een ready proces\n");
		stud->next = ready_proc;
		if (ready_proc)
		{
		    ready_proc->prev = stud;
		}
		ready_proc = stud;
		stud->prev = NULL;
		break;
	    case io:
		printf ("Het proces is bezig met I/O\n");
		stud->next = io_proc;
		if (io_proc)
		{
		    io_proc->prev = stud;
		}
		io_proc = stud;
		stud->prev = NULL;
		break;
	    case defunct:
		printf ("Het proces is defunct\n");
		stud->next = defunct_proc;
		if (defunct_proc)
		{
		    defunct_proc->prev = stud;
		}
		defunct_proc = stud;
		stud->prev = NULL;
		break;
	    default:
		printf ("My fault....\n");
		print_statistics ();
		exit (0);
	    }
	    my->in_queue = stud;
	}
	my = my->next;
    }
    stud = ready_proc;
    if (stud)
    {
	my = (my_pcb *) stud->SIM_pcb;
	CPU = my;
    } else
    {
	CPU = NULL;
    }
}

static void
ReadyProcess (pcb ** ready_proc, pcb ** io_proc)
{

    /*
     * How do we indicate that a process is ready? Its state should be ready
     * - even though the process is still in the I/O queue
     */

    pcb    *stud, *last_ready, *next;
    my_pcb *my;

    /* Find the last process in the ready queue, to append new
       ready processes to... */

    stud = *ready_proc;
    last_ready = stud;
    while (stud)
    {
	last_ready = stud;
	stud = stud->next;
    }

    /* Now get all ready processes from the IO queues and move them
       to the end of the ready queue */

    stud = *io_proc;
    while (stud)
    {
	my = (my_pcb *) stud->SIM_pcb;
	next = stud->next;
	if (my->state == ready)
	{
	    my->CPU_burst = (my->CPU_need - my->CPU_used) /
			(1 + my->IO_cycles);
	    my->CPU_burst *= (0.6 + 0.8 * genrand_real1());
	    if (stud == *io_proc)
	    {
		*io_proc = next;
	    } else
	    {
		stud->prev->next = next;
	    }
	    if (stud->next)
	    {
		next->prev = stud->prev;
	    }
	    if (last_ready)
	    {
		last_ready->next = stud;
		stud->prev = last_ready;
		stud->next = NULL;
	    } else
	    {
		*ready_proc = stud;
		stud->prev = stud->next = NULL;
	    }
	    last_ready = stud;
	    my->in_queue = *ready_proc;
	}
	stud = next;
    }
}

static void
IOProcess (pcb ** io_proc, pcb ** ready_proc)
{

    /*
     * How do we indicate that a process wants to do I/O? Simple - the only
     * process that could want to switch is the currently executing process.
     * If this procedure is called, that process wants to switch.
     */

    pcb    *stud, *last_io, *next;
    my_pcb *my;

    stud = *io_proc;
    last_io = stud;
    while (stud)
    {
	last_io = stud;
	stud = stud->next;
    }

    my = CPU;
    stud = my->stud_pcb;
    my->state = io;
    *ready_proc = next = stud->next;
    if (next)
    {
	next->prev = NULL;
    }
    if (last_io)
    {
	last_io->next = stud;
	stud->prev = last_io;
	stud->next = NULL;
    } else
    {
	*io_proc = stud;
	stud->prev = stud->next = NULL;
    }
    my->in_queue = *io_proc;
}

static void
FinishProcess (pcb ** defunct_proc, pcb ** ready_proc)
{

    /*
     * How do we indicate that a process wants to do quit? Simple - the only
     * process that could want to quit is the currently executing process. If
     * this procedure is called, that process wants to quit.
     */

    pcb    *stud, *next;
    my_pcb *my;


    my = CPU;
    stud = my->stud_pcb;
    my->state = defunct;
    *ready_proc = next = stud->next;
    if (next)
    {
	next->prev = NULL;
    }
    stud->next = *defunct_proc;
    if (stud->next)
    {
	stud->next->prev = stud;
    }
    *defunct_proc = stud;
    my->in_queue = *defunct_proc;
}

static void
PostNew ()
{

    /*
     * A new process was created and the student scheduler has been called.
     * The only plausible possible action would be the allocation of memory
     * to this process. Yet, we will first check the consistency of all pcbs
     * etc
     */

    CheckAll ();
}

static void
PostTime ()
{
    CheckAll ();
}

static void
DoIO ()
{

    /*
     * We should find out if there are any I/O devices free, and if so, if
     * there are processes waiting for that particular device
     */
    long    i;
    my_pcb *my;
    pcb    *stud;

    for (i = 0; i < IO_COUNT; i++)
    {
/******************************************************************
   IO_burst[0] 3 always
   IO_burst[1] 1 -- 5 uniform, avg 3.
   IO_burst[2] 4 -- 16 uniform, avg (10),

   i.e. only device 2 can lead to IO-saturation. multiply by a time factor to
   allow scaling w.r.t. CPU load.
*******************************************************************/
	if (IO[i] == NULL)
	{
	    stud = io_proc;
	    while (stud)
	    {
		my = (my_pcb *) stud->SIM_pcb;
		if (my->io_q == i)
		{
		    IO[i] = my;
		    my->T_IO = TIME;
		    if (my->IO_cycles < 1)
			my->IO_cycles = 1;
		    switch (i)
		    {
		case 0:
		        my->IO_burst[i] = 3.0;
			break;
		case 1:
			my->IO_burst[i] = 1.0 + 4.0 * genrand_real1();
			break;
		case 2:
			my->IO_burst[i] = 4.0 + 12.0 * genrand_real1();
			break;
		    }
		    my->IO_burst[i] *= IO_timefactor;
		    my->IO_cycles -= 1;
		    break;	/* while (stud) */
		}
		stud = stud->next;
	    }
	}
    }
}

static void
PostReady ()
{
    CheckAll ();
    DoIO ();
}

static void
PostIO ()
{
    CheckAll ();
    DoIO ();
}

static void
PostFinish ()
{
    CheckAll ();
}

static  event_type
FindNextEvent ()
{
/* The behaviour of the various processes generating events may differ.
   The NewProcess process will run at a fixed time - that is easy at least
   Each of the I/O processes will complete at a predictable moment, they
   will continue processing the associated task until finished.
   The CPU process will always process the task at the head of the
   ready-list (if it exists) until the next event. We will see again
   later */

    double  T_next, T;
    my_pcb *my, *next_proc;
    event_type next_event;
    long    i;

    next_event = NewProcess_event;
    T_next = T_next_new;
    next_proc = NULL;

    if (T_slice < T_next)
    {
	next_event = Time_event;
	T_next = T_slice;
	next_proc = CPU;
    }
    my = CPU;
    if (my)
    {
	T = TIME + my->CPU_burst;
	if (T < T_next)
	{
	    next_proc = my;
	    T_next = T;
	    if (my->CPU_burst + my->CPU_used >= my->CPU_need)
	    {
		next_event = Finish_event;
	    } else
	    {
		next_event = IO_event;
	    }
	}
    }
    for (i = 0; i < IO_COUNT; i++)
    {
	my = IO[i];
	if (my)
	{
	    T = my->IO_burst[i] + my->T_IO;
	    if (T < T_next)
	    {
		T_next = T;
		next_proc = my;
		next_event = Ready_event;
	    }
	}
    }

    /*
     * Now we know which event will be next. Whatever the next event will be,
     * we must advance the task on the CPU in order to keep things
     * consistent. Also record some statistics.
     */

    T_inc = T_next - TIME;
    if (CPU)
    {
	if (CPU->CPU_used == 0)
	{
	    CPU->T_CPU = TIME;
	}
	if (get_stats)
	{
	    CPU_util += T_inc;
	}
	CPU->CPU_used += T_inc;
	CPU->CPU_burst -= T_inc;
	if (CPU->CPU_burst < eps)
	{
	    CPU->CPU_burst = 0;
	}
    }

    /*
     * In case of a ready event, we will now mark the I/O device as empty
     * and the associated process as ready. Record some more statistics
     */

    if (get_stats)
    {
	for (i = 0; i < IO_COUNT; i++)
	{
	    if (IO[i])
		IO_util[i] += T_inc;
	    if (max_io_q[i] < len_io_q[i])
		max_io_q[i] = len_io_q[i];
	    av_io_q[i] += T_inc * len_io_q[i];
	}
	MEM_util += T_inc * MEM_in_use;
	av_new_q += T_inc * len_new_q;
	av_cpu_q += T_inc * len_cpu_q;
	av_def_q += T_inc * len_def_q;
	if (max_new_q < len_new_q)
	    max_new_q = len_new_q;
	if (max_cpu_q < len_cpu_q)
	    max_cpu_q = len_cpu_q;
	if (max_def_q < len_def_q)
	    max_def_q = len_def_q;
    }
    if (next_event == Ready_event)
    {
	i = next_proc->io_q;
	IO[i] = NULL;
	next_proc->state = ready;
	next_proc->IO_used[i] += next_proc->IO_burst[i];
	next_proc->io_q = (i + 1) % IO_COUNT;
    }
    TIME = TIME + T_inc;

    return (next_event);
}

long
rm_process (pcb ** process)
{
    my_pcb *my;
    pcb    *stud;
    long    pn;

    stud = *process;
    my = (my_pcb *) stud->SIM_pcb;
    pn = n_samples % N_SAMPLES;
    n_samples++;
    n_count++;
    T_mem_alloc[pn] = my->T_mem_alloc - my->T_create;
    T_first_CPU[pn] = my->T_CPU - my->T_create;
    T_execution[pn] = TIME - my->T_mem_alloc;
    T_turn_ar[pn] = TIME - my->T_create;
    MEM_in_use -= my->MEM_need;

    /*
       Remove stud from its queue
     */

    if (defunct_proc == stud)
    {
	defunct_proc = stud->next;
	if (defunct_proc)
	{
	    defunct_proc->prev = NULL;
	}
    } else
    {
	stud->prev->next = stud->next;
	if (stud->next)
	{
	    stud->next->prev = stud->prev;
	}
    }
    free (stud);
    stud = *process = NULL;

    if (my->prev)
    {
	my->prev->next = my->next;
    } else
    {
	first = my->next;
    }
    if (my->next)
    {
	my->next->prev = my->prev;
    } else
    {
	last = my->prev;
    }
    free (my);
    return (0);
}

int ntry, choice;
double rr;
int main (int argc, char *argv[])
{
/* The simulation program had better be implemented as a good, oldfashioned
   discrete-event simulator

   At each event it should decide which of the systemprocessors should
   execute. These processors are:

   JES - the job-entry system
   CPU - the CPU
   IOn - the n identical IO systems.

   Upon completion these system processors will generate one of the
   five possible scheduler events:

   NewProcess_event
   Time_event
   Ready_event
   IO_event
   Finish_event

   This event will be passed to the scheduler, after which the
   scheduler can rearrange the process queues.

   Inspection of the process queues will indicate which system-event
   will occur next.

   The problem that we face here is that at every call to the scheduler,
   the current process may have been preempted.

   Preemption of the CPU implies an immediate CPU event, affecting the
   previously current process.

   In order to obtain a nice uniform structure, we will describe
   each system-processor with two functions - the event-executer.
   */

    long    N_to_create = 7;
    long    ranseed = 1579;
    int rv;
    char dummy;

    if (argc > 1)
    {
        ranseed = atoi(argv[1]);
    }
    printf ("Simulatie van geheugen-toewijzing en proces-scheduling\n");
    printf ("Versie 2013-2014\n");
    do {

        printf ("Geef de gewenste CPU belasting (tussen 0 en 1):");
        LOAD_FACTOR = 0.75;
        rv = scanf ("%lf", &LOAD_FACTOR);
        if (rv < 1)
        {
            /* skip invalid input */
            rv = scanf("%c", &dummy);
        }
        printf ("Gelezen waarde: %f\n", LOAD_FACTOR);
    } while ((0 >= LOAD_FACTOR) || (1.0 <= LOAD_FACTOR));
    do {
    	printf ("Geef de gewenste IO belasting (tussen 0 en 1):");
        IO_timefactor = 0.7;
    	rv = scanf ("%lf", &IO_timefactor);
        if (rv < 1)
        {
            /* skip invalid input */
            rv = scanf("%c", &dummy);
        }
    	printf ("Gelezen waarde: %f\n", IO_timefactor);
    } while ((0 >= IO_timefactor) || (1.0 <= IO_timefactor));
    IO_timefactor /= LOAD_FACTOR;
    do {
    	printf ("Geef de gewenste geheugenbelasting (tussen 0 en 1):");
        MEM_load = 0.8;
    	rv = scanf ("%lf", &MEM_load);
        if (rv < 1)
        {
            /* skip invalid input */
            rv = scanf("%c", &dummy);
        }
    	printf ("Gelezen waarde: %f\n", MEM_load);
    } while ((0 >= MEM_load) || (1.0 <= MEM_load));

    printf ("Geef aantal aan te maken processen:");
    rv = scanf ("%ld", &N_to_create);
    if (rv < 1)
    {
        /* skip invalid input */
        rv = scanf("%c", &dummy);
    }

    //nTry scanf
    do {
        printf ("Geef de gewenste nTry (tussen 0 en INT_MAX, 0 = geen ntry):");
        rv = scanf ("%d", &ntry);
        if(rv < 1){
            /* skip invalid input */
            rv = scanf("%c", &dummy);
        }
        printf ("Gelezen waarde: %d\n", ntry);
    } while (ntry < 0 || ntry > INT_MAX);


    do {
        printf ("Geef de gewenste Set Slice (tussen 0 en DBL_MAX):");
        rv = scanf ("%lf", &rr);
        if(rv < 1){
            /* skip invalid input */
            rv = scanf("%c", &dummy);
        }
        printf ("Gelezen waarde: %lf\n", rr);
    } while (rr < 0 || rr > DBL_MAX);

    do {
        printf("Geef de gewenste algoritme keus:");
        printf ("%s\n", "Geef de gewenste algoritme, kies uit:");
        printf("%s", "1. FCFS, 2. Round Robin, 3. Shortest Job First, 4. Biggest Job First: ");
        rv = scanf ("%d", &choice);
        if(rv < 1){
            /* skip invalid input */
            rv = scanf("%c", &dummy);
        }
        printf ("Gelezen waarde: %d\n", choice);
    } while (choice < 1 || choice > 4);

    printf ("Gelezen waarde: %ld\n", N_to_create);
    N_to_create = (N_to_create < 5) ? 5 : ((N_to_create > N_SAMPLES) ?
					   N_SAMPLES : N_to_create);
    printf ("Gebruikte waarde: %ld\n", N_to_create);

    init_genrand (ranseed);
    my_init_sim ();

    new_proc = io_proc = ready_proc = defunct_proc = NULL;
    T_start = 0;

    finale = sluit_af;
    reset_stats = my_reset_stats;

    cur_event = NewProcess_event;

    while (proc_num < 100)
    {
	switch (cur_event)
	{
	case NewProcess_event:

	    /*
	     * We must create a new process and add it to the new process
	     * queue, awaiting the allocation of memory
	     */

	    NewProcess (&new_proc);
	    schedule (cur_event);

	    /*
	     * Find out what the student has done, and schedule the next
	     * event accordingly. Report any illogical actions or errors
	     */

	    PostNew ();
	    break;
	case Time_event:

	    /*
	     * A process has used up its time-slice. The scheduler can
	     * re-arrange the ready queue if it wants. We need not do
	     * anything, except reset the timer
	     */

	    set_slice (9.9e12);
	    schedule (cur_event);
	    PostTime ();
	    break;
	case Ready_event:

	    /*
	     * A process has finished its I/O action. We move it from the I/O
	     * queue to the ready queue and call the scheduler
	     */

	    ReadyProcess (&ready_proc, &io_proc);
	    schedule (cur_event);
	    PostReady ();
	    break;
	case IO_event:

	    /*
	     * The currently executing process starts I/O We move it from the
	     * head of the ready queue to the io queue
	     */

	    IOProcess (&io_proc, &ready_proc);
	    schedule (cur_event);
	    PostIO ();
	    break;
	case Finish_event:

	    /*
	     * The currently executing process has finished. We move it from
	     * the ready queue to the defunct queue, awaiting reclamation of
	     * its memory
	     */

	    FinishProcess (&defunct_proc, &ready_proc);
	    schedule (cur_event);

	    /*
	     * After this operation the defunct queue should, once again, be
	     * empty.
	     */

	    PostFinish ();
	    break;
	default:
	    /*
	       This should never ever happen
	     */

	    printf ("Het simulatie programma kent event nr %d niet\n",
		    cur_event);
	    print_statistics ();
	    exit (0);
	}

	cur_event = FindNextEvent ();
	if (n_msgs > MAX_MES)
	{
	    printf ("\n*****************************************\n%s\n",
		    "Te veel fouten - programma wordt afgebroken");
	    break;
	}
    }
    n_samples = 0;
    T_start = TIME;
    reset_stats ();
    get_stats = 1;
    while (proc_num < N_to_create + 100)
    {
	switch (cur_event)
	{
	case NewProcess_event:

	    /*
	     * We must create a new process and add it to the new process
	     * queue, awaiting the allocation of memory
	     */

	    NewProcess (&new_proc);
	    schedule (cur_event);

	    /*
	     * Find out what the student has done, and schedule the next
	     * event accordingly. Report any illogical actions or errors
	     */

	    PostNew ();
	    break;
        case Time_event:

            /*
             * A process has used up its time-slice. The scheduler can
             * re-arrange the ready queue if it wants. We need not do
             * anything, except reset the timer
             */

            set_slice (9.9e12);
            schedule (cur_event);
            PostTime ();
            break;
        case Ready_event:

            /*
             * A process has finished its I/O action. We move it from the I/O
             * queue to the ready queue and call the scheduler
             */

            ReadyProcess (&ready_proc, &io_proc);
            schedule (cur_event);
            PostReady ();
            break;
        case IO_event:

            /*
             * The currently executing process starts I/O We move it from the
             * head of the ready queue to the io queue
             */

            IOProcess (&io_proc, &ready_proc);
            schedule (cur_event);
            PostIO ();
            break;
        case Finish_event:

	    /*
	     * The currently executing process has finished. We move it from
	     * the ready queue to the defunct queue, awaiting reclamation of
	     * its memory
	     */

	    FinishProcess (&defunct_proc, &ready_proc);
	    schedule (cur_event);

	    /*
	     * After this operation the defunct queue should, once again, be
	     * empty.
	     */

	    PostFinish ();
	    break;
	default:
	    /*
	       This should never ever happen
	     */

	    printf ("Het simulatie programma kent event nr %d niet\n",
		    cur_event);
	    print_statistics ();
	    exit (0);
	}

	cur_event = FindNextEvent ();
	if (n_msgs > MAX_MES)
	{
	    printf ("\n*****************************************\n%s\n",
		    "Te veel fouten - programma wordt afgebroken");
	    break;
	}
    }
    print_statistics ();
    return ((n_msgs > MAX_MES));
}
