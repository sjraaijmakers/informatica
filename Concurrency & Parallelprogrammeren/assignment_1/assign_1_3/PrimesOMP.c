
// OpenMP example program

// finds the number of primes between 2 and n; uses the Sieve of
// Eratosthenes, deleting all multiples of 2, all multiples of 3, all
// multiples of 5, etc.; NOT EFFICIENT

// tested using the Omni compiler, omcc; not special #include or library
// linking needed; works even on uniprocessor systems, i.e.  still
// produces a threaded program

#include <stdio.h>
#include <math.h>

#define MAX_N 100000000
#define MAX_THREADS 100

// global variables, all shared by all threads by default
int n,  // range to check for primeness
    totwork,  // total number of bases checked
    // in the end, prime[i] = 1 if i 
    // prime, else 0
    prime[MAX_N+1],  
    nextbase,  // next sieve multiplier to be used
    dbg,  // debug flag (1 means yes)
    dbgwait=0;

// "crosses out" all odd multiples of k, from k*k on, k odd
void crossout(int k)
{  int i;

   for (i = k; i*k <= n; i += 2)  {
      prime[i*k] = 0;
   }
}

int worker()  
{  int lim,base,work,me,nth;

   me = omp_get_thread_num();
   nth = omp_get_num_threads();
   printf("%d  %d\n",me,nth);
   if (dbg) while (!dbgwait) ;
   // no need to check multipliers bigger than sqrt(n)
   lim = sqrt(n);
   // keep track of how much work this thread does
   work = 0;  
   do  {
      // get next sieve multiplier, avoiding duplication across threads
      // by placing an OpenMP critical section here
      #pragma omp critical
      {  base = nextbase += 2; }
      if (base <= lim)  {
         // record one more base done by this thread
         work++;  
         // don't bother with crossing out if base is a known composite
         if (prime[base])  
            crossout(base);
      }
      else return work; 
   } while (1);
}

int main(int argc, char **argv)
{  int nprimes,  // number of primes found 
       i;

   n = atoi(argv[1]);
   dbg = atoi(argv[2]);
   // assume all i prime until shown otherwise
   for (i = 2; i <= n; i++) prime[i] = 1;
   // cross out the evens separately
   for (i = 2; 2*i <= n; i++) prime[2*i] = 0;
   nextbase = 1;

   // split into parallel threads; see tutorial notes, end of this file
   #pragma omp parallel 
   {  totwork = worker(); 
      printf("%d values of base done\n",totwork);
   }
   // back to single thread now; report results; no need for barrier, as
   // all threads must have finished for us to reach this point
   nprimes = 0;
   for (i = 2; i <= n; i++)  
      if (prime[i]) nprimes++;
   printf("number of primes found:%d\n",
      nprimes);
}

// general notes:  

// the "parallel" clause says, "Have each thread do this block"
// (enclosed by braces); code not set up by a "parallel" pragma is done
// only by the master thread

// on some platforms, might need #pragma omp flush at the end of
// crossout()

