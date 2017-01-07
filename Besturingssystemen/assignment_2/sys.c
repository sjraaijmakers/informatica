#include "myalloc.h"
#include "xcheck.h"
#include <assert.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <limits.h>
#include <math.h>

/*
  2 roles of the simulator:
  1) compute / maintain metrics
  2) check consistency

  For #1 we use a set of simple counters (stat_alloc, stat_free etc),
  a set of watermarks (stat_max*), and a set of running time-integrals
  (stat_t*) to compute final average values. See below and sim_report()
  for details.

  For #2 we use a couple of strategies:
  - an array of bank metadata structs to check bank allocations/deallocations
    and magic markers to support: a) the visualization of which banks are actually accessed
    and b) illegal accesses to bank deallocations.
  - an array of word metadata structs to check that allocation ranges do not overlap.
 */


// private data:
static struct ram_info* pinfo = 0;

struct bank_info {
    bool active;
    unsigned char magic;
    size_t stat_activate;
    size_t stat_deactivate;
    size_t last_stime;
};
// size: nmodules * nbanks_per_module
static struct bank_info *bank_info = 0;

struct word_info {
    void *start;
    bool allocated;
};
// size: nomdules * nbanks_per_module * bank_size / sizeof(intmax_t)
static struct word_info *word_info = 0;
static size_t words_per_module = 0;

// flags
static size_t visualize = 0, last_viz = 0;
static bool trace = false;
static bool found_error = false;

// render buffer for the module visualization
static char *render = 0;
static size_t render_size = 0;


#define ACT_TIME 200

static size_t current_stime = 0;
static size_t last_stime = 0;
// "simple" computation steps, used to compute stime, also perf (op/...)
static size_t stat_steps = 0;

// number of currently allocated bytes, per module; also used to compute failed allocations
static size_t* cur_alloc;
// failed allocations despite free space
static size_t stat_failed_allocs = 0;

// simple counters for final statistics
static size_t stat_alloc = 0, stat_free = 0;

// integral/max external overhead: total activated banks minus currently allocated
static size_t stat_tovh = 0,  stat_maxovh = 0;
// integral/max module utilization spread
static float stat_stddevmax = 0, stat_tstddev = 0;
// integral/max bank utilization
static size_t stat_tactive = 0, stat_activemax = 0;

// logfile
static FILE *logfile;


// private functions:
static bool ispowerof2(size_t x);
static void reset_banks(void);
static void scan_modules(size_t *);

// public API:
const struct ram_info* hw_raminfo(void)
{
    xcheckf(pinfo != 0, "hw_raminfo can only be called after hw_init");
    return pinfo;
}

void sim_init(size_t nmodules, size_t nbanks_per_module,
              size_t bank_size, size_t viz, bool tr, FILE *logf)
{
    size_t i;

    logfile = logf;

    xcheckf(pinfo == 0, "hw_init can only be called once");

    // Alloc space for the RAM descriptor.

#define msim "hw_init: failed to allocate memory"
    xcheckf((pinfo = malloc(sizeof(*pinfo))) != 0, msim);

    // Store the configuration.

    pinfo->bank_size = bank_size;
    xcheck(0 != pinfo->bank_size);
    xcheck(pinfo->bank_size >= sizeof(intmax_t) && ispowerof2(pinfo->bank_size));

    pinfo->nbanks_per_module = nbanks_per_module;
    xcheck(0 != pinfo->nbanks_per_module);

    pinfo->nmodules = nmodules;
    xcheck(0 != pinfo->nmodules);

    visualize = viz;
    trace = tr;

    // Allocate and initialize the simulation structures.

    void **parr;
    xcheckf((parr = malloc(sizeof(void*) * pinfo->nmodules)) != 0, msim);
    for (i = 0; i < pinfo->nmodules; ++i)
    {
        xcheckf(0 == posix_memalign(parr + i, pinfo->bank_size,
                                    pinfo->bank_size * pinfo->nbanks_per_module), msim);
    }
    pinfo->module_addrs = parr;

    // allocated bytes per module
    xcheckf(0 != (cur_alloc = calloc(pinfo->nmodules, sizeof cur_alloc[0])), msim);
    // bank metadata
    xcheckf(0 != (bank_info = calloc(pinfo->nmodules * pinfo->nbanks_per_module, sizeof bank_info[0])), msim);
    // word metadata
    words_per_module = pinfo->nbanks_per_module * pinfo->bank_size / sizeof(intmax_t);
    xcheckf(0 != (word_info = calloc(pinfo->nmodules * words_per_module, sizeof word_info[0])), msim);

    reset_banks();

    // viz array
    render_size = pinfo->nmodules * (2 + pinfo->nbanks_per_module) + 2;
    xcheckf(0 != (render = malloc(render_size)), msim);

#undef msim

    if (visualize)
        fprintf(logfile, "Visualization columns: time, modules/banks, #active, overhead, overhead per allocated byte\n");
}

static
void reset_banks(void)
{
    for (size_t m = 0; m < pinfo->nmodules; ++m)
        for (size_t b = 0; b < pinfo->nbanks_per_module; ++b)
            if (!bank_info[m * pinfo->nbanks_per_module + b].active)
            {
                for (size_t w = m * words_per_module + b * pinfo->bank_size / sizeof(intmax_t);
                     w < m * words_per_module + (b + 1) * pinfo->bank_size / sizeof(intmax_t);
                     ++w)
                    xcheckf(!word_info[w].allocated,
                            "found allocated region "
                            "at offset %p in deactivated bank %zu of module %zu", (void*)(w*sizeof(intmax_t)), b, m);

                unsigned char magic;
                do magic = rand() % 256; while (magic == 42);

                memset((char*)pinfo->module_addrs[m] + b * pinfo->bank_size, (int)magic, pinfo->bank_size);
                bank_info[m * pinfo->nbanks_per_module + b].magic = magic;
            }
}


void hw_activate(size_t module, size_t bank)
{
    xcheckf(pinfo != 0, "hw_activate: cannot call before hw_init");
    xcheckf(module < pinfo->nmodules, "hw_activate: invalid module %zu", module);
    xcheckf(bank < pinfo->nbanks_per_module, "hw_activate: invalid bank %zu", bank);

    size_t offset = module * pinfo->nbanks_per_module + bank;

    bool already_active = bank_info[offset].active;
    xcheckf(!already_active, "hw_activate: bank %zu of module %zu already active", bank, module);
    bank_info[offset].active = true;
    ++bank_info[offset].stat_activate;

    if (bank_info[offset].last_stime + (unsigned)ACT_TIME > current_stime)
        // first "wait" for previous deactivate to finish, if any
        current_stime += ACT_TIME - (current_stime - bank_info[offset].last_stime);
    bank_info[offset].last_stime = current_stime;
    current_stime += ACT_TIME; // activate always waits

    reset_banks();
}

void hw_deactivate(size_t module, size_t bank)
{
    xcheckf(pinfo != 0, "hw_deactivate: cannot call before hw_init");
    xcheckf(module < pinfo->nmodules, "hw_deactivate: invalid module %zu", module);
    xcheckf(bank < pinfo->nbanks_per_module, "hw_deactivate: invalid bank %zu", bank);

    size_t offset = module * pinfo->nbanks_per_module + bank;

    bool already_active = bank_info[offset].active;
    xcheckf(already_active, "hw_deactivate: bank %zu of module %zu already inactive", bank, module);
    bank_info[offset].active = false;
    ++bank_info[offset].stat_deactivate;

    if (bank_info[offset].last_stime + (unsigned)ACT_TIME > current_stime)
        // first "wait" for previous deactivate to finish, if any
        current_stime += ACT_TIME - (current_stime - bank_info[offset].last_stime);
    bank_info[offset].last_stime = current_stime;

    reset_banks();
}

static
size_t toggle_check_allocated(void *p, size_t sz, bool check, const char *fmt)
{
    size_t m;
    for (m = 0; m < pinfo->nmodules; ++m)
        if ((char*)p >= (char*)pinfo->module_addrs[m] &&
            (char*)p < (char*)pinfo->module_addrs[m] + pinfo->bank_size * pinfo->nbanks_per_module)
            break;
    xcheckf(m < pinfo->nmodules, "mm_alloc/mm_free: %p lies outside of all modules", p);
    xcheckf((char*)p + sz <= (char*)pinfo->module_addrs[m] + pinfo->bank_size * pinfo->nbanks_per_module,
            "mm_alloc/mm_free: allocated range (%p, size %zu) extends past boundary of module %zu",
            p, sz, m);

    size_t woffset_in_module = ((char*)p - (char*)pinfo->module_addrs[m]) / sizeof(intmax_t);
    size_t wi_offset = m * words_per_module + woffset_in_module;

    xcheckf(word_info[wi_offset].allocated == check, fmt, p);

    // mark all words in region
    for (size_t w = wi_offset; w < wi_offset + (sz + sizeof(intmax_t) - 1) / sizeof(intmax_t); ++w)
    {
        if (check)
        {
            // check for already alloc: also check the start pointer is OK, then erase it
            xcheckf(word_info[w].start == p,
                    "mm_free tries to free word %p, which is part of another region starting at %p",
                    p, word_info[w].start);
            word_info[w].start = 0;
        }
        else
        {
            // check for already free: mark the start pointer
            word_info[w].start = p;
        }
        word_info[w].allocated = !check;
    }

    return m;
}

void sim_count_alloc(void *p, size_t sz)
{
    if (trace)
        fprintf(logfile, "mm_alloc(st, %zu); // -> %p\n", sz, p);

    ++stat_alloc;

    if (p)
    {
        // is aligned?
        xcheckf((uintptr_t)p % sizeof(intmax_t) == 0, "mm_alloc: %p not aligned", p);

        // ensure this was free until now
        size_t m = toggle_check_allocated(p, sz, false, "mm_alloc: %p is already allocated");

        // then keep count of total allocated bytes for stats
        cur_alloc[m] += sz;
    }
    else
    {
        // only count a failed alloc if it could theoretically fit the remaining space
        // in any of the modules. So we need to check that it does not fit in all modules.
        bool can_fit = false;
        for (size_t m = 0; m < pinfo->nmodules; ++m)
            if (sz < (pinfo->bank_size * pinfo->nbanks_per_module) - cur_alloc[m])
            {
                can_fit = true;
                break;
            }
        if (can_fit)
            ++stat_failed_allocs;
    }

}

void sim_count_free(void *p, size_t sz)
{
    if (trace)
        fprintf(logfile, "mm_free(st, %p);\n", p);

    ++stat_free;

    if (p)
    {
        // ensure this was allocated until now
        size_t m = toggle_check_allocated(p, sz, true, "mm_free: %p is already free");

        xcheckf(cur_alloc[m] >= sz, "trying to free more bytes (%zu) in module %zu than currently allocated (%zu)", sz, m, cur_alloc[m]);
        cur_alloc[m] -= sz;
    }
}

void sim_check(void)
{
    xcheck(pinfo != 0);

    ++stat_steps;
    ++current_stime;

    // Compute current time
    size_t stime = current_stime;
    size_t time_delta = stime - last_stime;
    last_stime = stime;

    // Active banks
    size_t activepm[pinfo->nmodules];
    scan_modules(activepm);
    size_t active = 0;
    for (size_t i = 0; i < pinfo->nmodules; ++i)
        active += activepm[i];

    stat_tactive += active * time_delta;
    if (active > stat_activemax)
        stat_activemax = active;

    // Imbalance across modules

    float mean = (float)active / pinfo->nmodules;
    float expected = 0;
    for (size_t i = 0; i < pinfo->nmodules; ++i)
    {
        float x = (float)activepm[i] - mean;
        expected += x * x;
    }
    float stddev = sqrt(expected);

    stat_tstddev += stddev * time_delta;
    if (stddev > stat_stddevmax)
        stat_stddevmax = stddev;

    // Overhead sum / max
    size_t total_alloc = 0;
    for (size_t m = 0; m < pinfo->nmodules; ++m)
        total_alloc += cur_alloc[m];
    size_t overhead = active * pinfo->bank_size - total_alloc;
    stat_tovh += overhead * time_delta;
    if (overhead > stat_maxovh)
        stat_maxovh = overhead;

    if (visualize && (last_viz + visualize) <= stat_steps)
    {
        float ovhpb = total_alloc ? (float)overhead/(float)total_alloc : (float)overhead;
        fprintf(logfile, "%6zu %s %zu %zu %.2f\n", stime, render, active, overhead, ovhpb);
        last_viz = stat_steps;
    }

    if (found_error)
    {
        fprintf(stderr, "Found consistency error, aborting simulation.\n");
        abort();
    }
}

void sim_report(void)
{
    size_t stime = last_stime;

    size_t stat_activate = 0, stat_deactivate = 0;
    for (size_t i = 0; i < pinfo->nmodules * pinfo->nbanks_per_module; ++i)
    {
        stat_activate += bank_info[i].stat_activate;
        stat_deactivate += bank_info[i].stat_deactivate;
    }

    fprintf(logfile, "## end of simulation ##\n");
    fprintf(logfile, "# cnt mm_alloc/mm_free: %zu/%zu\n", stat_alloc, stat_free);
    fprintf(logfile, "# cnt failed allocations despite free space: %zu (%.1f%%)\n", stat_failed_allocs,
            100. * (float)stat_failed_allocs / (float)stat_alloc);
    fprintf(logfile, "# cnt hw_activate/hw_deactivate: %zu/%zu\n", stat_activate, stat_deactivate);
    fprintf(logfile, "# max number of banks active: %zu\n", stat_activemax);
    fprintf(logfile, "# cnt banks left activated at end: %zu\n", stat_activate - stat_deactivate);
    fprintf(logfile, "# avg external overhead (bytes): %.3f\n", (float)stat_tovh / (float)stime);
    fprintf(logfile, "# max external overhead (bytes): %zu\n", stat_maxovh);
    if (pinfo->nmodules > 1)
    {
        fprintf(logfile, "# avg module imbalance factor: %.3f\n", stat_tstddev / (float)stime);
        fprintf(logfile, "# max module imbalance factor: %.3f\n", stat_stddevmax);
    }
    fprintf(logfile, "# total time (virtual ms): %zu\n", stime);
    fprintf(logfile, "# total energy usage (virtual mJ): %zu\n", stat_tactive);
    float w = 1e-3 * (float)stat_tactive / (1e-3 * (float)stime);
    fprintf(logfile, "# avg power usage (virtual W): %.3f\n", w);
    fprintf(logfile, "#\n");
    fprintf(logfile, "# performance (op / s): %.3f\n", 1000. * (float)stat_steps / (float)stime);
    fprintf(logfile, "# performance/watt (op / J): %.3f\n", 1000. * (float)stat_steps / (float)stat_tactive);
}

void sim_exit(void)
{
    xcheck(pinfo != 0);
    for (size_t i = 0; i < pinfo->nmodules; ++i)
        free(pinfo->module_addrs[i]);
    free((void*)pinfo->module_addrs);
    free(pinfo);
    pinfo = 0;

    free(bank_info);
    bank_info = 0;

    free(word_info);
    word_info = 0;

    free(render);
    render = 0;
}

static
bool check_bank_magic(size_t module, size_t bank, void *bp)
{
    unsigned char magic = bank_info[module * pinfo->nbanks_per_module + bank].magic;
    size_t lmagic = 0;
    for (size_t i = 0; i < sizeof(size_t); ++i)
        lmagic = (lmagic << 8) | magic;

    for (size_t *p = bp; (char*)p < (char*)bp + pinfo->bank_size; ++p)
        if (*p != lmagic)
            return false;
    return true;
}

static
void scan_modules(size_t nactive[])
{
    size_t m, b;
    char *s = render;
    bool access_to_deactivated_bank = false;

#define S (xcheck((s - render) < (long)render_size), s++)
    for (m = 0; m < pinfo->nmodules; ++m)
    {
        nactive[m] = 0;
        if (visualize) *S = '[';
        for (b = 0; b < pinfo->nbanks_per_module; ++b)
        {
            bool active = bank_info[m * pinfo->nbanks_per_module + b].active;
            char *p = (char*)pinfo->module_addrs[m] + b * pinfo->bank_size;
            if (active)
            {
                ++nactive[m];
                if (visualize) *S = check_bank_magic(m, b, p) ? 'o' : 'O';
            }
            else
            {
                bool check = check_bank_magic(m, b, p);
                if (visualize) *S = check ? '.' : 'X';
                if (!check)
                {
                    fprintf(stderr, "Detected invalid write to deactivated bank %zu in module %zu!\n", b, m);
                    access_to_deactivated_bank = true;
                }
            }
        }
        if (visualize) *S = ']';
    }
    if (visualize) *S = '\0';
#undef S

    found_error = found_error || access_to_deactivated_bank;
}

static inline
bool ispowerof2(size_t x)
{
    return ((x != 0) && !(x & (x - 1)));
}
