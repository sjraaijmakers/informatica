#include "myalloc.h"

struct mm_state
{
    size_t next;
};

mm_state_t* mm_initialize(void)
{
    const struct ram_info *info = hw_raminfo();

    // We use the first bank as storage for the management data
    // structure.
    char *base = (char*)info->module_addrs[0];
    hw_activate(0, 0);

    // Initialize the management to start allocating from the
    // 3rd bank, leaving a "protection" hole.
    mm_state_t* st = (mm_state_t*)base;
    st->next = 2;

    return st;
}

void *mm_alloc(mm_state_t* st, size_t nbytes)
{
    const struct ram_info *info = hw_raminfo();

    // How many banks are needed to satisfy the request?
    size_t nbanks = (nbytes + info->bank_size - 1) / info->bank_size;

    // No space left -> request fails, this is OK.
    if (st->next + nbanks > info->nbanks_per_module)
        return 0;

    // Activate all needed banks.
    for (size_t i = 0; i < nbanks; ++i)
        hw_activate(0, st->next + i);

    // Then return the right pointer.
    void *p = (char*)info->module_addrs[0] + st->next * info->bank_size;
    st->next += nbanks;

    return p;
}

void mm_free(mm_state_t* st, void *ptr)
{
    // nothing here.
    (void)ptr, (void)st;
}
