#include "myalloc.h"

struct mm_state
{
    // ...YOUR CODE HERE...
};

mm_state_t* mm_initialize(void)
{
    const struct ram_info *info = hw_raminfo();

    // Use the data in *info to make space for your own mm_state_t.
    // Don't use malloc()!

    // ...YOUR CODE HERE...

    return 0;
}

void *mm_alloc(mm_state_t* st, size_t nbytes)
{
    // ...YOUR CODE HERE...
    return 0;
}

void mm_free(mm_state_t* st, void *ptr)
{
    // ...YOUR CODE HERE...
}
