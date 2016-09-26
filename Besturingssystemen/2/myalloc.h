#ifndef MYALLOC_H
#define MYALLOC_H

#include <stddef.h>

// The management data should be stored in struct mm_state.
// The definition of this struct can be done in mm.c.
struct mm_state;
typedef struct mm_state mm_state_t;

// mm_initialize(): initialize the management structures.
// This function is called once, before the others.
mm_state_t* mm_initialize(void);

// mm_alloc(): provide space to the requesting program.
void *mm_alloc(mm_state_t* st, size_t nbytes);
// mm_free(): give space back to the memory manager / OS.
void mm_free(mm_state_t* st, void *ptr);

// Services from the virtual hardware:

// hw_raminfo(): informs the other functions about the RAM structure.
struct ram_info {
    size_t bank_size; // unit: bytes
    size_t nbanks_per_module;
    size_t nmodules;
    void* const *module_addrs;
};
const struct ram_info* hw_raminfo(void);

// hw_activate(): activate a RAM bank.
void hw_activate(size_t module, size_t bank);
// hw_deactivate(): de-activate a RAM bank.
void hw_deactivate(size_t module, size_t bank);

#endif
