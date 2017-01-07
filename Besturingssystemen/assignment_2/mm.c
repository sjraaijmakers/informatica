// Program simulates an allocation to RAM with multiple modules
// Steven Raaijmakers & Kjeld Oostra

#include "myalloc.h"
#include "stdio.h"
#include <inttypes.h>

// Defines module, needs to be aligned to 8 (size = 88)
// Also points to first header in module
struct mm_state {
    struct mm_chunk *head;
    int number;
    mm_state_t* next;
    char banks[64]; // minimum size!
};

// Defines a piece of memory which (size = 40)
typedef struct mm_chunk {
    size_t size;
    size_t module;
    char used;
    struct mm_chunk *next;
    struct mm_chunk *prev;
} mm_chunk;

// Align function: to align Number to Align
int align(int number, int align){
    return number + (align - (number % align));
}

// Init. RAM
// Give each module a state-header & one chunk-header to describe free space
mm_state_t* mm_initialize(void) {
    const struct ram_info *info = hw_raminfo();
    size_t size_of_state = sizeof(mm_state_t);

    // Check if nbanks isn't more than what we allocated (64)
    if(info->nbanks_per_module > 64){
        size_of_state = offsetof(mm_state_t, banks) + info->nbanks_per_module;
        size_of_state = align(size_of_state, (sizeof(intmax_t)));
    }

    // Init all modules
    for(size_t i = 0; i < info->nmodules; i++){
        mm_state_t* new_state = (mm_state_t*)info->module_addrs[i];
        new_state->number = i;
        // Init banks[] to 0
        for(size_t j = 0; j < info->nbanks_per_module; j++){
            new_state->banks[j] = 0;
        }
        // Check what next module could be
        new_state->next = NULL;
        if(i < info->nmodules - 1){
            new_state->next = (mm_state_t*)info->module_addrs[i + 1];
        }
        // Create first head
        mm_chunk* head = (mm_chunk*)(info->module_addrs[i] + size_of_state);
        head->size = (info->bank_size * info->nbanks_per_module) - (sizeof(mm_chunk) + size_of_state);
        head->module = i;
        head->used = 0;
        head->prev = NULL;
        head->next = NULL;
        new_state->head = head;
        // Activate first bank
        hw_activate(i, 0);
        new_state->banks[0] = 1;
    }
    return (mm_state_t*)info->module_addrs[0];
}

// Calculate bank by given address (in it's module)
size_t calculate_bank(char* adress, mm_state_t* module){
    const struct ram_info *info = hw_raminfo();
    size_t bank = (adress - (char*)module) / info->bank_size;
    return bank;
}

// Check if Left bank can be turned off
// Searches to the left while this bank is in the same bank
int check_left(mm_chunk* first, mm_state_t* module){
    mm_chunk* left = first;
    int first_bank = calculate_bank((char*)first, module);
    int left_bank = first_bank;
    first_bank++;
    // Search all chunks in same bank
    while(first_bank == left_bank && left){
        if(left->used){
            first_bank--;
            break;
        }
        left = left->prev;
        left_bank = calculate_bank((char*)left, module);
    }
    return first_bank;
}

// Check if right bank can be turned off
// Searches to the right while this bank is in the same bank
int check_right(mm_chunk* first, mm_state_t* module){
    mm_chunk* right = (mm_chunk*)((char*)first + sizeof(mm_chunk) + first->size);
    int last_bank = calculate_bank((char*)right + sizeof(mm_chunk), module);
    int right_bank = last_bank;
    last_bank--;
    // Search all chunks in same bank
    while(last_bank == right_bank && right){
        if(right->used){
            last_bank++;
            break;
        }
        right = right->prev;
        right_bank = calculate_bank((char*)right + sizeof(mm_chunk), module);
    }
    return last_bank;
}

// Activate banks from first to last (if bank isn't on already)
void activate_banks(size_t first, size_t last, mm_state_t* module){
    for(unsigned int i = first; i <= last; i++){
        if(module->banks[i] == 0){
            hw_activate(module->number, i);
            module->banks[i] = 1;
        }
    }
}
// Deactivate banks from first to last (if bank isn't off already)
void deactivate_banks(mm_chunk* first, mm_state_t* module){
    int first_bank = check_left(first, module);
    int last_bank = check_right(first, module);
    for(int i = first_bank; i < last_bank; i++){
        if(module->banks[i] == 1){
            hw_deactivate(module->number, i);
            module->banks[i] = 0;
        }
    }
}

// Allocate piece of memory
void *mm_alloc(mm_state_t* st, size_t nbytes){
    nbytes = align(nbytes, (sizeof(intmax_t)));
    mm_state_t* current_state = st; size_t to_allocate = nbytes + sizeof(mm_chunk);

    // Check all modules
    while(current_state){
        // Within module, check all headers
        mm_chunk* current_chunk = current_state->head;
        while(current_chunk){
            // If has chunk has enough space to use:
            if(!current_chunk->used && to_allocate <= current_chunk->size){
                current_chunk->used = 1;
                // Calculate bank addresses + activate
                size_t begin_bank = calculate_bank((char*)current_chunk, current_state);
                char* end_address = (char*)current_chunk + sizeof(mm_chunk) + to_allocate;
                size_t end_bank = calculate_bank(end_address, current_state);
                activate_banks(begin_bank, end_bank, current_state);

                // Create new header
                mm_chunk* new_chunk = (mm_chunk*)((char*)current_chunk + to_allocate);
                new_chunk->prev = current_chunk;
                new_chunk->used = 0;
                new_chunk->size = current_chunk->size - to_allocate;
                new_chunk->module = current_state->number;
                current_chunk->size = nbytes;
                new_chunk->next = NULL;
                if(current_chunk->next){
                    current_chunk->next->prev = new_chunk;
                    new_chunk->next = current_chunk->next;
                }
                current_chunk->next = new_chunk;
                // Return address of allocated piece (not the header)
                return (char*)current_chunk + sizeof(mm_chunk);
            }
            current_chunk = current_chunk->next;
        }
        current_state = current_state->next;
    }
    // Failed allocating
    return 0;
}

// Changes links when removing piece of code which results in a gap
void free_gap(mm_chunk* prev, mm_chunk* next){
    prev->size = prev->size + (sizeof(mm_chunk) + prev->next->size) + (sizeof(mm_chunk) + next->size);
    if(next->next){
        prev->next = next->next;
        next->next->prev = prev;
    }
    else {
        prev->next = NULL;
    }
}

// Changes links from piece when left is empty
void free_left(mm_chunk* prev){
    prev->size = prev->size + (sizeof(mm_chunk) + prev->next->size);
    if(prev->next->next){
        prev->next->next->prev = prev;
        prev->next = prev->next->next;
    }
    else {
        prev->next = NULL;
    }
}

// Changes links from piece when right is empty
void free_right(mm_chunk* next){
    next->prev->size = next->prev->size + (sizeof(mm_chunk) + next->size);
    if(next->next){
        next->next->prev = next->prev;
        next->prev->next = next->next;
    }
    else {
        next->prev->next = NULL;
    }
}

// Free allocated piece, precies 30 regels!!!!?!
void mm_free(mm_state_t* st, void *ptr) {
    mm_chunk* current_chunk = (mm_chunk*)(ptr - sizeof(mm_chunk));
    mm_state_t* current_state = st;
    for(size_t i = 0; i < current_chunk->module; i++){ // Get to correct state
        current_state = current_state->next;
    }
    current_chunk->used = 0;
    // When free'ing results in a gap
    if((current_chunk->prev && current_chunk->next) &&
     (!current_chunk->prev->used && !current_chunk->next->used)){
        free_gap(current_chunk->prev, current_chunk->next);
    }
    // When free'ing has a left gap
    else if(current_chunk->prev && !current_chunk->prev->used){
        free_left(current_chunk->prev);
    }
    // When free'ing has a right gap
    else if(current_chunk->next && !current_chunk->next->used){
        free_right(current_chunk->next);
    }
    deactivate_banks(current_chunk, current_state);
}
