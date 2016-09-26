#include "myalloc.h"
#include "stdio.h"

struct mm_state {
    struct mm_chunk *head;
    int number;
    mm_state_t* next;
    char banks[64];
};

typedef struct mm_chunk {
    size_t size;
    size_t bank;
    size_t module;
    size_t number;
    char used;
    struct mm_chunk *next;
    struct mm_chunk *prev;
} mm_chunk;

mm_state_t* mm_initialize(void) {
    const struct ram_info *info = hw_raminfo();
    for(size_t i = 0; i < info->nmodules; i++){
        mm_state_t* new_state = (mm_state_t*)info->module_addrs[i];
        new_state->number = i;
        for(int j = 0; j < 64; j++){
            new_state->banks[j] = 0;
        }
        new_state->next = NULL;
        if(i < info->nmodules - 1){
            new_state->next = (mm_state_t*)info->module_addrs[i + 1];
        }
        mm_chunk* head = (mm_chunk*)(info->module_addrs[i] + sizeof(mm_state_t));
        head->size = (info->bank_size * info->nbanks_per_module) - (sizeof(mm_chunk) + sizeof(mm_state_t));
        head->bank = 0;
        head->module = i;
        head->used = 0;
        head->prev = NULL;
        head->next = NULL;
        head->number = 0;

        new_state->head = head;

        hw_activate(i, 0);
        new_state->banks[0] = 1;
    }
    return (mm_state_t*)info->module_addrs[0];
}

void activate_banks(mm_chunk* first, mm_chunk* last, mm_state_t* module){
    for(unsigned int i = first->bank; i <= last->bank; i++){
        if(module->banks[i] == 0){
            hw_activate(module->number, i);
            module->banks[i] = 1;
        }
    }
}

void *mm_alloc(mm_state_t* st, size_t nbytes){
    const struct ram_info *info = hw_raminfo();
    nbytes = nbytes + (8 - (nbytes % 8));

    mm_state_t* current_state = st;
    while(current_state){
        mm_chunk* current_chunk = current_state->head;
        while(current_chunk){
            if(current_chunk->used == 0 && (nbytes + sizeof(mm_chunk)) <= current_chunk->size){
                mm_chunk* new_chunk = (char*)current_chunk + sizeof(mm_chunk) + nbytes;
                new_chunk->prev = current_chunk;
                new_chunk->used = 0;
                new_chunk->number = current_chunk->number + 1;
                new_chunk->bank = ((char*)current_chunk - (char*)current_state + nbytes) / info->bank_size;
                new_chunk->size = current_chunk->size - (nbytes + sizeof(mm_chunk));
                printf("%d", new_chunk->size);
                new_chunk->module = current_state->number;
                printf("Current adress: %d, New adress: %d", (char*)current_chunk, (char*)new_chunk);
                printf(" (difference: %d)    ", (char*)current_chunk - (char*)new_chunk);
                printf("Sizes: Cur: %d, To be allocated: %d + %d, New: %d\n", current_chunk->size, nbytes, sizeof(mm_chunk), new_chunk->size);

                current_chunk->used = 1;
                current_chunk->size = nbytes;
                new_chunk->next = NULL;
                if(current_chunk->next){
                    current_chunk->next->prev = new_chunk;
                    new_chunk->next = current_chunk->next;
                }
                current_chunk->next = new_chunk;

                activate_banks(current_chunk, new_chunk, current_state);

                void *p = (char*)current_chunk + (sizeof(mm_chunk) + (8 - (sizeof(mm_chunk) % 8)));
                return p;
            }
            current_chunk = current_chunk->next;
        }
        current_state = current_state->next;
    }
    return 0;
}

void mm_free(mm_state_t* st, void *ptr) {
}
