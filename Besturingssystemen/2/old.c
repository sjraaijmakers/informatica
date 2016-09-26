#include "myalloc.h"
#include "stdio.h"

struct mm_state
{
    size_t next;
};

// header met info over delen van geheugen
typedef struct mm_chunk {
    size_t size;
    size_t bank;
    char used;
    struct mm_chunk *next;
    struct mm_chunk *prev;
} mm_chunk;

mm_state_t* mm_initialize(void)
{
    const struct ram_info *info = hw_raminfo();
    // first header
    mm_chunk* head = (mm_chunk*)info->module_addrs[0];
    head->prev = NULL;
    head->next = NULL;
    head->bank = 0;
    head->used = 0;
    head->size = info->bank_size - sizeof(mm_chunk);

    // Use the data in *info to make space for your own mm_state_t.
    hw_activate(0, 0);
    return (mm_state_t*)head;
}

void activate_banks(mm_chunk* first, mm_chunk* last){
    int start = first->bank;
    int end = last->bank;
    if(first->prev){
        start++;
    }
    if(last->next){
        end--;
    }
    for(int i = start; i <= end; i++){
        hw_activate(0, 1 + i);
    }
}
void *mm_alloc(mm_state_t* st, size_t nbytes)
{
    const struct ram_info *info = hw_raminfo();
    mm_chunk* current = (mm_chunk*)st;
    while(current){
        if(!current->used && nbytes + sizeof(mm_chunk) <= current->size){
            // new header
            mm_chunk* new = (size_t*)current + nbytes;
            printf("%d", nbytes);
            new->prev = current;
            new->used = 0;
            new->bank = ((size_t)current - (size_t)st) / info->bank_size;
            printf("used bits: %d\n", ((size_t)current - (size_t)st));


            new->size = current->size - (nbytes + sizeof(mm_chunk));

            // set current-header
            current->used = 1;
            current->size = nbytes;

            if(current->next){
                current->next->prev = new;
                new->next = current->next;
            }
            else {
                new->next = NULL;
            }
            current->next = new;
            // activeren van banken
            activate_banks(current, new);
            void *p = (char*)new + sizeof(mm_chunk);
            return p;
        }
        current = current->next;
    }
    // no space
    return 0;
}

void mm_free(mm_state_t* st, void *ptr)
{


    // ...YOUR CODE HERE...
}
