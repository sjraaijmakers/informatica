/*
    Authors: Marcus van Bergen & Steven Raaijmakers
    Description: Program simulates a memory- & cpu scheduler
*/

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "schedule.h"
#include "mem_alloc.h"

/* Protypes */
pcb* queue_add(pcb* node, pcb* queue);
pcb* queue_remove(pcb* node, pcb* queue);
void queue_print(pcb* queue);

bool proc_action(pcb* node);
pcb* bosjf(pcb* queue, int small);
pcb* round_robin(pcb* queue);

/* This variable will simulate the allocatable memory */
static long memory[MEM_SIZE];

/* The actual CPU scheduler is implemented here */
static void CPU_scheduler(){
    extern int choice;
    if(!ready_proc){
        return;
    }
    switch(choice){
    case 1: // default
        return;
    case 2: // round robin
        ready_proc = round_robin(ready_proc);
        break;
    case 3: // shortest
        ready_proc = bosjf(ready_proc, 1);
        break;
    case 4: // biggest
        ready_proc = bosjf(ready_proc, 0);
        break;
    default:
        return;
        break;
    }
}

/* Proc action */
bool proc_action(pcb* node){
    int index = mem_get(node->MEM_need);
    if(index >= 0){
        node->MEM_base = index;
        new_proc = queue_remove(node, new_proc);
        ready_proc = queue_add(node, ready_proc);
        return true;
    }
    return false;
}

/* Biggest or shortest job first */
pcb* bosjf(pcb* queue, int small){
    /* Find shortest process (based on process' MEM_need) */
    pcb* bos_proc = queue;
    pcb* current = queue;
    while(current){
        if(small && (current->MEM_need < bos_proc->MEM_need)){
            bos_proc = current;
        }
        else if(!small && (current->MEM_need > bos_proc->MEM_need)){
            bos_proc = current;
        }
        current = current->next;
    }
    /* If smallest is the first one, return queue */
    if(bos_proc == queue){
        return queue;
    }
    /* Remove proc from  queue and put it in the front */
    queue = queue_remove(bos_proc, queue);
    bos_proc->next = queue;
    bos_proc->prev = NULL;
    queue->prev = bos_proc;
    return queue = bos_proc; /* bos_proc is new front (= queue) */
}

/* Round Robin */
pcb* round_robin(pcb* queue){
    extern double rr;
    set_slice(rr);
    /* Make copy & set queue head to queue tail */
    pcb* tmp_head = queue;
    pcb* tmp_queue = queue;
    tmp_queue = queue_remove(tmp_head, tmp_queue);
    tmp_queue = queue_add(tmp_head, tmp_queue);
    return tmp_queue;
}

/* Add node to queue */
pcb* queue_add(pcb* node, pcb* queue){
    /* If queue is empty, create a new one */
    if(!queue){
        queue = node;
        queue->next = NULL;
        queue->prev = NULL;
    }
    /* Add node at end of queue */
    else {
        pcb* current = queue;
        while(current->next){
            current = current->next;
        }
        current->next = node;
        node->prev = current;
        node->next = NULL;
    }
    return queue;
}

/* Remove node from queue */
pcb* queue_remove(pcb* node, pcb* queue){
    /* If node is head of queue */
    if(queue == node){
        queue = NULL;
        if(node->next){
            queue = node->next;
            node->next->prev = NULL;
        }
    }
    /* Change links */
    if(node->prev && node->next){
        node->prev->next = node->next;
        node->next->prev = node->prev;
    }
    else if(node->prev){
        node->prev->next = NULL;
    }
    else if(node->next){
        node->next->prev = NULL;
    }
    node->next = NULL;
    node->prev = NULL;
    return queue;
}

/* Ntry, only do n more actions when execution of process fails */
void ntries(pcb* node, int ntry){
    int i = 0;
    while(node && i < ntry){
        proc_action(node);
        i++;
        node = node->next;
    }
}

/* Give memory to process (membase is where the action happens) */
static void GiveMemory(){
    extern int ntry;
    pcb *current = new_proc;
    while(current){
        if(!proc_action(current) && ntry != 0){
            ntries(current, ntry);
            return;
        }
        current = current->next;
    }
}

/* Here we reclaim the memory of a process after it has finished */
static void ReclaimMemory(){
    pcb *proc;
    proc = defunct_proc;
    while(proc){
        if(proc->your_admin){
            free(proc->your_admin);
        }
        mem_free(proc->MEM_base);
        proc->MEM_base = -1;
        rm_process(&proc);
        proc = defunct_proc;
    }
}

static void my_finale(){
}

/* The main scheduling routine */
void schedule(event_type event){
    static int first = 1;

    if(first){
        mem_init(memory);
        finale = my_finale;
        first = 0;
    }

    switch(event){
    case NewProcess_event:
        GiveMemory();
        break;
    case Time_event:
        CPU_scheduler();
        break;
    case IO_event:
        CPU_scheduler();
        break;
    case Ready_event:
        CPU_scheduler();
        break;
    case Finish_event:
        ReclaimMemory();
        GiveMemory();
        CPU_scheduler();
        break;
    default:
        printf("I cannot handle event nr. %d\n", event);
        break;
    }
}
