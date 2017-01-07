//  Program simulates a mini-unix shell
// source 1: http://www.csl.mtu.edu/cs4411.ck/www/NOTES/process/fork/shell.c
// source 2: http://stackoverflow.com/questions/916900/having-trouble-with-fork-pipe-dup2-and-exec-in-c/917700#917700
#include "ast.h"

#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <sys/wait.h>

// fix for compatibility issue with stdlib.h
int setenv(const char *envname, const char *envval, int overwrite);
int unsetenv(const char *name);

void commands(node_t* node, int frk, int dont_wait);

// Catch Ctrl + C
void signal_catch(int def) {
    struct sigaction sa;
    sigaction(SIGINT, NULL, &sa);
    if(def){
        sa.sa_handler = SIG_DFL;
    }
    else {
        sa.sa_handler = SIG_IGN;
    }
    sigaction(SIGINT, &sa, NULL);
}

// Initialize
// Check for Ctrl + C input
void initialize(void){
    signal_catch(0);
}

// Handle pre-defined functions
int functions(node_t* node){
    // Exit shell (give argv[1] as a number)
    if (strcmp(node->command.program, "exit") == 0) {
        if(node->command.argc > 1){
            exit(atoi(node->command.argv[1]));
        }
        else {
            exit(0);
        }
    }
    // CD command
    if (strcmp(node->command.program, "cd") == 0) {
        chdir(node->command.argv[1]);
        return 1;
    }
    // Set
    else if (strcmp(node->command.program, "set") == 0) {
        if(node->command.argc > 0) {
            // split at '=' sign
            char* equal_pos = strchr(node->command.argv[1], '=');
            unsigned long varname_len = ((unsigned long) equal_pos) - ((unsigned long) node->command.argv[1]);
            char varname[varname_len + 1];
            // copy variable name
            strncpy(varname, node->command.argv[1], varname_len);
            varname[varname_len] = '\0';
            char* varvalue = equal_pos + 1;
            // set environmental var
            setenv(varname, varvalue, 0);
        }

        return 1;
    }
    // Unset
    else if (strcmp(node->command.program, "unset") == 0) {
        unsetenv(node->command.argv[1]);
        return 1;
    }
    else {
        return 0;
    }
}

// Execute program (via node)
void execute(node_t* node, int frk, int dont_wait){
    // check for pre-defined functions functions
    if(functions(node)) {
        return;
    }
    // Else fork new process & create new program
    int status;
    pid_t child = 0;
    if(frk){
        child = fork();
    }
    if (child < 0) {
        perror("Error in fork process ");
        exit(EXIT_FAILURE);
    }
    else if (!child) {
        // Set back signaction to DFL
        signal_catch(1);
        if (execvp(node->command.program, node->command.argv) < 0) {
            perror("Error in execution ");
            exit(EXIT_FAILURE);
        }
    }
    else {
        if(!dont_wait) {
            waitpid(child, &status, 0);
        }
    }
}

// Function for piped commands
// For each cmd make a new pipe, link each pipe to its previous
// Save all pids, and wait
void piped(node_t* node){
    int pids[node->pipe.n_parts], old_pipe[2];
    for (unsigned int i = 0; i < node->pipe.n_parts; i++) {
        int new_pipe[2];
        if (i < node->pipe.n_parts - 1) { // create pipe when multiple cmds
            pipe(new_pipe);
        }
        pid_t child = fork();
        if (!child) { // if child
            if (i > 0) {
                dup2(old_pipe[0], 0); close(old_pipe[0]); close(old_pipe[1]);
            }
            if (i < node->pipe.n_parts - 1) {
                close(new_pipe[0]); dup2(new_pipe[1], 1); close(new_pipe[1]);
            }
            commands(node->pipe.parts[i], 0, 0);
        }
        else { // no child
            if (i > 0) {
                close(old_pipe[0]); close(old_pipe[1]);
            }
            if (i < node->pipe.n_parts - 1) {
                old_pipe[0] = new_pipe[0]; old_pipe[1] = new_pipe[1];
            }
        }
        pids[i] = child;
    }
    if (node->pipe.n_parts > 1) {
        close(old_pipe[0]); close(old_pipe[1]);
    }
    for (unsigned int i = 0; i < node->pipe.n_parts; i++) {
        int status;
        waitpid(pids[i], &status, 0);
    }
}

// Handle command line input
void commands(node_t* node, int frk, int dont_wait){
    switch(node->type){
        case NODE_COMMAND:
            execute(node, frk, dont_wait);
            break;
        case NODE_PIPE:
            piped(node);
            break;
        case NODE_SEQUENCE: // run first always with a fork, second depends on enviroment
            commands(node->sequence.first, 1, 0);
            commands(node->sequence.second, frk, 0);
            break;
        case NODE_REDIRECT:
            break;
        case NODE_SUBSHELL:
            if (!fork()) {
                execute(node->subshell.child, 1, 0);
                exit(0);
            }
            break;
        case NODE_DETACH: // run without waiting
            // fork and execute in child process, do not wait for process id
            if (!fork()) {
                commands(node->detach.child, 1, 1);
                exit(0);
            }
            break;
        default:
            perror("");
            break;
    }
}

// Run command + free
void run_command(node_t* node){
    commands(node, 1, 0);
    free_tree(node);
}
