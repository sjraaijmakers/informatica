#ifndef SHELL_H
#define SHELL_H

// added

void initialize(void);
void signal_catch(int def);

struct tree_node;
void functions(struct tree_node* node);
void execute(struct tree_node* node, int frk, int dont_wait);
void piped(struct tree_node* node);
void commands(struct tree_node* node, int dont_wait);
void run_command(struct tree_node* n);


// ...

#endif
