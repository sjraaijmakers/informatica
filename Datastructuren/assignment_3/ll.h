typedef struct list list;

struct list;
struct node;

struct list* list_init();
void list_add(struct list *l, char *d);
char* list_at_index(struct list *l, int index);
char* list_remove(struct list *l, char *d);
void list_print(struct list *l);
int list_cleanup(struct list *l);
