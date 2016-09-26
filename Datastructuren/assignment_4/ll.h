typedef struct list list;
typedef struct node node;

struct list;
struct node;

// new
struct tnode* tnode_at_index(struct list *l, int index);
char char_at_index(struct list *l, int index);
int list_size(struct list *l);
struct tnode* parent(struct list* l);

struct list* list_init();
struct tnode* list_search(struct list *l, char c);

void list_add(struct list *l, char c, void* child);
void list_remove(struct list *l, char c);
void list_print(struct list *l);
int list_cleanup(struct list *l);
