typedef struct tnode tnode;
typedef struct trie trie;

struct tnode;
struct trie;

// Helpers
void hang(struct tnode* parent, char* w, unsigned int index);
void erase(struct tnode* start);
void print_sub(struct tnode *t, char* word, int depth);
int trie_count_help(struct tnode *t);

struct trie* trie_init();

/* Add word w to trie t.
 * Return: ...
 */
int trie_add(struct trie* t, char* w);

/* Lookup word w in trie t.
 * Return: ...
 */
int trie_lookup(struct trie* t, char* w);

/* Remove word w from trie t.
 * Return: ...
 */
int trie_remove(struct trie* t, char* w);

/* Print all the words in trie t starting with prefix.
 */
void trie_prefix(struct trie* t, char* prefix);

/* Print all the words in trie t.
 */
void trie_print(struct trie* t);

/* Count words in the trie.
 * Return: ...
 */
int trie_count(struct trie* t);

/* Cleanup trie.
 * Return: number of words free'd.
 */
int trie_cleanup(struct trie* t);
