typedef struct{
    int *data;
    int occupied;
    int nextin;
    int nextout;
    pthread_mutex_t buflock;
    pthread_cond_t items;
    pthread_cond_t space;
} buffer;

typedef struct{
    buffer *incoming_buffer;
    buffer *outgoing_buffer;
    int filter_number;
} filter_args;

void erat(int argc, const char* argv[]);
void create_thread(int filter_number, buffer *incomming);
void * filter_thread(void *arguments);
void calc(void);
void kill_all_threads(void);
