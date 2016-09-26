// Steven Raaijmakers
// Program can autocomplete words with a given prefix

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>
#include <assert.h>
#include <string.h>
#include <time.h>
#include <stdio.h>

#include "trie.h"

#define BUF_SIZE 10000
#define DELIM "-!?\",. \n"

void usage(char* prog) {
    fprintf(stderr, "Usage %s [-p] prefix   : Print words with prefix\n"
                    "         [-r] rmfile   : Remove words in rmfile\n"
                    "         [-P] rmfile   : Print trie\n"
                    "         filename\n", prog);
    exit(EXIT_SUCCESS);
}

int main(int argc, char* argv[]) {
    FILE *infile;
    char *w = NULL, *prefix = NULL, *rmfile_name = NULL;
    char *linebuf;
    int opt, added = 0, print = 0;
    struct trie* t;

    /* Handle command line arguments. */
    while ((opt = getopt(argc, argv, "Pp:r:")) != -1) {
        switch(opt) {
            case 'p':
                prefix = optarg;
                break;
            case 'P':
                print = 1;
                break;
            case 'r':
                rmfile_name = optarg;
                break;
            default: /* '?' */
                usage(argv[0]);
        }
    }
    if (optind >= argc) {
        usage(argv[0]); // expect at least input filename.
    }

    /* Open input file. */
    infile = fopen(argv[optind], "r");
    if (!infile) {
        perror(argv[optind]);
        exit(EXIT_FAILURE);
    }

    /* Initialize the trie. */
    t = trie_init();

    /* Read lines from infile, tokenize every line and add
     * each word to the trie. */
    linebuf = malloc(BUF_SIZE);
    assert(linebuf);
    while (fgets(linebuf, BUF_SIZE, infile) != NULL) {
        w = strtok(linebuf, DELIM); // strtok receives the new line of text.
        while (w) {
            if (trie_add(t, w))
                added += 1;
            w = strtok(NULL, DELIM); // get next token from current line.
        }
    }

    printf("Words added to trie: %d\n", added);
    printf("Number of words in trie: %d\n", trie_count(t));

    if (prefix) {
        printf("All words starting with %s:\n", prefix);
        trie_prefix(t, prefix);
        /* Autocomplete prefix here.. */
    }

    if (rmfile_name) {
        /* Remove words in rmfile_name from trie here.. */
    }

    if (print) {
        printf("All words stored:\n");
        trie_print(t);
    }

    /* Cleanup */
    free(linebuf);
    fclose(infile);
    printf("Number of words freed: %d\n", trie_cleanup(t));
    return EXIT_SUCCESS;
}
