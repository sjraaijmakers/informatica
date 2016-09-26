// Steven Raaijmakers, 10804242
// Program can decode an encoded text

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "ll.h"
#include "debug.h"

#define BUF_SIZE 512

#define DELIM "!?\",. \n"

// Source: http://stackoverflow.com/questions/9753346/determine-if-a-c-string-is-a-valid-int-in-c
int isnumber(const char *str){
    if(*str == '-'){
        ++str;
    }
    if(!*str){
        return 0;
    }
    while(*str){
        if(!isdigit(*str)){
            return 0;
        }
        else {
            ++str;
        }
    }
    return 1;
}

int main(int argc, char *argv[]) {
    FILE *infile;
    char *linebuf, *s, *e, *w;
    char sep;
    struct list* dummy_list;

    if (argc < 2) {
        // No filename, read from stdin.
        infile = stdin;
    }
    else {
        infile = fopen(argv[1], "r");
        if (!infile) {
            perror(argv[1]);
            exit(EXIT_FAILURE);
        }
    }

    // Use debug print like this:
    DEBUG_PRINT(("argc: %d\n", argc));

    linebuf = malloc(BUF_SIZE);
    assert(linebuf);

    // dummy list init
    dummy_list = list_init();
    while (fgets(linebuf, BUF_SIZE, infile) != NULL) {
        s = linebuf; // First word starts at line beginning.
        while ((e = strpbrk(s, DELIM)) != NULL) {
            sep = *e; // Store separator
            *e = '\0'; // End word.

            /* char* s now points the current word, and sep to the
             * current separator. */

             w = s;

             // Check if word contains *
             if(s[strlen(s)-1] == '*'){
                s[strlen(s)-1] = '\0';
                char* tmp = malloc(sizeof(strlen(s)+1));
                strcpy(tmp, s);
                list_add(dummy_list, tmp);
             }
             // Check for ~
             else if(s[strlen(s)-1] == '~'){
                 s[strlen(s)-1] = '\0';
                 list_remove(dummy_list, s);
             }
             // Check if is number
             else if(isnumber(s)){
                 int num = atoi(s);
                 w = list_at_index(dummy_list, num);
             }

             printf("%s%c", w, sep);

             s = e + 1; // Next word.
        }
    }

    // Use debug_do like this:
    DEBUG_DO(list_print(dummy_list));
    // Cleanup.
    free(linebuf);
    fclose(infile);
    list_cleanup(dummy_list);
    dummy_list = NULL;

    return EXIT_SUCCESS;
}
