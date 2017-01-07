#ifndef XALLOC_H
#define XALLOC_H

#include <stdlib.h>
#include <string.h>
#include <assert.h>

static inline void* xrealloc(void *p, size_t n)
{
    p = realloc(p, n);
    assert(p != 0);
    return p;
}

static inline void* xalloc(size_t n)
{
    void *p = malloc(n);
    assert(p != 0);
    return p;
}

static inline char* xstrdup(char *s)
{
    size_t l = strlen(s);
    char *p = xalloc(l + 1);
    strcpy(p, s);
    return p;
}

#endif
