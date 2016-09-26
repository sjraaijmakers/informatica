#ifndef XCHECK_H
#define XCHECK_H

#include <stdlib.h>
#include <stdio.h>

static inline
void xabort(const char *str) {
    fprintf(stderr, "%s", str);
    abort();
}
static inline
void xcheck_(int exp, const char *str)
{
    if (!exp)
        xabort(str);
}

#define xstrify_(N) # N
#define xstrify(N) xstrify_(N)
#define xcheck(Exp) xcheck_((Exp),     \
    __FILE__ ":" xstrify(__LINE__)     \
    ": assertion failed: " #Exp "\n")

#define xcheckf(Exp, Fmt, ...) do {                    \
        if (!(Exp)) {                                  \
            fprintf(stderr, Fmt, ## __VA_ARGS__);      \
            fputc('\n', stderr);                       \
            xabort(__FILE__ ":" xstrify(__LINE__)      \
                   ": assertion failed: " #Exp "\n");  \
        }                                              \
    } while (0)


#endif
