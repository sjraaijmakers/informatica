/* Useful debug macros. */

#ifdef DEBUG
#define DEBUG_PRINT(x) printf x
#define DEBUG_DO(x) x
#else
#define DEBUG_PRINT(x) do {} while (0)
#define DEBUG_DO(x) do {} while (0)
#endif

