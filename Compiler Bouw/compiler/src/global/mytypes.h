#ifndef _CIVCC_MYTYPES_H_
#define _CIVCC_MYTYPES_H_


typedef enum { MO_not, MO_neg, MO_unknown } monop;

typedef enum { BO_add, BO_sub, BO_mul, BO_div, BO_mod,
               BO_lt, BO_le, BO_gt, BO_ge, BO_eq, BO_ne,
               BO_and, BO_or, BO_unknown } binop;

typedef enum {BT_bool, BT_int, BT_float, BT_void, BT_unknown} basictype;


#endif
