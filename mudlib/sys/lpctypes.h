#ifndef LPC_LPCTYPES_H
#define LPC_LPCTYPES_H

/* compile time types, from functionlist() */

#define TYPE_UNKNOWN    0       /* This type must be casted */
#define TYPE_NUMBER     1
#define TYPE_STRING     2
#define TYPE_VOID       3
#define TYPE_OBJECT     4
#define TYPE_MAPPING    5
#define TYPE_FLOAT      6
#define TYPE_ANY        7       /* Will match any type */
#define TYPE_CLOSURE    8
#define TYPE_SYMBOL     9
#define TYPE_QUOTED_ARRAY 10
#define TYPE_STRUCT     11
#define TYPE_BYTES      12
#define TYPE_LWOBJECT   13
#define TYPE_COROUTINE  14
#define TYPE_LPCTYPE    15

#define TYPE_MOD_POINTER        0x0040  /* Pointer to a basic type        */

/* runtime types, from typeof() */

#define T_INVALID       0x0
#define T_LVALUE        0x1
#define T_NUMBER        0x2
#define T_STRING        0x3
#define T_POINTER       0x4
#define T_OBJECT        0x5
#define T_MAPPING       0x6
#define T_FLOAT         0x7
#define T_CLOSURE       0x8
#define T_SYMBOL        0x9
#define T_QUOTED_ARRAY  0xa
#define T_STRUCT        0xb
#define T_BYTES         0xc
#define T_LWOBJECT      0xd
#define T_COROUTINE     0xe
#define T_PYTHON        0xf
#define T_LPCTYPE       0x10

/* Closure types, stored as secondary type info */

#ifndef __DRIVER_SOURCE__

#define CLOSURE_LFUN            0  /* lfun in an object */
  /* Code 1: currently unused, used to be CLOSURE_ALIEN_LFUN */
#define CLOSURE_IDENTIFIER      2  /* variable in this object */
#define CLOSURE_PRELIMINARY     3
    /* Efun closure used in a static initialization */
#define CLOSURE_BOUND_LAMBDA    4  /* Bound unbound-lambda closure */
#define CLOSURE_LAMBDA          5  /* normal lambda closure */
#define CLOSURE_UNBOUND_LAMBDA  6  /* unbound lambda closure. */

#ifdef __PYTHON__
#  define CLOSURE_OPERATOR      (0xe000)
#  define CLOSURE_PYTHON_EFUN   (0xe800)
#else
#  define CLOSURE_OPERATOR      (0xe800)
#endif
#define CLOSURE_EFUN            (0xf000)
#define CLOSURE_SIMUL_EFUN      (0xf800)

#define CLOSURE_IS_LFUN(x)		(((x)&~1) == 0)
#define CLOSURE_IS_IDENTIFIER(x)	((x) == CLOSURE_IDENTIFIER)
#define CLOSURE_IS_BOUND_LAMBDA(x)	((x) == CLOSURE_BOUND_LAMBDA)
#define CLOSURE_IS_LAMBDA(x)		((x) == CLOSURE_LAMBDA)
#define CLOSURE_IS_UNBOUND_LAMBDA(x)	((x) == CLOSURE_UNBOUND_LAMBDA)
#define CLOSURE_IS_SIMUL_EFUN(x) (((x) & 0xf800) == CLOSURE_SIMUL_EFUN)
#define CLOSURE_IS_EFUN(x)	 (((x) & 0xf800) == CLOSURE_EFUN)
#define CLOSURE_IS_OPERATOR(x)	 (((x) & 0xf800) == CLOSURE_OPERATOR)
#ifdef __PYTHON__
#  define CLOSURE_IS_PYTHON_EFUN(x)	(((x) & 0xf800) == CLOSURE_PYTHON_EFUN)
#endif

#endif /* __DRIVER_SOURCE__ */

#endif /* LPC_LPCTYPES_H */
