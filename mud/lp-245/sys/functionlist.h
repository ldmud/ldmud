#ifndef FUNCTIONLIST_H
#define FUNCTIONLIST_H

#include "/sys/lpctypes.h"

#define NAME_INHERITED      0x80000000 /* Defined by inheritance         */
#define TYPE_MOD_STATIC     0x40000000 /* Static function or variable    */
#define TYPE_MOD_NO_MASK    0x20000000 /* The nomask => not redefineable */
#define TYPE_MOD_PRIVATE    0x10000000 /* Can't be inherited             */
#define TYPE_MOD_PUBLIC     0x08000000 /* Force inherit through private  */
#define TYPE_MOD_VARARGS    0x04000000 /* Used for type checking         */
#define TYPE_MOD_PROTECTED  0x01000000 /* cannot be called externally    */
#define NAME_CROSS_DEFINED  0x00080000 /* function defined from other program */
#define NAME_HIDDEN         0x00000800 /* Not visible for inheritance    */
#define NAME_UNDEFINED      0x00000200 /* Not defined yet                */

#define RETURN_FUNCTION_NAME	0x01
#define RETURN_FUNCTION_FLAGS	0x02
#define RETURN_FUNCTION_TYPE	0x04
#define RETURN_FUNCTION_NUMARG	0x08
#define RETURN_FUNCTION_ARGTYPE 0x10 /* not implemented */

#endif /* FUNCTIONLIST_H */
