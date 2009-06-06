#ifndef BYTECODE_H__
#define BYTECODE_H__ 1

/*---------------------------------------------------------------------------
 * Types and Macros used to describe bytecode
 *
 *---------------------------------------------------------------------------
 * While topically part of exec.h, the actual bytecode types and macros
 * have been exported into this file to reduce the coupling on exec.h.
 * Most files only need to know about bytecode_t and bytecode_p, and not
 * about all the other stuff in exec.h .
 *
 * Bytecode
 * --------
 *
 * As the name conveys, the LPC programs are compiled into a bytecode,
 * assuming 8-Bit-Bytes. Since we have more than 256 opcodes, the less
 * often used instructions are encoded in two-byte opcodes: a prefix
 * byte and an sub-opcode. The translation opcode -> prefix:sub-opcode
 * is defined in the instrs[] table using the .prefix and .opcode
 * fields.
 *
 * To achieve platform independance, the driver does not operate directly
 * with 'char's and 'char *'s, but instead with 'bytecode_t' and
 * 'bytecode_p's. Combined with some macros this allows the implementation
 * even on platforms with CHAR_BITs != 8.
 * TODO: This support is far from complete/comprehensive, and some values
 * TODO:: in the bytecode are stored in host-sizes and -layouts.
 *
 * The bytecode itself is divided into functions: the code for every
 * function is (except for absolute jumps) selfcontained and prepended
 * by a header holding the name of the function, and the number and types
 * of expected arguments. The advantage is that for a given function
 * the driver does not need to know if it is part of a program or a
 * lambda closure compiled at runtime: in both cases all necessary
 * information about the function is right where its code is.
 *
 * The maximum size of a program is limited by the biggest offset
 * that can be stored in the 'functions' array, currently 1 MByte.
 * A lot of internal offset counters are shorts even, though so far
 * this never caused a problem.
 *---------------------------------------------------------------------------
 */

#include "driver.h"
#include "typedefs.h"


/* --- Byte code ---
 *
 * The program code is stored as byte code. The following definitions
 * and macros allow its implementation even on platforms with more than
 * 8 bits per character.
 * TODO: This portability is far from complete, and not used everywhere,
 * TODO:: not even in the compiler.
 *
 *   bytecode_t: an integral type holding the numbers 0..255.
 *   bytecode_p: an integral type addressing a bytecode. This need not
 *               be a pointer.
 *
 *   bytecode_t GET_CODE(bytecode_p p)
 *   bytecode_t LOAD_CODE(bytecode_p p)
 *     Return the bytecode from *p, the LOAD_ variant then increments p.
 *
 *   void PUT_CODE(bytecode_p p, bytecode_t c)
 *   void STORE_CODE(bytecode_p p, bytecode_t c)
 *   void RSTORE_CODE(bytecode_p p, bytecode_t c)
 *     Store the bytecode c in *p, the STORE_ variant then increments p.
 *     The RSTORE_ variant pre-decrements p.
 *
 *    char GET_INT8(p) , LOAD_INT8(p)
 *   uchar GET_UINT8(p), LOAD_UINT8(p)
 *     Return the 8-Bit (unsigned) int stored at <p>, the LOAD_ variants
 *     then increment <p>.
 *
 *   void PUT_INT8(p, char c),   STORE_INT8(p, char c)
 *   void PUT_UINT8(p, uchar c), STORE_UINT8(p, uchar c)
 *     Store the 8-Bit (unsigned) int <c> into <p>, the STORE_ variants
 *     then increment <p>.
 *
 *   void GET_SHORT ([unsigned] short d, bytecode_p p)
 *   void LOAD_SHORT([unsigned] short d, bytecode_p p)
 *     Load the (unsigned) short 'd' stored at <p>, the LOAD_ variant
 *     then increments <p>.
 *     TODO: Currently, all SHORTs must be 2 bytes.
 *
 *   void PUT_SHORT (bytecode_p p, [unsigned] short d)
 *   void STORE_SHORT(bytecode_p p, [unsigned] short d)
 *   void RSTORE_SHORT(bytecode_p p, [unsigned] short d)
 *     Store the (unsigned) short <d> into <p>, the STORE_ variant
 *     then increments <p>. The RSTORE_ variant pre-decrements <p>.
 *     TODO: Currently, all SHORTs must be 2 bytes.
 *
 *   void GET_INT16 ([unsigned] int16 d, bytecode_p p)
 *   void LOAD_INT16([unsigned] int16 d, bytecode_p p)
 *     Load the (unsigned) int16 'd' stored at <p>, the LOAD_ variant
 *     then increments <p>.
 *
 *   void GET_LONG ([unsigned] long d, bytecode_p p)
 *   void LOAD_LONG([unsigned] long d, bytecode_p p)
 *     Load the (unsigned) long 'd' stored at <p>, the LOAD_ variant
 *     then increments <p>.
 *     TODO: Currently, all LONGs must be 4 bytes.
 *
 *   void PUT_LONG (bytecode_p p, [unsigned] long d)
 *   void STORE_LONG(bytecode_p p, [unsigned] long d)
 *   void RSTORE_LONG(bytecode_p p, [unsigned] long d)
 *     Store the (unsigned) long <d> into <p>, the STORE_ variant
 *     then increments <p>. The RSTORE_ variant pre-decrements <p>.
 *     TODO: Currently, all LONGs must be 4 bytes.
 *
 *   void LOAD_INT32([unsigned] int32 d, bytecode_p p)
 *   void GET_INT32([unsigned] int32 d, bytecode_p p)
 *     Load the (unsigned) in32 'd' stored at <p>, the LOAD_ variant
 *     then increments <p>.
 *
 *   void STORE_INT32([unsigned] int32 d, bytecode_p p)
 *   void PUT_INT32([unsigned] int32 d, bytecode_p p)
 *     Store the (unsigned) int32 'd' at <p>, the STORE_ variant
 *     then increments <p>.
 */

#if CHAR_BIT == 8

#define GET_CODE(p)      (*(p))
#define LOAD_CODE(p)     (*(p)++)

#define PUT_CODE(p,c)    (*(p) = (c))
#define STORE_CODE(p,c)  (*(p)++ = (c))
#define RSTORE_CODE(p,c)  (*--(p) = (c))

/* TODO: all these casts yield rvalues, so they shouldn't compile
 * TODO:: (and on xlc on AIX some of them indeed don't).
 */
#define GET_UINT8(p)     (*((unsigned char *)(p)))
#define GET_INT8(p)      (*((signed char *)(p)))
#define LOAD_UINT8(p)    (*((unsigned char *)(p)++))
#define LOAD_INT8(p)     (*((signed char *)(p)++))

#define PUT_UINT8(p,c)   (*((unsigned char *)(p)) = (c))
#define PUT_INT8(p,c)    (*((signed char *)(p)) = (c))
#define STORE_UINT8(p,c) (*((unsigned char *)(p)++) = (c))
#define STORE_INT8(p,c)  (*((signed char *)(p)++) = (c))

/* TODO: These generic mem-macros should go into a macros.h. See also
 * TODO:: how mudos does it.
 * Note: the lowlevel BYTE macros can't use MACRO(), since this is
 * needed on the next abstraction level, and a macro can't be nested
 * into itself.
 */
#define LOAD_2BYTE(d,p) ( ((char *)&(d))[0] = *(char *)(p)++, \
                          ((char *)&(d))[1] = *(char *)(p)++)

#define GET_2BYTE(d,p)  ( ((char *)&(d))[0] = ((char *)(p))[0], \
                          ((char *)&(d))[1] = ((char *)(p))[1] )

#define STORE_2BYTE(p,d) do {\
                             unsigned char * _q, ** _qq; \
                             _q = (unsigned char *)(p); \
                             _qq = (unsigned char **)&(p); \
                             _q[0] = ((unsigned char *)&(d))[0]; \
                             _q[1] = ((unsigned char *)&(d))[1]; \
                             *_qq += 2; \
                         } while(0)

#define RSTORE_2BYTE(p,d) do {\
                             unsigned char * _q, ** _qq; \
                             _q = (unsigned char *)(p); \
                             _qq = (unsigned char **)&(p); \
                             _q[-2] = ((unsigned char *)&(d))[0]; \
                             _q[-1] = ((unsigned char *)&(d))[1]; \
                             *_qq -= 2; \
                         } while(0)

#define PUT_2BYTE(p,d)  ( ((char *)(p))[0] = ((char *)&(d))[0], \
                          ((char *)(p))[1] = ((char *)&(d))[1] )

#define LOAD_3BYTE(d,p) ( (d) =   ((*(unsigned char *)(p)++) << 16) \
                                | ((*(unsigned char *)(p)++) <<  8) \
                                | (*(unsigned char *)(p)++) )

#define GET_3BYTE(d,p) ( (d) =   ((((unsigned char *)(p))[0]) << 16) \
                               | ((((unsigned char *)(p))[1]) <<  8) \
                               | (((unsigned char *)(p))[2]) )

#define STORE_3BYTE(p,d) do {\
                             unsigned char * _q, ** _qq; \
                             unsigned long _d = (unsigned long)(d); \
                             _q = (unsigned char *)(p); \
                             _qq = (unsigned char **)&(p); \
                             _q[0] = (unsigned char) (_d >> 16) \
                             _q[1] = (unsigned char) (_d >> 8) \
                             _q[2] = (unsigned char) (_d) \
                             *_qq += 3; \
                         } while(0)

#define PUT_3BYTE(p,d)  ( ((unsigned char *)(p))[0] = (unsigned char)((d) >> 16), \
                          ((unsigned char *)(p))[1] = (unsigned char)((d) >> 8), \
                          ((unsigned char *)(p))[2] = (unsigned char)(d) )

#define LOAD_4BYTE(d,p) ( ((char *)&(d))[0] = *(char *)(p)++, \
                          ((char *)&(d))[1] = *(char *)(p)++, \
                          ((char *)&(d))[2] = *(char *)(p)++, \
                          ((char *)&(d))[3] = *(char *)(p)++ )

#define GET_4BYTE(d,p)  ( ((char *)&(d))[0] = ((char *)(p))[0], \
                          ((char *)&(d))[1] = ((char *)(p))[1], \
                          ((char *)&(d))[2] = ((char *)(p))[2], \
                          ((char *)&(d))[3] = ((char *)(p))[3] )

#define STORE_4BYTE(p,d) ( *(unsigned char *)(p)++ = ((char *)&(d))[0], \
                           *(unsigned char *)(p)++ = ((char *)&(d))[1], \
                           *(unsigned char *)(p)++ = ((char *)&(d))[2], \
                           *(unsigned char *)(p)++ = ((char *)&(d))[3] )

#define PUT_4BYTE(p,d)  ( ((char *)(p))[0] = ((char *)&(d))[0], \
                          ((char *)(p))[1] = ((char *)&(d))[1], \
                          ((char *)(p))[2] = ((char *)&(d))[2], \
                          ((char *)(p))[3] = ((char *)&(d))[3] )


#if SIZEOF_SHORT == 2
#  define GET_SHORT(d,p)    GET_2BYTE(d,p)
#  define LOAD_SHORT(d,p)   LOAD_2BYTE(d,p)
#  define PUT_SHORT(p,d)    MACRO(unsigned short _us = (unsigned short)d; \
                                  PUT_2BYTE(p,_us);)
#  define STORE_SHORT(p,d)  MACRO(unsigned short _us = (unsigned short)d; \
                                 STORE_2BYTE(p,_us);)
#  define RSTORE_SHORT(p,d) MACRO(unsigned short _us = (unsigned short)d; \
                                 RSTORE_2BYTE(p,_us);)
#else
#  error "Unsupported size of short."
#endif /* SIZEOF_SHORT */

#if SIZEOF_LONG == 4
#  define GET_LONG(d,p)    GET_4BYTE(d,p)
#  define LOAD_LONG(d,p)   LOAD_4BYTE(d,p)
#  define PUT_LONG(p,d)    MACRO(unsigned long _us = (unsigned long)d; \
                                 PUT_4BYTE(p,_us);)
#  define STORE_LONG(p,d)  MACRO(unsigned long _us = (unsigned long)d; \
                                 STORE_4BYTE(p,_us);)
#  define RSTORE_LONG(p,d) MACRO(unsigned long _us = (unsigned long)d; \
                                 RSTORE_4BYTE(p,_us);)
#elif SIZEOF_LONG == 8 && SIZEOF_INT == 4
#  define GET_LONG(d,p)    MACRO(int _ui; GET_4BYTE(_ui,p); \
                                 d = _ui;)
#  define LOAD_LONG(d,p)   MACRO(int _ui; LOAD_4BYTE(_ui,p); \
                                 d = _ui;)
#  define PUT_LONG(p,d)    MACRO(unsigned int _ui = (unsigned int)d; \
                                 PUT_4BYTE(p,_ui);)
#  define STORE_LONG(p,d)  MACRO(unsigned int _ui = (unsigned int)d; \
                                 STORE_4BYTE(p,_ui);)
#  define RSTORE_LONG(p,d) MACRO(unsigned int _ui = (unsigned int)d; \
                                 RSTORE_4BYTE(p,_ui);)
#else
#  error "Unsupported size of long."
#endif /* SIZEOF_LONG */

#define LOAD_INT16(d,p)  LOAD_2BYTE(d,p)

#define LOAD_INT32(d,p)  LOAD_4BYTE(d,p)
#define GET_INT32(d,p)   GET_4BYTE(d,p)
#define PUT_INT32(p,d)   PUT_4BYTE(p,d)
#define STORE_INT32(p,d) STORE_4BYTE(p,d)

#endif /* CHAR_BIT */

#ifndef GET_CODE
#  error "No bytecode type defined."
#endif

/***************************************************************************/

#endif /* BYTECODE_H__ */
