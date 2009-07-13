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
 *
 * TODO: Instead of using *_LONG and *_SHORT we should introduce specific
 * TODO::types for specific tasks (like a type for offsets in the bytecode
   TODO::which can be different on different hosts and needs). In that way
   TODO::we should get rid of the *_SHORT and *_LONG variants.
 */

#if CHAR_BIT == 8

/* Types with specific semantics */
/* The basic code type encoding one instruction (bytecode_t) is in typedefs.h. 
// Type for storing offsets in the bytecode */
typedef int32_t         bc_offset_t;
typedef int16_t         bc_shortoffset_t;

// get_foo(), put_foo(), load_foo(), store_foo() and rstore_foo() functions
#include "bytecode_gen.h"

#if SIZEOF_SHORT != 2
#  error "Unsupported size of short."
#endif /* SIZEOF_SHORT */

/* some macros for compatibility */
#define GET_CODE(p)        get_code(p)
#define PUT_CODE(p,c)      put_code((p),(c))
#define LOAD_CODE(p)       load_code(&(p))
#define STORE_CODE(p,c)    store_code(&(p),(c))
#define RSTORE_CODE(p,c)   rstore_code(&(p),(c))

#define GET_UINT8(p)       get_uint8(p)
#define PUT_UINT8(p,c)     put_uint8((p),(c))
#define LOAD_UINT8(p)      load_uint8(&(p))
#define STORE_UINT8(p,c)   store_uint8(&(p),(c))

#define GET_SHORT(d,p)     (d = get_short((p)))
#define LOAD_SHORT(d,p)    (d = load_short(&(p)))
#define PUT_SHORT(p,d)     put_short((p),(d))
#define STORE_SHORT(p,d)   store_short(&(p),(d))
#define RSTORE_SHORT(p,d)  rstore_short(&(p),(d))

#endif /* CHAR_BIT */

/*
#ifndef GET_CODE
#  error "No bytecode type defined."
#endif
*/

#endif /* BYTECODE_H__ */

/* Tester: Coogan@tubmud, Arkas@Unitopia, largo@wunderland */

