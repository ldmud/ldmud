#ifndef SWITCH_H__
#define SWITCH_H__ 1

/*---------------------------------------------------------------------------
 * Data structures used mainly by the switch code generation.
 *
 *---------------------------------------------------------------------------
 * The switch() instruction is implemented using a highly efficient table
 * lookup system. The code for the instruction is generated in two places
 * (LPC compiler and lambda compiler), therefore some of the 'internal'
 * datastructures need to be published here.
 *
 * The compiler makes sure that there is always a 'default' case
 * and that all execution paths eventually execute a F_BREAK.
 *
 * The layout created by the LPC compiler is this:
 *
 *     switch b1 a2 b2 [b3 [b4] ]
 *            instructions (sans the first byte 'i0')...
 *            l[]
 *            [c0 [c1]]
 *            a0 a1 i0
 *            v*n
 *            o*n
 *            [d0]
 *
 * b1 & 0x03 is 0, marking this switch statement as unaligned.
 * Since for an efficient search the tables v*n and o*n must be
 * 4-Byte aligned (TODO: on some machines 8-Byte), the interpreter
 * will on first execution of such a switch align it (using
 * closure:align_switch()) by arranging the bytes a0..a2 around
 * the tables. The alignment can't be done at compilation time,
 * because code generated after the switch may cause to move
 * the switch code back- or forward. The aligned layout is this:
 *
 *     switch b1 b2 [b3 [b4] ]
 *            instructions...
 *            l[]
 *            [c0 [c1]]            <-- p0 = pc + offset
 *            a0..
 *            v[]                  <-- tabstart
 *            o[]                  <-- end_tab = pc + offset + tablen
 *            ..a2                 <-- p1
 *            [d0]
 *
 *  b1 (bits 1..0) = len: the length in bytes needed to store
 *        'offset', 'tablen', 'default offset', 'o*n' and the
 *        length of lookup tables for table ranges.
 *  b1 (bits 7..2) = tablen lo
 *  c0 = tablen mid (optional)
 *  c1 = tablen hi  (optional)
 *  b2 = offset lo
 *  b3 = offset med (optional)
 *  b4 = offset hi  (optional)
 *  a0, a1 = default-case offset lo and med in host byte order
 *  d0     = default-case offset hi (optional)
 *  a2 'type' (bits 0..4): start position for search (used to index
 *                         a table with the real offsets)
 *            (bit  5)   : 0: numeric switch , 1: string switch
 *            (bits 6..7): in an unaligned switch, the true value
 *                         of <len> (b1, bits 1..0).
 *  l[]: range lookup table: each <len> bytes, network byte order
 *       (numeric switch only)
 *  v[]: case values, char* or p_int, host byte order
 *  o[]: case offsets : each <len> bytes, network byte order
 *
 * The case value table v[] holds (sorted numerically) all values
 * appearing in the case statements, both singular values and range
 * bounds. Range bound values (which are inclusive) always appear
 * next to each other.
 *
 * The offset table o[] holds the associated offset with
 * this interpretation:
 *
 *   singular cases: jump destination offsets relative to pc.
 *
 *   range cases:    the 'offset' for the lower bound is 1, the
 *                   offset for the upper bound gives the jump
 *                   destination relative to pc.
 *
 *   lookup ranges:  the 'offset' for the lower bound is 0, the
 *                   offset for the upper bound is an offset
 *                   pointing into the lookup table.
 *                   The real jump offset is then
 *                     l[o[i] + <value> - lower-bound].
 *
 *   The lookup ranges are used for an efficient implementation of
 *   sparse ranges like 'case 0: case 2: case 5: ...'.
 *---------------------------------------------------------------------------
 */

#define ZERO_AS_STR_CASE_LABEL ((char *)&main)
  /* This value is used to compile 'case 0' into switch on string types.
   * We can't use the NULL pointer for this purpose, which is used when
   * looking up a string that is not in the tabled string table and
   * therefore must not be found.
   * This value must not be misinterpreted as tabled string.
   * If the string handling is changed, this value must be adapted.
   */

#define CASE_BLOCKING_FACTOR 256 /* must be >= 2 */
   /* case_list_entry_t's are allocated in blocks of this count.
    */

/* Bitmasks and -values for selected bytes in the switch statement:
 */

/* The B1 byte: */

#define SWITCH_VALUELEN  0x03  /* Bitmask */
  /* The length in bytes needed to store 'offset', 'tablen',
   * 'default offset', 'o*n' and the length of lookup tables for table
   * ranges.
   * If the value is 0, the switch is unaligned and the value is stored
   * in SWITCH_TYPE_VALUELEN.
   */

#define SWITCH_TABLEN        0xfc  /* Bitmask */
#define SWITCH_TABLEN_SHIFT  2     /* Leftshift */
  /* The low bits of the table length.
   */

/* The TYPE byte: */

#define SWITCH_START  0x1f  /* Bitmask */
  /* Start position for search (used to index a table with the real offsets).
    */

#define SWITCH_TYPE   0x20  /* Bitflag */
  /* Flag: 0: numeric switch , 1: string switch
   */

#define SWITCH_TYPE_VALUELEN        0xc0  /* Bitmask */
#define SWITCH_TYPE_VALUELEN_SHIFT  6     /* Leftshift */
  /* In an unaligned switch, the true value of SWITCH_VALUELEN.
   */

/* --- case_list_entry_t: description of one case value ---
 */
struct case_list_entry_s
{
    case_list_entry_t *next;
    p_int key;   /* Numeric case value, or a shared string casted */
    p_int addr;
      /* Offset of the associated code from the SWITCH+1.
       * If 1: this and the next entry denote a range.
       * If 0: this and the next entry denote a sparse lookup range.
       * TODO: Make this selfcontained?
       */
    p_int line;
      /* Line number of the case. It's 0 for range ends.
       */
};

/* --- case_state_t: compilation state of one switch() ---
 */

struct case_state_s
{
    case_list_entry_t *free_block;
      /* Currently used allocation block in the case_blocks list.
       */
    case_list_entry_t *next_free;
      /* First used entry in .free_block. The blocks are used
       * from the end, and the first cl_entry is reserved to link
       * the block list.
       */
    case_list_entry_t *list0;
    case_list_entry_t *list1;
      /* Head and second element of the active case list.
       * Both elements are in direct access to handle ranges (which are
       * encoded using two entries).
       */
    case_list_entry_t *zero;
    case_state_t      *previous;
      /* The case_state of the surrounding switch(), when compiling
       * a nested switch().
       */
    p_int              default_addr;
    char               some_numeric_labels;
    char               no_string_labels;
};

/* --- Variables (in closure.c) --- */
extern case_state_t case_state;

/* TODO: Move all these functions, a generic code generator
 * TODO:: and the execution function in its own file.
 * Nice would be an oo approach using a opaque type and the
 * functions: init(), add_case(), add_range(), generate().
 * An OO approach would also easily solve the problem that sequential
 * top-level switch()s accumulate in their memory usage.
 */

/* --- Prototypes (in closure.c) --- */
extern case_list_entry_t *new_case_entry(void);
extern void free_case_blocks(void);
extern void store_case_labels( p_int total_length
                             , p_int default_addr
                             , Bool numeric
                             , case_list_entry_t *zero
                             , bytecode_p (*get_space)(p_int)
                             , void (*move_instructions)(int, p_int)
                             , void (*cerror)(const char *)
                             , void (*cerrorl)(const char *, const char*, int, int)
);

#endif  /* SWITCH_H__ */
