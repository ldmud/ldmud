/*---------------------------------------------------------------------------
 * Unicode character properties.
 *
 *---------------------------------------------------------------------------
 * These are tables for unicode character properties used by the grapheme
 * detection routine.
 */

/* The Grapheme Cluster Break property as described in the
 * Unicode Standard Annex #29 "Unicode Text Segmentation".
 */
enum unicode_grapheme_cluster_break
{
    GRAPHEME_CONTROL,
    GRAPHEME_CARRIAGE_RETURN,
    GRAPHEME_LINE_FEED,
    GRAPHEME_SPACING_MARK,
    GRAPHEME_EXTEND,
    GRAPHEME_PREPEND,
    GRAPHEME_HANGUL_SYLLABLE_L,
    GRAPHEME_HANGUL_SYLLABLE_V,
    GRAPHEME_HANGUL_SYLLABLE_LV,
    GRAPHEME_HANGUL_SYLLABLE_T,
    GRAPHEME_HANGUL_SYLLABLE_LVT,
    GRAPHEME_REGIONAL_INDICATOR,
    GRAPHEME_ZERO_WIDTH_JOINER,
    GRAPHEME_OTHER,

    GRAPHEME_BREAK, /* Used in the algorithm. */
};

/* A simplified character width, derived from the East-Asian Width
 * property described in the Unicode Standard Annex #11.
 */
enum unicode_character_width
{
    WIDTH_NEUTRAL,
    WIDTH_NARROW,
    WIDTH_WIDE,
};

/* Information about a single character. */
struct unicode_char_data_s
{
    enum unicode_grapheme_cluster_break gcb;
    enum unicode_character_width width;
};

/* The Lookup into the character database is done using two lookup tables:
 * Index into unicode_table1 is the character >> UNICODE_TABLE_SHIFT.
 * The result of table1 plus the remaining UNICODE_TABLE_SHIFT bits from the
 * character yield the index into unicode_table2. unicode_table2 contains
 * the index into unicode_char_data.
 */
enum
{
    UNICODE_TABLE_SHIFT = 8
};

extern unsigned short unicode_table1[];
extern unsigned char unicode_table2[];
extern struct unicode_char_data_s unicode_char_data[];
