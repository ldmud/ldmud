/*---------------------------------------------------------------------------
 * Pattern Parser v 3.1  (non-compat mode)
 * (C) Copyright 1991 JnA (jna@cd.chalmers.se)
 *
 *---------------------------------------------------------------------------
 * TODO: Improve this efun, make it more general.
 * EFUN parse_command
 *
 *     int parse_command (string cmd, object  env, string fmt, mixed &var, ...)
 *     int parse_command (string cmd, object* arr, string fmt, mixed &var, ...)
 *
 * parse_command() is basically a spiffed up sscanf operating on word basis
 * and targeted at recognizing object descriptions from command strings.
 *
 * The efun takes the command string <cmd> and the object(s) <env>/<arr>
 * and tries to match it against the format string <fmt>. Successfully
 * matched elements are assigned to the variables <var>....
 * The result from the efun is 1 if the command could be fully matched,
 * and 0 otherwise.
 *
 * If the objects are given as a single object <env>, the efun matches
 * against the given object and all objects contained therein. Otherwise,
 * if the objects are given as an array <arr> of objects, the efun
 * matches only against the given objects.
 *
 * The format string <fmt> consists of words, syntactic markers, and
 * %-directives for the values to parse and return in the variables.
 * A typical example is " 'get' / 'take' %i " or
 * " 'spray' / 'paint' [paint] %i ". The elements in detail are:
 *
 *    'word': obligatory text
 *    [word]: optional text
 *    /     : Alternative marker
 *    %o    : Single item, object
 *    %s    : Any text
 *    %w    : Any word
 *    %p    : One of a list of prepositions.
 *            If the variable associated with %p is used to pass
 *            a list of words to the efun, the matching will take
 *            only against this list.
 *    %l    : Living objects
 *    %i    : Any objects
 *    %d    : Number >= 0, or when given textual: 0-99.
 *
 * A <word> in this context is any sequence of characters not containing
 * a space. 'living objects' are searched by calls to the (simul)efuns
 * find_player() and find_living(): both functions have to accept a name
 * as argument and return the object for this name, or 0 if there
 * is none.
 *
 * The results assigned to the variables by the %-directives are:
 *
 *    %o : returns an object
 *    %s : returns a string of words
 *    %w : returns a string of one word
 *    %p : if passed empty: a string
 *         if passed as array of words: var[0] is the matched word
 *    %i : returns an array with the following content:
 *           [0]: int: the count/number recognized in the object spec
 *                     > 0: a count (e.g. 'three', '4')
 *                     < 0: an ordinal (e.g. 'second', 'third')
 *                     = 0: 'all' or a generic plural such as 'apples'
 *           [1..]: object: all(!) objects matching the item description.
 *                          In the <env> form this may be the whole
 *                          recursive inventory of the <env> object.
 *         It is up to the caller to interpret the recognized numeral
 *         and to apply it on the list of matched objects.
 *    %l : as %i, except that only living objects are returned.
 *
 * %i and %l match descriptions like 'three red roses','all nasty bugs'
 * or 'second blue sword'.
 *
 * Note: Patterns of type: "%s %w %i" might not work as one would expect.
 * %w will always succeed so the arg corresponding to %s will always be empty.
 *
 *
 * To make the efun useful it must have a certain support from the mudlib:
 * it calls a set of functions in objects to get the information it needs
 * to parse a string.
 *
 *   1. string *parse_command_id_list()
 *       Normal singular names of the object.
 *
 *   2. string *parse_command_plural_id_list() - optional
 *       Plural forms of the names returned by 1.
 *       If this function doesn't exist, the parser tries to pluralize
 *       the names returned by 1.
 *
 *   3. string *parse_command_adjectiv_id_list() -  optional
 *       All adjectives associated with this object.
 *
 * All names and adjectives may consist of several words separated
 * by spaces.
 *
 * These functions should exist in all objects and are therefore best
 * put into a mandatory inherit file (e.g. /std/object.c).
 *
 * In addition the master object may offer the same functions to provide
 * reasonable defaults (like 'thing' as generic singular name):
 *
 *      string *parse_command_id_list()
 *        - Would normally return: ({ "one", "thing" })
 *
 *      string *parse_command_plural_id_list()
 *        - Would normally return: ({ "ones", "things", "them" })
 *
 *      string *parse_command_adjectiv_id_list()
 *        - Would normally return ({ "iffish" })
 *
 * Two additional functions in the master object provide the default
 * list of prepositions (needed for %p) and the single 'all' word:
 *
 *      string *parse_command_prepos_list()
 *        - Would normally return: ({ "in", "on", "under", "behind", "beside" })
 *
 *      string parse_command_all_word()
 *        - Would normally return: "all"
 *---------------------------------------------------------------------------
 * TODO: A proper localisation would at least put all the following into the
 * TODO:: master object as well.
 *
 * If you want to use a different language than English, you need to write
 * a small file 'parse_local.c' and include it into parse.c at the
 * marked position.
 *
 * The 'parse_local.c' has to contain your localized pluralmaker and
 * the textual number words and should look like this:
 *
 *   ---------- SNIP ----------
 *   #define PARSE_FOREIGN
 *
 *   char *parse_to_plural(str)
 *       char *str;
 *   {
 *
 *       * Your own plural converter for your language *
 *
 *   }
 *
 *     * The numberwords below should be replaced for the new language *
 *
 *   static char *ord1[] = {"", "first", "second", "third", "fourth", "fifth",
 *                          "sixth", "seventh", "eighth", "nineth", "tenth",
 *                          "eleventh", "twelfth", "thirteenth", "fourteenth",
 *                          "fifteenth", "sixteenth", "seventeenth",
 *                          "eighteenth","nineteenth"};
 *
 *   static char *ord10[] = {"", "", "twenty","thirty","forty","fifty","sixty",
 *                           "seventy", "eighty","ninety"};
 *
 *   static char *sord10[] = {"", "", "twentieth", "thirtieth", "fortieth",
 *                            "fiftieth", "sixtieth","seventieth", "eightieth",
 *                            "ninetieth"};
 *
 *   static char *num1[] = {"", "one","two","three","four","five","six",
 *                          "seven","eight","nine","ten",
 *                          "eleven","twelve","thirteen","fourteen","fifteen",
 *                          "sixteen", "seventeen","eighteen","nineteen"};
 *
 *   static char *num10[] = {"", "", "twenty","thirty","forty","fifty","sixty",
 *                          "seventy", "eighty","ninety"};
 *   ---------- SNIP ----------
 *---------------------------------------------------------------------------
 */

#include "driver.h"

#if defined(USE_PARSE_COMMAND)

#include "typedefs.h"

#include <stdio.h>
#include <ctype.h>
#include <time.h>

#include "parse.h"

#include "actions.h"
#include "array.h"
#include "gcollect.h"
#include "interpret.h"
#include "lex.h"
#include "main.h"
#include "mstrings.h"
#include "object.h"
#include "simulate.h"
#include "stdstrings.h"
#include "svalue.h"
#include "wiz_list.h"
#include "xalloc.h"

/* For a localisation of parse_command(),
 * #include "parse_local.c"
 * here.
 */

/*-------------------------------------------------------------------------*/
/* Some useful string macros
 */
#define PREFIXED(x,y) (strncmp(x, y, strlen(x)) == 0)

/*-------------------------------------------------------------------------*/
/* To make parse_command() reentrant, the module maintains a list
 * of previous contexts using this structure:
 */
typedef struct parse_context_s parse_context_t;

struct parse_context_s
{
  parse_context_t *previous;

  vector_t *id,   *plid,   *adjid;
  vector_t *id_d, *plid_d, *adjid_d, *prepos;
  string_t *allword;
    /* This context: the lists of ids and such. */

  vector_t *wvec, *patvec, *obvec;
    /* Next context(!): word, pattern and object vector */
};

/*-------------------------------------------------------------------------*/

/* Arrays holding the constituent words of textual numbers from 0 to 99.
 * The numbers are constructed by concatenation.
 */

#ifndef PARSE_FOREIGN

static char *ord1[] = {"", "first", "second", "third", "fourth", "fifth"
                      , "sixth", "seventh", "eighth", "nineth", "tenth"
                      , "eleventh", "twelfth", "thirteenth", "fourteenth"
                      , "fifteenth", "sixteenth", "seventeenth"
                      , "eighteenth","nineteenth"};
  /* The ordinals from 1 to 19, also used to build the ordinals 20..99.
   */

static char *ord10[] = { "", "", "twenty","thirty","forty","fifty","sixty"
                       , "seventy", "eighty","ninety"};
  /* The first word for the ordinals 20..99.
   */

static char *sord10[] = { "", "", "twentieth", "thirtieth", "fortieth"
                        , "fiftieth", "sixtieth","seventieth", "eightieth"
                        , "ninetieth"};
  /* The ordinals 20, 30, ..., 90.
   */

static char *num1[] = { "", "one","two","three","four","five","six"
                      , "seven","eight","nine","ten"
                      , "eleven","twelve","thirteen","fourteen","fifteen"
                      , "sixteen", "seventeen","eighteen","nineteen"};
  /* The numbers 1 to 19, also used to build the numbers 20..99.
   */

static char *num10[] = { "", "", "twenty","thirty","forty","fifty","sixty"
                       , "seventy", "eighty","ninety"};
  /* The first word for the numbers 20..99.
   */

#endif

/*-------------------------------------------------------------------------*/

static svalue_t find_living_closures[2]
  = { { T_INVALID }, { T_INVALID } };
  /* The closures to the functions 'find_living' and 'find_player',
   * which are generated at runtime to be able to find simul-efuns with
   * these names.
   * TODO: This too should go into a master function.
   */

static parse_context_t *gPrevious_context = NULL;
  /* The list of previous contexts.
   */

/* The following variables 'cache' the various lists read from the
 * matched objects and the master object. The original values are
 * save when parse_command() is called, making the efun re-entrant.
 */
static vector_t *gId_list      = NULL;
static vector_t *gPluid_list   = NULL;
static vector_t *gAdjid_list   = NULL;
  /* Arrays of the lists from the objects matched against.
   * For example gId_list[2] returns the singular name list for
   * the third object.
   * The arrays are filled on demand only. svalue-0s denote entries
   * yet to fill, svalue-1s are entries where the object doesn't provide
   * the particular information.
   */

static vector_t *gId_list_d    = NULL;
static vector_t *gPluid_list_d = NULL;
static vector_t *gAdjid_list_d = NULL;
static vector_t *gPrepos_list  = NULL;
static string_t *gAllword      = NULL;
  /* The lists and the 'all' word from the master object.
   */

/*-------------------------------------------------------------------------*/
static object_t *
find_living_object (string_t *name, Bool player)

/* Find the living (<player> is false) or player (<player> is true)
 * with the name <name>.
 * Return the found object, or NULL if not found.
 *
 * The functions calls the (simul)efuns 'find_living' resp.
 * 'find_player' for this purpose.
 */

{
    svalue_t *svp;

    /* Get or create the closure for the function to call */
    svp = &find_living_closures[player ? 1 : 0];
    if (svp->type == T_INVALID)
    {
        /* We have to create the closure */
        symbol_efun(player ? STR_PC_FIND_PLAYER : STR_PC_FIND_LIVING, svp);
    }

    /* Call the closure */
    inter_sp++;
    put_ref_string(inter_sp, name);
    call_lambda(svp, 1);
    pop_stack();
      /* In theory this could lose the last ref to the result object.
       * In praxis, those objects have more refs.
       */

    return inter_sp[1].type != T_OBJECT ? NULL : inter_sp[1].u.ob;
} /* find_living_object() */

/*-------------------------------------------------------------------------*/

#ifdef GC_SUPPORT

void
clear_parse_refs (void)

/* GC support: Clear the references of all memory held by the parser.
 */

{
    clear_ref_in_vector( find_living_closures
                       , sizeof find_living_closures / sizeof(svalue_t)
    );
} /* clear_parse_refs() */

/*-------------------------------------------------------------------------*/
void
count_parse_refs (void)

/* GC support: Count the references of all memory held by the parser.
 */

{
    count_ref_in_vector( find_living_closures
                       , sizeof find_living_closures / sizeof(svalue_t)
    );
} /* count_parse_refs() */

#endif /* GC_SUPPORT */

#ifndef PARSE_FOREIGN

/*-------------------------------------------------------------------------*/
static string_t *
parse_one_plural (string_t *str)

/* Change the singular noun <str> to a plural and return it.
 * The result is either a new string with one reference, or <str> itself
 * with an added reference.
 */

{
    static char pbuf[100];  /* Result buffer */

    char   ch, ch2;  /* Last two characters in <str> */
    size_t sl;       /* Last index in <str> */

    sl = mstrsize(str);
    if (sl < 3 || sl > sizeof(pbuf) - 10)
        return str;
    sl--;

    /* Copy <str> except for the last char into pbuf */
    ch = get_txt(str)[sl];
    ch2 = get_txt(str)[sl-1];
    strcpy(pbuf, get_txt(str)); pbuf[sl] = '\0';

    /* Try to make plural based on the last two chars */
    switch (ch)
    {
    case 's':
    case 'x':
    case 'h':
        return new_mstring(strcat(pbuf, "ses"));

    case 'y':
        return new_mstring(strcat(pbuf, "ies"));

    case 'e':
        if (ch2 == 'f')
        {
            pbuf[sl-1] = 0;
            return new_mstring(strcat(pbuf, "ves"));
        }
    }

    /* Some known special cases */
    if (mstreq(str, STR_PC_CORPSE)) return ref_mstring(STR_PC_CORPSES);
    if (mstreq(str, STR_PC_TOOTH)) return ref_mstring(STR_PC_TEETH);
    if (mstreq(str, STR_PC_FOOT)) return ref_mstring(STR_PC_FEET);
    if (mstreq(str, STR_PC_MAN)) return ref_mstring(STR_PC_MEN);
    if (mstreq(str, STR_PC_WOMAN)) return ref_mstring(STR_PC_WOMEN);
    if (mstreq(str, STR_PC_CHILD)) return ref_mstring(STR_PC_CHILDREN);
    if (mstreq(str, STR_PC_SHEEP)) return ref_mstring(STR_PC_SHEEP);

    /* Default: just append 's' */
    pbuf[sl] = ch;
    return new_mstring(strcat(pbuf, "s"));
} /* parse_one_plural() */

/*-------------------------------------------------------------------------*/
static string_t *
parse_to_plural (string_t *str)

/* Change the singular name <str> to a plural name. The result is a new
 * string with one reference.
 *
 * The algorithm groups the <str> into runs delimited by 'of' (e.g. "the box
 * of the king" and pluralizes the last word before each 'of' and the last
 * word in the string (giving "the boxes of the kings").
 * TODO: TubMud has a good plural maker.
 */

{
    vector_t *words;
    svalue_t  stmp;
    string_t *sp;
    size_t    il;
    Bool      changed;

    /* If it's a single word, it's easy */
    if (!(strchr(get_txt(str), ' ')))
        return parse_one_plural(str);

    /* Multiple words, possible grouped into runs delimited by 'of':
     * pluralize the last word in the string, and the last word
     * before each 'of'.
     */
    words = explode_string(str, STR_SPACE);

    for (changed = MY_FALSE, il = 1; (p_int)il < VEC_SIZE(words); il++)
    {
        if ((mstreq(words->item[il].u.str, STR_PC_OF))
         || (p_int)il+1 == VEC_SIZE(words))
         {
            /* Got one to pluralize */
            sp = parse_one_plural(words->item[il-1].u.str);
            if (sp != words->item[il-1].u.str)
            {
                put_string(&stmp, sp);
                transfer_svalue(&words->item[il-1], &stmp);
                changed = MY_TRUE;
            }
            else
                free_mstring(sp); /* Reuse the old reference */
        }
    }

    /* If nothing changed, just return a copy of the original */
    if (!changed)
    {
        free_array(words);
        return ref_mstring(str);
    }

    /* We changed it: return the new name */
    sp = implode_string(words, STR_SPACE);
    free_array(words);
    return sp;
} /* parse_to_plural() */

#endif /* PARSE_FOREIGN */

/*-------------------------------------------------------------------------*/
static void
load_lpc_info (size_t ix, object_t *ob)

/* Load the relevant information (singular names, plural names and adjectives)
 * for object <ob> into position <ix> of the cache lists, unless already
 * loaded.
 *
 * If the object does not provide plural names, they are synthesized from
 * the singular names.
 */

{
    Bool make_plural = MY_FALSE;  /* TRUE: synthesize plurals */
    svalue_t * ret;

    if (!ob || ob->flags & O_DESTRUCTED)
        return;

    /* Get the plural names, if any.
     */
    if (gPluid_list
     && VEC_SIZE(gPluid_list) > (p_int)ix
     && gPluid_list->item[ix].type == T_NUMBER
     && gPluid_list->item[ix].u.number == 0
       )
    {
        ret = apply(STR_PC_P_ID_LIST, ob, 0);
        if (ret && ret->type == T_POINTER)
            assign_svalue_no_free(&gPluid_list->item[ix], ret);
        else
        {
            make_plural = MY_TRUE;
            gPluid_list->item[ix].u.number = 1;
        }
    }

    /* Get the singular names and, if desired, synthesize the
     * plural names.
     */
    if (gId_list
     && VEC_SIZE(gId_list) > (p_int)ix
     && gId_list->item[ix].type == T_NUMBER
     && gId_list->item[ix].u.number == 0
     && !(ob->flags & O_DESTRUCTED) )
    {
        ret = apply(STR_PC_ID_LIST, ob, 0);
        if (ret && ret->type == T_POINTER)
        {
            assign_svalue_no_free(&gId_list->item[ix], ret);

            if (make_plural)
            {
                /* Pluralize the singular names */

                vector_t *tmp, *sing;
                svalue_t sval;
                string_t *str;
                size_t il;

                tmp = allocate_array((size_t)VEC_SIZE(ret->u.vec));
                if (!tmp)
                    errorf("(parse_command) Out of memory: array[%lu] for "
                          "plural names.\n"
                         , (unsigned long)VEC_SIZE(ret->u.vec));
                sing = ret->u.vec;
                for (il = 0; (p_int)il < VEC_SIZE(tmp); il++)
                {
                    if (sing->item[il].type == T_STRING)
                    {
                        str = parse_to_plural(sing->item[il].u.str);
                        put_string(&sval, str);
                        transfer_svalue_no_free(&tmp->item[il],&sval);
                    }
                }
                put_array(&sval, tmp);
                transfer_svalue_no_free(&gPluid_list->item[ix], &sval);
            }
        }
        else
        {
            gId_list->item[ix].u.number = 1;
        }
    }

    /* Get the adjectives, if any.
     */
    if (gAdjid_list
     && VEC_SIZE(gAdjid_list) > (p_int)ix
     && gAdjid_list->item[ix].type == T_NUMBER
     && gAdjid_list->item[ix].u.number == 0
     && !(ob->flags & O_DESTRUCTED) )
    {
        ret = apply(STR_PC_ADJ_LIST, ob, 0);
        if (ret && ret->type == T_POINTER)
            assign_svalue_no_free(&gAdjid_list->item[ix], ret);
        else
            gAdjid_list->item[ix].u.number = 1;
    }
} /* load_lpc_info() */

/*-------------------------------------------------------------------------*/
static void
parse_error_handler (error_handler_t *arg UNUSED)

/* The current parse_command() processing was interrupted by an error.
 * Clean up the current context and restore the previous context.
 */

{
#ifdef __MWERKS__
#    pragma unused(arg)
#endif
    parse_context_t *old;

    old = gPrevious_context;

    /* Delete and free the id arrays. */

    if (gId_list)
        free_array(gId_list);

    if (gPluid_list)
        free_array(gPluid_list);

    if (gAdjid_list)
        free_array(gAdjid_list);

    if (gId_list_d)
        free_array(gId_list_d);

    if (gPluid_list_d)
        free_array(gPluid_list_d);

    if (gAdjid_list_d)
        free_array(gAdjid_list_d);

    if (gPrepos_list)
        free_array(gPrepos_list);

    if (gAllword)
        free_mstring(gAllword);

    /* Restore the previous lists */

    gId_list_d    = old->id_d;
    gPluid_list_d = old->plid_d;
    gAdjid_list_d = old->adjid_d;
    gPrepos_list  = old->prepos;
    gId_list      = old->id;
    gPluid_list   = old->plid;
    gAdjid_list   = old->adjid;
    gAllword      = old->allword;

    /* Free the local arrays */
    free_array(old->wvec);
    free_array(old->patvec);
    free_array(old->obvec);

    gPrevious_context = old->previous;
    xfree(old);
} /* parse_error_handler() */

/*-------------------------------------------------------------------------*/
static INLINE void
stack_put (svalue_t *pval, svalue_t *sp, size_t pos, int max)

/* Store the value <pval> into the lvalue <sp>[<pos>].
 * If <pval> is NULL, <sp>[<pos>] not a lvalue or <pos> >= <max>,
 * nothing happens - which is a good thing as this function stores
 * the parsed results into the variables passed to the efun, and we
 * never know what the wizards are going to pass there.
 */

{
    if (pval && pos < (size_t)max && sp[pos].type == T_LVALUE)
        transfer_svalue(sp[pos].u.lvalue, pval);
} /* stack_put() */

/*-------------------------------------------------------------------------*/
static svalue_t *
slice_words (vector_t *wvec, size_t from, size_t to)

/* Return an imploded string of words from <wvec>[<from>..<to>] as tabled
 * string svalue.
 * Return NULL if there is nothing to slice.
 */

{
    vector_t        *slice;
    string_t        *tx;
    static svalue_t  stmp;

    if (from > to)
        return NULL;

    slice = slice_array(wvec, from, to);

    if (VEC_SIZE(slice))
        tx = implode_string(slice, STR_SPACE);
    else
        tx = NULL;

    free_array(slice);
    if (tx)
    {
        put_string(&stmp, make_tabled(tx));
        return &stmp;
    }
    else
        return NULL;
} /* slice_words() */

/*-------------------------------------------------------------------------*/
static int
find_string (string_t *str, vector_t *wvec, size_t *cix_in)

/* Test if the (multi-word) string <str> exists in the array of words <wvec>
 * at or after position <cix_in>.
 * If found, return the starting position in <wvec> and set <cix_in> to
 * the position of the last word of the found string.
 * If not round, return -1, <cix_in> will be set to the end of <wvec>.
 */

{
    int fpos;
    string_t *p1;
    char *p2;
    vector_t *split;

    /* Step through wvec and look for a match */
    for (; (p_int)*cix_in < VEC_SIZE(wvec); (*cix_in)++)
    {
        p1 = wvec->item[*cix_in].u.str;

        /* Quick test: first character has to match */
        if (get_txt(p1)[0] != get_txt(str)[0])
            continue;

        if (mstreq(p1, str)) /* str was one word and we found it */
            return (int)*cix_in;

        if (!(p2 = strchr(get_txt(str), ' ')))
            continue;

        /* If str is a multiword string and we need to make some special checks
        */
        if ((p_int)*cix_in + 1 == VEC_SIZE(wvec))
            continue;

        split = explode_string(str, STR_SPACE);

        /* Now: wvec->size - *cix_in = 2: One extra word
         *                           = 3: Two extra words
         */
        if (!split || VEC_SIZE(split) > (VEC_SIZE(wvec) - (p_int)*cix_in))
        {
            if (split)
                free_array(split);
            continue;
        }

        /* Test if the following words match the string */
        fpos = (int)*cix_in;
        for (; *cix_in < (size_t)(VEC_SIZE(split) + fpos); (*cix_in)++)
        {
            if (!mstreq(split->item[*cix_in-fpos].u.str,
                       wvec->item[*cix_in].u.str))
                break;
        }

        /* If all of split matched, we found it */
        if ((p_int)(*cix_in - fpos) == VEC_SIZE(split))
        {
            (*cix_in)--; /* point to the last matched word */
            return fpos;
        }

        /* Not found: continue search */
        *cix_in = fpos;

    }

    /* Not found */

    return -1;
} /* find_string() */

/*-------------------------------------------------------------------------*/
static int
member_string (string_t *str, vector_t *svec)

/* Test if string <str> is member of the array <svec>.
 * Return the position if found, and -1 otherwise.
 */

{
    size_t il;

    if (!svec)
        return -1;

    for (il = 0; (p_int)il < VEC_SIZE(svec); il++)
    {
        if (svec->item[il].type != T_STRING)
            continue;

        if (mstreq(svec->item[il].u.str, str))
            return (int)il;
    }

    return -1;
} /* member_string() */

/*-------------------------------------------------------------------------*/
static Bool
check_adjectiv (size_t obix, vector_t *wvec, size_t from, size_t to)

/* Check if the command words <wvec>[<from>..<to>] match the adjectives
 * for object <obix>.
 * Return TRUE if yes.
 */

{
    size_t    il;
    size_t    sum;    /* Total length of command words tested */
    size_t    back;
    Bool      fail;   /* TRUE if not found */
    string_t *adstr;
    char     *adstrp;
    vector_t *ids;    /* Adj list of the object */

    /* Get the objects adj-list if existing */
    if (gAdjid_list->item[obix].type == T_POINTER)
        ids = gAdjid_list->item[obix].u.vec;
    else
        ids = NULL;

    /* Scan the given command words, sum up their length and
     * test if all of them match the adjectives given.
     */
    for (sum = 0, fail = MY_FALSE, il = from; il <= to; il++)
    {
        sum += mstrsize(wvec->item[il].u.str) + 1;
        if ((member_string(wvec->item[il].u.str, ids) < 0)
         && (member_string(wvec->item[il].u.str, gAdjid_list_d) < 0))
        {
            fail = MY_TRUE;
        }
    }

    /* Simple case: all adjs were single words and matched.
    */
    if (!fail)
        return MY_TRUE;

    if (from == to)
        return MY_FALSE;

    /* It could be that some of the adjectives provided by the object are
     * multi-words; in that case the above loop would signal a mismatch.
     *
     * To find these, concatenate the command words with spaces and
     * test them against the single adjective strings.
     * TODO: This test could be implemented faster.
     */

    adstr = NULL;
    adstrp = xalloc(sum);  /* Workspace */

    /* Test the adjectives one after the other */
    for (il = from; il < to;)
    {
        /* For every adjective, perform a greedy match first, ie
         * try to match the longer concatenated strings before
         * the shorter ones.
         */
        for (back = to; back > il; back--)
        {
            /* Catenate the adjective from the command words */
            adstrp[0] = '\0';
            for (sum = il; sum <= back; sum++)
            {
                if (sum > il)
                    strcat(adstrp, " ");
                strcat(adstrp, get_txt(wvec->item[sum].u.str));
            }

            adstr = new_mstring(adstrp);

            if ((member_string(adstr, ids) >= 0)
             || (member_string(adstr, gAdjid_list_d) >= 0))
            {
                /* Found: continue search after this matched adjective */
                il = back + 1;
                break;
            }

            /* Not found: abort */
            mstring_free(adstr);
            xfree(adstrp);
            return MY_FALSE;
        }
    }

    /* Found: clean up and return */
    mstring_free(adstr);
    xfree(adstrp);
    return MY_TRUE;
} /* check_adjectiv() */

/*-------------------------------------------------------------------------*/
static svalue_t *
number_parse( vector_t *obvec UNUSED  /* in: array of objects to match against */
            , vector_t *wvec          /* in: array of words to match */
            , size_t   *cix_in        /* in-out: position in wvec */
            , Bool     *fail          /* out: TRUE on mismatch */
            )

/* Interpret the words in wvec[cix_in] as numeric descriptor, parse it
 * and return an int-svalue with the result:
 *   > 0: a number ('one', 'two', 'three', or the number given)
 *   < 0: an ordinal ('first', 'second')
 *   = 0: any ('zero', 0, the gAllword)
 * On failure, return NULL.
 *
 * On return, <fail> is set to the success state of the match, and <cix_in>
 * has been set past the parsed number.
 */

{
#ifdef __MWERKS__
#    pragma unused(obvec)
#endif

    static svalue_t stmp;  /* Result buffer */

    size_t cix;
    int ten, ones, num;

    cix = *cix_in; *fail = MY_FALSE;

    ones = 0;

    /* First try to parse the number in digit representation */
    if (sscanf(get_txt(wvec->item[cix].u.str), "%d", &num))
    {
        if (num >= 0)
        {
            (*cix_in)++;
            put_number(&stmp, num);
            return &stmp;
        }
        *fail = MY_TRUE;
        return NULL; /* Only nonnegative numbers allowed */
    }

    /* Is it the 'all' word? */
    if (gAllword && mstreq(wvec->item[cix].u.str, gAllword))
    {
        (*cix_in)++;
        put_number(&stmp, 0);
        return &stmp;
    }

    /* Test the number against every known textual number.
     */
    for (ten = 0; ten < 10; ten++)
    {
        char *second;

        /* Test if the first part of the word matches */
        if (!PREFIXED(num10[ten], get_txt(wvec->item[cix].u.str)))
            continue;

        /* Yup, now match the rest */
        second = get_txt(wvec->item[cix].u.str) + strlen(num10[ten]);

        for (ones = 0; ones < 10; ones++)
        {
            char *tmp;

            tmp = (ten>1) ? num1[ones] : num1[ten*10+ones];
            if (!strcmp(second, tmp))
            {
                (*cix_in)++;
                put_number(&stmp, ten*10+ones);
                return &stmp;
            }
        } /* for (ones) */
    } /* for (ten) */

    /* Test the number against every known textual ordinal.
     */
    for (ten = 0; ten < 10; ten++)
    {
        char *second;

        /* Multiples of 10 have their own words */
        if (!strcmp(sord10[ten], get_txt(wvec->item[cix].u.str)))
        {
            (*cix_in)++;
            put_number(&stmp, -(ten*10+ones));
            return &stmp;
        }

        /* Test if the first part of the word matches */
        if (!PREFIXED(ord10[ten], get_txt(wvec->item[cix].u.str)))
            continue;

        /* Yup, now match the rest */
        second = get_txt(wvec->item[cix].u.str) + strlen(ord10[ten]);

        for(ones = 1; ones < 10; ones++)
        {
            char *tmp;

            tmp = (ten > 1) ? ord1[ones] : ord1[ten*10+ones];
            if (!strcmp(second, tmp))
            {
                (*cix_in)++;
                put_number(&stmp, -(ten*10+ones));
                return &stmp;
            }
        }
    }

    /* Nothing matches */
    *fail = MY_TRUE;
    return NULL;
} /* number_parse() */

/*-------------------------------------------------------------------------*/
static Bool
match_object (size_t obix, vector_t *wvec, size_t *cix_in, Bool *plur)

/* Test if a given object <obix> matches the description <wvec>[<cix_in>..].
 * If <plur> is TRUE, only the plural description is considered, otherwise
 * both plural and singular.
 *
 * Return TRUE if the object matches; <plur> will be set to true if the
 * plural description matched, and <cix_in> will point to the word after
 * the matched description.
 * Return FALSE if it didn't match.
 */
{
    vector_t *ids;    /* Id-list to test against */
    int       cplur;  /* Which id-list to test (0..3) */
    size_t    il, old_cix;
    int       pos;
    string_t  *str;

    /* Loop over the four lists of ids */
    for (cplur = *plur ? 2 : 0; cplur < 4; cplur++)
    {
        switch (cplur)
        {
        case 0:  /* Global singular ids */
            if (!gId_list_d)
                continue;
            ids = gId_list_d;
            break;

        case 1:  /* Object singular ids */
            if (!gId_list
             ||  VEC_SIZE(gId_list) <= (p_int)obix
             ||  gId_list->item[obix].type != T_POINTER)
                continue;
            ids = gId_list->item[obix].u.vec;
            break;

        case 2:  /* Global plural ids */
            if (!gPluid_list_d)
                continue;
            ids = gPluid_list_d;
            break;

        case 3:  /* Object plural ids */
            if (!gPluid_list
             || VEC_SIZE(gPluid_list) <= (p_int)obix
             || gPluid_list->item[obix].type != T_POINTER)
                continue;
            ids = gPluid_list->item[obix].u.vec;
            break;

        default:
            fatal("match_object() called with invalid arguments\n");
        }

        if (!ids)
            fatal("match_object(): internal error\n");

        /* Loop over the ids and find a match */
        for (il = 0; (p_int)il < VEC_SIZE(ids); il++)
        {
            if (ids->item[il].type == T_STRING)
            {
                str = ids->item[il].u.str;  /* A given id of the object */
                old_cix = *cix_in;
                if ((pos = find_string(str, wvec, cix_in)) >= 0)
                {
                    /* Id matched, now check a possible adjective */
                    if ((size_t)pos == old_cix
                     || check_adjectiv(obix, wvec, old_cix, pos-1))
                    {
                        if (cplur > 1)
                            *plur = MY_TRUE;
                        return MY_TRUE;
                    }
                }
                *cix_in = old_cix;
            }
        } /* for(il) */
    } /* for (cplur) */

    /* Doesn't match */
    return MY_FALSE;
} /* match_object() */

/*-------------------------------------------------------------------------*/
static svalue_t *
item_parse (vector_t *obvec, vector_t *wvec, size_t *cix_in, Bool *fail)

/* Try to match as many objects in <obvec> as possible onto the description
 * given in commandvector <wvec>[<cix_in>..].
 * Result is a vector with the found objects, and the first element is
 * a number returning a found numeral: 0 for 'all' or a generic plural,
 * > 0: for a numeral 'one', 'two' etc, < 0 for an ordinal 'first', 'second', etc.
 * <cix_in> is updated and <fail> is set to FALSE.
 *
 * On failure, return NULL, update <cix_in> and set <fail> to TRUE.
 */

{
    static svalue_t stmp;  /* Result buffer */

    vector_t *tmp;
    vector_t *ret;
    svalue_t *pval;
    size_t    cix, tix;
    size_t    max_cix;     /* Highest cix used in matching */
    Bool      plur_flag;   /* Plural numeral */
    Bool      match_all;   /* 'all' numeral */
    size_t    obix;

    /* Intermediate result vector */
    tmp = allocate_array(VEC_SIZE(obvec) + 1);

    /* Try to parse a numeral */
    if ( NULL != (pval = number_parse(obvec, wvec, cix_in, fail)) )
        assign_svalue_no_free(&tmp->item[0], pval);

    if (pval && pval->u.number > 1)
    {
        plur_flag = MY_TRUE;
        match_all = MY_FALSE;
    }
    else if (pval && pval->u.number == 0)
    {
        plur_flag = MY_TRUE;
        match_all = MY_TRUE;
    }
    else
    {
        plur_flag = MY_FALSE;
        match_all = MY_TRUE;
    }

    /* Scan the object vector and try to match each one of it */
    for (max_cix = *cix_in, tix = 1, obix = 0; (p_int)obix < VEC_SIZE(obvec); obix++)
    {
        *fail = MY_FALSE;
        cix = *cix_in;

        if (obvec->item[obix].type != T_OBJECT)
            continue;

        /* Command was something like "get all": accept all objects */
        if ((p_int)cix == VEC_SIZE(wvec) && match_all)
        {
            assign_svalue_no_free(&tmp->item[tix++], &obvec->item[obix]);
            continue;
        }

        /* Get the id-info for this object */
        load_lpc_info(obix, obvec->item[obix].u.ob);

        if (obvec->item[obix].u.ob->flags & O_DESTRUCTED) /* Oops */
            continue;

        if (match_object(obix, wvec, &cix, &plur_flag))
        {
            assign_svalue_no_free(&tmp->item[tix++],&obvec->item[obix]);
            max_cix = (max_cix < cix) ? cix : max_cix;
        }
    }

    if (tix < 2)
    {
        /* No object matched: failure */
        *fail = MY_TRUE;
        free_array(tmp);
        if (pval)
            (*cix_in)--;
        return NULL;
    }
    else
    {
        /* We got matches: now compute the results */
        if ((p_int)(*cix_in) < VEC_SIZE(wvec))
            *cix_in = max_cix + 1;
        ret = slice_array(tmp, 0, tix-1);
        if (!pval)
        {
            put_number(ret->item, plur_flag ? 0 : 1);
        }
        free_array(tmp);
    }

    /* Return the result */
    put_array(&stmp, ret);
    return &stmp;
} /* item_parse() */

/*-------------------------------------------------------------------------*/
static svalue_t *
living_parse (vector_t *obvec, vector_t *wvec, size_t *cix_in, Bool *fail)

/* Try to match as many living objects in <obvec> as possible onto the
 * description given in commandvector <wvec>[<cix_in>..].
 * Result is a vector with the found objects, and the first element is
 * a number returning a found numeral: 0 for 'all' or a generic plural,
 * > 0: for a numeral 'one', 'two' etc, < 0 for an ordinal 'first', 'second',
 *      etc.
 * <cix_in> is updated and <fail> is set to FALSE.
 *
 * On failure, return NULL, update <cix_in> and set <fail> to TRUE.
 */

{
    static svalue_t stmp;  /* Result buffer */

    vector_t  *live;
    svalue_t  *pval;
    object_t  *ob;
    size_t     obix, tix;

    *fail = MY_FALSE;

    /* Fill live with all living objects from <obvec> */

    tix = 0;
    live = allocate_array(VEC_SIZE(obvec));

    for (obix = 0; (p_int)obix < VEC_SIZE(obvec); obix++)
    {
        if (obvec->item[obix].type != T_OBJECT)
            continue;
        if (obvec->item[obix].u.ob->flags & O_ENABLE_COMMANDS)
            assign_svalue_no_free(&live->item[tix++], &obvec->item[obix]);
    }

    /* If we have living objects, simply call item_parse() on
     * that array. If that succeeds, we have our result.
     */
    if (tix)
    {
        pval = item_parse(live, wvec, cix_in, fail);
        if (pval)
        {
            free_array(live);
            return pval;
        }
    }

    free_array(live);

    /* We can't find a matching living object in obvec, but
     * maybe the command names a player or living by name.
     */
    ob = find_living_object(wvec->item[*cix_in].u.str, MY_TRUE);
    if (!ob)
        ob = find_living_object(wvec->item[*cix_in].u.str, MY_FALSE);

    if (ob)
    {
        put_ref_object(&stmp, ob, "living_parse");
        (*cix_in)++;
        return &stmp;
    }

    /* Not found */
    *fail = MY_TRUE;
    return NULL;
} /* living_parse() */

/*-------------------------------------------------------------------------*/
static svalue_t *
single_parse (vector_t *obvec, vector_t *wvec, size_t *cix_in, Bool *fail)

/* Find the first object in <obvec> matching the description in <wvec>[<cix_in>..]
 * and return it as an object svalue. <cix_in> is updated and <fail> is set
 * to false.
 *
 * If not found, return NULL, update <cix_in> and set <fail> to true.
 */

{
    size_t    cix, obix;
    Bool      plur_flag;
    svalue_t *osvp;

    /* Loop over the list of objects */
    osvp = obvec->item;
    for (obix = 0; (p_int)obix < VEC_SIZE(obvec); obix++, osvp++)
    {
        if (osvp->type != T_OBJECT)
            continue;

        *fail = MY_FALSE;
        cix = *cix_in;

        load_lpc_info(obix,osvp->u.ob);
        if (osvp->u.ob->flags & O_DESTRUCTED) /* Oops */
            continue;

        plur_flag = MY_FALSE;
        if (match_object(obix, wvec, &cix, &plur_flag))
        {
            *cix_in = cix+1;
            (void)ref_object(osvp->u.ob, "single_parse");
            return osvp;
        }
    }

    /* Not found */
    *fail = MY_TRUE;
    return NULL;
} /* single_parse() */

/*-------------------------------------------------------------------------*/
static svalue_t *
prepos_parse (vector_t *wvec, size_t *cix_in, Bool *fail, svalue_t *prepos)

/* Match the commandwords <wvec>[<cix_in>..] against a list of prepositions.
 * On return, <cix_in> has been updated and <fail> gives the success.
 *
 * If <prepos> is NULL or not an array of strings, the match takes place
 * against the prepositions given by the master object. The result will be
 * a static string-svalue with the matched preposition.
 *
 * If <prepos> is an array of strings, it the list of prepositions matched
 * against. On success, the result is the <prepos> array and first element
 * of the array will be the matched preposition (which has been swapped against
 *  the original content of that element).
 */

{
  static svalue_t stmp;

  vector_t *pvec, *tvec;
  string_t *tmp;
  size_t    pix, tix;

  /* Determine list to match against */
  if (!prepos || prepos->type != T_POINTER)
      pvec = gPrepos_list;
  else
      pvec = prepos->u.vec;

  for (pix = 0; (p_int)pix < VEC_SIZE(pvec); pix++)
  {
      if (pvec->item[pix].type != T_STRING)
          continue;

      tmp = pvec->item[pix].u.str;
      if (!strchr(get_txt(tmp),' '))
      {
          /* A single word match */

          if (mstreq(tmp, wvec->item[*cix_in].u.str))
          {
              (*cix_in)++;
              break;
          }
      }
      else
      {
          /* Multiword match */

          tvec = explode_string(tmp, STR_SPACE);
          for (tix = 0; (p_int)tix < VEC_SIZE(tvec); tix++)
          {
              if ((p_int)(*cix_in+tix) >= VEC_SIZE(wvec)
               || (!mstreq(wvec->item[*cix_in+tix].u.str, tvec->item[tix].u.str))
                 )
                  break;
          }
          tix = ((p_int)tix == VEC_SIZE(tvec)) ? 1 : 0;
          if (tix)
              (*cix_in) += VEC_SIZE(tvec);
          free_array(tvec);
          if (tix)
              break;
      }
  }

  if ((p_int)pix == VEC_SIZE(pvec))
  {
      *fail = MY_TRUE;
  }
  else if (pvec != gPrepos_list)
  {
      /* We received a prepos list: now move the found preposition
       * to the front.
       */
      stmp = pvec->item[0];
      pvec->item[0] = pvec->item[pix];
      pvec->item[pix] = stmp;
      ref_array(pvec); /* The caller will free the prepos at some point. */
      *fail = MY_FALSE;
  }
  else
  {
      /* We found a preposition in the master's list. */
      assign_svalue_no_free(&stmp, &pvec->item[pix]);
      return &stmp;
  }

  return prepos;

} /* prepos_parse() */

/*-------------------------------------------------------------------------*/
static svalue_t *
one_parse ( vector_t *obvec       /* in: array of objects to match against */
          , string_t *pat         /* in: word */
          , vector_t *wvec        /* in: array of command words */
          , size_t   *cix_in      /* in-out: position in wvec */
          , Bool     *fail        /* out: TRUE if mismatch */
          , svalue_t *prep_param  /* current lvalue stack position for %p */
          )

/* Match a single pattern, consuming the words from the wvec.
 * On return, <fail> gives the status of the match, and <pix_in> and <cix_in>
 * have been updated. Direct result is the next matched value in
 * a static buffer, or NULL.
 *
 * The function does not handle alternatives or '%s', and is called
 * from sub_parse().
 */

{
    static svalue_t stmp;  /* The result buffer */

    char      ch;          /* Command character */
    svalue_t *pval;
    char     *str1, *str2;

    /* Nothing left to parse? */
    if ((p_int)*cix_in == VEC_SIZE(wvec))
    {
        *fail = MY_TRUE;
        return NULL;
    }

    /* Get the command character */
    ch = get_txt(pat)[0];
    if (ch == '%')
    {
        ch = get_txt(pat)[1];
    }

    pval = NULL;

    /* Interpret the possible patterns */
    switch (ch)
    {
    case 'i': case 'I':  /* Match an item */
        pval = item_parse(obvec, wvec, cix_in, fail);
        break;

    case 'l': case 'L':  /* Match a living item */
        pval = living_parse(obvec, wvec, cix_in, fail);
        break;

    case 's': case 'S':
        *fail = MY_FALSE; /* This is a double %s in pattern, skip it */
        break;

    case 'w': case 'W':  /* Match the next word */
        put_string(&stmp, make_tabled_from(wvec->item[*cix_in].u.str));
        pval = &stmp;
        (*cix_in)++;
        *fail = MY_FALSE;
        break;

    case 'o': case 'O':  /* Match an object */
        pval = single_parse(obvec, wvec, cix_in, fail);
        break;

    case 'p': case 'P':  /* Match a preposition */
        pval = prepos_parse(wvec, cix_in, fail, prep_param);
        break;

    case 'd': case 'D':  /* Match a number */
        pval = number_parse(obvec, wvec, cix_in, fail);
        break;

    case '\'':           /* Match a required word */
        str1 = &get_txt(pat)[1];
        str2 = get_txt(wvec->item[*cix_in].u.str);
        if (strncmp(str1, str2, strlen(str1)-1) == 0
         && strlen(str1) == strlen(str2)+1)
        {
            *fail = MY_FALSE;
            (*cix_in)++;
        }
        else
            *fail = MY_TRUE;
        break;

    case '[':            /* Match an optional word */
        str1 = &get_txt(pat)[1];
        str2 = get_txt(wvec->item[*cix_in].u.str);
        if (strncmp(str1, str2, strlen(str1)-1) == 0
         && strlen(str1) == strlen(str2)+1)
        {
            (*cix_in)++;
        }
        *fail = MY_FALSE;
        break;

    default:
        *fail = MY_FALSE; /* Skip invalid patterns */
    }

    return pval;
} /* one_parse() */

/*-------------------------------------------------------------------------*/
static svalue_t *
sub_parse ( vector_t *obvec   /* in: array of objects to match against */
          , vector_t *patvec  /* in: array of pattern elements */
          , size_t   *pix_in  /* in-out: position in patvec */
          , vector_t *wvec    /* in: array of command words */
          , size_t   *cix_in  /* in-out: position in wvec */
          , Bool     *fail    /* out: TRUE if mismatch */
          , svalue_t *sp      /* current lvalue stack position for %p */
          )

/* Parse a vector of words against a pattern from the given position.
 * On return, <fail> gives the status of the match, and <pix_in> and <cix_in>
 * have been updated. Direct result is the next matched value, or NULL.
 *
 * The function handles all pattern elements except '%s' and is called
 * by e_parse_command().
 */

{
    size_t    cix, pix;  /* Local positions */
    Bool      subfail;
    svalue_t *pval;

    /* There must be something left to match */
    if ((p_int)*cix_in == VEC_SIZE(wvec))
    {
        *fail = MY_TRUE;
        return NULL;
    }

    cix = *cix_in; pix = *pix_in; subfail = MY_FALSE;

    /* Try to parse a single pattern element */
    pval = one_parse( obvec, patvec->item[pix].u.str
                    , wvec, &cix, &subfail, sp);

    /* If no match (so far), try the next alternative.
     * There must be at least one '/' following in the pattern array.
     */
    while (subfail)
    {
        pix++;
        cix = *cix_in;

        while ((p_int)pix < VEC_SIZE(patvec)
            && mstreq(patvec->item[pix].u.str, STR_SLASH))
        {
            subfail = MY_FALSE;
            pix++;
        }

        if (!subfail && (p_int)pix < VEC_SIZE(patvec))
            pval = one_parse( obvec, patvec->item[pix].u.str, wvec, &cix
                            , &subfail, sp);
        else
        {
            /* No '/': failure */
            *fail = MY_TRUE; *pix_in = pix-1;
            return NULL;
        }
    }

    /* We have a match: skip remaining alternatives */
    if ((p_int)pix+1 < VEC_SIZE(patvec)
     && mstreq(patvec->item[pix+1].u.str, STR_SLASH))
    {
        while ((p_int)pix+1 < VEC_SIZE(patvec)
            && mstreq(patvec->item[pix+1].u.str, STR_SLASH))
        {
            pix += 2;
        }
        pix++; /* Skip last alternate after last '/' */
        if ((p_int)pix >= VEC_SIZE(patvec))
            pix = VEC_SIZE(patvec)-1;
    }

    /* That's it: return the result */
    *cix_in = cix;
    *pix_in = pix;
    *fail = MY_FALSE;
    return pval;
} /* sub_parse() */

/*-------------------------------------------------------------------------*/
Bool
e_parse_command ( string_t *cmd          /* Command to parse */
                , svalue_t *ob_or_array  /* Object or array of objects */
                , string_t *pattern      /* Special parsing pattern */
                , svalue_t *stack_args   /* Pointer to lvalue args on stack */
                , int       num_arg      /* Number of lvalues on stack */
                )

/* EFUN parse_command()
 *
 * This function implements the parse_command() efun, called from interpret.c.
 * Result is TRUE on success, and FALSE otherwise.
 */

{
    static error_handler_t error_handler_addr;

    vector_t        *obvec = NULL;  /* Objects to match against */
    vector_t        *patvec;        /* Elements in pattern <pattern> */
    vector_t        *wvec;          /* Words in command <cmd> */
    parse_context_t *old;
    Bool             fail;          /* TRUE if the match failed */
    size_t           pix;           /* Index in patvec */
    size_t           cix;           /* Index in wvec */
    size_t           six;           /* Index to the lvalues on the stack */
    svalue_t        *pval;          /* Result from a subparse */

    /* Pattern and commands can not be empty
     */
    if (!mstrsize(cmd) || !mstrsize(pattern))
        return MY_FALSE;

    /* Prepare some variables */

    xallocate(old, sizeof *old, "parse context");

    wvec = explode_string(cmd, STR_SPACE);
    if (!wvec)
        wvec = allocate_array(0);

    patvec = explode_string(pattern, STR_SPACE);
    if (!patvec)
        patvec = allocate_array(0);

    if (ob_or_array->type == T_POINTER)
    {
        /* There might be more references to this array, which could cause
         * real nightmares if load_lpc_info() changes the array.
         */
        check_for_destr(ob_or_array->u.vec);
        obvec =
          slice_array(ob_or_array->u.vec, 0, VEC_SIZE(ob_or_array->u.vec) - 1);
    }
    else if (ob_or_array->type == T_OBJECT)
    {
        obvec = deep_inventory(ob_or_array->u.ob, /* take_top: */ MY_TRUE, /* depth: */ 0);
    }
    else
    {
        free_array(wvec);
        free_array(patvec);
        xfree(old);
        errorf("Bad second argument to parse_command()\n");
    }

    /* Save the previous context and set up the error handler */

    old->id       = gId_list;
    old->plid     = gPluid_list;
    old->adjid    = gAdjid_list;
    old->id_d     = gId_list_d;
    old->plid_d   = gPluid_list_d;
    old->adjid_d  = gAdjid_list_d;
    old->prepos   = gPrepos_list;
    old->allword  = gAllword;
    old->wvec     = wvec;
    old->patvec   = patvec;
    old->obvec    = obvec;
    old->previous = gPrevious_context;
    gPrevious_context = old;

    push_error_handler(parse_error_handler, &error_handler_addr);

    /* Make space for the list arrays */

    gId_list    = allocate_array(VEC_SIZE(obvec));
    gPluid_list = allocate_array(VEC_SIZE(obvec));
    gAdjid_list = allocate_array(VEC_SIZE(obvec));

    /* Get the default ids of 'general references' from master object
    */
    pval = apply_master(STR_PC_ID_LIST, 0);
    if (pval && pval->type == T_POINTER)
    {
        gId_list_d = ref_array(pval->u.vec);
    }
    else
        gId_list_d = NULL;

    pval = apply_master(STR_PC_P_ID_LIST, 0);
    if (pval && pval->type == T_POINTER)
    {
        gPluid_list_d = ref_array(pval->u.vec);
    }
    else
        gPluid_list_d = NULL;

    pval = apply_master(STR_PC_ADJ_LIST, 0);
    if (pval && pval->type == T_POINTER)
    {
        gAdjid_list_d = ref_array(pval->u.vec);
    }
    else
        gAdjid_list_d = NULL;

    pval = apply_master(STR_PC_PREPOS, 0);
    if (pval && pval->type == T_POINTER)
    {
        gPrepos_list = ref_array(pval->u.vec);
    }
    else
        gPrepos_list = allocate_array(0);

    pval = apply_master(STR_PC_ALLWORD,0);
    if (pval && pval->type == T_STRING)
        gAllword = ref_mstring(pval->u.str);
    else
        gAllword = NULL;

    /* Loop through the pattern. Handle %s but not '/'
    */
    for (six = 0, cix = 0, fail = MY_FALSE, pix = 0
        ; (p_int)pix < VEC_SIZE(patvec); pix++)
    {
        pval = NULL;
        fail = MY_FALSE;

        if (mstreq(patvec->item[pix].u.str, STR_PERCENT_S))
        {
            /* If at the end of the pattern, %s matches everything left
             * in the wvec.
             * Otherwise it matches everything up to the next pattern
             * element.
             */
            if ((p_int)pix == VEC_SIZE(patvec)-1)
            {
                pval = slice_words(wvec, cix, VEC_SIZE(wvec)-1);
                cix = VEC_SIZE(wvec);
            }
            else
            {
                size_t fword, ocix, fpix;

                ocix = fword = cix;
                fpix = ++pix;

                /* Try parsing the next pattern element at increasingly
                 * further distances from the current position in wvec.
                 * The loop ends when a match is found, or wvec is exhausted.
                 */
                do {
                    fail = MY_FALSE;
                    pval = sub_parse(obvec, patvec, &pix, wvec, &cix, &fail
                                     , (six < (size_t)num_arg) ? stack_args[six].u.lvalue
                                                       : 0);
                    if (fail)
                    {
                        cix = ++ocix;
                        pix = fpix;
                    }
                } while (fail && (p_int)cix < VEC_SIZE(wvec));

                /* If we failed to find a match, the whole pattern string
                 * doesn't match. Otherwise store the wvec slice between
                 * the current position and the match into the next
                 * variable.
                 */
                if (!fail)
                {
                    stack_put(pval, stack_args, six+1, num_arg);
                    pval = slice_words(wvec, fword, ocix-1);
                    stack_put(pval, stack_args, six++, num_arg);
                    pval = NULL;
                }
            }
        }
        else if (!mstreq(patvec->item[pix].u.str, STR_SLASH))
        {
            /* Everything else is handled by sub_parse() */
            pval = sub_parse( obvec, patvec, &pix, wvec, &cix, &fail
                            , (six < (size_t)num_arg) ? stack_args[six].u.lvalue : 0);
        }

        if (!fail && pval)
            stack_put(pval, stack_args, six++, num_arg);
        else if (fail)
            break;
    } /* for() */

    /* Also fail when there are words left to parse and pattern exhausted.
    */
    if ((p_int)cix < VEC_SIZE(wvec))
        fail = MY_TRUE;

    pop_stack(); /* Clean up via the error handler */

    return !fail;
} /* e_parse_command() */

#endif /* USE_PARSE_COMMAND */

/***************************************************************************/

