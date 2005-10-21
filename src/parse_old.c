/*---------------------------------------------------------------------------
 * Pattern Parser v 2.0  (compat mode)
 * (C) Copyright 1990 JnA (jna@cd.chalmers.se)
 *
 *---------------------------------------------------------------------------
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
 * If <env> is 0, environment(this_player()) is used as default.
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
 *    %l    : A single living object
 *    %s    : Any text
 *    %w    : Any word
 *    %p    : One of a list of prepositions.
 *            If the variable associated with %p is used to pass
 *            a list of words to the efun, the matching will take
 *            only against this list.
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
 *    %l : returns a living
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
 *
 * %i match descriptions like 'three red roses','all nasty bugs'
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
 *   1. int id (string txt)
 *       txt is an object name of the form "adj1 adj2 ... name".
 *       The function has to return non-zero if txt is a valid (singular) name
 *       for this particular object.
 *
 *   2. int plural_id (string txt)
 *       txt is an object name of the form "adj1 adj2 ... name".
 *       The function has to return non-zero if txt is a valid plural name
 *       for this particular object.
 *
 *   3. string adjectiv_id()
 *       When parsing commands like "get all red ones", the result from this
 *       function is used to construct the text passed to id(); in this
 *       example "red <adjectiv_id>". If this function doesn't exist,
 *       the last word from the result of short() is used instead.
 *
 * TODO: This module uses rather clumsy programming.
 * TODO: Comments are still sparse.
 *---------------------------------------------------------------------------
 */

#include "driver.h"

#if defined(SUPPLY_PARSE_COMMAND)

#include "typedefs.h"

#include <stdio.h>
#include <ctype.h>
#include <time.h>

#include "parse.h"

#include "actions.h"
#include "array.h"
#include "closure.h"
#include "interpret.h"
#include "gcollect.h"
#include "main.h"
#include "object.h"
#include "random.h"
#include "simulate.h"
#include "stdstrings.h"
#include "svalue.h"
#include "wiz_list.h"
#include "xalloc.h"

/*-------------------------------------------------------------------------*/
/* Some useful macros
 */
#define EQ(x,y)  (strcmp(x,y)==0)
#define EQN(x,y) (strncmp(x,y,strlen(x))==0)
#define EMPTY(x) (x[0] == '\0')

#define KLUDGELEN 200  /* Size of the string buffers */
 
#define LVALUE svalue_t

/*-------------------------------------------------------------------------*/

typedef struct altern_objects_s altern_objects_t;

struct altern_objects_s
{
  altern_objects_t *next;
  object_t *ao_obj;
};

/* Type of an element from the parse pattern string.
 */
enum ptype_e
  { EP = 0  /* End Parse marker */
  , SI      /* %o single item */
  , IT      /* %i  items */
  , US      /* %l  single living object */
  , PP      /* %p  prepositions */
  , TX      /* %s  string of words */
  , DTX     /* 'word' */
  , OTX     /* [word] */
  , ALT     /* /   alternates */
  , W1      /* %w  string of one word */
  , NUM     /* %d  integer */
  };

typedef enum ptype_e ptype_t;

/*-------------------------------------------------------------------------*/

static char gMword[KLUDGELEN];
  /* Text inside '' or [].
   */

static char gFword[KLUDGELEN];
  /* Temp word gotten by getfirst().
   */

static altern_objects_t *gOblist;
  /* List of accessible objects.
   */

static char gAdjective[4*KLUDGELEN];
  /* all adjectives before objname
   */
   
static LVALUE *gCarg;
  /* Current argument to %_
   */
   
static int gWantnum;
  /* Number of wanted items 0 = all
   */
   
static altern_objects_t *gPobjects;
  /* List of parsed objects
   */
   
static LVALUE *gTxarg;
  /* Argument of LPCvariable to store %s
   */
   
static LVALUE *gForprepos;
  /* Save arg* here for findprepos
   */
   
static LVALUE *gTopStack;
  /* arg* to arg after my last
   */
   
static svalue_t sv_tmp;
  /* Buffer for function results.
   */

static svalue_t find_living_closures[2]
  = { { T_INVALID }, { T_INVALID } };
  /* The closures to the functions 'find_living' and 'find_player',
   * which are generated at runtime to be able to find simul-efuns with
   * these names.
   * TODO: This too should go into a master function.
   */

/*-------------------------------------------------------------------------*/
static object_t *
find_living_object (char *name, Bool player)

/* Find the living (<player> is false) or player (<player> is true)
 * with the name <name>.
 * Return the found object, or NULL if not found.
 *
 * The functions calls the (simul)efuns 'find_living' resp.
 * 'find_player' for this purpose.
 */

{
    static char *function_names[2] = { "find_living", "find_player"};

    svalue_t *sp, *svp;

    sp = inter_sp;
    sp++;

    /* Get or create the closure for the function to call */
    svp = &find_living_closures[player ? 1 : 0];
    if (svp->type == T_INVALID)
    {
        /* We have to create the closure */
        put_old_string(sp, make_shared_string(function_names[player ? 1 : 0]));
        if (!sp->u.string)
            error("(old_parse_command) Out of memory (%lu bytes) for string\n"
                 , strlen(function_names[player ? 1 : 0]));
        inter_sp = sp;
        symbol_efun(sp);
        *svp = *sp;
        inter_sp = sp - 1;
    }

    /* Call the closure */
    put_old_string(sp, make_shared_string(name));
    if ( !sp->u.string)
        error("(old_parse_command) Out of memory (%lu bytes) for result\n"
             , strlen(name));
    inter_sp = sp;
    call_lambda(svp, 1);
    pop_stack();
    
    return sp->type != T_OBJECT ? NULL : sp->u.ob;
} /* find_living_object() */

/*-------------------------------------------------------------------------*/

#ifdef GC_SUPPORT

void
clear_old_parse_refs (void)

/* GC support: Clear the references of all memory held by the parser.
 */

{
    clear_ref_in_vector( find_living_closures
                       , sizeof find_living_closures / sizeof(svalue_t)
    );
} /* clear_parse_refs() */

/*-------------------------------------------------------------------------*/
void
count_old_parse_refs (void)

/* GC support: Count the references of all memory held by the parser.
 */

{
    count_ref_in_vector( find_living_closures
                       , sizeof find_living_closures / sizeof(svalue_t)
    );
} /* count_parse_refs() */

#endif /* GC_SUPPORT */

/*-------------------------------------------------------------------------*/
static char *
backstrchr (char *apa, char ch)

/* Search for character <ch> in string <apa> and return a pointer to
 * the following character, or NULL if not found.
 */

{
    char *sp;

    sp = strrchr(apa, ch);
    return sp ? sp+1 : NULL;
} /* backstrchr() */

/*-------------------------------------------------------------------------*/
static char *
lowercase (char *apa)

/* Convert string <apa> to lowercase in-place and also return it.
 */

{
    char *bepa;

    bepa = apa;
    while (*bepa)
    {
        if (*bepa > 'A' && *bepa <= 'Z')
            *bepa += 'a'-'A';
        bepa++;
    }
    return apa;
} /* lowercase() */

/*-------------------------------------------------------------------------*/
static void
fixlist (object_t *first, altern_objects_t **parent, int *antal)

/* Add all objects in the inventory of <first> (including itself) to
 * the list <parent>. <antal> holds the number of elements in the parent
 * list.
 */

{
    object_t *cur;
    altern_objects_t *this, *taill;

    cur = first;
    this = *parent;
    if (!cur)
        return;
    while (cur)
    {
        if (cur->contains)
            fixlist(cur->contains, &this, antal);
        taill = this;
        this = xalloc(sizeof(*this));
        this->ao_obj = ref_object(cur, "parse->fixlist");
        (*antal)++;
        this->next = taill;
        cur = cur->next_inv;
    }
    *parent = this;
} /* fixlist() */

/*-------------------------------------------------------------------------*/
static int
makeobjlist (altern_objects_t **alist, object_t *src)

/* Create a list of the theoretically accessible objects from <src>
 * and store it in *<alist>.
 * If <src> is NULL; environment(command_giver) is used.
 * Result is the number of objects found.
 */

{
    object_t *env, *cur;
    altern_objects_t *this;
    int cnt;
    
    *alist = NULL;

    if (!src)
        env = command_giver->super;
    else
        env = src;
    if (!env)
        return 0;
    if (!(env->contains))
        return 0;

    cur = env->contains;
    this = NULL;
    cnt = 0;
    fixlist(cur, &this, &cnt);
    *alist = this;
    return cnt;
} /* makeobjlist() */

/*-------------------------------------------------------------------------*/
static int
itnumalt (void)

/* Return the number of objects in the gPobjects list.
 */

{
    int ant;
    altern_objects_t *ao;

    ant = 0;
    ao = gPobjects;
    while (ao)
    {
        ant++;
        ao = ao->next;
    }
    return ant;
} /* itnumalt() */

/*-------------------------------------------------------------------------*/
static object_t *
italt (int a)

/* Return a pointer to the <a>th object in the gPobjects list.
 */

{
    int ant;
    altern_objects_t *ao;

    ant = 0;
    ao = gPobjects;
    while (ao)
    {
        ant++;
        if (ant == a)
            return ao->ao_obj;
        ao = ao->next;
    }

    return NULL;
} /* italt() */

/*-------------------------------------------------------------------------*/
static void
italt_new (void)

/* Free the list gPobjects.
 */

{
    altern_objects_t *ao;
    altern_objects_t *ao2;

    ao2 = ao = gPobjects;
    gPobjects = NULL;
    while (ao)
    {
        ao = ao->next;
        free_object(ao2->ao_obj, "parse->italt_new()");
        xfree(ao2);
        ao2 = ao;
    }
} /* italt_new() */

/*-------------------------------------------------------------------------*/
static void
italt_loadall (void)

/* Create gPobjects as the list of all the theoretically accessible
 * objects.
 */

{
    if (gPobjects) italt_new();
    makeobjlist(&gPobjects,0);
} /* italt_loadall() */

/*-------------------------------------------------------------------------*/
static void
italt_put (object_t *obj)

/* Put object <obj> at the end of the list gPobjects.
 */

{
    altern_objects_t *ao, *old;

    ao = gPobjects;
    old = ao;
    while (ao)
    {
        old = ao;
        ao = ao->next;
    }

    ao = xalloc(sizeof(*ao));
    if (old)
        old->next = ao;
    else
        gPobjects = ao;

    ao->ao_obj = ref_object(obj, "parse->italt_put()");
    ao->next = NULL;
} /* italt_put() */

/*-------------------------------------------------------------------------*/
static char *
getfirst (char **cmd)

/* Put the first word of <cmd> into gFword and point <cmd> to the
 * next word. Result is gFword.
 */

{
    size_t pos;
    Bool inqoute;
    char ch, *st;

    st = *cmd;
    strcpy(gFword,"");
    if (st[0] == '\0')
        return gFword;
  
    ch = ' ';
    pos = 0;
    inqoute = MY_FALSE;
    while (st[0] <= ' ' && st[0]) /* Skip leading spaces */
        st++;

    if (st[0] == '\'' || st[0] == '[')
    {
        inqoute=MY_TRUE;
        ch = st[0];
        gFword[0] = st[0];
        pos=1;
        st++;
    }
    
    if (ch == '[')
        ch = ']';

    while (pos < KLUDGELEN-1 && st[0] && st[0] != ch)
    {
        gFword[pos++] = st[0];
        st++;
        if (st[0] && st[0] < ' ')  /* Replace ctrl chars */
            st[0]=' ';
    }

    if (inqoute && pos < KLUDGELEN-1)
    {
        gFword[pos++] = ch;
        if (st[0])
            st++;
    }

    gFword[pos] = '\0';
    *cmd = st;
    
    return gFword;
} /* getfirst() */

/*-------------------------------------------------------------------------*/
static char *
lookfirst (char *cmd)

/* Put first word of <cmd> into gFword without incrementing the callers
 * <cmd> pointer. Result is gFword.
 */

{
    return getfirst(&cmd);
} /* lookfirst() */

/*-------------------------------------------------------------------------*/
static int
call_obj (char *fnamn, object_t *on, char *apa)

/* Call the lfun <on>:<fnamn>(<apa>). If the result is a number, return it,
 * otherwise return 0.
 */

{
    svalue_t *ret;

    push_volatile_string(inter_sp, apa);
    ret = apply(fnamn, on, 1);
    if (!ret) return 0;
    if ( ret->type == T_NUMBER) return ret->u.number;
    return 0;
} /* call_obj() */

/*-------------------------------------------------------------------------*/
static char *
singfix (char *str)

/* Change the noun <str> from pluralform to singularform.
 * Result is a pointer to a static buffer or to a constant string.
 */

{
    static char sing[KLUDGELEN];
    char *sp;
    int sl;

    sl = strlen(str);
    if (sl < 2)
        return str;

    sp = &str[sl-3];
    strcpy(sing,str);
    sing[sl-3] = '\0';

    if (EQ(str,"corpses")) return "corpse";
    if (EQ(sp,"ses")) return strcat(sing,"s");
    else if (EQ(sp,"xes")) return strcat(sing,"x");
    else if (EQ(sp,"hes")) return strcat(sing,"h");
    else if (EQ(sp,"ies")) return strcat(sing,"y");
    else if (EQ(sp,"ves")) return strcat(sing,"fe");
    else if (sp[2]=='s') {
        strcat(sing,sp); sing[sl-1]=0;
        return sing;
    }

    if (EQ(str,"teeth")) return "tooth";
    if (EQ(str,"feet")) return "foot";
    if (EQ(str,"men")) return "man";
    if (EQ(str,"women")) return "woman";
    if (EQ(str,"children")) return "child";
    if (EQ(str,"sheep")) return "sheep";

    return str;
} /* singfix() */

/*-------------------------------------------------------------------------*/
static int
matchadjective (char *adjs)

/* Check with objs in gOblist if accepts all adjectives in <adjs>, and return
 * the number of objects which do.
 *
 * This is used when the name of the object is not know, ie in cmds
 * like 'get all the red ones'.
 *
 * If the function STR_PC_ADJ_ID doesn't exist in the objects, the last word
 * from STR_PC_SHORT is used as object name and set as suffix to the adjectives
 * in calls to id().
 */

{
    char ad[KLUDGELEN], tot[2*KLUDGELEN], *sp, *sp2;
    altern_objects_t *ao;
    object_t *on;
    svalue_t *ret;

    /* Loop over the gOblist */
    for (ao = gOblist; ao; ao=ao->next)
    {
        on = ao->ao_obj;
        sp = adjs;
        while (on && sp && *sp)
        {
            while (*sp == ' ')
                sp++;
            sp2 = strchr(sp,' ');
            if (sp2)
            {
                *sp2 = '\0';
                strcpy(ad, sp);
                *sp2 = ' ';
                sp = sp2;
                if (!call_obj(STR_PC_ADJ_ID, on, ad))
                    on = NULL; /* Not ok */
            }
            sp = sp2;
        }
        
        if (on)
            italt_put(on);
        else
        {
            on = ao->ao_obj;
            ret = apply(STR_PC_SHORT,on,0);
            if (ret && ret->type==T_OLD_STRING)
            {
                sp = ret->u.string;
                sp2 = backstrchr(sp,' ');
                if (sp2)
                    sp = sp2;
                sprintf(tot, "%s%s", adjs, sp);
                lowercase(tot);
                if (!call_obj(STR_ID,on,tot))
                    on = NULL;
            }
            else
                on = NULL;
        }
        if (on)
            italt_put(on);
    }
    
    return itnumalt();
} /* matchadjective() */

/*-------------------------------------------------------------------------*/
static Bool
check_for_general (char *onam, Bool plur)

/* Return true if the word <oname> is one of the general words like 'those'.
 */
{

    static char* plurpron[] = {"those","them","these","ones","$"};
    static char* singpron[] = {"this","one","it","$"};
    size_t ilop;

    if (EMPTY(onam))
        return MY_FALSE;

    if (plur)
    {
        for (ilop = 0; !EQ(plurpron[ilop],"$"); ilop++)
            if (EQ(plurpron[ilop], onam))
                return MY_TRUE;
    }
    else
    {
        for (ilop = 0; !EQ(singpron[ilop],"$"); ilop++)
            if (EQ(singpron[ilop], onam))
                return MY_TRUE;
    }

    return MY_FALSE;
} /* check_for_general() */

/*-------------------------------------------------------------------------*/
static int
order_num (char *wd)

/* Parse a (positive) ordinal (incl. zero) from the command <wd> and return it.
 * If there is no ordinal, return -1.
 *
 * The function recognizes digits, and numbers expressed in words.
 */

{
    static char *onums[] = {"first","second","third","fourth","fifth","sixth",
                    "seventh","eighth","nineth","tenth",
                    "eleventh","twelfth","thirteenth","fourteenth","fifteenth","sixteenth",
                    "seventeenth","eighteenth","nineteenth","dummy"};
    static char *onumt[] = {"twenty","thirty","forty","fifty","sixty","seventy",
                    "eighty","ninety"};
    static char *onumta[] = {"twentieth","thirtieth","fortieth","fiftieth","sixtieth","seventieth",
                    "eightieth","ninetieth"};

    char ns[KLUDGELEN];

    int ilop, nm;

    if (EMPTY(wd))
        return -1;
        
    /* Test for simple textual numbers */
    for (ilop = 1; ilop < 20; ilop++)
    {
        if (EQ(onums[ilop-1],wd))
            return ilop;
    }

    /* Test for composite textual numbers */
    /* TODO: This could be done better */
    for (nm = 0; nm < 8; nm++)
        for (ilop = 0; ilop < 9; ilop++)
        {
            if (ilop > 0)
            {
                sprintf(ns, "%s%s", onumt[nm], onums[ilop-1]);
                if (EQ(ns,wd))
                    return 20+nm*10+(ilop);
            }
            else if (EQ(onumta[nm],wd))
                return 20+nm*10+(ilop);
        }

    return -1;
} /* order_num() */

/*-------------------------------------------------------------------------*/
static int
numeric (char *wd)

/* Parse a positive number (incl. zero) from the command <wd> and return it.
 * If there is no number, return -1.
 *
 * The function recognizes digits, and numbers expressed in words.
 */
 
{
    static char *nums[] = {"one","two","three","four","five","six",
                    "seven","eight","nine","ten",
                    "eleven","twelve","thirteen","fourteen","fifteen","sixteen",
                    "seventeen","eighteen","nineteen"};
    static char *numt[] = {"twenty","thirty","forty","fifty","sixty","seventy",
                    "eighty","ninety"};

    char ns[KLUDGELEN];
 
    int ilop, nm;

    if (EMPTY(wd))
        return -1;
  
    /* Test for digit numerics */
    if (sscanf(wd, "%d", &nm))
        return (nm>=0) ? nm : -1;
      
    /* Test for simple textual numbers */
    if (EQ(wd,"a") || EQ(wd,"an"))
        return 1;

    for (ilop = 1; ilop < 20; ilop++)
        if (EQ(nums[ilop-1], wd))
            return ilop;

    /* Test for composite textual numbers */
    /* TODO: This could be done better */
    for (nm = 0; nm < 8; nm++)
        for (ilop = 0; ilop < 9; ilop++)
        {
            if (ilop > 0)
            {
                sprintf(ns,"%s%s",numt[nm],nums[ilop-1]);
                if (EQ(ns,wd))
                    return 20+nm*10+(ilop);
            }
            else if (EQ(numt[nm], wd))
                return 20+nm*10+(ilop);
        }

    return -1;
} /* numeric() */

/*-------------------------------------------------------------------------*/
static object_t *
matchobject2 (char ** cmd, Bool plur)

/* Search commandstring for "adj1 adj2 ... adjN objectname" and matching
 * object. If found, return the object and store the matched adjectives
 * in gAdjective as "adj1 adj2...".
 */

{
    object_t *on;
    altern_objects_t *ao;
    char *st, *ocmd, totname[2*KLUDGELEN], tot2[2*KLUDGELEN];

    ocmd = *cmd;
    strcpy(gAdjective, "");

    /* Loop over the words in cmd, catenating them in gAdjective,
     * until a complete id (adjectives and name) is complete.
     */
    st = lowercase(getfirst(cmd));
    while (st[0])
    {
        if (check_for_general(st, plur)
         && matchadjective (gAdjective)
           )
            return italt(1);

        sprintf(totname, "%s%s", gAdjective, st);
        if (plur)
            sprintf(tot2,"%s%s", gAdjective, singfix(st));

        /* Loop over the object list with the id so far */
        for (ao = gOblist; ao; ao = ao->next)
        {
            on = NULL;
            if (plur)
            {
                if (call_obj(STR_PC_PLURAL_ID, ao->ao_obj, totname))
                    on = ao->ao_obj;
                else if (call_obj(STR_ID, ao->ao_obj, tot2))
                    on = ao->ao_obj;
            }
            else if (call_obj(STR_ID, ao->ao_obj, totname))
                on = ao->ao_obj;
            if (on)
                italt_put(on);
        } /* for (ao) */
        
        if (!itnumalt())
        {
            strcat(gAdjective, st);
            strcat(gAdjective, " ");
            st = lowercase(getfirst(cmd));
        }
        else
            st[0] = '\0'; /* also breaks the loop */
    } /* while(st[0]) */

    if (itnumalt())
        return italt(1);
        
    *cmd = ocmd;
    return NULL;
} /* matchobject2() */

/*-------------------------------------------------------------------------*/
static object_t *
finditem (char **cmd, Bool plur)

/* Search the command for a valid object description and return the
 * object, if found.
 * The function calls itself recursively for plurals.
 */

{
    int nm;
    object_t *pn;
    char *ocmd, w1[KLUDGELEN];

    ocmd = *cmd;
    strcpy(w1, lowercase(getfirst(cmd)));
    italt_new(); /* Clear alternate list */
    gWantnum = 1; /* Parsed number of desired objects */

    if (EMPTY(w1))
    {
        *cmd = ocmd;
        return NULL;
    }

    /* Must be hardcode skipped for recursive with plural
     */
    if (EQ("the", w1))
    {
        *cmd = ocmd;
        getfirst(cmd);
        if (NULL != (pn = finditem(cmd,plur)))
            return pn;
        else
        {
            *cmd=ocmd;
            return NULL;
        }
    }

    /* Check things like: some <objname>
     */
    else if (EQ("some",w1) && !plur)
    {
        *cmd = ocmd;
        getfirst(cmd);
        /* Skip of in : some of the bottles */
        if (EQ(lowercase(lookfirst(*cmd)),"of"))
            getfirst(cmd);

        nm = random_number(6) + 2; /* "some" means random 2..7 */
        if (NULL != (pn = finditem(cmd, MY_TRUE)))
        {
            gWantnum=nm;
            return pn;
        }
    }

    /* Check things like: three <objname>
     */
    else if (!plur && (nm = numeric(w1)) > 0)
    {
        *cmd = ocmd;
        getfirst(cmd);
        /* Skip of in : two of the bottles */
        if (EQ(lowercase(lookfirst(*cmd)),"of"))
            getfirst(cmd);
        if (NULL != (pn = finditem(cmd, (nm != 1))))
        {
            gWantnum=nm;
            return pn;
        }
    }

    /* Check things like: third <objname>
     */
    else if (!plur && (nm = order_num(w1)) > 0)
    {
        *cmd = ocmd;
        getfirst(cmd);
        /* Skip of in : second of the bottles */
        if (EQ(lowercase(lookfirst(*cmd)),"of"))
        {
            getfirst(cmd);
            if (NULL != (pn = finditem(cmd,MY_TRUE)))
            {
                /* Fix second of the bottles */
                gWantnum = -nm;
                return pn;
            }
        }
        else
        {
            if (NULL != (pn = finditem(cmd,MY_FALSE)))
            {
                /* Fix second bottle */
                gWantnum = -nm;
                return pn;
            }
        }
    }


    /* Check things like: all <objname>
     */
    else if (EQ("all",w1) && !plur)
    {
        *cmd = ocmd;
        getfirst(cmd);
        /* Skip of in : all of the bottles */
        if (EQ(lowercase(lookfirst(*cmd)), "of"))
        {
            getfirst(cmd);
        }
        if (NULL != (pn = finditem(cmd,MY_TRUE)))
        {
            /* Handle: get all apples */
            gWantnum = 0; /* 0 means 'all' */
            return pn;
        }
        else
        {
            /* Handle: get all */
            italt_loadall();
            pn = italt(1);
            gWantnum = 0; /* 0 means 'all' */
        }
    }

    /* Search for: adj1 adj2 ... adjN objectname
     */
    else
    {
        *cmd = ocmd;
        if (plur)
            pn = matchobject2(cmd, plur);
        else
        {
            /* Standard singular */
            pn = matchobject2(cmd, MY_FALSE);
            if (!pn)
            {
                /* Handle things of type: get apples */
                pn = matchobject2(cmd, MY_TRUE);
                if (pn)
                    gWantnum = 0; /* Default this to all */
            }
        }
    }

    if (!pn)
        *cmd = ocmd;

    return pn;
} /* finditem() */

/*-------------------------------------------------------------------------*/
static Bool
findobject (char **cmd)

/* Find match in command <cmd> for %i in pattern.
 * If found, store it in gCarg and return TRUE.
 */

{
    int nm,s;
    object_t *ob;

    if (finditem(cmd, MY_FALSE))
    {
        if (gCarg)
        {
            vector_t *p;

            nm = itnumalt();
            p = allocate_array(nm+1);
            put_number(p->item, gWantnum);

            /* Make array in reverse order from italt() because
             * makeobjlist() has reversed the order on entry
             */
            for (s = 1; s <= nm; s++)
            {
                ob = italt(nm+1-s);
                if (ob)
                    put_ref_object(p->item+s, ob, "old_parse_command");
            }
            put_array(&sv_tmp, p);
            transfer_svalue(gCarg->u.lvalue, &sv_tmp);
        }
        return MY_TRUE;
    }

    return MY_FALSE;
} /* findobject() */

/*-------------------------------------------------------------------------*/
static object_t *
findplay (char **cmd)

/* Find match in command <cmd> for %l in pattern and return it.
 * If found, also store it in gCarg.
 */

{
    object_t *pn;
    char w1[KLUDGELEN];

    strcpy(w1, lowercase(lookfirst(*cmd)));
    /* can be fixed later with call to LPC command_giver->query_real_name()
     * if (EQ(w1,"me")) strcpy(w1,getmyname_jp());
     * if (EQ(w1,"myself")) strcpy(w1,getmyname_jp());
     */
    pn = find_living_object(w1, MY_TRUE);  /* Find player by name */
    if (pn)
    {
        getfirst(cmd);
        if (gCarg)
        {
            put_ref_object(&sv_tmp, pn, "old_parse_command(%l)");
            transfer_svalue(gCarg->u.lvalue, &sv_tmp);
        }
    }

    return pn;
} /* findplay() */

/*-------------------------------------------------------------------------*/
static Bool
findword (char **cmd, svalue_t *v)

/* Find match in command <cmd> for %p in the wordlist <v>.
 * If found, store it in gCarg and return TRUE.
 */

{
    char *w;
    vector_t *p;
    svalue_t sv;
    size_t cnt, m;
    int f;

    w = lookfirst(*cmd); lowercase(w);
    p = v->u.vec;
    f = -1;
    for (cnt = 0, m = VEC_SIZE(p); cnt < m; cnt++)
    {
        if (p->item[cnt].type == T_OLD_STRING
         && strcmp(p->item[cnt].u.string,w) == 0)
        {
            f=cnt; cnt=m;
        }
    }

    if (f < 0)
        return MY_FALSE;

    getfirst(cmd);
    if (!f)
        return MY_TRUE; /* Match and word in first element */

    /* Swap element 0 and f in array */
    sv = p->item[0];
    p->item[0] = p->item[f];
    p->item[f] = sv;
    
    return MY_TRUE;
} /* findword() */

/*-------------------------------------------------------------------------*/
static Bool
findprepos (char **cmd)

/* Find match in command <cmd> for %p in pattern.
 * If found, store it in gCarg and return TRUE.
 */

{
    static char *hard_prep[] = {"on","in","under","from","behind","beside",0};

    char *w;
    svalue_t *v;
    size_t cnt;

    if (gForprepos)
    {
        v = gForprepos->u.lvalue;
        if (v && v->type == T_POINTER)
            return findword(cmd, v);
    }

    /* No wordlist sent, use hard coded prepositions and return a string
     */
    w = lookfirst(*cmd);
    lowercase(w);
    for (cnt = 0; hard_prep[cnt]; cnt++)
    {
        if (strcmp(w, hard_prep[cnt]) == 0)
        {
            getfirst(cmd); /* Skip this word */
            if (gCarg)
            {
                put_malloced_string(&sv_tmp, string_copy(w));
                transfer_svalue(gCarg->u.lvalue, &sv_tmp);
                return MY_TRUE;
            }
        }
    }
    
    return MY_FALSE;
} /* findprepos() */

/*-------------------------------------------------------------------------*/
static Bool
findsingle (char **cmd)

/* Find match in command <cmd> for %o in pattern.
 * If found, store it in gCarg and return TRUE.
 */

{
    if (finditem(cmd, MY_FALSE))
    {
        if (itnumalt() == 1 && gCarg)
        {
            put_ref_object(&sv_tmp, italt(1), "old_parse_command(%o)" );
            transfer_svalue(gCarg->u.lvalue, &sv_tmp);
        }
        return MY_TRUE;
    }
    return MY_FALSE;
} /* findsingle() */

/*-------------------------------------------------------------------------*/
static ptype_t
get1ps (char **parsep, LVALUE **lin, Bool skip)

/* Get the first parsetype from the pattern <parsep> and, if not <skip>,
 * set <lin>.
 */

{
    char *cod, ch;
    ptype_t pt;
    LVALUE *l;

    pt = EP;
    cod = getfirst(parsep);
    l = *lin;

    /* Determine the ptype for the next parse pattern */
    ch = cod[0];
    if (ch == '%')
        ch = cod[1];

    switch (ch)
    {
    case 'i': case 'I': pt = IT; break;
    case 'l': case 'L': pt = US; break;
    case 's': case 'S': pt = TX; break;
    case 'w': case 'W': pt = W1; break;
    case 'o': case 'O': pt = SI; break;
    case 'p': case 'P': pt = PP; break;
    case 'd': case 'D': pt = NUM; break;
    case '\'': sscanf(cod,"'%[^\']",gMword); pt = DTX; break;
#if 0
    case '[': sscanf(cod,"\[%[^\135]",gMword); pt=OTX; /* 135 is oct for ] */
#else
    case '[': sscanf(cod,"\\[%[^]]",gMword); pt=OTX;
#endif
        if (gMword[strlen(gMword)-1]==']') gMword[strlen(gMword)-1]=0;
        break;
    case '/': pt = ALT; break;
    }

    gCarg = NULL;

    if (skip || pt == DTX || pt == OTX || pt == ALT)
        return pt;

    /* Pattern takes a variable as argument */
    if (l != gTopStack)
    {
        gForprepos = l;
        gCarg = l;
        l++;
    }
    else
    {
        gCarg = NULL;
        gForprepos = NULL;
    }
    
    *lin = l;
    return pt;
} /* get1ps() */

/*-------------------------------------------------------------------------*/
static void
addword (char *d, char *s)

/* Add " <s>" to <d>.
 * If gTxarg is set, a copy of the result is stored there.
 */

{
    if (strlen(d) + strlen(s) < KLUDGELEN-2)
    {
        if (!EMPTY(d))
            strcat(d, " ");
        strcat(d,s);
    }
    if (gTxarg)
    {
        put_malloced_string(&sv_tmp, string_copy(d));
        transfer_svalue(gTxarg->u.lvalue, &sv_tmp);
    }
} /* addword() */

/*-------------------------------------------------------------------------*/
Bool
e_old_parse_command ( char     *cs           /* Command to parse */
                , svalue_t *ob_or_array  /* Object or array of objects */
                , char     *ps           /* Special parsing pattern */
                , svalue_t *dest_args    /* Pointer to lvalue args on stack */
                , int       num_arg      /* Number of lvalues on stack */
                )

/* EFUN parse_command()
 *
 * This function implements the old parse_command() efun, called from
 * interpret.c.  Result is TRUE on success, and FALSE otherwise.
 */

{
    int     altflag;
    int     txflag;
    ptype_t ptyp;     /* Parsetype: %o %i %l %p %s 'word' [word] / %w */

    char *parsep;                /* Parsepattern */
    char *cmd;                   /* Command to be parsed */
    char *ops,*ocs;              /* Temporary parse and command */
    char tx_jp[KLUDGELEN];       /* Fill up string for %s */
    char *tx_save_parsep = NULL; /* Where to continue parsing when a word was added
                                  * to a string
                                  */
    char *tx_end_first_pattern = NULL;
    LVALUE *l;                   /* Argument pointer in dest_args */

    /* No need to parse against destructed objects */
    if (ob_or_array->type == T_OBJECT
     && (ob_or_array->u.ob->flags & O_DESTRUCTED))
        return MY_FALSE;

    /* In mudlib 3.0 we will not have this. */
    if (cs[0] == '@')
        error("Unsupported @ construct.\n");

    ocs = cmd = string_copy(cs);

    /* Get the accessible objects
     */
    if (ob_or_array->type == T_OBJECT)
        makeobjlist(&gOblist, ob_or_array->u.ob);
    else
    {
        /* Copy list of given accessible objects in reverse order */
        vector_t *v;
        svalue_t *vv;
        size_t cnt;
      
        v = ob_or_array->u.vec;
        italt_new();
        for (cnt = 0; cnt < VEC_SIZE(v); cnt++)
        {
          vv = &(v->item[VEC_SIZE(v)-1-cnt]);
          if (vv->type == T_OBJECT)
              italt_put(vv->u.ob);
        }
        gOblist = gPobjects;
        gPobjects = NULL;
    }

    ops = parsep = string_copy(ps);
    gPobjects = NULL;

    /* Start parsing
     */
    txflag = -1;
    gCarg = NULL;
    gTxarg = NULL;
    gTopStack = dest_args + num_arg;
    l = dest_args;

    /* Parse loop */
    ptyp = get1ps(&parsep, &l, MY_FALSE); /* Get first pattern from parse */
    while (ptyp != EP)
    {
        altflag = 0;
        switch(ptyp) /* See which pattern type to search for */
        {
        case EP: break;

        case SI: if (findsingle(&cmd)) altflag = 1; /* %o */ break;
        case IT: if (findobject(&cmd)) altflag = 1; /* %i */ break;
        case US: if (findplay(&cmd))   altflag = 1; /* %l */ break;
        case PP: if (findprepos(&cmd)) altflag = 1; /* %p */ break;

        case TX: /* %s */
            txflag = 1;
            strcpy(tx_jp,"");
            gTxarg = gCarg;
            altflag = 1;
            tx_save_parsep = parsep;
            break;

        case DTX: /* 'word' */
            if (EQ(lowercase(gMword),lowercase(lookfirst(cmd))))
            {
                getfirst(&cmd);
                altflag = 1;
            }
            break;

        case OTX: /* [word] */
            if (EQ(lowercase(gMword),lowercase(lookfirst(cmd))))
            {
                getfirst(&cmd);
            }
            altflag = 1;
                /* Always take next parsepattern type [word] is opt */
            break;

        case ALT: /* / */
            altflag = 1; /* Should not be found here, if so skip it */
            break;

        case W1: /* %w */
            gTxarg = gCarg;
            strcpy(tx_jp,"");
            altflag = 1;
            addword(tx_jp,getfirst(&cmd));
            break;

        case NUM: /* %d */
            if ((altflag = numeric(lookfirst(cmd))) > 0)
            {
                if (gCarg)
                {
                    put_number(&sv_tmp, altflag);
                    transfer_svalue(gCarg->u.lvalue, &sv_tmp);
                }
                altflag = 1;
                getfirst(&cmd);
            }
            break;
        } /* switch(ptyp) */

        /* Pattern checked altflag==1 indicates match
        */
        if (altflag) /* Pattern matched, fetch next and end string input */
        {
            if (ptyp == TX)
            {
                ptyp = get1ps(&parsep, &l, MY_FALSE);
                tx_end_first_pattern = parsep;
            }
            else
            {
                ptyp = get1ps(&parsep, &l, MY_FALSE);
                txflag = -1; /* End string input if not just started */
            }
            while (ptyp == ALT) /* Skip left over alternatives */
            {
                ptyp = get1ps(&parsep, &l, MY_TRUE);  /* Skip this pattern */
                ptyp = get1ps(&parsep, &l, MY_FALSE); /* Next real pattern or ALT */
            }
        }
        else /* Pattern did not match */
        {
            char *a;
            LVALUE *try;
            ptype_t tmp;
          
            a = parsep;
            try = l;
            tmp = get1ps(&a, &try, MY_FALSE);
            if (tmp == ALT)
            {
                parsep = a;                           /* Skip ALT */
                ptyp = get1ps(&parsep, &l, MY_FALSE); /* Next real pattern or ALT */
            }
            else
            {
                if (txflag >= 0 && *getfirst(&cmd))
                {
                    /* %s is defined, add word and try pattern again */
                    addword(tx_jp, gFword);
                    if (parsep != tx_end_first_pattern)
                    {
                        parsep = tx_save_parsep;
                        ptyp = get1ps(&parsep, &l, MY_FALSE);
                    }
                }
                else
                    break; /* Impossible to match pattern, exit */
            }
        }
    } /* while() */

    /* End of pattern reached, what have got? What is left?
    */
    if (txflag >= 0) /* It ended with a %s, fill up the string */
    {
        while (!EMPTY(cmd))
            addword(tx_jp,getfirst(&cmd));
    }

    if (!EMPTY(cmd))  /* No match so set ptyp != EP */
        ptyp = ALT; 

    /* Now clean up our mess, no alloced mem should remain
     */
    xfree(ocs);
    xfree(ops);
    if (gPobjects)  /* Free alternate object list */
        italt_new();
    if (gOblist)
    {
        /* Free list of accessible objects */
        gPobjects = gOblist;
        italt_new();
    }

    return (ptyp == EP);
} /* e_old_parse_command() */

#endif /* SUPPLY_PARSE_COMMAND */

/***************************************************************************/

