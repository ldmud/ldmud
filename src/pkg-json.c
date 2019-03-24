/*------------------------------------------------------------------
 * JSON Efuns.
 * support for javascript object notation
 * depends on the json-c library
 * for more information see:
 *     http://www.json.org
 *     http://oss.metaparadigm.com/json-c/
 *     https://github.com/jehiah/json-c
 *
 *------------------------------------------------------------------
 * This file holds the efuns interfacing with json-c / libjson.
 *
 *   efuns:
 *    json_parse()
 *    json_serialize()
 *------------------------------------------------------------------
 */

#include "driver.h"

#ifdef USE_JSON

#include "pkg-json.h"

#ifdef HAS_JSONC
#include <json-c/json.h>
#elif defined(HAS_JSON)
#include <json/json.h>
#endif

#include "array.h"
#include "mapping.h"
#include "structs.h"
#include "mstrings.h"
#include "interpret.h"
#include "simulate.h"
#include "xalloc.h"

#ifndef DEBUG
#define NDEBUG
#endif
#include <assert.h>

struct json_error_handler_s {
    error_handler_t     head;
    struct json_object *jobj;
};

static void json_error_cleanup(error_handler_t *arg) __attribute__((nonnull(1)));
static INLINE Bool push_json_error_handler(struct json_object *jobj) __attribute__((nonnull(1)));
static void ldmud_json_walker(svalue_t *key, svalue_t *val, void *parent) __attribute__((nonnull(1,2,3)));
static INLINE void ldmud_json_attach(struct json_object *parent, const char *key, struct json_object *val) __attribute__((nonnull(1,3)));
svalue_t *ldmud_json_parse (svalue_t *sp, struct json_object *val) __attribute__((nonnull(1,2)));
struct json_object *ldmud_json_serialize (svalue_t *sp, struct json_object *parent, const char *key) __attribute__((nonnull(1)));

/*-------------------------------------------------------------------------*/
/*                           EFUNS                                         */
/*-------------------------------------------------------------------------*/
svalue_t *
f_json_parse (svalue_t *sp)
/* EFUN json_parse()
 *
 *   mixed json_parse(string jsonstr)
 *
 * This efun parses the JSON object encoded as string in <jsonstr> into a
 * suitable LPC type.
 *
 * Handles the following JSON types:
 *   <null>        -> int (0)
 *   <boolean>     -> int (0 or 1)
 *   <int | int64> -> int
 *   <double>      -> float
 *   <string>      -> string
 *   <object>      -> mapping
 *   <array>       -> arrays
 * All other JSON types cause a runtime error.
 *
 * CAVEATS: 64 bit wide integers can only be parsed losslessly on hosts with
 *          a 64 bit wide LPC int and json-c library newer than 0.90.
 *
 * BUGS: __FLOAT_MIN__ is not serialized/parsed losslessly.
 *
 * TODO: introduce (dynamic) evalcost for recursive calls.
 */
{
    struct json_object *parsed;

    // parse text into json object
    parsed = json_tokener_parse(get_txt(sp->u.str));

#ifdef HAS_JSONC
    if (!parsed)
#else
    if (!parsed || is_error(parsed))
#endif
    {
        errorf("json_parse(): could not parse string - probably illegal JSON format.\n");
    }
    // Push errorhandler with json object in case ldmud_json_parse calls errorf().
    if (!push_json_error_handler(parsed))
    {
        json_object_put(parsed);
        errorf("json_parse(): could not allocate memory for error handler.\n");
    }
    
    // inter_sp now points to one value above our argument (sp). We free the
    // argument and let ldmud_json_parse() build the new svalue at that position
    // on the stack.
    free_svalue(sp);
    ldmud_json_parse (sp, parsed);
    
    // free errorhandler and the JSON object.
    // (Error handler is in inter_sp (sp+1), our new svalue is in sp.)
    free_svalue(inter_sp);
    --inter_sp;
    
    return sp;
} /* f_json_parse() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_json_serialize (svalue_t *sp)
/* EFUN json_serialize()
 *
 *   string json_serialize(mixed value)
 *
 * This efun creates a JSON object from the given LPC variable and returns the
 * object encoded as a LPC string. For container types like arrays, mappings
 * and structs, this will be done recursively.
 *
 * Only the following LPC types are serialized. All other LPC types cause a 
 * runtime error.
 * 
 *   <int>        -> JSON int
 *   <float>      -> JSON double
 *   <string>     -> JSON string
 *   <mapping>    -> JSON objects
 *   <array>      -> JSON arrays
 *   <struct>     -> JSON objects
 *
 * CAVEATS: On host platforms with 64 bit wide integers, the whole value range
 *          of the LPC int can only be serialized if the used json-c version
 *          on the host system supports it. Otherwise they could be truncated.
 *          Structs can be serialized, but since they are serialized into a
 *          JSON object, they will be parsed into LPC mappings. If you need a
 *          LPC struct, you have to use to_struct() later on.
 *
 * BUGS: __FLOAT_MIN__ is not serialized/parsed losslessly.
 *
 * TODO: introduce (dynamic) evalcost for recursive calls.
 */

{
    struct json_object *parent = NULL;
    struct json_object *jobj = NULL;
    
    // In case of simple types, this is straight-forward. But for 'container'
    // types like arrays, mappings, structs, it gets more complicated.
    switch(sp->type)
    {
        case T_NUMBER:
        case T_FLOAT:
        case T_STRING:
            // just create a json object containing the value.
            jobj = ldmud_json_serialize(sp, NULL, NULL);
            parent = jobj;
            break;
        default:
            // In this case, the process may be recursive and there is a chance
            // that errorf() is called and the ldmud_json_serialize() does not
            // return. To prevent leakages, a dummy json array is created and 
            // pushed within an error handler onto the value stack so that it
            // is freed in case of errors. The 'real' json object will then be
            // created as the first element of this array.
            parent = json_object_new_array();
            break;
    }
    if (!parent)
    {
        errorf("json_serialize(): could not create root JSON object (may be out of memory?).\n");
        return sp;  // not reached
    }
    // Push errorhandler with the json object in case there is a later call
    // to errorf().
    if (!push_json_error_handler(parent))
    {
        json_object_put(parent);
        errorf("json_serialize(): could not allocate memory for error handler.\n");
    }
    if (parent != jobj) // for container types
    {
        // create the json object with the 'real' data. It will be attached to
        // parent for freeing in case of errors.
        jobj = ldmud_json_serialize(sp, parent, NULL);
    }
    // inter_sp now points to one value above our argument (sp).
    // We free the argument and let put_c_string() put the new string at that
    // position on the stack.
    free_svalue(sp);
    put_c_string(sp, json_object_to_json_string(jobj));

    // Free the (parent) JSON object and the error handler. Since all created
    // JSON must be attached somehow to the parent object or its children, all
    // children will be freed as well.
    // (Error handler is in inter_sp (sp+1), our new string is in sp.)
    free_svalue(inter_sp);
     --inter_sp; // points now to sp with our new string.
    
    return sp;
} /* f_json_serialize() */


/*-------------------------------------------------------------------------*/
/*                           IMPLEMENTATION                                */
/*-------------------------------------------------------------------------*/

/*-------------------------------------------------------------------------*/
static INLINE Bool
push_json_error_handler(struct json_object * jobj)
/* An error handler is pushed onto the value stack so that the given json_object
 * is safely freed either by manually freeing the svalue on the stack or during
 * stack unwinding during errorf().
 * inter_sp has to point to the top-of-stack before calling and is updated to
 * point to the error handler svalue!
 */
{
    struct json_error_handler_s *handler;
    /* get the memory for the handler first and fail if out-of-memory */
    handler = xalloc(sizeof(*handler));
    if (!handler)
    {
        return FALSE;
    }
    handler->jobj = jobj;
    /* now push error handler onto the value stack */
    push_error_handler(json_error_cleanup, &(handler->head));
    return TRUE;
} /* alloc_with_error_handler */

/*-------------------------------------------------------------------------*/
static void
json_error_cleanup (error_handler_t *arg)
/* Frees the json object contained in the error handler and the handler.
 * Called from free_svalue() (e.g. during stack unwinding in case of errors).
 */
{
    struct json_error_handler_s *info = (struct json_error_handler_s *)arg;
    // free the referenced jobj (decrease refcounter)
    if (info->jobj)
        json_object_put(info->jobj);
    xfree(info);
}

/*-------------------------------------------------------------------------*/
svalue_t *
ldmud_json_parse (svalue_t *sp, struct json_object *jobj)
/*
    * Creates an svalue containing the data in the json object <jobj> and
    * stores it in <sp>.
    * Handles the following JSON types:
    *   <null>        -> T_NUMBER (0)
    *   <boolean>     -> T_NUMBER (0 or 1)
    *   <int | int64> -> T_NUMBER
    *   <double>      -> T_FLOAT
    *   <string>      -> T_STRING
    *   <object>      -> T_MAPPING
    *   <array>       -> T_POINTER
    * All other JSON types cause a runtime error.
    *
    * <jobj> MUST point to a valid struct json_object and <sp> MUST point to
    * an empty svalue_t.
    *
    * WARNING: might call (indirectly) errorf() and not return.
 */
{
    // although the function recurses, this static value does not pose a
    // problem: it is only used in one place for a short time and
    // freed _before_ it recurses. In some conditions the string it is leaked
    // and freed now.
    static svalue_t mkey = { T_INVALID };
    if (mkey.type == T_STRING)
        free_svalue(&mkey);
    
    switch(json_object_get_type(jobj)) {
    case json_type_null:
        put_number(sp, 0);
        break;
    case json_type_boolean:
        put_number(sp, json_object_get_boolean(jobj));
        break;
    case json_type_int:
        {
#ifdef JSON_64_SUPPORT
        int64_t val = json_object_get_int64(jobj);
#if PINT_MAX < INT64_MAX
        // if val may exceed the numerical limits of our p_int.
        if (val < PINT_MIN || val > PINT_MAX)
            warnf("json_parse(): 64 bit long integer %"PRId64" was truncated to 32 bit.\n",
                  val);
#endif
#else
        int32_t val = json_object_get_int(jobj);
        // val is only 32 bit wide, no check needed.
#endif // JSON_64_SUPPORT
        put_number(sp, val);
        }
        break;
    case json_type_double:
        put_float(sp, json_object_get_double(jobj));
        break;
    case json_type_string:
        put_c_string(sp, json_object_get_string(jobj));
        break;
    case json_type_object:
      {
        // create a new mapping and put it in <sp> immediately - if there is an
        // error in the recursive call to ldmud_json_parse() or in some other
        // function it would leak otherwise.
        mapping_t *m = allocate_mapping(json_object_get_object(jobj)->count, 1);
        put_mapping(sp, m);
        
        json_object_object_foreach(jobj, key, val) {
            svalue_t *mval;
            put_c_string(&mkey, key);
            // mkey may be leaked, but freed during next call to this function.
            mval = get_map_lvalue(m, &mkey);
            free_svalue(&mkey);
            if (!mval)
            {
                errorf("json_parse(): Out of memory, could not get mapping lvalue.\n");
            }
            // mval does not need to be freed first, because m is a fresh
            // mapping and the key is unique in JSON objects.
            ldmud_json_parse(mval, val);
        }
        
        break;
      }
    case json_type_array:
      {
        // create a new array and put it in <sp> immediately - if there is an
        // error in the recursive call to ldmud_json_parse() it would leak 
        // otherwise.
        int size = json_object_array_length(jobj);
        vector_t *v = allocate_array(size);
        put_array(sp, v);
        
        struct array_list *alist = json_object_get_array(jobj);
        for (int i = 0; i < size; ++i) {
            ldmud_json_parse(&(v->item[i]), array_list_get_idx(alist, i));
        }
        
        break;
      }
    default:
      errorf("json_parse(): unknown json object type.\n");
    }
    return sp;
} // ldmud_json_parse

/*-------------------------------------------------------------------------*/
static void
ldmud_json_walker(svalue_t *key, svalue_t *val, void *parent)
/*
   * Adds svalue <val> to json object <parent> under the key <key>.
   * 
   * Note: <key> must be of type T_STRING.
   *       The mapping MUST have at least one value per key.
   *       Does only serialize the value <val> is pointing to (i.e in mappings
   *       then first value of a key is serialized).
   *
   * WARNING: might call errorf().
*/
{
    struct json_object *jobj = (struct json_object *)parent;
    if (key->type != T_STRING)
    {
        errorf("json_serialize(): JSON supports only string keys, but got: %s\n",
               typename(key->type));
        /* NOTREACHED */
        return;
    }
    ldmud_json_serialize(val, jobj, get_txt(key->u.str));
} // ldmud_json_walker

/*-------------------------------------------------------------------------*/
static INLINE void
ldmud_json_attach(struct json_object *parent, const char *key, struct json_object *val)
/*
   * Attaches the object <val> to the object <parent>. If <key> is given, the
   * <parent> is assumed to be a json object, otherwise a json array.
   * Asserts that the <parent> is of the correct type.
   * <parent> MUST be a valid json object or array.
 */
{
    if (key)
    {
        assert(json_object_get_type(parent) == json_type_object);
        json_object_object_add(parent,key,val);
    }
    else
    {
        assert(json_object_get_type(parent) == json_type_array);
        json_object_array_add(parent, val);
    }
}

/*-------------------------------------------------------------------------*/
struct json_object *
ldmud_json_serialize (svalue_t *sp, struct json_object *parent, const char *key)
/*
   * Creates a JSON object containing the data of the svalue <sp> points to.
   * To do this, it calls itself recursively if needed (for container types).
   * Only T_NUMBER, T_FLOAT, T_STRINGS, T_POINTER, T_MAPPING and T_STRUCT are 
   * serialized. All other LPC types cause a runtime error.
   *
   * The returned JSON object will have one reference count (Refcount of json-c).
   *
   * The created object will be attached to the json object or array in <parent>
   * to prevent leakages in case of errors. The top-level <parent> must be
   * placed on the value stack within an error handler by the top-level caller.
   * <parent> MUST be a valid json object if <key> is given.
   * If <parent> is given, but no <key>, <parent> is assumed to be an array.
   * If neither <parent> nor <key> are given, the created object will not be
   * attached to another object but only returned. Do NOT do this for container
   * like LPC types!
   *
   * WARNING: might call errorf() and not return.
 */
{
    struct json_object *jobj;
    svalue_t *val = get_rvalue(sp, NULL);

    switch((val != NULL ? val : sp)->type) {
    case T_NUMBER:
#ifdef JSON_64_SUPPORT
        jobj = json_object_new_int64(val->u.number);
#else
        if (val->u.number > INT32_MAX || val->u.number < INT32_MIN)
            warnf("json_serialize(): truncated 64 bit long number %ld to 32 bit due missing support in JSON-C.\n",
                  val->u.number);
        jobj = json_object_new_int(val->u.number);
#endif   // JSON_64_SUPPORT

        if (parent) ldmud_json_attach(parent, key, jobj);
        break;
    
    case T_FLOAT:
        jobj = json_object_new_double(READ_DOUBLE(val));
        if (parent) ldmud_json_attach(parent, key, jobj);
        break;
    
    case T_STRING:
        jobj = json_object_new_string(get_txt(val->u.str));
        if (parent) ldmud_json_attach(parent, key, jobj);
        break;
    
    case T_POINTER:
        jobj = json_object_new_array();
        // the created object has to be attached to the parent immediately to
        // prevent any memory leaks in case there is a call to errorf() later.
        ldmud_json_attach(parent, key, jobj);

        for (int i = 0; i < VEC_SIZE(val->u.vec); ++i)
            ldmud_json_serialize(&val->u.vec->item[i], jobj, NULL);
        
        break;
    
    case T_MAPPING:
        if (val->u.map->num_values != 1)
          errorf("json_serialize(): can only serialize mappings with width 1, "
                 "but got mapping with width %ld.\n",val->u.map->num_values);
        
        jobj = json_object_new_object();
        // the created object has to be attached to the parent immediately to
        // prevent any memory leaks in case there is a call to errorf() later.
        ldmud_json_attach(parent, key, jobj);

        walk_mapping(val->u.map, &ldmud_json_walker, jobj);
        break;
    case T_STRUCT:
    {
        struct_t  * st = val->u.strct;
        jobj = json_object_new_object();
        // the created object has to be attached to the parent immediately to
        // prevent any memory leaks in case there is a call to errorf() later.
        ldmud_json_attach(parent, key, jobj);
        
        // Now loop over all members and assign the data
        for (int i  = 0; i < struct_size(st); ++i)
        {
            ldmud_json_serialize(&(st->member[i]),jobj,get_txt(st->type->member[i].name));
        }
        break;
    }

    case T_LVALUE:
    {
        /* Must be a range, all other would have been handled by get_rvalue(). */
        struct protected_range_lvalue* r = sp->u.protected_range_lvalue;
        if (r->vec.type == T_STRING)
        {
            jobj = json_object_new_string_len(get_txt(r->vec.u.str) + r->index1, r->index2 - r->index1);
            if (parent) ldmud_json_attach(parent, key, jobj);
        }
        else
        {
            jobj = json_object_new_array();
            if (parent) ldmud_json_attach(parent, key, jobj);

            for (mp_int i = r->index1; i < r->index2; ++i)
                ldmud_json_serialize(r->vec.u.vec->item + i, jobj, NULL);
        }
        break;
    }

    default: /* those are unimplemented */
        errorf("json_serialize(): can't serialize LPC type %s\n",
               typename(sp->type));
        break;
    }
    return jobj;
} // ldmud_json_serialize

/***************************************************************************/
#endif /* USE_JSON */
