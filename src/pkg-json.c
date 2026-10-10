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
#include "ptrtable.h"
#include "simulate.h"
#include "xalloc.h"

#ifndef DEBUG
#define NDEBUG
#endif
#include <assert.h>
#include <stdio.h>

/* Maximum nesting depth accepted by json_serialize().
 *
 * The serializer recurses once per container level, and so do json-c's
 * printer and its object destructor afterwards. Without a limit a
 * sufficiently nested (but perfectly acyclic) value overflows the C stack:
 * with the default 8 MB stack the recursion dies at a nesting depth of
 * roughly 40000. The limit below keeps a wide margin to that, while still
 * being far deeper than anything json-c can read back - its tokener
 * defaults to a maximum nesting depth of 32.
 */
#define MAX_JSON_NESTING_DEPTH 1000

/* Context for one json_serialize() call.
 *
 * <ptable> holds the containers on the current path from the root to the
 * value being serialized, so that a container containing itself can be
 * detected. <depth> is the length of that path.
 */
struct json_serialize_ctx {
    struct pointer_table *ptable;
    int                   depth;
};

/* Payload for walk_mapping(): the JSON object to fill and the context. */
struct json_walk_ctx {
    struct json_object        *parent;
    struct json_serialize_ctx *ctx;
};

struct json_error_handler_s {
    error_handler_t       head;
    struct json_object   *jobj;
    struct pointer_table *ptable;
};

static void json_error_cleanup(error_handler_t *arg) __attribute__((nonnull(1)));
static INLINE bool push_json_error_handler(struct json_object *jobj, struct pointer_table *ptable) __attribute__((nonnull(1)));
static void ldmud_json_walker(svalue_t *key, svalue_t *val, void *extra) __attribute__((nonnull(1,2,3)));
static INLINE void ldmud_json_attach(struct json_object *parent, const char *key, struct json_object *val) __attribute__((nonnull(1,3)));
svalue_t *ldmud_json_parse (svalue_t *sp, struct json_object *val) __attribute__((nonnull(1,2)));
struct json_object *ldmud_json_serialize (svalue_t *sp, struct json_object *parent, const char *key, struct json_serialize_ctx *ctx) __attribute__((nonnull(1,4)));

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
    if (!push_json_error_handler(parsed, NULL))
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
    struct json_serialize_ctx ctx = { NULL, 0 };

    // In case of simple types, this is straight-forward. But for 'container'
    // types like arrays, mappings, structs, it gets more complicated.
    switch(sp->type)
    {
        case T_NUMBER:
        case T_FLOAT:
        case T_STRING:
            // just create a json object containing the value.
            jobj = ldmud_json_serialize(sp, NULL, NULL, &ctx);
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
    if (parent != jobj) // for container types
    {
        // The container may contain itself, directly or indirectly. The
        // pointer table records the containers on the path from the root to
        // the value currently being serialized, so that such a cycle is
        // detected instead of recursing until the C stack is exhausted.
        ctx.ptable = new_pointer_table();
        if (!ctx.ptable)
        {
            json_object_put(parent);
            errorf("json_serialize(): could not allocate memory for the pointer table.\n");
        }
    }
    // Push errorhandler with the json object in case there is a later call
    // to errorf().
    if (!push_json_error_handler(parent, ctx.ptable))
    {
        json_object_put(parent);
        if (ctx.ptable)
            free_pointer_table(ctx.ptable);
        errorf("json_serialize(): could not allocate memory for error handler.\n");
    }
    if (parent != jobj) // for container types
    {
        // create the json object with the 'real' data. It will be attached to
        // parent for freeing in case of errors.
        jobj = ldmud_json_serialize(sp, parent, NULL, &ctx);
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
static INLINE bool
push_json_error_handler(struct json_object * jobj, struct pointer_table *ptable)
/* An error handler is pushed onto the value stack so that the given json_object
 * and the given pointer table (which may be NULL) are safely freed either by
 * manually freeing the svalue on the stack or during
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
        return false;
    }
    handler->jobj = jobj;
    handler->ptable = ptable;
    /* now push error handler onto the value stack */
    push_error_handler(json_error_cleanup, &(handler->head));
    return true;
} /* alloc_with_error_handler */

/*-------------------------------------------------------------------------*/
static void
json_error_cleanup (error_handler_t *arg)
/* Frees the json object contained in the error handler, the pointer table
 * and the handler.
 * Called from free_svalue() (e.g. during stack unwinding in case of errors).
 */
{
    struct json_error_handler_s *info = (struct json_error_handler_s *)arg;
    // free the referenced jobj (decrease refcounter)
    if (info->jobj)
        json_object_put(info->jobj);
    if (info->ptable)
        free_pointer_table(info->ptable);
    xfree(info);
}

/*-------------------------------------------------------------------------*/
static void
json_enter_container (struct json_serialize_ctx *ctx, void *container)
/* Note that <container> is entered, i.e. put onto the path of containers
 * currently being serialized.
 *
 * Raises an error if <container> is already on that path (which means the
 * value contains itself and the recursion would not terminate), or if the
 * path has become too long.
 *
 * <container> may be NULL for container-like values without an identity of
 * their own (e.g. a range of an array); those only count towards the depth.
 */
{
    if (container != NULL)
    {
        struct pointer_record *prec;

        prec = find_add_pointer(ctx->ptable, container, MY_TRUE);
        if (prec == NULL)
            errorf("json_serialize(): out of memory.\n");
        if (prec->id_number)
            errorf("json_serialize(): cyclic structure.\n");
        prec->id_number = 1;
    }

    if (++ctx->depth > MAX_JSON_NESTING_DEPTH)
        errorf("json_serialize(): nesting too deep, limit is %d.\n"
              , MAX_JSON_NESTING_DEPTH);
} /* json_enter_container() */

/*-------------------------------------------------------------------------*/
static void
json_leave_container (struct json_serialize_ctx *ctx, void *container)
/* Note that <container> has been serialized completely and is no longer on
 * the path, so that it may legitimately appear again as a sibling.
 *
 * Not called when the serialization was aborted by an error - in that case
 * the whole table is freed by the error handler anyway.
 */
{
    if (container != NULL)
    {
        struct pointer_record *prec;

        prec = lookup_pointer(ctx->ptable, container);
        if (prec != NULL)
            prec->id_number = 0;
    }
    ctx->depth--;
} /* json_leave_container() */

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
        int64_t val = json_object_get_int64(jobj);
#if PINT_MAX < INT64_MAX
        // if val may exceed the numerical limits of our p_int.
        if (val < PINT_MIN || val > PINT_MAX)
            warnf("json_parse(): 64 bit long integer %"PRId64" was truncated to 32 bit.\n",
                  val);
#endif
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
ldmud_json_walker(svalue_t *key, svalue_t *val, void *extra)
/*
   * Adds svalue <val> to the json object in <extra> under the key <key>.
   *
   * <extra> points to a struct json_walk_ctx holding the JSON object to fill
   * and the serialization context.
   * 
   * Note: <key> must be of type T_STRING.
   *       The mapping MUST have at least one value per key.
   *       Does only serialize the value <val> is pointing to (i.e in mappings
   *       then first value of a key is serialized).
   *
   * WARNING: might call errorf().
*/
{
    struct json_walk_ctx *walk = (struct json_walk_ctx *)extra;
    if (key->type != T_STRING)
    {
        errorf("json_serialize(): JSON supports only string keys, but got: %s\n",
               sv_typename(key));
        /* NOTREACHED */
        return;
    }
    ldmud_json_serialize(val, walk->parent, get_txt(key->u.str), walk->ctx);
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
ldmud_json_serialize (svalue_t *sp, struct json_object *parent, const char *key, struct json_serialize_ctx *ctx)
/*
   * Creates a JSON object containing the data of the svalue <sp> points to.
   * To do this, it calls itself recursively if needed (for container types).
   *
   * <ctx> carries the containers on the current path (for cycle detection)
   * and the current nesting depth. Serializing a value that contains itself
   * or is nested too deeply raises an error.
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
        jobj = json_object_new_int64(val->u.number);
        if (parent) ldmud_json_attach(parent, key, jobj);
        break;
    
    case T_FLOAT:
    {
        // We'll make sure that it won't be printed as an integer.
        char buf[100];
        size_t size = snprintf(buf, sizeof(buf), "%.17g", READ_DOUBLE(val));

        if (strspn(buf, "0123456789") == size && size + 2 < sizeof(buf))
        {
            // Consists only of digits, we add the ".0" to the string.
            buf[size] = '.';
            buf[size+1] = '0';
            buf[size+2] = 0;
        }
        else
            buf[sizeof(buf)-1] = 0;

        jobj = json_object_new_double_s(READ_DOUBLE(val), buf);
        if (parent) ldmud_json_attach(parent, key, jobj);
        break;
    }
    
    case T_STRING:
        jobj = json_object_new_string(get_txt(val->u.str));
        if (parent) ldmud_json_attach(parent, key, jobj);
        break;
    
    case T_POINTER:
        json_enter_container(ctx, val->u.vec);

        jobj = json_object_new_array();
        // the created object has to be attached to the parent immediately to
        // prevent any memory leaks in case there is a call to errorf() later.
        ldmud_json_attach(parent, key, jobj);

        for (int i = 0; i < VEC_SIZE(val->u.vec); ++i)
            ldmud_json_serialize(&val->u.vec->item[i], jobj, NULL, ctx);

        json_leave_container(ctx, val->u.vec);
        break;

    case T_MAPPING:
    {
        struct json_walk_ctx walk;

        if (val->u.map->num_values != 1)
          errorf("json_serialize(): can only serialize mappings with width 1, "
                 "but got mapping with width %"PRIdPINT".\n",
                 val->u.map->num_values);

        json_enter_container(ctx, val->u.map);

        jobj = json_object_new_object();
        // the created object has to be attached to the parent immediately to
        // prevent any memory leaks in case there is a call to errorf() later.
        ldmud_json_attach(parent, key, jobj);

        walk.parent = jobj;
        walk.ctx = ctx;
        walk_mapping(val->u.map, &ldmud_json_walker, &walk);

        json_leave_container(ctx, val->u.map);
        break;
    }
    case T_STRUCT:
    {
        struct_t  * st = val->u.strct;

        json_enter_container(ctx, st);

        jobj = json_object_new_object();
        // the created object has to be attached to the parent immediately to
        // prevent any memory leaks in case there is a call to errorf() later.
        ldmud_json_attach(parent, key, jobj);

        // Now loop over all members and assign the data
        for (int i  = 0; i < struct_size(st); ++i)
        {
            ldmud_json_serialize(&(st->member[i]),jobj,get_txt(st->type->member[i].name), ctx);
        }

        json_leave_container(ctx, st);
        break;
    }

    case T_LVALUE:
    {
        /* Must be a range, all other would have been handled by get_rvalue(). */
        if (sp->x.lvalue_type == LVALUE_PROTECTED_RANGE
         && sp->u.protected_range_lvalue->vec.type == T_STRING)
        {
            struct protected_range_lvalue* r = sp->u.protected_range_lvalue;
            jobj = json_object_new_string_len(get_txt(r->vec.u.str) + r->index1, r->index2 - r->index1);
            if (parent) ldmud_json_attach(parent, key, jobj);
        }
        else if (sp->x.lvalue_type == LVALUE_PROTECTED_RANGE
             && sp->u.protected_range_lvalue->vec.type == T_BYTES)
        {
            errorf("json_serialize(): can't serialize LPC type %s\n",
                   typename(T_BYTES));
        }
        else
        {
            struct range_iterator it;
            if (!get_iterator(sp, &it, true))
                fatal("Illegal lvalue type %d\n", sp->x.lvalue_type);

            // A range has no identity of its own to register, but it still
            // adds a level to the nesting depth.
            json_enter_container(ctx, NULL);

            jobj = json_object_new_array();
            if (parent) ldmud_json_attach(parent, key, jobj);

            while (true)
            {
                svalue_t *item = it.next_value(&it);
                if (!item)
                    break;
                ldmud_json_serialize(item, jobj, NULL, ctx);
            }

            json_leave_container(ctx, NULL);
        }
        break;
    }

    default: /* those are unimplemented */
        errorf("json_serialize(): can't serialize LPC type %s\n",
               sv_typename(sp));
        break;
    }
    return jobj;
} // ldmud_json_serialize

/***************************************************************************/
#endif /* USE_JSON */
