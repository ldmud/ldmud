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

#include <json/json.h>
#include "array.h"
#include "mapping.h"
#include "mstrings.h"
#include "interpret.h"
#include "simulate.h"

svalue_t *ldmud_json_parse (svalue_t *sp, struct json_object *val);
struct json_object *ldmud_json_serialize (svalue_t *sp);

/*-------------------------------------------------------------------------*/
/*                           EFUNS                                         */
/*-------------------------------------------------------------------------*/
svalue_t *
f_json_parse (svalue_t *sp)
/* EFUN json_parse()
 *
 *   mixed json_parse(string jsonstr)
 *
 * This efun parses a JSON object encoded in as string in <jsonstr> into a
 * suitable LPC type.
 */
{
    struct json_object *parsed;

    // parse text into json object
    parsed = json_tokener_parse(get_txt(sp->u.str));

    free_svalue(sp);
    ldmud_json_parse (sp, parsed);
    json_object_put(parsed); // free the JSON object
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
 * object encoded as a LPC string.
 */

{
    struct json_object *jobj = ldmud_json_serialize(sp);
    free_svalue(sp);
    put_c_string(sp, json_object_to_json_string(jobj));
    json_object_put(jobj); // free json object

    return sp;
} /* f_json_serialize() */


/*-------------------------------------------------------------------------*/
/*                           IMPLEMENTATION                                */
/*-------------------------------------------------------------------------*/
svalue_t *
ldmud_json_parse (svalue_t *sp, struct json_object *jobj)
/*
    * WARNING: might call (indirectly) errorf().
 */
{
    if (jobj == NULL || is_error(jobj)) {
        errorf("json_parse(): error while parsing json object.\n");
        /* NOTREACHED */
        return sp;

    }

    switch(json_object_get_type(jobj)) {
    case json_type_null:
        put_number(sp, 0);
        break;
    case json_type_boolean:
        put_number(sp, json_object_get_boolean(jobj));
        break;
    case json_type_int:
        put_number(sp, json_object_get_int(jobj));
        break;
    case json_type_double:
        put_float(sp, json_object_get_double(jobj));
        break;
    case json_type_string:
        put_c_string(sp, json_object_get_string(jobj));
        break;
    case json_type_object:
      {
        mapping_t *m;

        m = allocate_mapping(json_object_get_object(jobj)->count, 1);

        json_object_object_foreach(jobj, key, val) {
            svalue_t mkey, *mval;
            put_c_string(&mkey, key);
            mval = get_map_lvalue(m, &mkey);
            free_svalue(&mkey);
            ldmud_json_parse(mval, val);
        }

        put_mapping(sp, m);
        break;
      }
    case json_type_array:
      {
        vector_t *v;
        struct array_list *a;
        int size, i;
        size = json_object_array_length(jobj);
        v = allocate_array(size);
        a = json_object_get_array(jobj);
        for (i = 0; i < size; i++) {
            ldmud_json_parse(&(v->item[i]), array_list_get_idx(a, i));
        }
        put_array(sp, v);
        break;
      }
    default:
      errorf("json_parse(): unknown json object type.\n");
    }
    return sp;
} // ldmud_json_parse

void
ldmud_json_walker(svalue_t *key, svalue_t *val, void *parent)
/*
   * Note: The mapping MUST have at least one value per key.
   *       Does only serialize the first value of a key.
   * WARNING: might call errorf().
*/
{
    struct json_object *obj = (struct json_object *)parent;
    if (key->type != T_STRING)
    {
        errorf("json_serialize(): can only serialize string keys.\n");
        /* NOTREACHED */
    }
    json_object_object_add(obj, get_txt(key->u.str),
                           ldmud_json_serialize(val));
} // ldmud_json_walker

struct json_object *
ldmud_json_serialize (svalue_t *sp)
/*
   * The returned JSON object will have one reference count.
 */
{
    struct json_object *jobj;
    switch(sp->type) {
    case T_NUMBER:
        jobj = json_object_new_int(sp->u.number);
        break;
    case T_STRING:
        jobj = json_object_new_string(get_txt(sp->u.str));
        break;
    case T_POINTER:
      {
        int i;
        jobj = json_object_new_array();
        for (i = VEC_SIZE(sp->u.vec) - 1; i >= 0; i--)
            json_object_array_put_idx(jobj, i,
                        ldmud_json_serialize(&sp->u.vec->item[i]));
        break;
      }
    case T_MAPPING:
      {
        jobj = json_object_new_object();
        if (sp->u.map->num_values != 1)
            errorf("json_serialize(): can only serialize mappings with width 1, "
                   "but got mapping with width %ld.\n",sp->u.map->num_values);
        walk_mapping(sp->u.map, &ldmud_json_walker, jobj);
        break;
      }
    case T_FLOAT:
        jobj = json_object_new_double(READ_DOUBLE(sp));
        break;
    default: /* those are unimplemented */
        errorf("json_serialize(): encountered unimplemented LPC type %s\n",
               typename(sp->type));
        break;
    }
    return jobj;
} // ldmud_json_serialize

/***************************************************************************/
#endif /* USE_JSON */
