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
    struct json_object *val = ldmud_json_serialize(sp);
    free_svalue(sp);
    put_c_string(sp, json_object_to_json_string(val));
    // TODO: free json object?
    return sp;
} /* f_json_serialize() */


/*-------------------------------------------------------------------------*/
/*                           IMPLEMENTATION                                */
/*-------------------------------------------------------------------------*/
svalue_t *
ldmud_json_parse (svalue_t *sp, struct json_object *val)
{
    if (is_error(val)) {
        errorf("json_inner_parse: error");
        /* NOTREACHED */
        return sp;

    }
    if (val == NULL) {
        /* TODO: I (fippo) am not sure, if this is a really good idea... */
        put_number(sp, 0);
        return sp;
    }
    switch(json_object_get_type(val)) {
    case json_type_null:
        put_number(sp, 0);
        break;
    case json_type_boolean:
        put_number(sp, json_object_get_boolean(val));
        break;
    case json_type_double:
        put_float(sp, json_object_get_double(val));
        break;
    case json_type_int:
        put_number(sp, json_object_get_int(val));
        break;
    case json_type_string:
        put_c_string(sp, json_object_get_string(val));
        break;
    case json_type_object:
      {
        mapping_t *m;
        struct lh_entry *e;
        char *key;
        struct json_object *newval;

        m = allocate_mapping(json_object_get_object(val)->count, 1);
        for (e = json_object_get_object(val)->head; e ? (key = (char*)e->k, newval = (struct json_object *)e->v, e) : 0; e = e->next) {
            svalue_t mkey, *mval;
            put_c_string(&mkey, key);
            mval = get_map_lvalue(m, &mkey);
            free_svalue(&mkey);
            ldmud_json_parse(mval, newval);
        }
        put_mapping(sp, m);
        break;
      }
    case json_type_array:
      {
        vector_t *v;
        struct array_list *a;
        int size, i;
        size = json_object_array_length(val);
        v = allocate_array(size);
        a = json_object_get_array(val);
        for (i = 0; i < size; i++) {
            ldmud_json_parse(&(v->item[i]), array_list_get_idx(a, i));
        }
        put_array(sp, v);
        break;
      }
    }
    return sp;
} // ldmud_json_parse

void
ldmud_json_walker(svalue_t *key, svalue_t *val, void *parent)
{
    struct json_object *obj = (struct json_object *)parent;
    if (key->type != T_STRING)
        errorf("json only serializes string keys\n");
        /* NOTREACHED */
    json_object_object_add(obj, get_txt(key->u.str),
                           ldmud_json_serialize(val));
} // ldmud_json_walker

struct json_object *
ldmud_json_serialize (svalue_t *sp) {
    struct json_object *val;
    switch(sp->type) {
    case T_NUMBER:
        val = json_object_new_int(sp->u.number);
        break;
    case T_STRING:
        val = json_object_new_string(get_txt(sp->u.str));
        break;
    case T_POINTER:
      {
        int i;
        val = json_object_new_array();
        for (i = VEC_SIZE(sp->u.vec) - 1; i >= 0; i--)
            json_object_array_put_idx(val, i,
                        ldmud_json_serialize(&sp->u.vec->item[i]));
        break;
      }
    case T_MAPPING:
      {
        int i;
        val = json_object_new_object();
        //TODO: Fehler: for ist unsinn und ldmud_json_walker braucht in extra
        //die Mappingbreite...
        for (i = 0; i < MAP_SIZE(sp->u.map); i++)
            walk_mapping(sp->u.map, &ldmud_json_walker, val);
        break;
      }
    case T_FLOAT:
        val = json_object_new_double(READ_DOUBLE(sp));
        break;
    default: /* those are unimplemented */
        val = json_object_new_object();
        break;
    }
    return val;
} // ldmud_json_serialize

/***************************************************************************/
#endif /* USE_FOO */
