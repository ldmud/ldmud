/*------------------------------------------------------------------
 * iksemel Efuns
 *
 * Efuns written and donated 2008 by Heiko Kopp.
 *------------------------------------------------------------------
 * This file holds the efuns interfacing with iksemel and provides 
 * functions for handling xml files and converting them between 
 * mappings and xml data strings.
 *
 *   efun: xml_
 *------------------------------------------------------------------
 */
#include "driver.h"
#include "machine.h"

#if defined(USE_XML) && defined(HAS_IKSEMEL)

#include <iksemel.h>
#include "array.h"
#include "xalloc.h"
#include "mapping.h"
#include "mstrings.h"
#include "simulate.h"
#include "interpret.h"
#include "pkg-iksemel.h"
#include "typedefs.h"

#include "../mudlib/sys/xml.h"

typedef struct attribute_walk_extra_s attribute_walk_extra_t;

/* This structure is used to walk all attributes as well as for error handling.
 * In case an error happens, this structure is called too.
 */
struct attribute_walk_extra_s
{
    iks *node;
    int size;

    char *tag_name;
};

/* This structure is used for error handling. In case of an error our handler
 * called with a pointer ot this structure.
 */
struct xml_cleanup_s
{
    error_handler_t head; /* push_error_handler saves the link to our handler here. */

    iks *node;
    iksparser *parser;
};

void *
iksemel_alloc(size_t size)
{
    return pxalloc(size);
}

void
iksemel_free(void *ptr)
{
    pfree(ptr);
}

void
add_string_to_mapping(mapping_t *map, char *skey, char *svalue)

/*
 * Adds a string value under the given key to the given mapping. In case the 
 * value already exists, it is overriden.
 */
{
    svalue_t key;
    svalue_t *value;

    /* change the c string into an string_t */
    put_c_string(&key, skey);

    /* get or insert key */
    value = get_map_lvalue(map, &key);

    /* free the string_t again */
    free_svalue(&key);

    /* free maybe existing value (should not happen, i hope) */
    free_svalue(value);

    /* change the value of the key to the given value */
    put_c_string(value, svalue);
}

void
parse_node(svalue_t *result, iks *node)

/*
 * Parses the xml DOM starting at node and returns the information on the 
 * stack.
 */
{
    int i = 0;
    int num_attributes = 0;
    int num_children = 0;

    iks *attribute;
    iks *child;

    vector_t *root = NULL;
    vector_t *children = NULL;
    mapping_t *attributes = NULL;

    /* lets figure out if the node is cdata or another tag */
    switch (iks_type(node))
    {
        case IKS_NONE:
        case IKS_ATTRIBUTE:
            /* we ignore those, as they will not occure for us */

            return;
        case IKS_CDATA:
            /* Add the string as result, and return */
            put_c_n_string(result, iks_cdata(node), iks_cdata_size(node));

            return;
        case IKS_TAG:
            break;
    }

    /* We have a tag here, so allocate a tag array with three elements 
     * (name, contents and attributes)
     */
    memsafe(root = allocate_array(XML_TAG_SIZE), sizeof(*root), "new tag array");

    /* Put the array as result */
    put_array(result, root);

    /* add name to array */
    put_c_string(&root->item[XML_TAG_NAME], iks_name(node));

    /* check if the node has any children */
    child = iks_child(node);
    if (child != NULL)
    {
        do
        {
            ++num_children;
        }
        while ((child = iks_next(child)));
    }

    if (0 < num_children)
    {
        /* children need to stay in the right order, so we create another
         * for them
         */
        memsafe(children = allocate_array(num_children), sizeof(*children)
                , "new tag contents array");

        /* Add the array of all children to the node */
        put_array(&root->item[XML_TAG_CONTENTS], children);

        /* get the first child */
        child = iks_child(node);

        do
        {
            /* recurse here cause the child can be a string or another node */
            parse_node(&children->item[i++], child);        
        }
        while ((child = iks_next(child)));
    }

    /* Finally, lets handle the attributes, we need to find out how many
     * attributes the node has, to allocate enough memory for them. If
     * no attributes exist, the part in the array will be empty */

    attribute = iks_attrib(node);
    if (attribute != NULL)
    {
        do 
        {
            ++num_attributes;
        }
        while ((attribute = iks_next(attribute)));
    }

    if (0 < num_attributes)
    {
        /* allocate new mapping */
        memsafe(attributes = allocate_mapping(num_attributes, 1), sizeof(*attributes)
                , "new attributes mapping");

        /* add the attributes to the array */
        put_mapping(&root->item[XML_TAG_ATTRIBUTES], attributes);

        /* get the first one */
        attribute = iks_attrib(node);

        do
        {
            add_string_to_mapping(attributes, iks_name(attribute), iks_cdata(attribute));
        }
        while ((attribute = iks_next(attribute)));
   }
}

void
walk_attribute_mapping(svalue_t *key, svalue_t *val, void *pextra)

/*
 * Callback for walk_mapping() used in generate_xml_node to add iks
 * attribute nodes to the node given in the pextra. 
 */
{
    char *ckey;
    attribute_walk_extra_t *extra = pextra;

    if (key->type == T_STRING)
    {
        ckey = get_txt(key->u.str);
    }
    else
    {
        errorf("Bad argument 1 to xml_generate(): expected string for attribute key of tag '%s'.\n"
                , extra->tag_name);

        /* NOTREACHED */
        return;
    }

    if (val->type == T_STRING)
    {
        memsafe(iks_insert_attrib(extra->node, ckey, get_txt(val->u.str))
                , sizeof(*ckey), "new iksemel attribute");
    }
    else
    {
        errorf("Bad argument 1 to xml_generate(): expected string for value of attribute '%s' of tag '%s'.\n"
                , ckey, extra->tag_name);
    }
}

static void
xml_cleanup(error_handler_t * arg)

/*
 * Takes care, that the node without parent (root node) is correctly freed in
 * case of an error and at the end of the f_generate_xml().
 */
{
    struct xml_cleanup_s * data;

    data = (struct xml_cleanup_s *)arg;

    if (data->node)
    {
        iks_delete(data->node);
    }
    
    if (data->parser)
    {
	iks_parser_delete(data->parser);
    }

    xfree(data);
} /* xml_cleanup() */

iks *
generate_xml_node(vector_t *vnode, iks *parent)

/* 
 * Generates a new iks node from the given array structure with the three 
 * elements (name, contents, attributes) and adds the node to the parent,
 * or in case this is empty, simply returns it. The contents element may 
 * contain other tags, so recursion may occur.
 */
{
    iks *node;
    svalue_t *element;
    char *name;
    struct xml_cleanup_s * rec_data;

    if ((mp_int) VEC_SIZE(vnode) != 3)
    {
        errorf("Bad arg 1 to xml_generate(): tag is not an array with 3 "
               "elements.\n");

        /* NOTREACHED */
        return NULL;
    }

    /* get the name, as this is essential */
    element = &vnode->item[XML_TAG_NAME];

    if (element->type != T_STRING)
    {
        errorf("Bad arg 1 to xml_generate(): first element of tag array not a "
               "string.\n");

        /* NOTREACHED */
        return NULL;
    }

    /* get the name */
    name = get_txt(element->u.str);

    /* depending whether there is a parent or not, we start the structure 
     * or add the node to the given one */
    if (parent == NULL)
    {
        memsafe(node = iks_new(name), 30, "new iksemel node");

        rec_data = xalloc(sizeof(*rec_data));
        if (rec_data == NULL)
        {
            iks_delete(node);

            errorf("generate_xml() Out of memory: (%lu bytes) for cleanup structure\n"
                    , (unsigned long) sizeof(*rec_data));

            /* NOTREACHED */
            return NULL;
        }
        rec_data->node = node;
	rec_data->parser = NULL;

        push_error_handler(xml_cleanup, &(rec_data->head));
    }
    else
    {
        memsafe(node = iks_insert(parent, name), 30, "insert new iksemel node");
    }

    /* now handle the attributes of this one */
    element = &vnode->item[XML_TAG_ATTRIBUTES];

    /* this might be absent */
    if (element->type == T_MAPPING)
    {
        attribute_walk_extra_t extra;

        extra.node = node;
        extra.size = element->u.map->num_values;
        extra.tag_name = name;

        /* walk the mapping and add all attributes */
        walk_mapping(element->u.map, &walk_attribute_mapping, &extra);
    }
    else if (element->type != T_NUMBER || element->u.number != 0)
    {
        errorf("Bad arg 1 to xml_generate(): second element of tag array not "
               "NULL/mapping.\n");

        /* NOTREACHED */
        return NULL;
    }

    /* now check, if the node has a contents */
    element = &vnode->item[XML_TAG_CONTENTS];

    /* this might even be absent */
    if (element->type == T_POINTER)
    {
        int size;
        int i;
        vector_t *contents;

        /* get the vector */
        contents = element->u.vec;

        /* get its size */
        size = (mp_int)VEC_SIZE(contents);
        
        for (i = 0; i < size; i++)
        {
            element = &contents->item[i];

            if (element->type == T_STRING)
            {
                /* found a cdata */
                memsafe(iks_insert_cdata(node, get_txt(element->u.str), mstrsize(element->u.str))
                            , mstrsize(element->u.str)
                            , "new iksemel node cdata");
            }
            else if (element->type == T_POINTER)
            {
                /* found a sub tag, as iks_insert will handle the insert we do
                 * not have anything to do with the result
                 */
                generate_xml_node(element->u.vec, node);
            }
        }
    }
    else if (element->type != T_NUMBER || element->u.number != 0)
    {
        errorf("Bad arg 1 to xml_generate(): third element of tag array not "
               "NULL/array.\n");

        /* NOTREACHED */
        return NULL;
    }

    return node;
}

void
pkg_iksemel_init()
{
    iks_set_mem_funcs(iksemel_alloc, iksemel_free);
}

/*=========================================================================*/

/*                           EFUNS                                         */

/*-------------------------------------------------------------------------*/

svalue_t *
f_xml_generate(svalue_t *sp)

/* EFUN xml_generate()
 *
 *     string xml_generate(mixed *xml)
 *
 * Converts the given <xml> array into an XML conform string, if
 * possible. The <xml> argument array must have the same structure
 * as xml_parse returns.
 *
 * In case the parameter does not follow these rules, errors are raised.
 * The method returns a valid XML string otherwise.
 */
{
    char *xml_string;
    iks *node;
    vector_t *root;

    /* get the root of the structure to be used */
    root = sp->u.vec;

    /* start generating the tree */
    node = generate_xml_node(root, NULL);
    
    /* At this point generate_xml_node() had
       put an error handler on the stack.
     */
    
    /* Clean up and return result */
    free_svalue(sp);

    /* get the xml string out of the stack */
    memsafe(xml_string = iks_string(iks_stack(node), node)
            , sizeof(*xml_string), "new xml string from node");

    /* send the xml string back onto the stack */
    put_c_string(sp, xml_string);

    /* clean up, this will free the root node too, as it calls our error handler */
    pop_stack();

    return sp;
}

svalue_t *
f_xml_parse(svalue_t * sp)

/* EFUN xml_parse()
 *
 *     mixed * xml_parse(string xml_text)
 *
 * Parses the given string <xml> as a XML conform string. The string must
 * have only one root tag, subsequent root tags are ignored.
 *
 * If the xml string is correct, an array is of three elements is
 * returned, where as the following indices are defined:
 *
 *     string XML_TAG_NAME
 *         The name of the XML tag.
 *
 *     mixed * XML_TAG_CONTENTS
 *         The contents of this xml tag as array. This array may
 *         contain either strings, or arrags of sub-tags again with
 *         three elements (see example)
 *
 *         If the xml tag does not contain anything, the element is
 *         set 0.
 *
 *     mapping XML_TAG_ATTRIBUTES
 *         All attributes given to the XML tag as mapping where the key
 *         is the attribute name and the value is its string value.
 *
 *         If the xml tag does not contain any attributes, this element
 *         is set 0.
 *
 * If the XML string is not well formed, or there is not enough memory to
 * parse the whole XML structure into the array an error is raised. In case
 * the XML string can't be parsed, cause it is not valid XML, 0 is returned.
 */
{
    struct xml_cleanup_s * rec_data;
    int err;

    memsafe(rec_data = xalloc(sizeof(*rec_data)), sizeof(*rec_data), "xml cleanup structure");
    rec_data->node = NULL;
    rec_data->parser = NULL;
    push_error_handler(xml_cleanup, &(rec_data->head));

    /* TODO: This can be implemented more efficient using the SAX interface. */
    memsafe(rec_data->parser = iks_dom_new(&(rec_data->node)), 50, "new iksemel parser");

    err = iks_parse(rec_data->parser, get_txt(sp->u.str), mstrsize(sp->u.str), 1);
    switch (err)
    {
    case IKS_OK:
        break;

    case IKS_NOMEM:
        errorf("Out of memory.\n");

        /* NOTREACHED */
        return sp;

    case IKS_BADXML:
        errorf("Bad arg 1 to xml_parse(): XML document not well formed (error "
               "in line %ld, byte %ld).\n", iks_nr_lines(rec_data->parser)
               , iks_nr_bytes(rec_data->parser));

        /* NOTREACHED */
        return sp;

    case IKS_HOOK:
        /* actually only used for a sax parser? */

        break;
    }

    /* we no longer need the string */
    free_svalue(sp);

    /* set 0 to always have a valid return */
    put_number(sp, 0);

    if (rec_data->node != NULL)
    {
        /* tree contains the tree now, this will put the resulting a */
        parse_node(sp, rec_data->node);
    }
    else
    {
        /* There was no XML tag or the tag was not closed properly. */
        errorf("Bad arg 1 to xml_parse(): XML document not well formed (premature end "
               "at line %ld, byte %ld).\n", iks_nr_lines(rec_data->parser)
               , iks_nr_bytes(rec_data->parser));
    }

    /* At the end, be nice and remove the rest
       using our error handler. */
    pop_stack();
 
    return sp;
}
 
#endif /* USE_XML && HAS_IKSEMEL */
