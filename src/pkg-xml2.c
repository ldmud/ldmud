/*------------------------------------------------------------------
 * xml_* Efuns
 *
 * Based on code written and donated 2009 by Heiko Kopp.
 *------------------------------------------------------------------
 * This file holds the efuns interfacing with libxml2 and provides 
 * functions for handling xml files and converting them between 
 * mappings and xml data strings.
 *
 *   efun: xml_
 *------------------------------------------------------------------
 */
#include "driver.h"
#include "machine.h"

#if defined(USE_XML) && defined(HAS_XML2)

#include <libxml/parser.h>
#include <libxml/xmlwriter.h>
#include <libxml/xmlreader.h>
#include "array.h"
#include "arraylist.h"
#include "xalloc.h"
#include "mapping.h"
#include "mstrings.h"
#include "simulate.h"
#include "interpret.h"
#include "pkg-xml2.h"
#include "typedefs.h"

#include "../mudlib/sys/xml.h"

/* Structure to walk over the attribute (properties in libxml2 jargon) to 
 * create property-nodes
 */
typedef struct attribute_walk_extra_s attribute_walk_extra_t;

/* Used to walk all attributes as well as for error handling. In case an 
 * error happens, this structure is called too.
 */
struct attribute_walk_extra_s
{
    xmlTextWriterPtr writer;
    char *tag_name;
};

/* Used for error handling. In case of an error our handler called with a 
 * pointer ot this structure.
 */
struct xml_cleanup_s
{
    error_handler_t head; /* push_error_handler saves the link to our handler here. */

    xmlTextReaderPtr reader;
    xmlTextWriterPtr writer;
    xmlBufferPtr buf;
};

static void *
xml_pkg_malloc (size_t size)

/*
 * Realize malloc with the driver-internal xalloc rather than a direct malloc()
 */
{
    return pxalloc(size);
}

static void
xml_pkg_free (void * ptr)

/*
 * Realize free with the driver-internal xfree rather than a direct free()
 */
{
    pfree(ptr);
}

static void *
xml_pkg_realloc (void * ptr, size_t size)

/*
 * Realize realloc() with the driver-internal rexalloc_traced() including file
 * and line rather the direct realloc()
 */
{
    return prexalloc(ptr, size);
}

static char *
xml_pkg_strdup (const char * str)
/*
 * Our own strdup() which uses our own memory allocator. Gets memory with 
 * pxalloc(), because it must not be subject to the garbage collector.
 */
{
    char *p;
    size_t len;
    
    len = strlen(str)+1;
    memsafe(p = pxalloc(len), len, "xml_pkg_strdup");
    if (p)
    {
        memcpy(p, str, len);
    }
    return p;
}

static void
add_string_to_mapping (mapping_t *map, const char *skey, const char *svalue)

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

static void
parse_node (svalue_t *result, xmlTextReaderPtr reader)

/*
 * Parses the xml document beginning at <node> and returns the information 
 * on the stack in <result>.
 */
{
    vector_t *element = NULL;
    svalue_t *children = NULL;
    /* We have a tag here, so allocate a tag array with three elements 
     * (name, contents and attributes)
     */
    memsafe(element = allocate_array(XML_TAG_SIZE), sizeof(*element)
           , "new tag array");

    /* Put the array as result */
    put_array(result, element);

    /* add name to array */
    put_c_string(&element->item[XML_TAG_NAME]
                , (const char *) xmlTextReaderConstName(reader));


    if (xmlTextReaderHasAttributes(reader))
    {
        mapping_t *attributes = NULL;

        /* allocate new mapping */
        memsafe(attributes = allocate_mapping(xmlTextReaderAttributeCount(reader), 1)
                , sizeof(*attributes), "new attributes mapping");

        /* add the attributes to the array */
        put_mapping(&element->item[XML_TAG_ATTRIBUTES], attributes);

        while (MY_TRUE)
        {
            int ret;

            ret = xmlTextReaderMoveToNextAttribute(reader);
            if (ret == 0)
                break;
            else if (ret < 0)
                errorf("(xml_parse) Error reading XML node.\n");

            add_string_to_mapping(attributes
                                 , (const char *) xmlTextReaderConstName(reader)
                                 , (const char *) xmlTextReaderConstValue(reader));
        }

        xmlTextReaderMoveToElement(reader);
    }

    if (!xmlTextReaderIsEmptyElement(reader))
        while (MY_TRUE)
        {
            int ret;
            int is_node = 0;

            ret = xmlTextReaderRead(reader);
            if (ret == 0)
                errorf("Bad arg 1 to xml_parse(): Premature end of data.\n");
            else if(ret < 0)
                errorf("(xml_parse) Error reading XML node.\n");

            switch (xmlTextReaderNodeType(reader))
            {
            case XML_READER_TYPE_END_ELEMENT:
                if (children != NULL)
                    finalize_arraylist(children);
                return;

            case XML_READER_TYPE_ELEMENT:
                is_node = 1;
                /* FALLTHROUGH */
            case XML_READER_TYPE_TEXT:
            case XML_READER_TYPE_CDATA:
                if (children == NULL)
                {
                    children = &(element->item[XML_TAG_CONTENTS]);
                    put_arraylist(children);
                }

                if (is_node)
                    parse_node(enhance_arraylist(children), reader);
                else
                    put_c_string(enhance_arraylist(children)
                                , (const char *) xmlTextReaderConstValue(reader));

                break;
            }
        }
}

static void
walk_attribute_mapping(svalue_t *key, svalue_t *val, void *pextra)

/*
 * Callback for walk_mapping() used in generate_xml_node to add
 * property-nodes to the node given in the <pextra>.
 */
{
    attribute_walk_extra_t *extra = pextra;
    int rc;

    if (key->type != T_STRING)
        errorf("Bad argument 1 to xml_generate(): expected string \
                for attribute key of tag '%s'.\n", extra->tag_name);

    if (val->type != T_STRING)
        errorf("Bad argument 1 to xml_generate(): expected string for \
                value of attribute '%s' of tag '%s'.\n"
              , get_txt(key->u.str), extra->tag_name);

    rc = xmlTextWriterWriteAttribute( extra->writer
                                    , (xmlChar *) get_txt(key->u.str)
                                    , (xmlChar *) get_txt(val->u.str));
    if (rc < 0)
         errorf("(xml_generate) Error writing attribute.\n");
}

static void
xml_cleanup (error_handler_t * arg)

/*
 * Takes care, that the xml document is correctly freed in case of an error 
 * and at the end of the f_generate_xml(). Additionally the xml-parser 
 * is cleaned up
 */
{
    struct xml_cleanup_s * data;

    data = (struct xml_cleanup_s *) arg;

    if (data->writer)
    {
       xmlFreeTextWriter(data->writer);
    }

    if (data->reader)
    {
       xmlFreeTextReader(data->reader);
    }

    if (data->buf)
    {
        xmlBufferFree(data->buf);
    }

    xfree(data);
} /* xml_cleanup() */

static void
write_xml_node(vector_t * vnode, xmlTextWriterPtr writer)

/* Writes a new xml node from the given array structure with the three
 * elements (name, contents, attributes) using <writer>.
 * The contents element may contain other tags, so recursion may occur.
 */
{
    svalue_t *element;
    char *name;
    int rc;

    if ((mp_int) VEC_SIZE(vnode) != 3)
    {
        errorf("Bad arg 1 to xml_generate(): tag is not an array with 3 \
                elements.\n");

        /* NOTREACHED */
        return;
    }

    /* get the name, as this is essential */
    element = &vnode->item[XML_TAG_NAME];

    if (element->type != T_STRING)
    {
        errorf("Bad arg 1 to xml_generate(): first element of tag array \
                not a string.\n");

        /* NOTREACHED */
        return;
    }

    name = get_txt(element->u.str);
    rc = xmlTextWriterStartElement(writer, (xmlChar *) name);
    if (rc < 0)
        errorf("(xml_generate) Error writing XML element.\n");

    /* now handle the attributes of this one */
    element = &vnode->item[XML_TAG_ATTRIBUTES];

    /* this might be absent */
    if (element->type == T_MAPPING)
    {
        attribute_walk_extra_t extra;

        extra.writer = writer;
        extra.tag_name = name;

        /* walk the mapping and add all attributes */
        walk_mapping(element->u.map, &walk_attribute_mapping, &extra);
    }
    else if (element->type != T_NUMBER || element->u.number != 0)
    {
        errorf("Bad arg 1 to xml_generate(): second element of tag array not "
               "NULL/mapping.\n");

        /* NOTREACHED */
        return;
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
                /* found content */
                rc = xmlTextWriterWriteString(writer, (xmlChar *) get_txt(element->u.str));
                if (rc < 0)
                    errorf("(xml_generate) Error writing plain text.\n");
            }
            else if (element->type == T_POINTER)
            {
                /* found a sub tag */
                write_xml_node(element->u.vec, writer);
            }
        }
    }
    else if (element->type != T_NUMBER || element->u.number != 0)
    {
        errorf("Bad arg 1 to xml_generate(): third element of tag array not "
               "NULL/array.\n");

        /* NOTREACHED */
    }

    rc = xmlTextWriterEndElement(writer);
    if (rc < 0)
        errorf("(xml_generate) Error finishing XML element.\n");
}

void
pkg_xml2_init ()
{
    // First override the default memory access functions
    xmlMemSetup(xml_pkg_free, xml_pkg_malloc, xml_pkg_realloc, xml_pkg_strdup);

    // Check for correct libxml version.
    LIBXML_TEST_VERSION
}

/*=========================================================================*/

/*                           EFUNS                                         */

/*-------------------------------------------------------------------------*/

svalue_t *
f_xml_generate (svalue_t *sp)

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
    struct xml_cleanup_s * rec_data;
    int rc;

    memsafe(rec_data = xalloc(sizeof(*rec_data)), sizeof(*rec_data)
            , "xml cleanup structure");
    rec_data->buf = NULL;
    rec_data->writer = NULL;
    rec_data->reader = NULL;
    push_error_handler(xml_cleanup, &(rec_data->head));

    /* the output buffer. */
    rec_data->buf = xmlBufferCreate();
    if (rec_data->buf == NULL)
        errorf("(xml_generate) Out of memory: temporary buffer.\n");

    rec_data->writer = xmlNewTextWriterMemory(rec_data->buf, 0);
    if (rec_data->writer == NULL)
        errorf("(xml_generate) Out of memory: XML writer.\n");

    rc = xmlTextWriterStartDocument(rec_data->writer, NULL, NULL, NULL);
    if (rc < 0)
        errorf("(xml_generate) Error starting XML document.\n");

    write_xml_node(sp->u.vec, rec_data->writer);

    rc = xmlTextWriterEndDocument(rec_data->writer);
    if (rc < 0)
        errorf("(xml_generate) Error finishing XML document.\n");

    /* Free the array. */
    free_svalue(sp);

    put_c_string(sp, (char *) rec_data->buf->content);

    /* The error handler will free the buffer
     * and XML writer.
     */
    pop_stack();

    return sp;
}

static void
xml_pkg_error_handler(void * userData, xmlErrorPtr error)
{
    if (error)
    {
        errorf("Bad arg 1 to xml_parse(): %s", error->message);
    }
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

    memsafe(rec_data = xalloc(sizeof(*rec_data)), sizeof(*rec_data)
            , "xml cleanup structure");
    rec_data->buf = NULL;
    rec_data->writer = NULL;
    rec_data->reader = NULL;
    push_error_handler(xml_cleanup, &(rec_data->head));

    /* Disable standard error functions, as they will print out errors
     * to stderr automatically. We do not want that.
     */
    xmlSetGenericErrorFunc(NULL, NULL);
    xmlSetStructuredErrorFunc(NULL, xml_pkg_error_handler);

    rec_data->reader = xmlReaderForMemory(get_txt(sp->u.str)
                                         , mstrsize(sp->u.str)
                                         , NULL, NULL, XML_PARSE_NOENT);

    if (rec_data->reader == NULL)
        errorf("(xml_generate) Out of memory: XML reader.\n");

    /* Put the result on top of the stack. */
    push_number(inter_sp, 0);

    /* Look for the first element. */
    do
    {
        int ret;

        ret = xmlTextReaderRead(rec_data->reader);
        if (ret == 0)
            errorf("Bad arg 1 to xml_parse(): Premature end of data.\n");
        else if(ret < 0)
            errorf("(xml_parse) Error reading XML node.\n");

        switch (xmlTextReaderNodeType(rec_data->reader))
        {
            case XML_READER_TYPE_ATTRIBUTE:
            case XML_READER_TYPE_TEXT:
            case XML_READER_TYPE_CDATA:
                errorf("Bad arg 1 to xml_parse(): Start tag expected.\n");

            case XML_READER_TYPE_ELEMENT:
                break;

            default:
                continue;
        }
    } while (MY_FALSE);

    /* Now parse the XML string. */
    parse_node(inter_sp, rec_data->reader);

    /* we no longer need the string */
    free_svalue(sp);

    *sp = *inter_sp;
    inter_sp--;

    /* At the end, be nice and remove the rest using our error handler. */
    pop_stack();
 
    return sp;
}
 
#endif /* USE_XML && HAS_XML2 */
