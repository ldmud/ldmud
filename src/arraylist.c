/*---------------------------------------------------------------------------
 * Arraylist utility functions.
 *
 *---------------------------------------------------------------------------
 * These functions help to assemble an array whose length is unknown at the
 * beginning. So at first all elements are kept in a singly linked list
 * until finalize_arraylist is called.
 *
 * The linked list is saved as an error handler in a svalue_t, so it will
 * destruct itself and all of its elements automatically when the svalue_t
 * is freed.
 *
 * This structure is not designed to survive a garbage_collection.
 * It should always be finalized or freed in the same execution thread.
 *---------------------------------------------------------------------------
 */

#include "driver.h"
#include "typedefs.h"

#include "array.h"
#include "arraylist.h"
#include "interpret.h"
#include "simulate.h"
#include "svalue.h"
#include "xalloc.h"

/*-------------------------------------------------------------------------*/
/* Types */

typedef struct arraylist_s arraylist_t;
typedef struct arraylist_element_s arraylist_element_t;

struct arraylist_s
{
    error_handler_t head; /* The error handler. */

    arraylist_element_t *first;
    arraylist_element_t *last;
    int num;
};

struct arraylist_element_s
{
    svalue_t value;
    arraylist_element_t * next;
};

/*-------------------------------------------------------------------------*/
static void
cleanup_arraylist (error_handler_t * list)

/* Our cleanup handler, called when the arraylist is freed.
 */
{
    arraylist_t * data = (arraylist_t *) list;
    arraylist_element_t * element = data->first;

    while (element)
    {
        arraylist_element_t * temp = element;

        element = element->next;

        free_svalue(&(temp->value));
        xfree(temp);
    }

    xfree(data);
}

/*-------------------------------------------------------------------------*/
void
put_arraylist (svalue_t * list)

/* Create an arraylist and put it into <list>.
 */
{
    arraylist_t * data;

    memsafe(data = xalloc(sizeof(*data)), sizeof(*data), "arraylist");

    data->head.fun = cleanup_arraylist;
    data->num = 0;
    data->first = NULL;
    data->last = NULL;

    list->type = T_ERROR_HANDLER;
    list->u.error_handler = &(data->head);
} /* put_arraylist() */

/*-------------------------------------------------------------------------*/
svalue_t *
enhance_arraylist (svalue_t * list)

/* Add an element to the arraylist to <list>.
 * It is the responsibility of the caller to ensure
 * that <list> is in fact an arraylist.
 */
{
    arraylist_t * data = (arraylist_t *) list->u.lvalue;
    arraylist_element_t * element;

    memsafe(element = xalloc(sizeof(*element)), sizeof(*element), "arraylist element");
    element->next = NULL;
    put_number(&(element->value), 0);

    if (data->last == NULL)
    {
        data->first = element;
        data->last = element;
    }
    else
    {
        data->last->next = element;
        data->last = element;
    }
    data->num++;

    return &(element->value);

} /* enhance_arraylist() */

/*-------------------------------------------------------------------------*/
void
finalize_arraylist (svalue_t * list)

/* Turn the arraylist in <list> into a proper array.
 */
{
    arraylist_t * data = (arraylist_t *) list->u.lvalue;
    arraylist_element_t * element;
    vector_t * result;
    svalue_t * item;

    result = allocate_array(data->num);

    element = data->first;
    item = result->item;

    while (element)
    {
        arraylist_element_t * temp = element;

        *item = element->value;

        item++;
        element = element->next;
        xfree(temp);
    }

    xfree(data);
    put_array(list, result);
} /* finalize_arraylist() */
