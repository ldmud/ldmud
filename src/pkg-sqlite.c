/*---------------------------------------------------------------------------
 * SQLite3 Database package.
 *
 * Based on code written and donated 2005 by Bastian Hoyer and Gnomi.
 *---------------------------------------------------------------------------
 */

#include "driver.h"

#ifdef USE_SQLITE
  
#include <errno.h>
#include <sqlite3.h>
#include <stddef.h>
#include <stdio.h>
#include <time.h>
#include <unistd.h>

#include "typedefs.h"
  
#include "my-alloca.h"
#include "array.h"
#include "interpret.h"
#include "mstrings.h"
#include "simulate.h"
#include "svalue.h"
#include "object.h"
#include "stdstrings.h"
#include "xalloc.h"

/*-------------------------------------------------------------------------*/
/* Types */

typedef struct sqlite_rows_s sqlite_rows_t;
typedef struct sqlite_dbs_s sqlite_dbs_t;

/* Since we don't know the number of rows while we retrieve the
 * rows from a query we save the data in a single-linked list first
 * and move them into an array after the retrieval has finished 
 */ 
struct sqlite_rows_s 
{
    vector_t * row;
    sqlite_rows_t * last;
};

/* This structure is used for error handling. In case of an error
 * our handler gets called with a pointer to this structure.
 */
struct sl_exec_cleanup_s
{
    error_handler_t head; /* push_error_handler saves the link to our
                             handler here. */

    sqlite3_stmt *stmt;
    sqlite_rows_t *rows;
};

/* Database connections should be bound to the object which opens 
 * database file. We will store all database connections in a 
 * linked list. 
 */ 
struct sqlite_dbs_s 
{
    sqlite3 * db;
    object_t * obj;
    sqlite_dbs_t * next;
    sqlite_dbs_t * prev;
};

/*-------------------------------------------------------------------------*/
/* Variables */

/* The list of database connections.
 */ 
static sqlite_dbs_t *head = NULL;

/*-------------------------------------------------------------------------*/
static sqlite_dbs_t *
find_db (object_t * obj) 

/* For object <obj>, find the database entry in the global list, and
 * return it.
 * Return NULL when not found.
 */

{
    sqlite_dbs_t *tmp = head;

    while (tmp)
    {
        if (tmp->obj==obj) return tmp;
        tmp=tmp->prev;
    }
    return NULL;
} /* find_db() */

/*-------------------------------------------------------------------------*/
static sqlite_dbs_t *
new_db()

/* Create a new database entry, link it into the global list, and return it.
 * On out of memory, return NULL.
 */

{
    sqlite_dbs_t *tmp;
    tmp = pxalloc (sizeof (*tmp));
    if (!tmp)
        return NULL;
   
    tmp->db = NULL;
    tmp->obj = NULL;
    tmp->next = NULL;
    tmp->prev = head;
    if (head)
        head->next=tmp;
    head=tmp;
   
    return tmp;
} /* new_db() */

/*-------------------------------------------------------------------------*/
static void
remove_db(sqlite_dbs_t *db)

/* Remove the database entry <db> from the global list.
 */

{
    if (db == head)
    {
       if (head->prev) 
       {
           head->prev->next = NULL;
           head = head->prev;
       }
        else
        {
           head = NULL;
       }
    }
    else
    {
       if (db->next) db->next->prev = db->prev;
       if (db->prev) db->prev->next = db->next;
    }
    pfree(db);
} /* remove_db() */

/*-------------------------------------------------------------------------*/
static int
my_sqlite3_authorizer (void * data, int what, const char* arg1, const char* arg2,
        const char* dbname, const char* view)

/* Callback function for SQLite to handle authorizations.
 */

{
    struct error_recovery_info error_recovery_info;
    svalue_t *save_sp, sarg1, sarg2;
    struct control_stack *save_csp;
    int val;
    
    switch(what)
    {
        case SQLITE_PRAGMA:
            /* PRAGMA name [ = value ]
             * PRAGMA function(arg)
             *
             *   arg1: name/function
             *   arg2: value/arg
             *   dbname/view: NULL
             */
            
            error_recovery_info.rt.last = rt_context;
            error_recovery_info.rt.type = ERROR_RECOVERY_APPLY;
            rt_context = (rt_context_t *)&error_recovery_info;

            save_sp = inter_sp;
            save_csp = csp;
            sarg1.type = T_INVALID;
            sarg2.type = T_INVALID;

            if (setjmp(error_recovery_info.con.text))
            {
                secure_apply_error(save_sp, save_csp, MY_FALSE);
                val = SQLITE_DENY;
            }
            else
            {
                if(arg1)
                    put_c_string(&sarg1, arg1);
                else
                    put_number(&sarg1, 0);
                
                if(arg2)
                    put_c_string(&sarg2, arg2);
                else
                    put_number(&sarg2, 0);

                if(privilege_violation2(STR_SQLITE_PRAGMA, &sarg1, &sarg2, inter_sp))
                    val = SQLITE_OK;
                else
                    val = SQLITE_DENY;
            }

            free_svalue(&sarg1);
            sarg1.type = T_INVALID;
            free_svalue(&sarg2);
            sarg2.type = T_INVALID;

            rt_context = error_recovery_info.rt.last;

            return val;

        case SQLITE_ATTACH:
            /* ATTACH "filename" AS "dbname"
             *
             *   arg1: filename
             *   arg2, dbname, view: NULL
             */

            /* SQLite3 doesn't allow the filename to be changed,
             * but at least we must convert an absolute pathname
             * to a relative one. So we have to deactivate it...
             */
            return SQLITE_DENY;

        default:
            return SQLITE_OK;
    }
} /* my_sqlite3_authorizer() */

/*-------------------------------------------------------------------------*/
Bool
sl_close (object_t *ob)

/* For object <ob>, find and close the database connection.
 * Return TRUE on success, FALSE if there wasn't one.
 */

{
    sqlite_dbs_t *db = find_db(ob);

    ob->open_sqlite_db = MY_FALSE;

    if (!db)
        return MY_FALSE;
   
    sqlite3_close(db->db);
    remove_db(db);
    return MY_TRUE;
} /* sl_close() */

/*-------------------------------------------------------------------------*/
svalue_t * 
f_sl_open (svalue_t *sp) 

/* EFUN sl_open
 *
 *   int sl_open(string filename)
 *
 * Opens the file <filename> for use as a SQLite database.
 * If the file doesn't exists it will be created.
 * Only one open file per object is allowed. On success this
 * function returns 1, otherwise usually an error is thrown.
 */

{
    string_t *file;
    sqlite3 *db;
    sqlite_dbs_t *tmp;
    int err;
   
    file = check_valid_path(sp->u.str, current_object, STR_SQLITE_OPEN , MY_TRUE);
    if (!file)
        errorf("Illegal use of sl_open('%s')\n", get_txt(sp->u.str));
   
    tmp = find_db (current_object);
    if (tmp)
    {
        free_mstring(file);
        errorf("The current object already has a database open.\n");
    }

    err = sqlite3_open (get_txt(file), &db);
    free_mstring(file);
    if (err)
    {
        const char* msg = sqlite3_errmsg(db);
        sqlite3_close(db);
        errorf("sl_open: %s\n", msg );
        /* NOTREACHED */
    }

    /* create a new chain link and hang on the old chain */
    tmp = new_db(); 
    if(!tmp)
    {
        sqlite3_close(db);
        errorf("(sl_open) Out of memory: (%lu bytes)\n",
                (unsigned long) sizeof(*tmp));
    }
   
    tmp->db = db;
    tmp->obj = current_object;
    current_object->open_sqlite_db = MY_TRUE;

    /* Synchronous is damn slow. Forget it. */
    sqlite3_exec(db, "PRAGMA synchronous = OFF", NULL, NULL, NULL);
    sqlite3_set_authorizer(db, my_sqlite3_authorizer, NULL);
  
    free_string_svalue (sp);
    put_number (sp, 1);
    return sp;
} /* f_sl_open() */

/*-------------------------------------------------------------------------*/
static void
sl_exec_cleanup (error_handler_t * arg)
{
    sqlite_rows_t *row;
    struct sl_exec_cleanup_s * data;
    
    data = (struct sl_exec_cleanup_s *)arg;
    
    if(data->stmt)
        sqlite3_finalize(data->stmt);

    row = data->rows;
    while(row)
    {
        sqlite_rows_t *temp;

        if(row->row)
            free_array(row->row);
        temp = row;
        row = row->last;
        pfree(temp);
    }

    xfree(data);
} /* sl_exec_cleanup() */

/*-------------------------------------------------------------------------*/
svalue_t * 
v_sl_exec (svalue_t * sp, int num_arg) 

/* EFUN sl_exec()
 *
 *   mixed* sl_exec(string statement, ...)
 *
 * Executes the SQL statement <statement> for the current
 * SQLite database. The SQL statement may contain wildcards like
 * '?' and '?nnn', where 'nnn' is an integer. These wildcards
 * can be given as further parameters to sl_exec. With '?nnn'
 * the number of a specific parameter can be given, the first
 * parameter has number 1.
 * 
 * If the statement returns data, sl_exec returns an array
 * with each row (which is itself an array of columns) as 
 * an element.
 */

{
    svalue_t *argp;
    sqlite_dbs_t *db;
    sqlite3_stmt *stmt;
    const char* tail;
    int err, rows, cols, num;
    struct sl_exec_cleanup_s * rec_data;
    vector_t * result;

    argp = sp - num_arg + 1; /* First argument: the SQL query */
    
    db = find_db (current_object);
    if (!db)
        errorf("The current object doesn't have a database open.\n");
    
    err = sqlite3_prepare(db->db, get_txt(argp->u.str), mstrsize(argp->u.str),
        &stmt, &tail);
    if(err)
    {
        const char* msg = sqlite3_errmsg(db->db);
        if(stmt)
            sqlite3_finalize(stmt);
        errorf("sl_exec: %s\n", msg);
        /* NOTREACHED */
    }
    
    /* Now bind all parameters. */
    for(argp++, num=1; argp <= sp; argp++, num++)
    {
        switch(argp->type)
        {
        default:
            sqlite3_finalize(stmt);
            errorf("Bad argument %d to sl_exec(): type %s\n",
                num+1, typename(argp->type));
            break; /* NOTREACHED */

        case T_FLOAT:
            sqlite3_bind_double(stmt, num, READ_DOUBLE(argp));
            break;

        case T_NUMBER:
            sqlite3_bind_int(stmt, num, argp->u.number);
            break;
    
        case T_STRING:
            sqlite3_bind_text(stmt, num, get_txt(argp->u.str),
                mstrsize(argp->u.str), SQLITE_STATIC);
            break;
        }
    }
    
    rows = 0;
    cols = sqlite3_column_count(stmt);

    rec_data = xalloc(sizeof(*rec_data));
    if(!rec_data)
    {
        sqlite3_finalize(stmt);
        errorf("(sl_exec) Out of memory: (%lu bytes) for cleanup structure\n",
            (unsigned long) sizeof(*rec_data));
    }
    rec_data->rows = NULL;
    rec_data->stmt = stmt;
    
    sp = push_error_handler(sl_exec_cleanup, &(rec_data->head));
    
    while((err = sqlite3_step(stmt)) == SQLITE_ROW)
    {
        int col;
        sqlite_rows_t *this_row;

        rows++;
        this_row = pxalloc(sizeof(*this_row));
        if(!this_row)
            errorf("(sl_exec) Out of memory: (%lu bytes)\n",
                (unsigned long) sizeof(*this_row));

        this_row->last = rec_data->rows;
        rec_data->rows = this_row;
        this_row->row = NULL; /* Because allocate_array may throw an error. */

        this_row->row = allocate_array(cols);
        if(!this_row->row)
            errorf("(sl_exec) Out of memory: row vector\n");
    
        for(col = 0; col < cols; col++)
        {
            svalue_t * entry;
            STORE_DOUBLE_USED;

            entry = this_row->row->item + col;

            switch(sqlite3_column_type(stmt, col))
            {
            default:
                errorf( "sl_exec: Unknown type %d.\n"
                      , sqlite3_column_type(stmt, col));
                break;

            case SQLITE_BLOB:
                errorf("sl_exec: Blob columns are not supported.\n");
                break;

            case SQLITE_INTEGER:
                put_number(entry, sqlite3_column_int(stmt, col));
                break;

           case SQLITE_FLOAT:
                entry->type = T_FLOAT;
                STORE_DOUBLE(entry, sqlite3_column_double(stmt, col));
                break;

            case SQLITE_TEXT:
                put_c_n_string( entry
                              , (char *)sqlite3_column_text(stmt, col)
                              , sqlite3_column_bytes(stmt, col));
                break;

            case SQLITE_NULL:
                /* All elements from this_row->row are initialized to 0. */
                break;
            }
        }
    }

    sqlite3_finalize(stmt);
    rec_data->stmt = NULL;
    
    switch(err)
    {
    default:
        errorf("sl_exec: Unknown return code from sqlite3_step: %d.\n", err);
        break;

    case SQLITE_BUSY:
        errorf("sl_exec: Database is locked.\n");
        break;

    case SQLITE_ERROR:
        errorf("sl_exec: %s\n", sqlite3_errmsg(db->db));
        break;

    case SQLITE_MISUSE:
        errorf("sl_exec: sqlite3_step was called inappropriately.\n");
        break;

    case SQLITE_DONE:
        break;
    }

    if(rows)
    {
        sqlite_rows_t *this_row;

        result = allocate_array(rows);
        if(!result)
            errorf("(sl_exec) Out of memory: result vector\n");

        this_row = rec_data->rows;
        while(rows--)
        {
            put_array(result->item + rows, this_row->row);
            this_row->row = NULL;
            this_row = this_row->last;
        }
    }
    else
        result = NULL;

    // Pop arguments and our error handler.
    // Our error handler gets called and cleans the row stuff.
    sp = pop_n_elems(num_arg + 1, sp) + 1; 
 
    if(rows)
        put_array(sp,result);
    else
        put_number(sp, 0);

    return sp;
} /* v_sl_exec() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_sl_insert_id (svalue_t * sp)

/* EFUN sl_insert_id()
 *
 *   int sl_insert_id()
 *
 * After inserting a line into a table with an AUTO_INCREMENT field,
 * this efun can be used to return the (new) value of the AUTO_INCREMENT
 * field.
 */

{
    sqlite_dbs_t *db = find_db(current_object);
    int id;
   
    if (!db)
        errorf("The current object doesn't have a database open.\n");
 
    id=sqlite3_last_insert_rowid(db->db);
    sp++;
    put_number(sp,id);
    return sp;
} /* f_sl_insert_id() */

/*-------------------------------------------------------------------------*/
svalue_t * 
f_sl_close (svalue_t * sp) 

/* EFUN sl_close()
 *
 *   void sl_close()
 *
 * Closes the SQLite database that is associated with the
 *  current object.
 */

{
    if (!sl_close(current_object)) 
        errorf("The current object doesn't have a database open.\n");
    return sp;
} /* f_sl_close() */

/*-------------------------------------------------------------------------*/

#endif /* USE_SQLITE */

/*************************************************************************/
