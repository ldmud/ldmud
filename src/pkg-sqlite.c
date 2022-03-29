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
    sqlite_dbs_t *db;
};

/* Database connections should be bound to the object which opens 
 * database file. We will store all database connections in a 
 * linked list. Also we have a busy flag to prevent reentrant
 * calls during privilege, error or warning handling.
 */
struct sqlite_dbs_s 
{
    sqlite3 * db;
    object_t * obj;
    sqlite_dbs_t * next;
    sqlite_dbs_t * prev;

    bool busy;
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
    tmp->busy = false;
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

    db->busy = true;
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
    sqlite3 *db;
    sqlite_dbs_t *tmp;
    int err;

    if (current_object.type != T_OBJECT)
        errorf("sl_open() without current object.\n");
    tmp = find_db (current_object.u.ob);
    if (tmp)
        errorf("The current object already has a database open.\n");

    err = sqlite3_open (get_txt(sp->u.str), &db);
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
    tmp->obj = current_object.u.ob;
    current_object.u.ob->open_sqlite_db = MY_TRUE;

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

    data->db->busy = false;
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
    
    if (current_object.type != T_OBJECT)
        errorf("sl_exec() without current object.\n");
    db = find_db (current_object.u.ob);
    if (!db)
        errorf("The current object doesn't have a database open.\n");
    else if (db->busy)
        errorf("Reentrant call to the same database.\n");

    db->busy = true;
    err = sqlite3_prepare(db->db, get_txt(argp->u.str), mstrsize(argp->u.str),
        &stmt, &tail);
    if(err)
    {
        const char* msg = sqlite3_errmsg(db->db);
        if(stmt)
            sqlite3_finalize(stmt);
        db->busy = false;
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
            db->busy = false;
            errorf("Bad argument %d to sl_exec(): type %s\n",
                num+1, typename(argp->type));
            break; /* NOTREACHED */

        case T_FLOAT:
            sqlite3_bind_double(stmt, num, READ_DOUBLE(argp));
            break;

        case T_NUMBER:
            if (sizeof(argp->u.number) > 4)
                sqlite3_bind_int64(stmt, num, argp->u.number);
            else
                sqlite3_bind_int(stmt, num, argp->u.number);
            break;

        case T_STRING:
            sqlite3_bind_text(stmt, num, get_txt(argp->u.str),
                mstrsize(argp->u.str), SQLITE_STATIC);
            break;

        case T_BYTES:
            sqlite3_bind_blob(stmt, num, get_txt(argp->u.str),
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
    rec_data->db = db;
    
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
                put_bytes_buf( entry
                             , sqlite3_column_blob(stmt, col)
                             , sqlite3_column_bytes(stmt, col));
                break;

            case SQLITE_INTEGER:
                if (sizeof(entry->u.number) >= 8)
                    put_number(entry, sqlite3_column_int64(stmt, col));
                else
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
    if (current_object.type != T_OBJECT)
        errorf("sl_insert_id() without current object.\n");
    sqlite_dbs_t *db = find_db(current_object.u.ob);
    int id;
   
    if (!db)
        errorf("The current object doesn't have a database open.\n");
    else if (db->busy)
        errorf("Reentrant call to the same database.\n");
 
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
    sqlite_dbs_t *db;
    if (current_object.type != T_OBJECT)
        errorf("sl_close() without current object.\n");
    db = find_db(current_object.u.ob);
    if (!db)
        errorf("The current object doesn't have a database open.\n");
    else if (db->busy)
        errorf("Reentrant call to the same database.\n");

    sl_close(current_object.u.ob);
    return sp;
} /* f_sl_close() */

/*-------------------------------------------------------------------------*/
static void *
sl_mem_malloc (int size)

/* malloc() implementation for SQLite to call our own allocator.
 */

{
    if (!has_xalloced_size())
    {
        /* We remember the size for sl_mem_size(). */
        void * ptr = pxalloc(size + sizeof(int));

        if (!ptr)
            return NULL;

        *((int*)ptr) = size;
        return ((char*)ptr) + sizeof(int);
    }

    return pxalloc(size);
} /* sl_mem_malloc() */

/*-------------------------------------------------------------------------*/
static void
sl_mem_free (void * ptr)

/* free() implementation for SQLite to call our own allocator.
 */

{
    if (!has_xalloced_size() && ptr)
        ptr = ((char*)ptr) - sizeof(int);

    pfree(ptr);
} /* sl_mem_free() */

/*-------------------------------------------------------------------------*/
static void *
sl_mem_realloc (void * ptr, int size)

/* free() implementation for SQLite to call our own allocator.
 */

{
    if (!has_xalloced_size())
    {
        if (!ptr)
            return sl_mem_malloc(size);

        ptr = ((char*)ptr) - sizeof(int);
        ptr = prexalloc(ptr, size + sizeof(int));

        if (!ptr)
            return NULL;

        *((int*)ptr) = size;
        return ((char*)ptr) + sizeof(int);
    }

    return prexalloc(ptr, size);
} /* sl_mem_realloc() */

/*-------------------------------------------------------------------------*/
static int
sl_mem_size (void * ptr)

/* malloc_usable_size() implementation for SQLite to call our own allocator.
 */

{
    if (!has_xalloced_size())
        return *(((int*)ptr) - 1);

    return (int)xalloced_usable_size(ptr);
} /* sl_mem_size() */

/*-------------------------------------------------------------------------*/
static int
sl_mem_roundup (int size)

/* Round the given size to size that malloc() would actually allocate.
 */

{
    return (int)xalloc_roundup(size);
} /* sl_mem_roundup() */

/*-------------------------------------------------------------------------*/
static int
sl_mem_init (void* data)

/* Initialize the allocator. We don't need to do that.
 */

{
    return SQLITE_OK;
} /* sl_mem_init() */

/*-------------------------------------------------------------------------*/
static void
sl_mem_shutdown (void* data)

/* Shutdown the allocator. We also don't need to do that.
 */

{
} /* sl_mem_shutdown() */

/*-------------------------------------------------------------------------*/
static void
sl_log (void* data UNUSED, int rc, const char* msg)

/* This function is called for each logging event in SQLite.
 */

{
    if (current_object.type != T_OBJECT)
        debug_message("%s SQLite: %s (rc=%d)\n", time_stamp(), msg, rc);
    else if ((rc & 0xff) == SQLITE_WARNING)
        warnf("SQLite: %s\n", msg);
    else
        debug_message("%s SQLite (%s): %s (rc=%d)\n", time_stamp(), get_txt(current_object.u.ob->name), msg, rc);
} /* sl_log() */

/*-------------------------------------------------------------------------*/
static int
sl_vfs_open (sqlite3_vfs* vfs, const char* name, sqlite3_file* file, int flags, int *out_flags)

/* This function implements the Open call for the LDMud's SQLite VFS.
 * It will just forward the call to the original VFS.
 */

{
    return ((sqlite3_vfs*)vfs->pAppData)->xOpen((sqlite3_vfs*)vfs->pAppData, name, file, flags, out_flags);
} /* sl_vfs_open() */

/*-------------------------------------------------------------------------*/
static int
sl_vfs_delete (sqlite3_vfs* vfs, const char* name, int sync_dir)

/* This function implements the Delete call for the LDMud's SQLite VFS.
 * It will just forward the call to the original VFS.
 */

{
    return ((sqlite3_vfs*)vfs->pAppData)->xDelete((sqlite3_vfs*)vfs->pAppData, name, sync_dir);
} /* sl_vfs_delete() */

/*-------------------------------------------------------------------------*/
static int
sl_vfs_access (sqlite3_vfs* vfs, const char* name, int flags, int* result)

/* This function implements the open call for the LDMud's SQLite VFS.
 * It will just forward the call to the original VFS.
 */

{
    return ((sqlite3_vfs*)vfs->pAppData)->xAccess((sqlite3_vfs*)vfs->pAppData, name, flags, result);
} /* sl_vfs_access() */

/*-------------------------------------------------------------------------*/
static int
sl_vfs_full_pathname (sqlite3_vfs* vfs, const char* name, int len, char* buf)

/* This function implements the FullPathname call for the LDMud's SQLite VFS.
 * Here we do our access check. SQLite guarantees that this will always
 * be called before sl_vfs_open().
 * Then we pass this on to the original VFS.
 */

{
    string_t *orig_file = new_unicode_mstring(name);
    string_t *new_file = check_valid_path(orig_file, current_object, STR_SQLITE_OPEN , MY_TRUE);
    char *native;
    int rc;

    free_mstring(orig_file);

    if (!new_file)
        return SQLITE_AUTH;

    native = convert_path_to_native(get_txt(new_file), mstrsize(new_file));
    if (!native || strlen(native) >= len)
    {
        free_mstring(new_file);
        return SQLITE_CANTOPEN;
    }

    rc = ((sqlite3_vfs*)vfs->pAppData)->xFullPathname((sqlite3_vfs*)vfs->pAppData, native, len, buf);
    free_mstring(new_file);

    return rc;
} /* sl_vfs_full_pathname() */

/*-------------------------------------------------------------------------*/
static int
sl_vfs_randomness (sqlite3_vfs* vfs, int len, char* buf)

/* This function implements the Randomness call for the LDMud's SQLite VFS.
 * It will just forward the call to the original VFS.
 */

{
    return ((sqlite3_vfs*)vfs->pAppData)->xRandomness((sqlite3_vfs*)vfs->pAppData, len, buf);
} /* sl_vfs_randomness() */

/*-------------------------------------------------------------------------*/
static int
sl_vfs_sleep (sqlite3_vfs* vfs, int microseconds)

/* This function implements the Sleep call for the LDMud's SQLite VFS.
 * It will just forward the call to the original VFS.
 */

{
    return ((sqlite3_vfs*)vfs->pAppData)->xSleep((sqlite3_vfs*)vfs->pAppData, microseconds);
} /* sl_vfs_sleep() */

/*-------------------------------------------------------------------------*/
static int
sl_vfs_current_time (sqlite3_vfs* vfs, double* val)

/* This function implements the xCurrentTime call for the LDMud's SQLite VFS.
 * It will just forward the call to the original VFS.
 */

{
    return ((sqlite3_vfs*)vfs->pAppData)->xCurrentTime((sqlite3_vfs*)vfs->pAppData, val);
} /* sl_vfs_current_time() */

/*-------------------------------------------------------------------------*/
static int
sl_vfs_get_last_error (sqlite3_vfs* vfs, int buflen, char* buf)

/* This function implements the GetLastError call for the LDMud's SQLite VFS.
 * It will just forward the call to the original VFS.
 */

{
    return ((sqlite3_vfs*)vfs->pAppData)->xGetLastError((sqlite3_vfs*)vfs->pAppData, buflen, buf);
} /* sl_vfs_get_last_error() */

/*-------------------------------------------------------------------------*/
static int
sl_vfs_current_time_int64 (sqlite3_vfs* vfs, sqlite3_int64* val)

/* This function implements the CurrentTimeInt64 call for the LDMud's SQLite VFS.
 * It will just forward the call to the original VFS.
 */

{
    return ((sqlite3_vfs*)vfs->pAppData)->xCurrentTimeInt64((sqlite3_vfs*)vfs->pAppData, val);
} /* sl_vfs_current_time_int64() */

/*-------------------------------------------------------------------------*/
void
pkg_sqlite_init ()

/* Initialize the SQLite library.
 *
 * We configure SQLite to use LDMud's memory allocator.
 * And we add a wrapper around the VFS to check the validity
 * of file paths.
 */

{
    static const sqlite3_mem_methods mem_methods = {
        sl_mem_malloc,
        sl_mem_free,
        sl_mem_realloc,
        sl_mem_size,
        sl_mem_roundup,
        sl_mem_init,
        sl_mem_shutdown,
        NULL
    };

    static sqlite3_vfs vfs_methods = {
        3,
        0,
        0,
        NULL,
        "ldmud",
        NULL,
        sl_vfs_open,
        sl_vfs_delete,
        sl_vfs_access,
        sl_vfs_full_pathname,

        NULL,
        NULL,
        NULL,
        NULL,

        sl_vfs_randomness,
        sl_vfs_sleep,
        sl_vfs_current_time,
        sl_vfs_get_last_error,
        sl_vfs_current_time_int64,

        NULL,
        NULL,
        NULL
    };

    sqlite3_vfs *orig_vfs;

    sqlite3_config(SQLITE_CONFIG_MALLOC, &mem_methods);
    sqlite3_config(SQLITE_CONFIG_LOG, &sl_log, NULL);

    /* Disable URI handling, so no other VFS can be selected. */
    sqlite3_config(SQLITE_CONFIG_URI, 0);

    orig_vfs = sqlite3_vfs_find(NULL);
    if (orig_vfs == NULL)
        fatal("SQLite: No VFS registered.\n");

    vfs_methods.szOsFile = orig_vfs->szOsFile;
    vfs_methods.mxPathname = orig_vfs->mxPathname;
    vfs_methods.pAppData = orig_vfs;

    sqlite3_vfs_register(&vfs_methods, 1);
} /* sl_init() */

/*-------------------------------------------------------------------------*/

#endif /* USE_SQLITE */

/*************************************************************************/
