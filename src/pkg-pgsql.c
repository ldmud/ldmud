/*---------------------------------------------------------------------------
 * PostgreSQL Database package.
 *
 * Based on code written and donated 2001 by Florian Heinz.
 *---------------------------------------------------------------------------
 * The interface to the PostgreSQL database is implemented through the
 * concept of a controlling object: when opening a database connection,
 * the LPC code has to provide a callback function. The object this function
 * is bound to is the controlling object: all queries to the database
 * will be issued by this object, and the responses will be sent to the
 * callback function.
 *
 * The interface is also asynchronous: the pg_query() efun just queues
 * the query with the database connection, and returns immediately. When
 * the database has finished working the query, the callback function is
 * called with the results.
 *
 * The callback function can be defined by name or by closure, and can
 * be defined with extra parameters:
 *
 *   void <callback>(int type, mixed ret, int id [, mixed extra...])
 *
 *     <type> is the type of the call, <id> identifies the query for which
 *     this call is executed:
 *
 *       PGRES_TUPLES_OK: <ret> is the result from a query.
 *                        It is either a mapping (field name as key, indexing
 *                        <n> values for n returned tuples), or an array
 *                        of arrays (one per row).
 *
 *       PGRES_COMMAND_OK: <ret> is a string which contains the
 *                         server response (e.g. on INSERT or DELETE) 
 *
 *       PGRES_BAD_RESPONSE,
 *       PGRES_NONFATAL_ERROR,
 *       PGRES_FATAL_ERROR: ret is the error-string
 *
 *
 *   void <callback>(int type, mixed ret [, mixed extra...])
 *
 *     <type> is the type of the call, which is not related a specific query:
 *
 *       PGCONN_SUCCESS: The database-connection was established, <ret> is
 *                       a dummy string.
 *       PGCONN_FAILED:  The database-connection failed, <ret> is the error
 *                       message.
 *         The first message to the callback after a call to pg_connect()
 *         is always one of these two.
 *
 *       PGRES_NOTICE: <ret> is a informational text.
 *
 *       PGCONN_ABORTED: If the connection to the backend fails
 *                       we try to re-establish (reset) it. If the
 *                       reset fails, the connection is closed and
 *                       this value is returned. Consider the
 *                       connection gone and don't try to close or 
 *                       otherwise operate further on it.
 *                       <ret> is a dummy string.
 *---------------------------------------------------------------------------
 */

#include "driver.h"

#ifdef USE_PGSQL

#include "typedefs.h"

#include "my-alloca.h"
#include <errno.h>
#include <stddef.h>
#include <time.h>
#include <unistd.h>

#ifdef HAVE_POLL

#include <sys/poll.h>

#else

#    if defined(_AIX)
#        include <sys/select.h>
#    endif

#endif /* HAVE_POLL */

#include <libpq-fe.h>

#include "pkg-pgsql.h"

#include "actions.h"
#include "array.h"
#include "gcollect.h"
#include "instrs.h"
#include "interpret.h"
#include "main.h"
#include "mapping.h"
#include "mstrings.h"
#include "simulate.h"
#include "stdstrings.h"
#include "xalloc.h"

#include "../mudlib/sys/pgsql.h"

/*-------------------------------------------------------------------------*/

#define MAX_RESETS 5   /* Number of reset tries, at max. one per second */

/*-------------------------------------------------------------------------*/
/* Types */

/* --- struct query_queue_s: one entry in the query queue
 * The queue is organized as single-linked list, one for each database
 * connection.
 */
struct query_queue_s
{
   long                   id;
   char                 * str;
   int                    flags;
   struct query_queue_s * next;
};

/* ---  struct dbconn_s: one active database connection.
 * The connections are held in a singly linked list.
 * There can be only one connection per object.
 */

struct dbconn_s
{
   callback_t                  callback;  /* The callback */
   
   PGconn                    * conn;
   PostgresPollingStatusType   pgstate;
   
   int                         state;
   int                         fd;
   int                         resets;

   time_t                      lastreset;
   time_t                      lastreply;

   struct query_queue_s      * queue;
   
   struct dbconn_s           * next;
};


/* Possible struct dbconn_s.states */

#define PG_UNCONNECTED 0
#define PG_CONNECTING  1
#define PG_RESETTING   2
#define PG_RESET_NEXT  3
#define PG_IDLE        4
#define PG_SENDQUERY   5
#define PG_WAITREPLY   6
#define PG_REPLYREADY  7


typedef struct dbconn_s      dbconn_t;
typedef struct query_queue_s query_queue_t;

/*-------------------------------------------------------------------------*/

static dbconn_t *head = NULL;
  /* The list of database connections.
   */

static long query_id = 1;
  /* The query ID counter, used to generate unique IDs.
   */

/*-------------------------------------------------------------------------*/
/* Forward Declarations */

static void pgnotice (dbconn_t *pgconn, const char *msg);

/*-------------------------------------------------------------------------*/
void
pg_setfds (fd_set *readfds, fd_set *writefds, int *nfds)

/* Called from the get_message() loop in comm.c, this function has to add
 * the fds of the database connections to the fd sets.
 */

{
    dbconn_t *ptr;
    
    for (ptr = head; ptr != NULL; ptr = ptr->next)
    {
        if (ptr->fd < 0)
            continue;
        if ((ptr->pgstate == PGRES_POLLING_WRITING)
         || (ptr->state == PG_SENDQUERY)
           )
            FD_SET(ptr->fd, writefds);
        FD_SET(ptr->fd, readfds);
        if (*nfds <= ptr->fd)
            *nfds = ptr->fd + 1;
    }
} /* pg_setfds() */

/*-------------------------------------------------------------------------*/
static dbconn_t *
find_current_connection (object_t * obj)

/* Find the <dbconn> which has a callback in the object <obj>.
 */

{
   dbconn_t *ptr = head;
   
   while (ptr && callback_object(&ptr->callback) != obj)
        ptr = ptr->next;
   
   return ptr;
} /* find_current_connection() */

/*=========================================================================*/

/*                             Queue management                            */

/*-------------------------------------------------------------------------*/
static query_queue_t *
queue (dbconn_t *db, const char *query)

/* Create a new query_queue entry for <query>, link it into the list
 * for database connection <db>, and return it.
 * The query string is duplicated.
 *
 * Throw an error when out of memory.
 */

{
    query_queue_t *tmp;
    
    tmp = db->queue;
    if (!tmp)
        tmp = db->queue = xalloc(sizeof(*tmp));
    else
    {
        while (tmp->next)
            tmp = tmp->next;
        tmp->next = xalloc(sizeof(*tmp));
        tmp = tmp->next;
    }

    if (!tmp)
    {
        outofmemory("new Postgres query");
        /* NOTREACHED */
        return NULL;
    }

    memset(tmp, 0, sizeof(*tmp));
    tmp->id  = query_id++;
    tmp->str = string_copy(query);
    tmp->flags = 0;
    
    return tmp;
} /* queue() */

/*-------------------------------------------------------------------------*/
static void
dequeue (dbconn_t *db)

/* Unqueue the first query from connection <db> and deallocate it.
 */

{
    query_queue_t *tmp;
  
    tmp = db->queue;
    db->queue = tmp->next;
    if (tmp->str)
        xfree(tmp->str);
    xfree(tmp);
} /* dequeue() */

/*-------------------------------------------------------------------------*/
static dbconn_t *
alloc_dbconn (void)

/* Allocate a new database connection structure, link it into the global
 * list and return it.
 *
 * Throw an error when out of memory.
 */

{
    dbconn_t *ret;
    
    memsafe(ret = xalloc(sizeof(*ret)), sizeof(*ret), "new DB connection");
    
    memset(ret, 0, sizeof(*ret));
    ret->next = head;
    ret->fd = -1;
    ret->state = PG_UNCONNECTED;
    head = ret;
    
    return ret;
} /* alloc_dbconn() */

/*-------------------------------------------------------------------------*/
static void
dealloc_dbconn (dbconn_t *del)

/* Unlink the database connection <del> from the list and deallocate it
 * and all resources held by it.
 */

{
    dbconn_t *ptr;
    
    if (!del)
        return;

    free_callback(&del->callback);

    if (del == head)
        head = head->next;
    else
    {
        ptr = head;
        while (ptr->next && (ptr->next != del))
            ptr = ptr->next;
        if (ptr->next)
            ptr->next = ptr->next->next;
    }
    while (del->queue)
      dequeue(del);
    xfree(del);
} /* dealloc_dbconn() */

/*=========================================================================*/

/*                          Connection management                          */

/*-------------------------------------------------------------------------*/
static int
pgconnect (dbconn_t *db, char *connstr)

/* Connect <db> to a database, using <connstr> for the connection parameters.
 * Return 0 on success, and -1 on failure.
 */

{
    db->conn = PQconnectStart(connstr);
    if (!db->conn)
        return -1;

    if (PQstatus(db->conn) == CONNECTION_BAD)
        return -1;
    
    PQsetNoticeProcessor(db->conn, (void*) pgnotice, db);
    db->fd = PQsocket(db->conn);
    db->state = PG_CONNECTING;
    db->pgstate = PGRES_POLLING_WRITING;
    return 0;
} /* pgconnect() */

/*-------------------------------------------------------------------------*/
static void
pgclose (dbconn_t *pgconn)

/* Close the database connection of <pgconn>.
 */

{
    pgconn->state = PG_UNCONNECTED;
    if (pgconn->conn)
        PQfinish(pgconn->conn);
    pgconn->conn = NULL;
    pgconn->fd = -1;
} /* pgclose() */

/*-------------------------------------------------------------------------*/
static void
pgreset (dbconn_t *pgconn)

/* Reset the connection to <pgconn>.
 */

{
    if (!PQresetStart(pgconn->conn))
    {
        pgclose(pgconn);
        
        if (callback_object(&pgconn->callback))
        {
            push_number(inter_sp, PGCONN_ABORTED);
            push_ref_string(inter_sp, STR_PG_RESET_FAILED);
            (void)apply_callback(&pgconn->callback, 2);
        }
        return;
    }
    
    pgconn->state = PG_RESETTING;
    pgconn->pgstate = PGRES_POLLING_WRITING;
} /* pgreset() */


/*=========================================================================*/

/*                     Database Result Handling                            */

/*-------------------------------------------------------------------------*/
static void
pgnotice (dbconn_t *pgconn, const char *msg)

/* Database connection <pgconn> wishes to send <msg> to the controlling
 * object.
 */

{
    current_object = callback_object(&pgconn->callback);
    command_giver = 0;
    current_interactive = 0;
    
    if (current_object != NULL)
    {
        push_number(inter_sp, PGRES_NOTICE);
        push_c_string(inter_sp, msg);
        (void)apply_callback(&pgconn->callback, 2);
    }
    else
    {
        debug_message("%s PG connection object destructed.\n", time_stamp());
        pgreset(pgconn);
    }
} /* pgnotice() */

/*-------------------------------------------------------------------------*/
static void
pgresult (dbconn_t *pgconn, PGresult *res)

/* The most recent query on <pgconn> returned the result <res>. Encode it into
 * a nice LPC data package and send it to the controlling object.
 * The query is removed from <pgconn>.
 */

{
    int type;
    
    current_object = callback_object(&pgconn->callback);
    command_giver = 0;
    current_interactive = 0;
    
    type = PQresultStatus(res);
    
    push_number(inter_sp, type);
    
    switch (type)
    {
    case PGRES_TUPLES_OK:
      {
        int nfields, ntuples, i, j;

        nfields = PQnfields(res);
        ntuples = PQntuples(res);
        
        if (pgconn->queue->flags & PG_RESULT_MAP)
        {
            /* Return the result as mapping */

            mapping_t *map;

            if (max_mapping_size
             && (nfields * (ntuples + 1)) > (p_int)max_mapping_size)
            {
                PQclear(res);
                dequeue(pgconn);
                errorf("Query result exceeded mappingsize limit.\n");
            }
            
            if (max_mapping_keys && nfields > (p_int)max_mapping_keys)
            {
                PQclear(res);
                dequeue(pgconn);
                errorf("Query result exceeded mappingsize limit.\n");
            }
            
            map = allocate_mapping(nfields, ntuples);
            if (!map)
            {
                push_number(inter_sp, 0);
                break;
            }
            
            for (i = 0; i < nfields; i++)
            {
                svalue_t * entry, fname;

                put_c_string(&fname, PQfname(res, i));
                entry = get_map_lvalue(map, &fname);
                free_svalue(&fname);
                if (!entry)
                    break;
                for (j = 0; j < ntuples; j++)
                    put_c_string(&entry[j], PQgetvalue(res, j, i));
            }
            
            push_mapping(inter_sp, map);
        }
        else
        {
            /* Return the result as array of arrays */

            vector_t * array;
            svalue_t * entry;

            if (max_array_size
             && (   (ntuples >= (p_int)max_array_size)
                 || (nfields >= (p_int)max_array_size))
               )
            {
                PQclear(res);
                dequeue(pgconn);
                errorf("Query result exceeded array limit.\n");
            }

            array = allocate_array(ntuples+1);
            if (!array)
            {
                push_number(inter_sp, 0);
                break;
            }
            
            entry = &array->item[0];
            put_array(entry, allocate_array(nfields));
            for (j = 0; j < nfields; j++)
                put_c_string(&entry->u.vec->item[j], PQfname(res, j));
            
            for (i = 0; i < ntuples; i++)
            {
                entry = &array->item[i+1];
                put_array(entry, allocate_array(nfields));
                for (j = 0; j < nfields; j++)
                    put_c_string(&entry->u.vec->item[j], PQgetvalue(res, i, j));
            }
            push_array(inter_sp, array);
        }
        break;
      }
      
    case PGRES_COMMAND_OK:
        push_c_string(inter_sp, PQcmdStatus(res));
        break;
       
     case PGRES_BAD_RESPONSE:
        push_c_string(inter_sp, PQerrorMessage(pgconn->conn));
        break;
       
     case PGRES_FATAL_ERROR:
     case PGRES_NONFATAL_ERROR:
        push_c_string(inter_sp, PQerrorMessage(pgconn->conn));
        break;
       
     default:
        PQclear(res);
        return;
    }
    
    if (callback_object(&pgconn->callback))
    {
        push_number(inter_sp, pgconn->queue->id);
        (void)apply_callback(&pgconn->callback, 3);
    }
    else
    {
        debug_message("%s PG connection object destructed.\n", time_stamp());
        pgreset(pgconn);
    }

    dequeue(pgconn);
    PQclear(res);
} /* pgresult() */


/*=========================================================================*/

/*                   Database Connection Handling                          */

/*-------------------------------------------------------------------------*/
static void
pg_process_connect_reset (dbconn_t *pgconn)

/* Reset the connection <pgconn>.
 */

{
    int reset;
    PostgresPollingStatusType (*pqpollhandler)(PGconn *);
    
    reset = (pgconn->state == PG_RESETTING);
    
    if (reset)
        debug_message("%s PGSQL Connection resetting.\n", time_stamp());
    
    pqpollhandler = reset ? PQresetPoll : PQconnectPoll;
    
    if (pgconn->pgstate != PGRES_POLLING_ACTIVE)
    {
        int rc;

#ifdef HAVE_POLL
        struct pollfd ufd;

        ufd.fd = pgconn->fd;
#else
        fd_set readfds, writefds;

        FD_ZERO(&readfds);
        FD_ZERO(&writefds);
#endif /* HAVE_POLL */

        switch (pgconn->pgstate)
        {
        case PGRES_POLLING_READING:
#ifdef HAVE_POLL
           ufd.events = POLLIN;
#else
           FD_SET(pgconn->fd, &readfds);
#endif /* HAVE_POLL */
           break;

        case PGRES_POLLING_WRITING:
#ifdef HAVE_POLL
           ufd.events = POLLOUT;
#else
           FD_SET(pgconn->fd, &writefds);
#endif /* HAVE_POLL */
           break;

        default:
           /* Shouldn't happen */
           break;
        }

#ifdef HAVE_POLL
        do {
            rc = poll(&ufd, 1, 0);
        } while (rc < 0 && errno == EINTR);

        if (rc > 0)
        {
           pgconn->pgstate = PGRES_POLLING_ACTIVE;
           if (ufd.revents & POLLIN)
               pgconn->lastreply = time(NULL);
        }
#else
        do {
            struct timeval timeout;

            timeout.tv_sec = 0;
            timeout.tv_usec = 0;
            rc = select(pgconn->fd+1, &readfds, &writefds, NULL, &timeout);
            if (rc >= 0 || errno != EINTR)
                break;
        } while (rc < 0 && errno == EINTR);

        if (rc > 0)
        {
           pgconn->pgstate = PGRES_POLLING_ACTIVE;
           if (FD_ISSET(pgconn->fd, &readfds))
               pgconn->lastreply = time(NULL);
        }
#endif /* HAVE_POLL */
    }
    
    if (pgconn->pgstate == PGRES_POLLING_ACTIVE)
        pgconn->pgstate = pqpollhandler(pgconn->conn);
    
    if (pgconn->pgstate == PGRES_POLLING_FAILED)
    {
        if (reset && (pgconn->resets < MAX_RESETS))
        {
            pgconn->resets++;
            pgconn->state = PG_RESET_NEXT;
            pgconn->lastreset = time(NULL);
        }
        else
        {
            if (callback_object(&pgconn->callback))
            {
                push_number(inter_sp, reset ? PGCONN_ABORTED : PGCONN_FAILED);
                push_c_string(inter_sp, PQerrorMessage(pgconn->conn));
                (void)apply_callback(&pgconn->callback, 2);
            }
            else
            {
                debug_message("%s PG connection object destructed.\n", time_stamp());
            }
            pgclose(pgconn);
        }
    }
    else if (pgconn->pgstate == PGRES_POLLING_OK)
    {
        if (!reset)
        {
           /* The program should not notice a successful reset */
            if (callback_object(&pgconn->callback))
            {
                push_number(inter_sp, PGCONN_SUCCESS);
                push_ref_string(inter_sp, STR_SUCCESS);
                (void)apply_callback(&pgconn->callback, 2);
            }
            else
            {
                debug_message("%s PG connection object destructed.\n", time_stamp());
                pgreset(pgconn);
            }
        }
        pgconn->resets = 0;
        if (pgconn->queue)
            pgconn->state = PG_SENDQUERY;
        else
            pgconn->state = PG_IDLE;
    }
} /* pg_process_connect_reset() */

/*-------------------------------------------------------------------------*/
static void
pg_process_query (dbconn_t *pgconn)

/* Query the connection <pgconn> for data and act on it.
 */

{
    int rc;

#ifdef HAVE_POLL
    struct pollfd ufd;
    
    ufd.fd = pgconn->fd;
    ufd.events = POLLIN;
#else
    struct fd_set readfds;

    FD_ZERO(&readfds);
    FD_SET(pgconn->fd, &readfds);

#endif /* HAVE_POLL */
    
    PQflush(pgconn->conn);

#ifdef HAVE_POLL

    do {
        rc = poll(&ufd, 1, 0);
    } while (rc < 0 && errno == EINTR);

#else

    do {
        struct timeval timeout;

        timeout.tv_sec = 0;
        timeout.tv_usec = 0;
        rc = select(pgconn->fd+1, &readfds, NULL, NULL, &timeout);
    } while (rc < 0 && errno == EINTR);

#endif /* HAVE_POLL */

    if (rc > 0)
    {
        pgconn->lastreply = time(NULL);
        PQconsumeInput(pgconn->conn);
        if (!PQisBusy(pgconn->conn))
            pgconn->state = PG_REPLYREADY;
    }
} /* pg_process_query() */

/*-------------------------------------------------------------------------*/
static void
pg_process_one (dbconn_t *pgconn)

/* Check the state of <pgconn> and take appropriate action.
 */

{
    PGresult *res;
    
    switch (pgconn->state)
    {
    case PG_CONNECTING:
    case PG_RESETTING:
        pg_process_connect_reset(pgconn);
        break;

    case PG_SENDQUERY:
    case PG_IDLE:
        if (pgconn->queue)
        {
            pgconn->lastreply = time(NULL);
            PQsendQuery(pgconn->conn, pgconn->queue->str);
            pgconn->state = PG_WAITREPLY;
        }
        break;

    case PG_WAITREPLY:
        pg_process_query(pgconn);
        break;

    case PG_UNCONNECTED:
        dealloc_dbconn(pgconn);
        break;

    case PG_RESET_NEXT:
        if (pgconn->lastreset != time(NULL))
            pgreset(pgconn);
        break;
    } /* switch() */
    
    /* Validate the connection */
    if ((PQstatus(pgconn->conn) != CONNECTION_OK)
     && (pgconn->state >= PG_IDLE)
       )
        pgreset(pgconn);
    
    /* If there is a result waiting, get it and forward
     * it to the controlling object.
     */
    if (pgconn->state == PG_REPLYREADY)
    {
        do
        {
            res = PQgetResult(pgconn->conn);
            if (!res)
            {
                pgconn->state = PG_IDLE;
                pg_process_one(pgconn);
                break;
            } else
                pgresult(pgconn, res);
        }
        while (!PQisBusy(pgconn->conn));
    }
} /* pg_process_one() */

/*-------------------------------------------------------------------------*/
void
pg_process_all (void)

/* Called from the get_message() loop in comm.c, this function checks
 * all known database connections for their status and takes appropriate
 * actions.
 */

{
    dbconn_t *ptr = head;
    Bool got_dead = MY_FALSE;
    
    while (ptr)
    {
        if (!callback_object(&ptr->callback)
         && ptr->state != PG_UNCONNECTED
           )
        {
            debug_message("%s PG connection object destructed.\n", time_stamp());
            pgclose(ptr);
            got_dead = MY_TRUE;
        }
        else
           pg_process_one(ptr);
        ptr = ptr->next;
    }

    if (got_dead)
        pg_purge_connections();
} /* pg_process_all() */


/*-------------------------------------------------------------------------*/
void
pg_purge_connections (void)

/* Check the list of database connections and purge all UNCONNECTED
 * connections and those with destructed callback objects.
 */

{
    dbconn_t *prev;
    
    while (head)
    {
        if (head->state != PG_UNCONNECTED
         && !callback_object(&head->callback)
           )
        {
            debug_message("%s PG connection object destructed.\n", time_stamp());
            pgclose(head);
        }
        if (head->state == PG_UNCONNECTED)
            dealloc_dbconn(head);
        else
            break;
    }

    if (head)
    {
        prev = head;
        while (prev->next)
        {
            if (prev->next->state != PG_UNCONNECTED
             && !callback_object(&prev->next->callback)
               )
            {
                debug_message("%s PG connection object destructed.\n", time_stamp());
                pgclose(prev->next);
            }
            if (prev->next->state == PG_UNCONNECTED)
                dealloc_dbconn(prev->next);
            else
                prev = prev->next;
        }
    }
} /* pg_purge_connections() */

/*-------------------------------------------------------------------------*/
static
Bool check_privilege (const char * efun_name, Bool raise_error, svalue_t * sp)

/* Check if the user has the privileges to execute efun <efun_name>.
 * The function executes a call to master->privilege_violation("mysql",
 * efun_name) and evaluates the result.
 * If the master result is TRUE, the function returns TRUE.
 * If the master result is FALSE, the function returns FALSE if <raise_error>
 * is FALSE, and raises an error if <raise_error> is true.
 */

{
    Bool rc;

    inter_sp = sp+1;
    put_c_string(inter_sp, efun_name);
    rc = privilege_violation(STR_PGSQL, inter_sp, inter_sp);
    free_svalue(inter_sp);
    inter_sp--;

    if (rc)
        return MY_TRUE;

    if (raise_error)
    {
        errorf("%s(): Privilege violation.\n", efun_name);
        /* NOTREACHED */
    }

    return MY_FALSE;
} /* check_privilege() */



/*=========================================================================*/

/*                           EFUNS                                         */

/*-------------------------------------------------------------------------*/
svalue_t *
v_pg_connect (svalue_t *sp, int num_arg)

/* EFUN pg_connect()
 *
 *   int pg_connect (string conn, string fun)
 *   int pg_connect (string conn, string fun, string|object obj, mixed extra, ...)
 *   int pg_connect (string conn, closure cl, mixed extra, ...)
 *
 * Open a database connection as directed by <conn>, and assign the
 * callback function <fun>/<cl> with the optional <extra> parameters to
 * it.
 *
 * The object holding the callback function becomes the controlling object;
 * obiously it is an error to assign more than one connection to the same
 * controlling object.
 *
 * The <conn> string is in the format accepted by Postgres' PQconnectStart()
 * API functions. Pass an empty string to use the default options, or
 * a string holding the '<key>=<value>' options separated by whitespace.
 * The most useful options are:
 *   dbname:   The database name
 *   user:     The user name to connect as.
 *   password: Password to be used.
 *
 * Return 0 on success, and -1 on failure.
 */

{
    dbconn_t   *db;
    int         st;
    int         error_index;
    callback_t  cb;
    object_t   *cb_object;
    svalue_t   *arg = sp - num_arg + 1;

    check_privilege(instrs[F_PG_CONNECT].name, MY_TRUE, sp);

    /* Get the callback information */

    error_index = setup_efun_callback(&cb, arg+1, num_arg-1);

    if (error_index >= 0)
    {
        vefun_bad_arg(error_index+2, arg);
        /* NOTREACHED */
        return arg;
    }
    inter_sp = sp = arg+1;
    put_callback(sp, &cb);

    cb_object = callback_object(&cb);
    if (!cb_object)
    {
        free_callback(&cb);
        errorf("pgconnect(): Callback object is destructed.\n");
        /* NOTREACHED */
        return arg;
    }

    /* Check the callback object if it has a connection already */

    db = find_current_connection(cb_object);
    if (db)
    {
        if (db->state == PG_UNCONNECTED)
            dealloc_dbconn(db);
        else
        {
            free_callback(&cb);
            errorf("pgconnect(): Already connected\n");
            /* NOTREACHED */
            return arg;
        }
    }

    /* Connect to the database */

    db = alloc_dbconn();
    db->callback = cb;
    
    st = pgconnect(db, get_txt(arg[0].u.str));
    if (st < 0)
        pgclose(db);

    free_svalue(arg); /* the callback entries are gone already */
    put_number(arg, st);
    return arg;
} /* f_pg_connect() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_pg_pending (svalue_t *sp)

/* EFUN pg_pending()
 *
 *   int pg_pending ()
 *   int pg_pending (object obj)
 *
 * Return the number of pending queries for the connection on the given
 * object <obj> (default is the current object). The object has no
 * database connection, return -1.
 */

{
    dbconn_t *db;
    int       count = -1;
    
    check_privilege(instrs[F_PG_PENDING].name, MY_TRUE, sp);

    db = find_current_connection(sp->u.ob);
    if (db)
    {
        query_queue_t * qu;

        for (count = 0, qu = db->queue
            ; qu != NULL
            ; count++, qu = qu->next
            ) NOOP;
    }
    
    free_svalue(sp);
    put_number(sp, count);
    return sp;
} /* f_pg_pending() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_pg_query (svalue_t *sp, int numarg)

/* EFUN pg_query()
 *
 *  int pg_query (string query)
 *  int pg_query (string query, int flags)
 *
 * Queue a new query <query> to the database connection on the current
 * object. Return the unique id of the query. The query result itself
 * will be passed as argument to the callback function.
 *
 * <flags> can be one of these values:
 *   PG_RESULT_ARRAY: Pass the query result as array.
 *   PG_RESULT_MAP:   Pass the query result as mapping.
 */

{
    dbconn_t *db;
    query_queue_t *q;
    int flags = PG_RESULT_ARRAY;
    
    check_privilege(instrs[F_PG_QUERY].name, MY_TRUE, sp);

    if (numarg == 2)
    {
        flags = sp->u.number;
        sp--;
    }
    
    db = find_current_connection(current_object);
    if (!db)
        errorf("pgquery(): not connected\n");

    q = queue(db, get_txt(sp->u.str));
    q->flags = flags;
    if (db->state == PG_IDLE)
        db->state = PG_SENDQUERY;

    free_svalue(sp);
    put_number(sp, q->id);
    return sp;
} /* f_pg_query() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_pg_close (svalue_t *sp)

/* EFUN pg_close()
 *
 *   void pg_close()
 *
 * Close the database connection for the current object, if there is one.
 */

{
    dbconn_t *db;
    
    check_privilege(instrs[F_PG_CLOSE].name, MY_TRUE, sp);

    db = find_current_connection(current_object);
    if (db)
        pgclose(db);
    
    return sp;
} /* f_pg_close() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_pg_conv_string (svalue_t *sp)

/* EFUN pg_escapeString
 *
 * string pg_conv_string(string input)
 *
 * Escape a string for use within an SQL command.
 */
{
    string_t *escaped;
    int size = mstrsize(sp->u.str);
    memsafe(escaped = alloc_mstring(2 * size), 2 * size
                                             , "escaped sql string");

    // PQescapeString(char *to, char *from, size_t length);
    PQescapeString( (unsigned char *)get_txt(escaped)
                  , (unsigned char *)get_txt(sp->u.str), size);
    free_string_svalue(sp);
    put_string(sp, escaped);
    return sp;
} /* pg_conv_string() */

/*=========================================================================*/

/*                          GC SUPPORT                                     */

#ifdef GC_SUPPORT

/*-------------------------------------------------------------------------*/
void
pg_clear_refs (void)

/* GC Support: Clear all references from the database connections
 */

{
    dbconn_t *dbconn;

    for (dbconn = head; dbconn != NULL; dbconn = dbconn->next)
    {
        clear_ref_in_callback(&(dbconn->callback));
    }
} /* pg_clear_refs() */

/*-------------------------------------------------------------------------*/
void
pg_count_refs (void)

/* GC Support: Count all references from the database connections
 */

{
    dbconn_t *dbconn;

    for (dbconn = head; dbconn != NULL; dbconn = dbconn->next)
    {
        query_queue_t *qu;

        note_malloced_block_ref(dbconn);
        count_ref_in_callback(&(dbconn->callback));

        for (qu = dbconn->queue; qu != NULL; qu = qu->next)
        {
            note_malloced_block_ref(qu);
            note_malloced_block_ref(qu->str);
        }
    }
} /* pg_count_refs() */

#endif /* GC_SUPPORT */

/*-------------------------------------------------------------------------*/

#endif /* USE_PGSQL */

/*************************************************************************/
