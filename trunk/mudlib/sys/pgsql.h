#ifndef _PGSQL_H
#define _PGSQL_H

/* Definitions for the PostgreSQL efuns */

#define PGRES_EMPTY_QUERY    0  /* Unimplemented */
#define PGRES_COMMAND_OK     1
#define PGRES_TUPLES_OK      2
#define PGRES_COPY_OUT       3  /* Unimplemented */
#define PGRES_COPY_IN        4  /* Unimplemented */
#define PGRES_BAD_RESPONSE   5
#define PGRES_NONFATAL_ERROR 6
#define PGRES_FATAL_ERROR    7
#define PGRES_NOTICE        99

#define PGCONN_SUCCESS 100
#define PGCONN_FAILED  101
#define PGCONN_ABORTED 102

#define PG_RESULT_ARRAY 0
#define PG_RESULT_MAP   1

#endif
