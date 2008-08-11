#include <stdlib.h>

#include <sql.h>
#include <sqlext.h>

#include "pkg-odbc.h"

#include "svalue.h"
#include "array.h"
#include "mapping.h"
#include "xalloc.h"

#define DEBUG_FUNC  1

//#define ODBC_DEBUG (DEBUG_FUNC) 
#define ODBC_DEBUG 0 

#define MEM_CHECK( x ) if ( !x ) { \
                          errorf( "Out of memory.\n" ); \
                          return( NULL ); \
                       }

/* prototypes: */
void          destruct_odbc_environment( void );

static void   push_db_connection( hDBC * handle );
static hDBC * pop_db_connection( void );
static int    dispose_db_connection( hDBC * handle );

static void   dispose_column_meta_data( COL_META_DATA * column );

static char * extract_diagnostics_info( SQLSMALLINT type, SQLHANDLE handle );
static void   raise_critical_sql_exception( hDBC * handle, char * msg );

static SQLSMALLINT map_column_type( SQLSMALLINT col_type, SQLSMALLINT digits );

/*************************************************************************
 * handle to the ODBC Environment                                        
 *************************************************************************/
static hODBCENV * hODBCEnv;

static char *
init_odbc_environment( void )
{
#if ODBC_DEBUG & DEBUG_FUNC
   printf( "call init_odbc_environment( )\n" );
#endif
   SQLRETURN ret;
   
   if ( hODBCEnv ) // already initialized 
      return( NULL );

   hODBCEnv = pxalloc( sizeof( hODBCENV ) );
   MEM_CHECK( hODBCEnv );

   ret = SQLAllocHandle( SQL_HANDLE_ENV, SQL_NULL_HANDLE, &(hODBCEnv->hODBCEnv) );
   if ( !SQL_SUCCEEDED( ret ) ) {
      
      destruct_odbc_environment();
      
      return( "Allocation of ODBC Environment failed.\n" );
   }

   ret = SQLSetEnvAttr( hODBCEnv->hODBCEnv, SQL_ATTR_ODBC_VERSION, (void*)SQL_OV_ODBC3, 0 );
   if ( !SQL_SUCCEEDED( ret ) ) {
      
      destruct_odbc_environment();
      
      return( "Could not set ODBC Environment Attributes.\n" );
   }

   
   hODBCEnv->next_hDBCon_ID = 1;
   hODBCEnv->hDBCons        = NULL;
   
#if ODBC_DEBUG & DEBUG_FUNC
   printf( "return init_odbc_environment( )\n" );
#endif
   return( NULL );
}

void
destruct_odbc_environment( void )
{
   if ( !hODBCEnv )
      return;

   if ( hODBCEnv->hODBCEnv ) {
      SQLFreeHandle( SQL_HANDLE_ENV, hODBCEnv->hODBCEnv );
      hODBCEnv->hODBCEnv = NULL;
   }
  
   while ( hODBCEnv->hDBCons ) {
      dispose_db_connection( pop_db_connection() );
   }
   
   pfree( hODBCEnv );
   hODBCEnv = NULL;
}

/************************************************************************* 
 * allocate_db_connection( ) . allocates a hDBC struct with the given ID                      
 * push_db_connection( ) ..... adds an allocated hDBC struct to the DB
 *                             connections 
 * pop_db_connection( ) ...... removes an allocated hDBC struct from  
 *                             the DB connections
 * get_db_connection_by_id( )  returns the requested handle and moves it on    
 *                             top of hODBCEnv->hDBCons                               
 * dispose_db_connection( ) .. disposes the passed connection         
 *************************************************************************/
static hDBC *
allocate_db_connection( int ID )
{
   hDBC      * handle;

   if ( !ID )
      return( NULL );

   handle = pxalloc( sizeof( hDBC ) );
   MEM_CHECK( handle );
   
   handle->ID = ID;
   
   handle->name    = NULL;
   
   handle->prev    = NULL;
   handle->next    = NULL;

   handle->hDBCon  = NULL;
   handle->hStmt   = NULL;

   handle->colcnt  = 0;
   handle->rowcnt  = 0;
   handle->columns = NULL;

   return( handle );
}

static void 
push_db_connection( hDBC * handle )
{
   if ( !handle || !hODBCEnv )
      return;
   
   if ( hODBCEnv->hDBCons ) {
      hODBCEnv->hDBCons->prev = handle;
      handle->next = hODBCEnv->hDBCons;
   }

   hODBCEnv->hDBCons = handle;
}

static hDBC *
pop_db_connection( void )
{
   hDBC * tmp;
   
   if ( !hODBCEnv || !hODBCEnv->hDBCons )
      return( NULL );

   tmp = hODBCEnv->hDBCons;
   
   hODBCEnv->hDBCons = tmp->next;
   tmp->next         = NULL;

   return( tmp );
}

static hDBC *
get_db_connection_by_id( int ID )
{
   hDBC * handle, 
        * prev, 
        * next;
   
   if ( !hODBCEnv->hDBCons )
      return( NULL );
   
   handle = hODBCEnv->hDBCons;

   while( (handle->ID != ID ) &&
          (handle = handle->next) );

   if ( !handle )
      return( NULL );

   if ( handle == hODBCEnv->hDBCons )
      return( handle );
   
   prev = handle->prev;
   next = handle->next;
      
   prev->next = next;
   if ( next )
      next->prev = prev;

   hODBCEnv->hDBCons->prev = handle;
   handle->prev = NULL;

   handle->next = hODBCEnv->hDBCons;

   hODBCEnv->hDBCons = handle;
   
   return( handle );
}

static int 
dispose_db_connection( hDBC * handle )
{
   int   ret;
   int   i;
   hDBC  * prev,
         * next;
         
   if ( !handle )
      return( 0 );

   ret = handle->ID;
  
   pfree( handle->name );
   handle->name = NULL;
   
   if ( handle->hDBCon ) {
      SQLDisconnect( handle->hDBCon );
      SQLFreeHandle( SQL_HANDLE_DBC, handle->hDBCon );
      handle->hDBCon  = NULL;
   }

   if ( handle->columns ) {
      for ( i = 0; i < handle->colcnt; ++i ) {
         dispose_column_meta_data( handle->columns[ i ] );
      }

      pfree( handle->columns );
      handle->columns = NULL;
   }
   
   pfree( handle );

   return ret;
}

/************************************************************************* 
 * allocate_column_meta_data( ) . allocates a COL_META_DATA struct          
 * dispose_column_meta_data( ) .. disposes a COL_META_DATA struct           
 *************************************************************************/
static COL_META_DATA *
allocate_column_meta_data( void )
{
   COL_META_DATA * column;

   column = pxalloc( sizeof( COL_META_DATA ) );
   MEM_CHECK( column ); 
 
   column->nr            = 0;
   column->name          = NULL;

   column->type          = 0;

   column->data.number_v = NULL;
   column->data.double_v = NULL;
   column->data.string_v = NULL;
   
   return( column );
}

static void 
dispose_column_meta_data( COL_META_DATA * column )
{
   if ( !column )
      return;

   pfree( column->name );
   column->name = NULL;

   pfree( column->data.number_v );
   column->data.number_v = NULL;

   pfree( column->data.double_v );
   column->data.double_v = NULL;
   
   pfree( column->data.string_v );
   column->data.string_v = NULL;
  
   pfree( column );
}

/*************************************************************************
 * f_sql_odbc_enabled( void )
 * 
 * returns TRUE if ODBC was enabled and the ODBC Environment is valid.
 *************************************************************************/
svalue_t *
f_sql_odbc_enabled( svalue_t * argv, int argc )
{
#if ODBC_DEBUG & DEBUG_FUNC
   printf( "call f_sql_odbc_enabled( )\n" );
#endif
   char * err;

   err = init_odbc_environment();

   argv++;
   
   if ( err ) {
      put_number( argv, 0 );
   } else {
      put_number( argv, 1 );
   }

#if ODBC_DEBUG & DEBUG_FUNC
   printf( "ret f_sql_odbc_enabled( )\n" );
#endif
   return( argv );
}

/*************************************************************************
 * f_sql_odbc_datasources( void )
 *
 * returns a mapping of all defined datasources.
 * ([ name : description ])
 *************************************************************************/
svalue_t *
f_sql_odbc_datasources( svalue_t * argv, int argc )
{
#if ODBC_DEBUG & DEBUG_FUNC
   printf( "call f_sql_odbc_datasources( )\n" );
#endif
   SQLRETURN   ret;
   
   char        * err;
   
   char        dsName[ 256 ],
               dsDescription[ 256 ];
   SQLSMALLINT dsNameLen,
               dsDescriptionLen;
   
   mapping_t   * map;
   svalue_t    * key;
   svalue_t    * value;

   argv++;

   err = init_odbc_environment();
   if ( err ) {
      errorf( err );
      return( NULL );
   }
   
   map = allocate_mapping( 0, 1 );
   MEM_CHECK( map );
   
   do {
      ret = SQLDataSources( hODBCEnv->hODBCEnv, SQL_FETCH_NEXT, dsName, 256, &dsNameLen,
                                                                dsDescription, 256, &dsDescriptionLen );
      if ( SQL_SUCCEEDED( ret ) ) {
         key = pxalloc( sizeof( svalue_t ) );
         put_malloced_string( key, string_copy( dsName ) );

         value = get_map_lvalue( map, key ); 
         MEM_CHECK( value );

         put_malloced_string( value, string_copy( dsDescription ) );
      }
      
   } while( SQL_SUCCEEDED( ret ) );

   if ( ret != SQL_NO_DATA ) {
      char * err;
      err = extract_diagnostics_info( SQL_HANDLE_ENV, hODBCEnv->hODBCEnv );

      free_mapping( map );

      errorf( (err?err:"Failed to fetch datasource information.\n") );
      return( NULL );
   }

   put_mapping( argv, map );
   
#if ODBC_DEBUG & DEBUG_FUNC
   printf( "ret f_sql_odbc_datasources( )\n" );
#endif
   return( argv );
}

/************************************************************************* 
 * f_sql_handles( void ) 
 *
 * returns an array containing the IDs of the currently opened connections
 *************************************************************************/
svalue_t *
f_sql_handles( svalue_t * argv, int argc )
{
#if ODBC_DEBUG & DEBUG_FUNC
   printf( "call f_sql_handles( )\n" );
#endif
   int  cnt, i;
   hDBC * tmp;

   vector_t    * vec;

   argv++;
   
   if ( !hODBCEnv )
      init_odbc_environment();
  
   if ( !hODBCEnv ) {
      put_number( argv, 0 );
      return( argv );
   }
   
   if ( !hODBCEnv->hDBCons ) {

      vec = allocate_array( 0 );
      MEM_CHECK( vec );

      put_array( argv, vec );
      return( argv );
   }

   tmp = hODBCEnv->hDBCons;
   cnt = 1;

   while( (tmp = tmp->next ) )
      cnt++;

   vec = allocate_array( cnt );
   MEM_CHECK( vec );

   tmp = hODBCEnv->hDBCons;
   for ( i = 0; i < cnt; ++i ) {
      put_number( vec->item + i, tmp->ID );
      tmp = tmp->next;
   }

   put_array( argv, vec );
#if ODBC_DEBUG & DEBUG_FUNC
   printf( "ret f_sql_handles( )\n" );
#endif
   return( argv );
}

/************************************************************************* 
 * f_sql_connect( string database, void|string user, void|string pwd )
 *
 * opens a connection to an ODBC database. returns the ID of the
 * created handle
 *************************************************************************/
svalue_t *
f_sql_connect ( svalue_t * argv, int argc )
{
#if ODBC_DEBUG & DEBUG_FUNC
   printf( "call f_sql_connect ( )\n" );
#endif
   SQLRETURN ret;
   SQLCHAR   * database,
             * user,
             * password;
   int       pos;
   hDBC      * handle;
   
   char      * err;
   
   database = NULL;
   user     = NULL;
   password = NULL;

   switch( argc ) {
      case 3 :
         TYPE_TESTV3( argv, T_STRING );
         password = argv->u.string;
      case 2 :
         pos = 2 - argc;
         TYPE_TESTV2( argv + pos, T_STRING );
         user     = (argv + pos)->u.string;
      case 1 :
         pos = 1 - argc;
         TYPE_TESTV1( argv + pos, T_STRING );
         database = (argv + pos)->u.string;
         break;
      default:
         errorf( "Too many arguments to sql_connect().\n" );
         return( NULL );
   }

   if ( !hODBCEnv && (err = init_odbc_environment()) ) {
      raise_critical_sql_exception( handle, err );
      return( NULL );
   }

   handle = allocate_db_connection( hODBCEnv->next_hDBCon_ID++ );
   MEM_CHECK( handle );

   ret = SQLAllocHandle( SQL_HANDLE_DBC, hODBCEnv->hODBCEnv, &(handle->hDBCon) );
   if ( !SQL_SUCCEEDED( ret ) ) {

      raise_critical_sql_exception( handle, extract_diagnostics_info( SQL_HANDLE_ENV, hODBCEnv->hODBCEnv ) );

      return( NULL );
   }

   SQLSetConnectAttr( handle->hDBCon, SQL_LOGIN_TIMEOUT, (SQLPOINTER *)5, 0 );
   SQLSetConnectAttr( handle->hDBCon, SQL_ATTR_AUTOCOMMIT, (SQLPOINTER *)SQL_AUTOCOMMIT_ON, 0 );

   ret = SQLConnect( handle->hDBCon, database, SQL_NTS,
                                     (user ? user : (SQLCHAR *)"\0"), SQL_NTS,
                                     (password ? password : (SQLCHAR *)"\0"), SQL_NTS);
   if ( !SQL_SUCCEEDED( ret ) ) {

      raise_critical_sql_exception( handle, extract_diagnostics_info( SQL_HANDLE_DBC, handle->hDBCon ) );

      return( NULL );
   }

   handle->name = string_copy( database );

   push_db_connection( handle );

   switch( argc ) {
      case 3:
         free_string_svalue( argv );
         argv--;
      case 2:
         free_string_svalue( argv );
         argv--;
      case 1:
         free_string_svalue( argv );
   }

   put_number( argv, handle->ID );
#if ODBC_DEBUG & DEBUG_FUNC
   printf( "ret f_sql_connect ( )\n" );
#endif
   return( argv );
}

/************************************************************************* 
 * f_sql_close( int handle )                                
 *
 * colses the connection with the given ID
 *************************************************************************/
svalue_t *
f_sql_close( svalue_t * argv, int argc )
{
#if ODBC_DEBUG & DEBUG_FUNC
   printf( "call f_sql_close( )\n" );
#endif
   int  id;
   hDBC * handle;

   TYPE_TEST1( argv, T_NUMBER );
   id = argv->u.number;

   if ( !(handle = get_db_connection_by_id( id )) ) {
      errorf( "Illegal handle for database.\n" );

      return( NULL );
   }

   id = dispose_db_connection( pop_db_connection() );

   if ( !hODBCEnv->hDBCons ) { //closing the last connection, close ODBC Environment
      destruct_odbc_environment();
   }
   
   free_svalue( argv );
   put_number( argv, id );

#if ODBC_DEBUG & DEBUG_FUNC
   printf( "ret f_sql_close( )\n" );
#endif

   return( argv );
}

/************************************************************************* 
 * f_sql_exec( int handle, string statement )
 *
 * executes an SQL statement
 *************************************************************************/
svalue_t *
f_sql_exec( svalue_t * argv, int argc )
{
#if ODBC_DEBUG & DEBUG_FUNC
   printf( "call f_sql_exec( )\n" );
#endif

   int         id, i;
   SQLCHAR     * statement;
   hDBC        * handle;

   //SQLSMALLINT cols;
   SQLRETURN   ret;

   TYPE_TEST2( argv, T_STRING );
   statement = string_copy( argv->u.string );
   free_string_svalue( argv );
   argv--;
   
   TYPE_TEST1( argv, T_NUMBER );
   id = argv->u.number;
   free_svalue( argv );

   if ( !(handle = get_db_connection_by_id( id )) ) {
      pfree( statement );
      errorf( "Illegal handle for database.\n" );
      return( NULL );
   }

   if ( handle->hStmt ) {
      //printf( "freeing statement\n" );
      ret = SQLFreeStmt( handle->hStmt, SQL_UNBIND );
      if ( !SQL_SUCCEEDED( ret ) ) {
          //printf( "SQLFreeStmt( handle->hStmt, SQL_UNBIND ) = %d\n", ret );
          pfree( statement );
          errorf( extract_diagnostics_info( SQL_HANDLE_STMT, handle->hStmt ) );
          return( NULL );
      }
      ret = SQLFreeHandle( SQL_HANDLE_STMT, handle->hStmt );
      if ( !SQL_SUCCEEDED( ret ) ) {
          //printf( "SQLFreeHandle( SQL_HANDLE_STMT, handle->hStmt ) = %d\n", ret );
          pfree( statement );
          errorf( extract_diagnostics_info( SQL_HANDLE_STMT, handle->hStmt ) );
          return( NULL );
      }
   }
   
   if ( handle->columns ) {
      for ( i = 0; i < handle->colcnt; ++i ) {
         dispose_column_meta_data( handle->columns[ i ] );
      }

      pfree( handle->columns );
      handle->columns = NULL;
   }

   //printf( "allocating statement \n" );
   ret = SQLAllocHandle( SQL_HANDLE_STMT, handle->hDBCon, &handle->hStmt );
   if ( !SQL_SUCCEEDED( ret ) ) {
      pfree( statement );
      errorf( extract_diagnostics_info( SQL_HANDLE_DBC, handle->hDBCon ) );

      return( NULL );
   }

   handle->colcnt = 0;
   handle->rowcnt = 0;
   
//   printf( "executing\n" );
   ret = SQLExecDirect( handle->hStmt, statement, SQL_NTS );
   pfree( statement );
   if ( !SQL_SUCCEEDED( ret ) ) {
      //printf( "XXX: %s\n", extract_diagnostics_info( SQL_HANDLE_STMT, handle->hStmt ) );
      put_number( argv, 0 );
      return( argv );
   }
/* getting number of columns. */
   //ret = SQLNumResultCols( handle->hStmt, &cols );
   ret = SQLNumResultCols( handle->hStmt, &handle->colcnt );
   if ( !SQL_SUCCEEDED( ret ) ) {
      put_number( argv, 0 );
      return( argv );
   }
  
   ret = SQLRowCount( handle->hStmt, &handle->rowcnt ); 
   if ( !SQL_SUCCEEDED( ret ) ) {
      put_number( argv, 0 );
      return( argv );
   }
   
   //handle->colcnt  = cols;
   if ( handle->colcnt ) {
      handle->columns = pxalloc( handle->colcnt * sizeof( COL_META_DATA* ) );
      MEM_CHECK( handle->columns );
   }

/* fetching meta data */
   COL_META_DATA * tmp;
   SQLCHAR       dColname[ 100 ];
   SQLSMALLINT   dColnameLen;
   SQLSMALLINT   dType;
   SQLSMALLINT   dDDigits;
   SQLSMALLINT   dNullable;
   SQLUINTEGER   dColSize;
   
   for ( i = 1; i <= handle->colcnt; ++i ) {
      ret = SQLDescribeCol( handle->hStmt, i, dColname, sizeof( dColname ), &dColnameLen, &dType, &dColSize, &dDDigits, &dNullable );
      if ( !SQL_SUCCEEDED( ret ) ) {
         put_number( argv, 0 );
         return( argv );
      }

      tmp = allocate_column_meta_data();
      MEM_CHECK( tmp );

      tmp->nr = i;
      tmp->name = string_copy( dColname );
      tmp->type = map_column_type( dType, dDDigits );

     // printf( "[%s] dColSize=%d dDDigits=%d\n", dColname, dColSize, dDDigits );

      SQLLEN len;
      switch( tmp->type ) {
         case T_NUMBER: 
            tmp->data.number_v = pxalloc( sizeof( SQL_C_LONG ) );
            *tmp->data.number_v = 0;
            SQLBindCol( handle->hStmt, i, SQL_C_LONG, tmp->data.number_v, 100, &len );
            break;

         case T_FLOAT:
            tmp->data.double_v = pxalloc( sizeof( SQL_C_DOUBLE ) );
            *tmp->data.double_v = 0;
            SQLBindCol( handle->hStmt, i, SQL_C_DOUBLE, tmp->data.double_v, 100, &len );
            break;

         default:
            tmp->data.string_v = pxalloc( (dColSize + 1) * sizeof( SQLCHAR ) );
            SQLBindCol( handle->hStmt, i, SQL_C_CHAR, tmp->data.string_v, (dColSize + 1), &len );
      }

      handle->columns[ i-1 ] = tmp;
   }
   
   put_number( argv, id );
#if ODBC_DEBUG & DEBUG_FUNC
   printf( "ret f_sql_exec( )\n" );
#endif
   return( argv );
}

/*************************************************************************
 * f_sql_column_names( int handle )                                      
 *
 * returns the returned column names of the last executed statement
 *************************************************************************/
svalue_t *
f_sql_column_names( svalue_t * argv, int argc )
{
#if ODBC_DEBUG & DEBUG_FUNC
   printf( "call f_sql_column_names( )\n" );
#endif
   int  id, cnt, i;
   hDBC * handle;

   vector_t    * vec;

   TYPE_TEST1( argv, T_NUMBER );
   id = argv->u.number;
   free_svalue( argv );

   if ( !(handle = get_db_connection_by_id( id )) ) {
      errorf( "Illegal handle for database.\n" );

      return( NULL );
   }

   if ( !handle->columns ) {

      vec = allocate_array( 0 );
      MEM_CHECK( vec );

      put_array( argv, vec );
      return( argv );
   }

   cnt = handle->colcnt;

   vec = allocate_array( cnt );
   MEM_CHECK( vec );

   for ( i = 0; i < cnt; ++i ) {
      if ( handle->columns[ i ] )
         put_malloced_string(vec->item + i, string_copy(  handle->columns[ i ]->name ) );
   }

   put_array( argv, vec );
#if ODBC_DEBUG & DEBUG_FUNC
   printf( "ret f_sql_column_names( )\n" );
#endif
   return( argv );
}

/*************************************************************************
 * f_sql_affected_rows( int handle ) 
 *
 * returns the number of rows in the resultset
 *************************************************************************/
svalue_t *
f_sql_affected_rows( svalue_t * argv, int argc )
{
#if ODBC_DEBUG & DEBUG_FUNC
   printf( "call f_sql_affected_rows( )\n" );
#endif
   int        id;
   hDBC       * handle;

   TYPE_TEST1( argv, T_NUMBER );
   id = argv->u.number;
   free_svalue( argv );

   if ( !(handle = get_db_connection_by_id( id )) ) {
      errorf( "Illegal handle for database.\n" );

      return( NULL );
   }

   if ( !handle->hStmt ) {
      put_number( argv, 0 );
      return( argv );
   }

   put_number( argv, handle->rowcnt );
#if ODBC_DEBUG & DEBUG_FUNC
   printf( "ret f_sql_affected_rows( )\n" );
#endif
   return( argv );
}

/*************************************************************************
 * mixed f_sql_fetch( int handle, void|int method ) 
 *
 * returns one row of the result set either as an array (default) 
 * or a mapping ( ([ column_name : data ])
 *************************************************************************/

static vector_t * 
fetch_into_vector( hDBC * handle )
{
   int       i;

   vector_t  * vec;   

   STORE_DOUBLE_USED;
   
   vec = allocate_array( handle->colcnt );
   MEM_CHECK( vec );

   for( i = 0; i < handle->colcnt; ++i ) {
      if ( !handle->columns[ i ] ) continue;

      //printf( " fetch_into_vector[%2d] ", i );

      switch( handle->columns[ i ]->type ){
         case T_FLOAT:
            //printf( "float=%f\n",  handle->columns[ i ]->data.double_v );
            STORE_DOUBLE( vec->item + i, *handle->columns[ i ]->data.double_v );

            ( vec->item + i)->type = T_FLOAT;
            break; 

         case T_NUMBER:
            //printf( "number=%d\n",  *handle->columns[ i ]->data.number_v );
            put_number( vec->item + i, *handle->columns[ i ]->data.number_v );
            break;

         case T_STRING: 
         default      :
            //printf( "string=%s\n",  handle->columns[ i ]->data.string_v );
            put_malloced_string( vec->item + i, string_copy(  handle->columns[ i ]->data.string_v ) );
            break;
      }
   }

   return( vec );
}

static mapping_t * 
fetch_into_mapping( hDBC * handle )
{
   int       i;

   mapping_t  * map;   
   svalue_t  * key,
             * value;

   STORE_DOUBLE_USED;

   map = allocate_mapping( handle->colcnt, 1 );
   MEM_CHECK( map );
         
   for( i = 0; i < handle->colcnt; ++i ) {
      if ( !handle->columns[ i ] ) continue;

      //printf( " fetch_into_mapping[%2d] ", i );
      
      key = pxalloc( sizeof( svalue_t ) ); 
      MEM_CHECK( key );
         
      put_malloced_string( key, string_copy( handle->columns[ i ]->name ) );
         
      value = get_map_lvalue( map, key );
      MEM_CHECK( value );

      switch( handle->columns[ i ]->type ){
         case T_FLOAT:
            //printf( "float=%f\n",  handle->columns[ i ]->data.double_v );
            STORE_DOUBLE( value, *handle->columns[ i ]->data.double_v );

            value->type = T_FLOAT;
            break; 

         case T_NUMBER:
            //printf( "number=%d\n",  *handle->columns[ i ]->data.number_v );
            put_number( value, *handle->columns[ i ]->data.number_v );
            break;

         case T_STRING: 
         default      :
            //printf( "string=%s\n",  handle->columns[ i ]->data.string_v );
            put_malloced_string( value, string_copy(  handle->columns[ i ]->data.string_v ) );
            break;
      }
   }

   return( map );
}


svalue_t *
f_sql_fetch( svalue_t * argv, int argc )
{
#if ODBC_DEBUG & DEBUG_FUNC
   printf( "call f_sql_fetch( )\n" );
#endif
   int       id, method = 0;
   hDBC      * handle;

   SQLRETURN ret;
   
   STORE_DOUBLE_USED;

   switch( argc ) {
      case 2 :
         TYPE_TEST2( argv, T_NUMBER );
         method = argv->u.number;
         free_svalue( argv );
         argv--;
      case 1 :
         TYPE_TEST1( argv, T_NUMBER );
         id = argv->u.number;
         free_svalue( argv );
         break;
      default:
         errorf( "Too many arguments to sql_fetch().\n" );
         return( NULL );
   }
   
   if ( !(handle = get_db_connection_by_id( id )) ) {
      errorf( "Illegal handle for database.\n" );

      return( NULL );
   }

   if ( !handle->hStmt ) {
      put_number( argv, 0 );
      return( argv );
   }

   //printf( "\nFetching ....\n" );
   ret = SQLFetch( handle->hStmt );
   if ( ret == SQL_NO_DATA ) {
      //printf( "NO_DATA\n" );

      put_number( argv, 0 );
      return( argv );
      
   } else if ( !SQL_SUCCEEDED( ret ) ) {
       put_number( argv, 0 );
       return( argv );
   }

   if ( !handle->columns ) {
      put_number( argv, 0 );
      return( argv );
   }
   
   if ( !method ) { //fetch as array
      put_array( argv, fetch_into_vector( handle ) );
   } else { //fetch as mapping 
      put_mapping( argv, fetch_into_mapping( handle ) );
   }
   
#if ODBC_DEBUG & DEBUG_FUNC
   printf( "ret f_sql_fetch( )\n" );
#endif
   return( argv );
}

/*************************************************************************
 * string f_sql_error( int handle )  
 *
 * returns the last encountered error
 *************************************************************************/
svalue_t *
f_sql_error( svalue_t * argv, int argc )
{
#if ODBC_DEBUG & DEBUG_FUNC
   printf( "call f_sql_error( )\n" );
#endif
   int  id, cnt, i;
   hDBC * handle;

   char * err;

   TYPE_TEST1( argv, T_NUMBER );
   id = argv->u.number;
   free_svalue( argv );

   if ( !(handle = get_db_connection_by_id( id )) ) {
      errorf( "Illegal handle for database.\n" );

      return( NULL );
   }

   if ( handle->hStmt ) {
      err = extract_diagnostics_info( SQL_HANDLE_STMT, handle->hStmt );
   } else {
      err = extract_diagnostics_info( SQL_HANDLE_DBC, handle->hDBCon );
   }
   
   if ( err ) {
      put_malloced_string( argv, err );
   } else {
      put_number( argv, 0 );
   }

#if ODBC_DEBUG & DEBUG_FUNC
   printf( "ret f_sql_error( )\n" );
#endif
   return( argv );
}

/*************************************************************************
 * extracts the dignosticinfo of an handle.
 *************************************************************************/
static char *
extract_diagnostics_info( SQLSMALLINT type, SQLHANDLE handle )
{
   char       ** tmp;
   char       * ret;
   int        i,
              j = 0;

   SQLINTEGER recnum;
   
   SQLGetDiagField( type, handle, 0, SQL_DIAG_NUMBER, &recnum, SQL_IS_INTEGER, 0 );

   if ( !recnum )
      return( NULL );

   tmp = pxalloc( recnum * sizeof( tmp ) );

   SQLCHAR     state[ 7 ];
   SQLINTEGER  native;
   SQLCHAR     text[1000],
               buffer[1000];
   SQLSMALLINT len;

  for ( i = 0; i < recnum; i++ ) {
      SQLGetDiagRec( type, handle, i+1, state, &native, text, sizeof( text ), &len );
      sprintf( buffer, "[%5s]%s\n", state, text );
      tmp[ i ] = string_copy( buffer );
      j += strlen( tmp[ i ] );
   }
   
   ret = pxalloc( (j + 1) * sizeof( char ) );
   for ( i = 0; i < recnum; i++ ) {
      if( i ) {
         strcat( ret, tmp[ i ] );
      } else {
         strcpy( ret, tmp[ i ] );
      }
      pfree( tmp[ i ] );
   }
   
   pfree( tmp );

   return( ret );
}

/*************************************************************************
 * raise_critical_sql_exception() closes the affected DB handle and      
 *                                aborts the execution                   
 *************************************************************************/
static void
raise_critical_sql_exception( hDBC * handle, char * msg )
{
   if ( handle )
      dispose_db_connection( handle );
   
   errorf( ( msg ? msg : "An unknown error occured during the current DB operation.\n" ) );
      
   abort();
}

/*************************************************************************/
/* maps SQL datatypes to LDMud datatypes                                 */
/*************************************************************************/
static SQLSMALLINT
map_column_type( SQLSMALLINT col_type, SQLSMALLINT digits )
{
   switch( col_type ) {
      case SQL_DECIMAL :
      case SQL_REAL    :
      case SQL_FLOAT   :
      case SQL_DOUBLE  : 
        //printf( "T_FLOAT\n" );
         return( T_FLOAT );
      
      case SQL_NUMERIC : if ( digits )
                            return( T_FLOAT );
                         else
                            return( T_NUMBER );

      case SQL_SMALLINT:
      case SQL_INTEGER :
      case SQL_BIT     :
      case SQL_TINYINT :
      case SQL_BIGINT  : 
        //printf( "T_NUMBER\n" );
                         return( T_NUMBER );

      default          : 
        //printf( "T_STRING\n" );
                         return( T_STRING );
   }
}

