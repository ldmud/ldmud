#ifndef PKG_ODBC_H__
#define PKG_ODBC_H__ 1


#include "driver.h"
#include "typedefs.h"

/* Data Structure */
/* Column meta data */
typedef struct col_meta_data_s
{
  SQLSMALLINT nr;             //the columns number
  SQLCHAR     * name;         //column name

  SQLSMALLINT type;           //LDMud data type (T_FLAOT|T_NUMBER|T_STRING)

  union {                     //pointer to the fetched data
     SQLINTEGER * number_v;
     SQLDOUBLE  * double_v;
     SQLCHAR    * string_v;
  } data;

} COL_META_DATA;

/* Databse Connection */
typedef struct dbc_s hDBC;
struct dbc_s
{
   int           ID;            //ID to identify the handle
   
   char          * name;        //name of the connection
   
   hDBC          * prev,        
                 * next;

   SQLHDBC       hDBCon;        //ODBC DB connection
   SQLHSTMT      hStmt;         //current executed statement

   SQLSMALLINT   colcnt;        //number of columns in the resultset
   SQLINTEGER    rowcnt;        //number of rows in the resultset

   COL_META_DATA ** columns;    
};

/* ODBC Environment */
typedef struct odbc_env_s 
{
   SQLHENV hODBCEnv;            //ODBC environment

   int     next_hDBCon_ID;      //ID used for the next created connection
   hDBC    * hDBCons;           //list of connections
   
   
} hODBCENV;



svalue_t * f_sql_odbc_enabled( svalue_t * argv, int argc );
svalue_t * f_sql_odbc_datasources( svalue_t * argv, int argc );

svalue_t * f_sql_handles( svalue_t * argv, int argc );
svalue_t * f_sql_connect ( svalue_t * argv, int argc );
svalue_t * f_sql_close( svalue_t * argv, int argc );

svalue_t * f_sql_exec( svalue_t * argv, int argc );
svalue_t * f_sql_column_names( svalue_t * argv, int argc );
svalue_t * f_sql_affected_rows( svalue_t * argv, int argc );
svalue_t * f_sql_fetch( svalue_t * argv, int argc );

svalue_t * f_sql_error( svalue_t * argv, int argc );
   

/* required type tests */
/* Typetests for xefuns/tefuns */

#define TYPE_TEST1(arg1,type1) \
  if ((arg1)->type != type1) \
            bad_xefun_arg(1, argv);

#define TYPE_TEST2(arg1,type1) \
  if ((arg1)->type != type1) \
            bad_xefun_arg(2, argv);

#define TYPE_TEST3(arg1,type1) \
  if ((arg1)->type != type1) \
            bad_xefun_arg(3, argv);

/* Typetests for vararg xefuns/vefuns */

#define TYPE_TESTV1(arg1,type1) \
  if ((arg1)->type != type1) \
            bad_xefun_vararg(1, argv);

#define TYPE_TESTV2(arg1,type1) \
  if ((arg1)->type != type1) \
            bad_xefun_vararg(2, argv);

#define TYPE_TESTV3(arg1,type1) \
  if ((arg1)->type != type1) \
            bad_xefun_vararg(3, argv);

#endif /* PKG_ODBC_H__ */

