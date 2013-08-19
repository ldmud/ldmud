#ifndef LPC_COMM_H_
#define LPC_COMM_H_

/* Mode values recognized by the efun get_combine_charset()
 * and get_connection_charset() */

#define CHARSET_VECTOR     0  /* Return the charset as bitvector array */
#define CHARSET_STRING     1  /* Return the charset as string */
#define CHARSET_QUOTE_IAC  2  /* Return the status of the IAC quoting */

/* return values for net_connect() */

#define NC_SUCCESS         0  /* Success! */
#define NC_EUNKNOWNHOST    1  /* host address could not be resolved */
#define NC_ENOSOCKET       2  /* socket could not be created */
#define NC_ENOBIND         3  /* socket could not be bound */
#define NC_ENOCONNECT      4  /* socket could not be connected */
#define NC_ECONNREFUSED    5  /* target address not listening */
#define NC_EMCONN          6  /* too many pending connections */
#define NC_ENORESSOURCES   7  /* insufficient system ressources */

#endif /* LPC_COMM_H_ */
