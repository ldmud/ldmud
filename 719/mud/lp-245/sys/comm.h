#ifndef LPC_COMM_H_
#define LPC_COMM_H_

/* Mode values recognized by the efun get_combine_charset()
 * and get_connection_charset() */

#define CHARSET_VECTOR     0  /* Return the charset as bitvector array */
#define CHARSET_STRING     1  /* Return the charset as string */
#define CHARSET_QUOTE_IAC  2  /* Return the status of the IAC quoting */

#endif /* LPC_COMM_H_ */
