#ifndef LPC_COMM_H_
#define LPC_COMM_H_

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
