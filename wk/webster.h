/*
 * webster.h - definitions for client and server
 *
 * David A. Curry
 * Purdue University
 * davy@ee.purdue.edu
 * April, 1986
 */

/*
 * Use "websterhost" as the name of the host, rather than a specific machine,
 * so we can just change an alias in /etc/hosts if we want to move
 * the database (rather than recompiling "webster")
 * ..sahayman 89/02/28
 */

#define WEBSTERHOST	"webster.cs.indiana.edu"  /* host where the server lives	*/
#define WEBSTERPORT	2627		/* port where the server lives	*/
#define WEBSTERNAME	"webster"	/* name of the service		*/
#define TIMEOUT		(1 * 60)	/* max 10 minutes idle time	*/
#define EOFCH		0200		/* EOF record from the server   */
#define MANYCH		'*'		/* match one or more characters	*/
#define ONECH		'%'		/* match one character		*/
