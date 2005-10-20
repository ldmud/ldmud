#ifndef _WIZLIST_H_
#define _WIZLIST_H_ 1

/* Indices returned by wizlist_info()
 */

#define WL_NAME 	0  /* Wizard name */
#define WL_COMMANDS	1  /* Number of commands executed */
#define WL_EVAL_COST	2  /* Evalcost spent on this wizard */
#define WL_HEART_BEATS  3  /* Heartbeats spent on this wizard */
#define WL_CALL_OUT	4  /* unimplemented */
#define WL_ARRAY_TOTAL	5  /* Arrays accounted for */
#define WL_EXTRA	6  /* Extra Wizinfo, if set */

#define WL_SIZE         7  /* Number of entries */

#endif /* _WIZLIST_H_ */
