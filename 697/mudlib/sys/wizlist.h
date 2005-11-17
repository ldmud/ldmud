#ifndef LPC_WIZLIST_H_
#define LPC_WIZLIST_H_ 1

/* Indices returned by wizlist_info()
 */

#define WL_NAME           0  /* Wizard name */
#define WL_COMMANDS       1  /* Number of commands executed */
#define WL_COST           2  /* Weighted evalcost spent on this wizard */
#define WL_GIGACOST       3  /* Weighted giga-evalcost spent on this wizard */
#define WL_TOTAL_COST     4  /* Total evalcost spent on this wizard */
#define WL_TOTAL_GIGACOST 5  /* Total giga-evalcost spent on this wizard */
#define WL_HEART_BEATS    6  /* Heartbeats spent on this wizard */
#define WL_CALL_OUT       7  /* unimplemented */
#define WL_ARRAY_TOTAL    8  /* Arrays accounted for */
#define WL_EXTRA          9  /* Extra Wizinfo, if set */

#define WL_SIZE          10  /* Number of entries */

#endif /* LPC_WIZLIST_H_ */
