#ifndef LPC_WIZLIST_H_
#define LPC_WIZLIST_H_ 1

/* Indices returned by wizlist_info()
 */

#define WL_NAME           0  /* Wizard name */
#define WL_COMMANDS       1  /* Number of commands executed */
#define WL_EVAL_COST      2  /* Evalcost spent on this wizard */
#define WL_HEART_BEATS    3  /* Heartbeats spent on this wizard */
#define WL_CALL_OUT       4  /* unimplemented */
#define WL_ARRAY_TOTAL    5  /* Arrays accounted for */
#define WL_MAPPING_TOTAL  6  /* Mappings accounted for */

#if defined(__LPC_STRUCTS__) || defined(USE_STRUCTS)

#define WL_STRUCT_TOTAL   7  /* Structs accounted for */
#define WL_EXTRA          8  /* Extra Wizinfo, if set */

#define WL_SIZE  8  /* Number of entries */

#else /* !__LPC_STRUCTS__ */

#define WL_EXTRA          7  /* Extra Wizinfo, if set */

#define WL_SIZE  7  /* Number of entries */

#endif /* __LPC_STRUCTS__ */

#endif /* LPC_WIZLIST_H_ */
