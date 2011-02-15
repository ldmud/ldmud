#ifndef LPC_RTLIMITS_H_
#define LPC_RTLIMITS_H_

/* Runtime limit index/tag values */

#define LIMIT_EVAL           (0)
#define LIMIT_ARRAY          (1)
#define LIMIT_MAPPING        (2)
#define LIMIT_MAPPING_KEYS   LIMIT_MAPPING
#define LIMIT_MAPPING_SIZE   (3)
#define LIMIT_BYTE           (4)
#define LIMIT_FILE           (5)
#define LIMIT_CALLOUTS       (6)
#define LIMIT_COST           (7)
#define LIMIT_MEMORY         (8)

#define LIMIT_MAX      (9)  /* Number of recognized limits */

/* Special limit values */

#define LIMIT_UNLIMITED    0   /* No limit */
#define LIMIT_KEEP       (-1)  /* Keep the old limit setting */
#define LIMIT_DEFAULT    (-2)  /* Use the default setting */

/* memory limits */
#define MALLOC_SOFT_LIMIT    1
#define MALLOC_HARD_LIMIT    2

/* low memory conditions */
#define NO_MALLOC_LIMIT_EXCEEDED     0
#define SOFT_MALLOC_LIMIT_EXCEEDED   MALLOC_SOFT_LIMIT
#define HARD_MALLOC_LIMIT_EXCEEDED   MALLOC_HARD_LIMIT

/* Flags for available reserves */
#define USER_RESERVE_AVAILABLE     0x1
#define MASTER_RESERVE_AVAILABLE   0x2
#define SYSTEM_RESERVE_AVAILABLE   0x4

#endif /* LPC_RTLIMITS_H_ */
