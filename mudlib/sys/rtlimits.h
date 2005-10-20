#ifndef __RTLIMITS_H__
#define __RTLIMITS_H__

/* Runtime limit index/tag values */

#define LIMIT_EVAL     0
#define LIMIT_ARRAY    1
#define LIMIT_MAPPING  2
#define LIMIT_BYTE     3
#define LIMIT_FILE     4

#define LIMIT_MAX      5  /* Number of recognized limits */

/* Special limit values */

#define LIMIT_UNLIMITED    0   /* No limit */
#define LIMIT_KEEP       (-1)  /* Keep the old limit setting */
#define LIMIT_DEFAULT    (-2)  /* Use the default setting */

#endif /* __RTLIMITS_H__ */
