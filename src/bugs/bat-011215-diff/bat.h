#ifndef BAT_H__
#define BAT_H__

#include "svalue.h"
#include "profil2.h"

svalue_t *f_all_shadow(svalue_t * sp);
svalue_t *f_first_shadow(svalue_t * sp);
svalue_t *f_last_shadow(svalue_t * sp);
svalue_t *f_next_shadow(svalue_t * sp);

svalue_t *f_all_objects(svalue_t * sp, int n);
svalue_t *f_first_object(svalue_t * sp, int n);
svalue_t *f_next_object(svalue_t * sp, int n);

svalue_t *f_count_objects(svalue_t * sp, int n);
svalue_t *f_count_lines(svalue_t *sp);

/* catch_write() stuff */

void   begin_catch_write();
char*  end_catch_write();

#define CATCH_WRITE_BUF_LEN  (16*1024)

extern char catch_write_buf[];
extern int catching_write;
extern int catch_write_len;

/* end of catch_write() stuff */

void word_init();

#endif
