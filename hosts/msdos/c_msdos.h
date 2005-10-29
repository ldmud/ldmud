/* communication class dispatcher - WA */

typedef struct _class_def {
    int base,max_sess;
    int (*close)();
    int (*read)();
    int (*write)(int,char);
    void (*echo)();
} CLASS_DEF;

extern int c_errno;

/* virtual consoles */

void vc_listen();
int vc_accept();
int vc_select();
void vc_init();
void vc_shutdown();

/* serial lines */

void ser_listen();
int ser_accept();
int ser_select();
void ser_init();
void ser_shutdown();
void timer_expire();
