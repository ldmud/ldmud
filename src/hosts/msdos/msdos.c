#include <ctype.h>
#include <string.h>
#include <memory.h>
#include <sys/types.h>
#include <sys/time.h>
#include <dos.h>


static long timer = 0L,timeout;


/*
 * Timer with a resolution of up to one millisecond (depends on hardware
 * capabilities).
 */


long milliseconds(timer)
long *timer;
{
    long now,diff;
    struct timeval time;

    gettimeofday(&time,0);
    now = time.tv_sec*1000L+(time.tv_usec+500L)/1000L+1L;
    if (*timer == 0L) diff = 0L;
    else {
        diff = now-*timer;
        if (diff < 0L) diff += 86400000L; /* midnight */
    }
    *timer = now;
    return diff;
}


/*
 * Timer code. Uses polling instead of interrupts which is okay for MS-DOS.
 */


extern int time_to_call_heart_beat, comm_time_to_call_heart_beat;


void start_timer(seconds)
int seconds;
{
    milliseconds(&timer);
    timeout = seconds*1000L;
}


void timer_expire()
{
    long diff;

    if (timer == 0) return;
    diff = milliseconds(&timer);
    if ((timeout -= diff) > 0) return;
    comm_time_to_call_heart_beat = 1;
}

/*
 * getdomainname fake function. declared in <djgppstd.h>
 * Note: Linux getdomainname behaves differently (?):
 *      would ret 0 if Ok, and do error(EINVAL) if name > len
 */
int
getdomainname(char *name, int len)
{
    strncpy(name, "MSDOS", len);
    /* should I ensure zero termination? */
    return 0;
}

/*
 * gethostname fake function. declared in <djgppstd.h>
 * was previously define in comm.h as
 * #define gethostname(n,l) strcpy(n,"PC386+")
 */
int gethostname(char *name, int len)
{
    strncpy(name, "PC386+", len);
    return 0;
}

/*
 * exp() approximation. Required because there's no 387 emulation for this
 * function yet. Unfortunately, any better approximation reveals strange
 * bugs in the emulator.
 */

double exp(x)
double x;
{
    return x <= -1.0 ? 1.0e-10 : 1.0+x;
}

    
/*
 * Very weak replacement for crypt().
 */

#if 0
/* Crypt is now in hosts/crypt.c */

#define rot(v,c) (((v) << c) | ((v) >> (8-c)))

static int mirror(x)
int x;
{
    return ((x & 1) << 7) | ((x & 2) << 5) | ((x & 4) << 3) | ((x & 8) << 1) |
      ((x & 16) >> 1) | ((x & 32) >> 3) | ((x & 64) >> 5) | ((x & 128) >> 7);
}

char *crypt(pw,salt)
char *pw;
char *salt;
{
    static char e64[] =
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ/abcdefghijklmnopqrstuvwxyz.0123456789";
    static char enc[9];
    char tp[9];
    int scrm,i,bpos,n;

    memset(tp,'\0',8);
    strncpy(tp,pw,8);
    tp[0] ^= enc[0] = salt[0];
    tp[3] ^= enc[1] = salt[1];
    scrm = 165;
    for (i = 7; i >= 0; i--) scrm = (tp[i] ^= scrm >> 1);
    for (i = 7; i; i--) tp[tp[i-1] & 7] ^= rot(tp[i],1);
    for (i = (tp[0] & 127)+19; i; i--)
        tp[tp[i & 7] & 7] ^= rot(tp[(tp[i & 7] >> 3) & 7],tp[i & 7] >> 6);
    for (i = 3; i >= 0; i--) tp[i] ^= mirror(tp[7-i]);
    bpos = 0;
    for (i = 7; i > 1; i--) {
        n = ((tp[bpos >> 3] >> (bpos & 7)) | (tp[(bpos >> 3)+1] << (8-(bpos &
          7)))) & 63;
        enc[i] = e64[n];
        bpos += 6;
    }
    enc[8] = '\0';
    return enc;
}
#endif


/*
 * Checks for legal MS-DOS file names.
 * WARNING: This breaks if users add their own character devices to DOS.
 */


#define upper(c) (isupper(c) ? c : toupper(c))


static int is(a,b) /* case-blind test for equality */
char *a,*b;
{
    while (*a || *b) {
        if (upper(*a) != upper(*b)) return 0;
        a++;
        b++;
    }
    return 1;
}


int valid_msdos(path)
char *path;
{
    char *name,*here;

    if (strchr(path,'\\')) return 0; /* better save than sorry ... */
    if (strchr(path,':')) return 0; /* \B: is okay for DOS .. *sigh* */
    if (name = strrchr(path,'/')) name++;
    else name = path;
    if (is(name,"NUL") || is(name,"CON") || is(name,"PRN") || is(name,"AUX") ||
      is(name,"COM1") || is(name,"COM2") || is(name,"COM3") || is(name,"COM4")
      || is(name,"LPT1") || is(name,"LPT2") || is(name,"LPT3") ||
      is(name,"LPT4")) return 0;
    if (here = strchr(name,'.'))
        if (strchr(here+1,'.')) return 0;
    return 1;
}


/*
 * Checks for file identity. Requires SHARE. Works only if no other programs
 * are accessing the files.
 */


static void dos_close(fd)
int fd;
{
    union REGS r;

    r.h.ah = 0x3e;
    r.x.bx = fd;
    int86(0x21,&r,&r);
}


int same_file(a,b)
char *a,*b;
{
    int fd_a,fd_b,same;

    if ((fd_a = bdosptr(0x3d,a,0)) < 0) return 0;
    dos_close(fd_a);
    if ((fd_b = bdosptr(0x3d,b,0x10)) < 0) return 0;
    if ((fd_a = bdosptr(0x3d,a,0x10)) < 0) same = 1;
    else {
        dos_close(fd_a);
        same = 0;
    }
    dos_close(fd_b);
    return same;
}
