#include "config.h"
#ifdef USE_TIOCGETP
#include <sys/ioctl.h>
#endif
#include <signal.h>
#include <stdio.h>
#ifdef USE_TCGETA
#include <termio.h>
#endif

char *rcv_message();

extern void prepare_ipc(), exit(), send_message(), read_server();

void catch_sig();

extern int getpid(), fork(), kill();

int child, parent, do_not_echo;

#ifdef USE_TIOCGETP
struct sigvec sig;
#endif

int main(argc, argv)
    int argc;
    char **argv;
{
    char buff[200];
    char old_buff[sizeof buff];
    int port = 2000;
#ifdef USE_TCGETA
    struct termio termio;
#endif
#ifdef USE_TIOCGETP
    struct sgttyb sgttyb;
#endif

    if (argc == 2)
	port = atoi(argv[1]);
    else if (argc != 1) {
	fprintf(stderr, "Usage: %s [port]\n", argv[0]);
	exit(1);
    }
    prepare_ipc(port);
    parent = getpid();
    child = fork();
    if (child == 0) {
	read_server();
	exit(0);
    }
#ifdef USE_TCGETA
    signal(SIGUSR1, catch_sig);
#endif
#ifdef USE_TIOCGETP
    sig.sv_handler = (int (*)())catch_sig;
    sig.sv_mask = SIGUSR1;
    sig.sv_flags = SV_INTERRUPT;
    if (sigvec(SIGUSR1, &sig, 0) == -1) {
	perror("sigvec");
	exit(1);
    }
#endif
    while(1) {
	if (do_not_echo) {
	    fflush(stdout);
#ifdef USE_TIOCGETP
	    if (ioctl(1, TIOCGETP, &sgttyb) == -1) {
		perror("ioctl TIOCGETP");
		exit(1);
	    }
	    sgttyb.sg_flags &= ~ECHO;
	    if (ioctl(1, TIOCSETP, &sgttyb) == -1) {
		perror("ioctl TIOCSETP");
		exit(1);
	    }
#endif
#ifdef USE_TCGETA
	    if (ioctl(0, TCGETA, &termio) == -1) {
		perror("ioctl TCGETA");
		exit(1);
	    }
	    termio.c_lflag &= ~ECHO;
	    if (ioctl(0, TCSETA, &termio) == -1) {
		perror("ioctl TCGETA");
		exit(1);
	    }
#endif
	}
	if (fgets(buff, sizeof buff, stdin) == 0)
	    continue;
#ifdef USE_TIOCGETP
	if (do_not_echo) {
	    sgttyb.sg_flags |= ECHO;
	    if (ioctl(0, TIOCSETP, &sgttyb) == -1) {
		perror("ioctl TIOCSETP");
		exit(1);
	    }
	    do_not_echo = 0;
	}
#endif
#ifdef USE_TCGETA
	if (do_not_echo) {
	    termio.c_lflag |= ECHO;
	    if (ioctl(0, TCSETA, &termio) == -1) {
		perror("ioctl TCGETA");
		exit(1);
	    }
	    do_not_echo = 0;
	}
#endif
	if (strcmp(buff, "!\n") == 0)
	    strcpy(buff, old_buff);
	else
	    strcpy(old_buff, buff);
	send_message(buff);
    }
    /*NOTREACHED*/
}

void read_server() {
    char *str;

    while(1) {
	str = rcv_message();
	if (str[0] == '\0') {
	    (void)kill(parent, SIGINT);
	    exit(0);
	}
	if (strncmp(str, "Password: ", 10) == 0)
	    kill(parent, SIGUSR1);
	(void)printf("%s", str);
	(void)fflush(stdout);
    }
}

void catch_sig()
{
#ifdef USE_TCGETA
    signal(SIGUSR1, catch_sig);
#endif
    
    do_not_echo = 1;
}
