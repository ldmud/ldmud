#define PROT(proto) ()
#include <ioctl.h>
#include <stdio.h>
#include <osbind.h>
#include <mintbind.h>
#include <signal.h>
#include <string.h>
#include <ctype.h>
#include "comm.h"
#include "config.h"
#include "telnet.h"

volatile struct sock_buff my_buff;
int echo_mode = 1;

void do_print() {
        char *str;
        unsigned char *command;
        static last_was_iac = 0;

        if ( my_buff.out_count ) {
            str = my_buff.out;
            str[my_buff.out_count] = 0;
            if (last_was_iac) {
                last_was_iac = 0;
                command = str;
                goto interpret_command;
            }
            while(command = (unsigned char*)strchr(str, IAC)) {
                *command++ = '\0';
                printf("%s", str);
interpret_command:
                switch (*command++) {
                  struct sgttyb my_sb;

                  case WONT:
                    switch (*command++) {
                      case TELOPT_ECHO:
#if 0
                        gtty(0, &my_sb);
                        my_sb.sg_flags |= ECHO;
                        stty(0, &my_sb);
#endif
                        echo_mode = 1;
                        break;
                      default:
                        printf("Unknown telnet command : WONT %d.\n",
                          command[-1]);
                        exit(-1);
                    }
                    break;
                  case WILL:
                    switch (*command++) {
                      case TELOPT_ECHO:
#if 0
                        gtty(0, &my_sb);
                        my_sb.sg_flags &= ~ECHO;
                        stty(0, &my_sb);
#endif
                        echo_mode = 0;
                        break;
                      default:
                        printf("Unknown telnet command : WILL %d.\n",
                          *--command);
                        exit(-1);
                    }
                    break;
                  case IP:
                    printf("Exiting.\n");
                    exit(0);
                  case IAC:
                    /* quoted IAC */
                    printf("%c", IAC);
                    break;
                  case 0:
                    last_was_iac = 1;
                    my_buff.out_count=0;
                    return;
                  default:
                    --command;
                    if (isascii(*command)) {
                        break;
                    }
                    printf("Unknown telnet command : %d.\n", *command);
                    exit(-1);
                }
                str = (char *)command;
            }
            printf("%s", str);
            my_buff.out_count=0;
            fflush(stdout);
        }
}

void fputchar(c)
char c;
{
    putchar(c);
}

main() {
    volatile struct bind *bind_ptr;
    struct cookie *cookie_ptr,*scan_ptr;
    char c;

    cookie_ptr = (struct cookie *)Setexc(0x168,0);
    scan_ptr = cookie_ptr - 1;
    do {
        if ( (++scan_ptr)->c == BIND )
            break;
    } while ( scan_ptr->c );
    if (!scan_ptr->c) {
        exit (-1);
    }
    bind_ptr = (struct bind *)scan_ptr->v;
    for (;bind_ptr && bind_ptr->port != PORTNO ;bind_ptr = bind_ptr->next)
        printf("searching the port\n");
    if (!bind_ptr)
       exit(-1);
    my_buff.in_count = 0;
    my_buff.out_count = 0;
    (void)Psignal(SIGUSR1, do_print);
    my_buff.pid = Pgetpid();
    while (bind_ptr->incoming) printf("waiting for free 'incoming' slot\n");
    bind_ptr->incoming = &my_buff;
    printf("starting main loop\n");
    for(;;){
	    int i;

	    c=Fgetchar(0,0); /* getc() evaluates ECHO mode too late :-( */
            i=0;
            while ( my_buff.in_count ){
                if (!(i++&0xfff)) printf("");
                usleep(1000);
            }
            i=0;
	    while ( i < 160 && c != '\r' ) {
		if (c == 0x7f) c='\b';
	        if (echo_mode) {
	            if (c == '\b') {
	                if (i) {
	                    fputchar(c);
	                    fputchar(' ');
	                    fputchar(c);
	                    fflush(stdout);
	                    i--;
	                }
		        c=Fgetchar(0,0);
	                continue;
	            }
	            fputchar(c);
	            fflush(stdout);
	        }
		my_buff.in[i++] = c;
		c=Fgetchar(0,0);
	    }
	    my_buff.in[i++] = c;
	    my_buff.in[i++] = '\n';
	    if (echo_mode) {
	        fputchar(c);
	        fputchar('\n');
	        fflush(stdout);
	    }
            my_buff.in_count = i;
          }
}

