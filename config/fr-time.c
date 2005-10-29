/*
 * This version of frontend uses timeout to read from socket.
 */
#include <signal.h>
#include <stdio.h>
#include <errno.h>
#include <curses.h>

char *rcv_message();

extern void prepare_ipc(), exit(), send_message(), read_server();
void catch_alarm();
extern int errno;
char prompt[100];

int screen_garbled;

int main() {
    char buff[200];
    int len = 0, ch;

    initscr();
    cbreak();
    noecho();
    nonl();
    scrollok(stdscr, 1);
    prepare_ipc();
    signal(SIGALRM, catch_alarm);
    alarm(1);
    while(1) {
	read_server();
	if (screen_garbled && len > 0) {
	    buff[len] = '\0';
	    printw("\n%s", prompt);
	    refresh();
	}
	screen_garbled = 0;
	ch = getch();
	if (ch != 0 && ch != -1) {
	    switch(ch) {
	    default:
		buff[len++] = ch;
		addch(ch);
		refresh();
		break;
	    case '\b':
		if (len == 0)
		    break;
		printw("\b \b");
		len--;
		refresh();
		break;
	    case 'u' - 'a' + 1:
		printw("^U\n");
		refresh();
		len = 0;
		break;
	    case '\n':
	    case '\r':
		addstr("\n");
		refresh();
		buff[len] = '\n';
		buff[len+1] = '\0';
		send_message(buff);
		len = 0;
		break;
	    }
	}
    }
}

void read_server() {
    char *str;

    while(1) {
	str = rcv_message();
	if (str == 0)
	    return;
	if (str[0] == '\0') {
	    endwin();
	    exit(0);
	}
	if (!screen_garbled)
	    save_prompt();
	printw("%s", str);
	screen_garbled = 1;
	refresh();
    }
}

/*
 * Do nothing but capturing the alarm, and setting up a new.
 */
void catch_alarm()
{
    signal(SIGALRM, catch_alarm);
    alarm(1);
}

save_prompt()
{
    int x, y;
    int i;

    getyx(stdscr, y, x);
    if (x == 0) {
	prompt[0] = '\0';
	return;
    }
    for (i=0; i<x; i++) {
	move(y, i);
	prompt[i] = inch();
    }
    move(y, x);
    prompt[i] = '\0';
}

	
