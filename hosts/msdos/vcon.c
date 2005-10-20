/* virtual terminals - WA */

/* updated Zilanthius 1994 */

#include <std.h>
#include <string.h>
#include <memory.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <dos.h>
#include <pc.h>
#include <go32.h>

#include "../../config.h"
#include "../../patchlevel.h"
#include "pcdef.h"
#include "c_msdos.h"
#include "version.h"
#include "color.h"


typedef unsigned short CHAR;

#define WIDTH          80
#define LINES          ScreenRows()
#define CONSOLES       10
#define STATUS_LINE    (LINES-2)
#define PAGE           (WIDTH*LINES)
#define PAGE0          (WIDTH*(LINES-1))
#define PAGE1          (WIDTH*(LINES-2))
#define PAGE2          (WIDTH*(LINES-3))
#define SHIFT          (4*PAGE0)
#define BUFFER         25000
#define ANSI_BUFFER    20
#define LINE_BUFFER    WIDTH
#define HISTORY_BUFFER 2000
#define HISTORY_SHIFT  500
#define TAB_WIDTH      7
#define KBD_QUEUE      256


/* Keyboard Codes */

#define F1              315
#define F10             324
#define PgUp            329
#define PgDn            337
#define CrsrUp          328
#define CrsrDn          336
#define CrsrLt          331
#define CrsrRt          333
#define Home            327
#define End             335
#define AltH            291
#define AltS            287
#define AltE            274
#define AltF            289
#define AltM            306
#define AltX            301
#define AltZ            300
#define DEL             339
#define INS             338
#define SHIFT_TAB       271
#define CTRL_BS         127
#define SPACE           32
#define ESC             27
#define CTRL_M          13    /* cr */
#define CTRL_J          10    /* lf */
#define TAB             9
#define BS              8
#define CTRL_G          7

#define IS_PRINT(X)     (X >= 0x20 && X <= 0x7e)
#define IS_INT(X)       (X >= 0x30 && X <= 0x39)
#define ZERO            0x30

#define ANSI_COLON      0x3b
#define ANSI_M          0x6d
#define ANSI_BRACKET    0x5b


#define error(c) { c_errno = (c); return -1; }

struct v_con {
    int echo,connected,ready,x;
    CHAR buffer[BUFFER+1], *top, *end, *screen;
    CHAR ansi_buffer[ANSI_BUFFER+1], ansi_ptr;
    CHAR line_buffer[LINE_BUFFER+1], *line_ptr;
    CHAR color;
} console[CONSOLES];

static unsigned video_address = 0xb8000UL;

static int current, hangup;
static CHAR bar_attrib;
static CHAR scroll;
static CHAR kbd_queue[KBD_QUEUE+1];
static CHAR *queue_end = kbd_queue;
static CHAR special = 0;
static CHAR history_buffer[HISTORY_BUFFER+1];
static CHAR *current_ptr = history_buffer;
static CHAR *history_ptr = history_buffer;
static CHAR *history_end = history_buffer;


/* prototypes */

static void spawn();
static void sys_editor();
static void sys_filemanager();
static void show_help();
static void beep();
static void page();
static void reset_page();
static void scan_kbd();
static CHAR wait_key();
static CHAR poll_key();
static CHAR get_special();
static CHAR any_key();
static CHAR *my_atoi();
static CHAR select_color();
static void init_display();
static void clear_display();
static int scrolling();
static void dsp_crlf();
static void dsp_bs();
static void dsp_tab();
static void status();
static void activate_console();
static void check_inactive();
static int access_console();
void vc_listen();
static int vc_read();
static int vc_write(int,char);
int vc_accept();
int vc_select();
static int vc_close();
static void vc_echo();
void vc_init();
void vc_shutdown();
static void init_buffers();
static void close_buffers();
static void shift_buffer();
static void append_buffer();
static void buffer_display();
static void display_screen();
static void clear_line_buffer();
static void append_line_buffer();
static void clear_ansi_buffer();
static void move_ansi_buffer();
static CHAR ansi_type();
static void set_ansi();
static void ansi_color();
static void toggle_video_mode();

/************************************************************************/
/* Bell */

static void beep()
{
  sound(2500);
  usleep(400);
  sound(0);
}


/***********************************************************************/
/* DOS shell escape */
extern char *mud_lib;
static void spawn()
{
    int out,x;

    if (getenv("COMSPEC") == NULL) return;
    ScreenClear();
    ScreenSetCursor(0,0);
    out = dup(1); /* 'cause stdout is usually redirected to a log file */
    dup2(2,1);
    system(getenv("COMSPEC"));
    dup2(out,1);
    close(out);
    chdir(mud_lib);
    activate_console(current);
}


/**********************************************************************/
/* system editor */

static void sys_editor()
{
    if (getenv("MUD_EDITOR") == NULL) return;
    system(getenv("MUD_EDITOR"));
    chdir(mud_lib);
    activate_console(current);
}


/**************************************************************************/
/* system file manager */

static void sys_filemanager()
{
    if (getenv("MUD_FILEMANAGER") == NULL) return;
    system(getenv("MUD_FILEMANAGER"));
    chdir(mud_lib);
    activate_console(current);
}



/*************************************************************************/
/* command history */

static int prev_history();

static void walk_history()
{
  CHAR *ptr; 
  
  ptr = history_buffer + HISTORY_SHIFT;
  while(*ptr++);
  current_ptr -= ptr - history_buffer;
  for(history_ptr = history_buffer; ptr < history_buffer + HISTORY_BUFFER;) {
    *history_ptr++ = *ptr++;
  }
  while(history_ptr < history_buffer + HISTORY_BUFFER) *history_ptr++ = 0;
  history_ptr = current_ptr;
  history_end = history_buffer;
  prev_history();
}

static int next_history()
{
  CHAR *ptr; 
  
  for(ptr = history_ptr;ptr < history_buffer + HISTORY_BUFFER && *ptr++;);
  if(ptr != history_buffer + HISTORY_BUFFER && *ptr) {
    history_ptr = ptr;
    return 1;
  }
  return 0;
}  
 
static int prev_history()
{
  CHAR *ptr;
  int flag = 1;

  if((ptr = history_ptr) != history_end) {
    do {
      while(ptr > history_end && *--ptr);
    }
    while(flag--);
    if(ptr != history_end) ptr++;
    if(*ptr) { 
      history_ptr = ptr;
      return 1;
    }
  }
  return 0;
}  

static void show_history()
{
  int i;
  CHAR buf[WIDTH];
  CHAR *ptr;

  for(i = 0, ptr = history_ptr; i < WIDTH && *ptr; i++) {
    if((*ptr & 0xff) == TAB) {
      do {
        buf[i++] = SPACE;
      }
      while(i < WIDTH && (i & TAB_WIDTH));
    }
    else {
      buf[i] = *ptr++;
    }
  }
  while(i < WIDTH) buf[i++] = SPACE;
  buffer_display(buf,WIDTH,PAGE0);
}

static void clean_history()
{
  CHAR *ptr = history_buffer;
  CHAR *cmd = history_ptr;
  CHAR *ptr2;

  while(ptr < cmd) {
    while(*ptr++ == *cmd++) {
      if(!*ptr && !*cmd) {
        ptr2 = ptr; 
        while(ptr > history_buffer && *--ptr);
        while(ptr > history_buffer) *--ptr2 = *--ptr;
        history_end = ptr2;
        while(ptr2 > history_buffer) *--ptr2 = 0;
        goto end_clean;
      }
    }
    cmd = history_ptr;
    while(*ptr++);
  }
  end_clean:
}


static void insert_history()
{
  CHAR *ptr, ch; 

  ptr = history_ptr;
  reset_page();
  if(!*ptr) {
    beep();
  }
  else {
    for(;(ch = *ptr & 0xff) && ch != CTRL_J; ptr++) { 
      *queue_end++ = ch;
      if(queue_end - kbd_queue == KBD_QUEUE) { 
        beep(); beep();
        break;
      }
    }
  }
}  

static void append_history(key)
int key;
{
  if(current_ptr == history_buffer + HISTORY_BUFFER - 1) walk_history();
  
  if(IS_PRINT(key) || key == TAB)
    key |= FG_G | FG_R | FG_B | FG_I;
  else if(current_ptr == history_buffer || *(current_ptr-1) == 0)
    return;
  else
    key = 0;
  if(!(*current_ptr++ = key)) {
    history_ptr = current_ptr;
    prev_history();
    clean_history();
    show_history();
  }
}

    
static void bs_history()
{
  if(current_ptr == history_buffer) return;
  if(*(current_ptr-1)) *--current_ptr = 0;
}


/**************************************************************************/
/* keyboard input queue handler */


static void scan_kbd()
{
    CHAR key;
    CHAR *walk;

    while (kbhit()) {
        if ((key = getkey()) == DEL || key == CTRL_BS)
            key = BS;
        if (key == SHIFT_TAB)
            key = TAB;
        if (key == CTRL_M)
            key = CTRL_J;

        if (key >= 256) {
            switch (key) {
                case AltH:
                    hangup = 1;
                    break;
                case AltS:
                    spawn();
                    break;
                case AltE:
                    sys_editor();
                    break;
                case AltF:
                    sys_filemanager();
                    break;
                case AltM:
                    toggle_video_mode();
                    break;
                case AltX:
                    shutdowngame();
                    break;
                case AltZ:
                    show_help();
                    break;
                case PgUp: case PgDn: case CrsrUp: case CrsrDn:
                case Home: case End:
                    page(key);
                    break;
                case CrsrRt: 
                    if (!next_history())
                        beep();
                    show_history();
                    break;
                case CrsrLt:
                    if (!prev_history())
                        beep();
                    show_history();
                    break;
                case INS:
                    insert_history();
                    return;
                default:
                    reset_page();
                    if (special) {
                        beep();
                    }
                    else {
                        special = key;
                    }
            } /* end switch */
        } /* end if key >= 256 */
        else {
            /* now insert key in buffer */
            reset_page();
            if ((queue_end - kbd_queue) == KBD_QUEUE) {
                beep();         /* buffer full */
            }
            else {
                *queue_end++ = key;
            }
        }
    } /* end while kbhit() */
}


static CHAR wait_key()
{
    CHAR *walk, key;

    do {
        scan_kbd();
    } while (queue_end == kbd_queue);

    /* take first char out of queue */
    key = *kbd_queue;
    for (walk = kbd_queue; walk < queue_end; walk++)
        *walk = *(walk + 1);

    queue_end--;
    return key;
}

static CHAR poll_key()
{
    scan_kbd();
    return (queue_end == kbd_queue) ? 0 : *kbd_queue;
}

static CHAR get_special()
{
    CHAR key;

    key = special;
    special = 0;
    return key;
}


static CHAR any_key()
{
    return special ? special : poll_key();
}


/**************************************************************************/
/* console handling */

static void activate_console(number)
int number;
{
    init_display(number);
    clear_display(console[number].color);
    current = number;
    if (console[current].buffer == NULL) {
        status(current, "UNAVAILABLE", 0);
        return;
    }
    display_screen(current);
    if (console[current].connected) {
        status(current, "CONNECTED", 0);
    }
    else {
        status(current, "AVAILABLE", 0);
    }
}


static void check_inactive()
{
    CHAR key;

    if (console[current].ready || console[current].connected || !any_key())
        return;

    while (key = get_special())
        if (key >= F1 && key <= F10)
            activate_console(key - F1);

    if (poll_key()) {
        wait_key();                     /* removes one key from queue */
        console[current].ready = 1;
    }
}


static int access_console(int vcon)
{
    if (vcon < 0 || vcon >= CONSOLES) {
        fprintf(stderr, "\nERROR: invalid console #%d\n", vcon);
        error(ECOMUSG);
    }
    if (console[vcon].connected)
        return 0;

    /* warning? and then raise an error? */
    fprintf(stderr, "\nWARNING: accessing inactive console #%d\n", vcon);
    error(ECOMUSG);
}


void vc_listen()
{
    init_buffers();
    activate_console(0);
    hangup = 0;
}


static int vc_write2(int vcon, char *msg, int len)
{
    if (access_console(vcon) < 0)
        error(ECOMUSG);

    while (len--)
        vc_write(vcon, *msg++);
    return 1;
}


static int vc_write(int vcon, char ch)
{
    switch (ch) {
        case '\n': 
            dsp_crlf(vcon);
            break;
        case '\t':
            dsp_tab(vcon);
            break;
        case '\b':
            dsp_bs(vcon);
            break;
        case '\a':
            beep();
            break;
        default :
            if (IS_PRINT(ch) || ch == ESC) {
                append_line_buffer(vcon, (CHAR) ch);
            }
    }
    return 1;
}


static int vc_read(int vcon, char *data, int length)
{
    int count;
    CHAR key;
    char tmp[6];
    int ansi;

    if (access_console(vcon) < 0)
        error(ECOMUSG);

    if (current != vcon) {
        if (poll_key() && !console[current].connected) {
            console[current].ready = 1;
        }
        return 0;
    }
    if (hangup) {
        hangup = 0;
        return 0;
    }
    for (count = 0; count < length && poll_key(); count++) {
        key = wait_key();

        if (console[vcon].echo) {
            switch (key) {
                case  CTRL_J:
                    strcpy(tmp, "\n");
                    break;
                case  BS:
                    strcpy(tmp, "\b \b");
                    break;
                default: 
                    if (!IS_PRINT(key) && key != ESC && key != CTRL_G) {
                        sprintf(tmp, "<%d>", key);
                    }
                    else {
                        sprintf(tmp, "%c", key);
                    }
            }
            vc_write2(vcon, tmp, strlen(tmp));

            if (IS_PRINT(key) || key == CTRL_J || key == TAB) {
                append_history(key);
            }
            else if (key == BS) {
                bs_history();
            }
        }
        *data++ = (char)key;
    }
    return count;
}


int vc_accept()
{
    int vcon;

    check_inactive();

    for (vcon = 0; vcon < CONSOLES; vcon++)
        if (console[vcon].ready) {
            console[vcon].connected = console[vcon].echo = 1;
            console[vcon].ready = 0;
            status(vcon, "CONNECTED", 0);
            return vcon;
        }

/*
    error(ECOMFLW);
    change to non fatal error code */
    error(EAGAIN);
}


/*ARGSUSED*/
int vc_select(mask, timeout, new_con)
int *mask;
struct timeval *timeout;
int *new_con;
{
    int vcon, copy, key;

    /*
     * get_special can't return more than one special
     * without re-reading the keyboard - hohum
     */
    while (key = get_special())
        if (key >= F1 && key <= F10)
            activate_console(key-F1);

    if (console[current].buffer && poll_key()) {
        /* look for new connections */
        if (*new_con && !console[current].connected) {
            console[current].ready = 1;
            *mask = 0;
            return 1;
        }
        *new_con = 0;

        /* see if there is input from current console (a connection) */
        if (console[current].connected && ((*mask >> current) & 1)) {
            *mask = 1 << current;
            return 1;
        }
    }
    /* to-do: read comm1.c and decide how to return.. */
    error(EINTR);
}


static int vc_close(int vcon)
{
    if (access_console(vcon) < 0)
        error(ECOMUSG);

    console[vcon].connected = 0;
    status(vcon, "AVAILABLE",0);
    return 0;
}


static void vc_echo(int vcon, int mode)
{
    if (access_console(vcon) >= 0)
        console[vcon].echo = mode;
}


void vc_init(CLASS_DEF *cd)
{
    vc_listen();
    cd->max_sess = CONSOLES;
    cd->close = vc_close;
    cd->read = vc_read;
    cd->write = vc_write;
    cd->echo = vc_echo;
}


void vc_shutdown()
{
    printf("\n[DETACHED]\n");
    ScreenSetCursor(0,0);
    ScreenClear();
}


/*************************************************************************/


static CHAR *my_atoi(value,str)
CHAR *value;
CHAR *str;
{
    while (*str && !IS_INT(*str))
        str++;
    for (*value = 0; IS_INT(*str); str++)
        *value = *value*10 + *str-ZERO;
    return str;
}


/*********************************************************************/
/* set color attributes */

static CHAR select_color(color)
char *color;
{
  CHAR attrib = 0;


  do {
    switch(*color) {
      case 'r':
       attrib |= FG_R;
      break;

      case 'g':
       attrib |= FG_G;
      break;
      
      case 'b':
       attrib |= FG_B;
      break;

      case 'R':
       attrib |= BG_R;
      break;

      case 'G':
       attrib |= BG_G;
      break;
      
      case 'B':
       attrib |= BG_B;
      break;

      case 'i': 
       attrib |= FG_I;
      break;

      case 'f':
       attrib |= BLINK;
      break;
    }
  }
  while(*++color);
  return attrib;
}


/********************************************************************/
/* initialise */

static void init_display(int vcon)
{
    char mono[] = "mono";
  
    if (getenv("MUD_MODE") != NULL && strcmp(getenv("MUD_MODE"), mono) == 0) {
        video_address = 0xb0000UL;
        console[vcon].color = 0x0700;
        bar_attrib = 0x70;
    }
    else {
        video_address = 0xb8000UL;
        if (getenv("MUD_SCREEN") != NULL) {
            console[vcon].color = select_color(getenv("MUD_SCREEN"));
        }
        else {
            console[vcon].color = FG;
        }
        if (getenv("MUD_BAR") != NULL) {
            bar_attrib = select_color(getenv("MUD_BAR")) >> 8;
        }
        else {
            bar_attrib = BG >> 8;
        }
   }
}

static void init_buffers()
{
    int con = CONSOLES;
    int i;

    while (con--) {
        console[con].connected = console[con].ready = console[con].x = 0;
        console[con].top = console[con].screen = console[con].buffer;
        console[con].end = console[con].buffer + PAGE1;
        init_display(con);  
        for (i = 0; i < BUFFER; i++) {
            console[con].buffer[i] = (SPACE | console[con].color);
        }
        clear_line_buffer(con);
    }

    memset(history_buffer, 0, sizeof(history_buffer));
    show_history();
}


static void shift_buffer(int vcon) 
{
  int i;

  if ((console[vcon].end - console[vcon].buffer) >= BUFFER - PAGE) {
    for (i = 0; i + SHIFT < BUFFER && console[vcon].buffer[i+SHIFT]; i++) {
      console[vcon].buffer[i] = console[vcon].buffer[i+SHIFT];
    }
    while(i < BUFFER) 
      console[vcon].buffer[i++] = ((SPACE & 0xff) | console[vcon].color);
    console[vcon].end -= SHIFT;
    console[vcon].top = console[vcon].end - PAGE2;
    console[vcon].buffer[BUFFER] = 0;
  }
}


static void append_buffer(int vcon, CHAR *ptr)
{
  for (;*ptr; ptr++) {
    shift_buffer(vcon);
    *console[vcon].end++ = *ptr;
  }
  while ((console[vcon].end - console[vcon].buffer)%WIDTH) {
    shift_buffer(vcon);
    *console[vcon].end++ = (SPACE | console[vcon].color);
  }
  console[vcon].top = console[vcon].end - PAGE2;
}


static int scrolling() {
  if(scroll) {
    scroll = (scroll != '*') ? '*' : '#';
    ScreenPutChar(scroll, bar_attrib, WIDTH - 2, STATUS_LINE);
    return 1;
  }
  return 0;
}


static void dsp_crlf(int vcon)
{
  if (!console[vcon].line_buffer[0]) { 
    console[vcon].line_buffer[0] = (SPACE | console[vcon].color);
  }
  append_buffer(vcon, console[vcon].line_buffer);
  clear_line_buffer(vcon);
  if (current == vcon) {
    if (!scrolling())
      display_screen(vcon);
  }
} 


static void dsp_bs(con)
int con;
{
  if (!console[con].x) { 
    beep();
    return;
  }
  *--console[con].line_ptr = 0;
  if (con == current) ScreenSetCursor(STATUS_LINE-1,--console[con].x);
}


static void dsp_tab(con)
int con;
{
    int length;

    for(length = 0; console[con].line_buffer[length]; length++);
    if(length >= WIDTH-2-TAB_WIDTH) {
      dsp_crlf(con);
    }
    else {
      do {
        append_line_buffer(con,SPACE);
      } while (++length & TAB_WIDTH);
    }
}

static void display_line_buffer(con)
int con;
{
    int i;
    CHAR buf[LINE_BUFFER];
    
    for (i = 0; i < LINE_BUFFER && console[con].line_buffer[i]; i++) {
        buf[i] = console[con].line_buffer[i];
    }
    console[con].x = i%WIDTH;
    ScreenSetCursor(STATUS_LINE - 1, console[con].x);
    while (i < LINE_BUFFER)
        buf[i++] = SPACE | console[con].color;
    buffer_display(buf,LINE_BUFFER,PAGE2);
}


static void display_screen(con)
int con;
{
  buffer_display(console[con].top,PAGE2,0);
  console[con].x = 0;
  ScreenSetCursor(STATUS_LINE-1,0);
  display_line_buffer(con);
  status(con,((console[con].connected) ? "CONNECTED" : "AVAILABLE"),0);
}

   
static void clear_line_buffer(con) 
int con;
{
  int i = LINE_BUFFER;

  console[con].line_ptr = console[con].line_buffer;
  while(i--) console[con].line_buffer[i] = 0;
  console[con].x = 0;
}


static void append_line_buffer(con,ch)
int con;
CHAR ch;
{
  if(!console[con].ansi_ptr) {
    if(ch == ESC) {
      console[con].ansi_buffer[console[con].ansi_ptr++] = ESC;
      return;
    }
    else {
      *console[con].line_ptr++ = ch | console[con].color;
    }
  }
  else {
    if(console[con].ansi_ptr + 1 == ANSI_BUFFER) {
      move_ansi_buffer(con);
      return;
    }
    console[con].ansi_buffer[console[con].ansi_ptr++] = ch;
    switch(console[con].ansi_ptr) {
      case 2:
     if(ch != ANSI_BRACKET) {
       move_ansi_buffer(con);
     }
     break;
      case 3:
     if(!IS_INT(ch)) {
       move_ansi_buffer(con);
     }
     break;
      default:
     if(!IS_INT(ch) && ch != ANSI_COLON && ch != ANSI_M) {
       move_ansi_buffer(con);
     }
     else if(ch == ANSI_M) {
       set_ansi(con,console[con].ansi_buffer);
       clear_ansi_buffer(con);
     }
    }
    return;
  }
  if(console[con].x >= WIDTH-1) {
    append_buffer(con,console[con].line_buffer);
    clear_line_buffer(con);
    if(con == current) {
      ScreenSetCursor(STATUS_LINE-1,0);
      if(!scrolling()) display_screen(con);
    }
  }
  else if(current == con) {
    if(!scroll) {
      ScreenPutChar(ch,console[con].color>>8,console[con].x,STATUS_LINE-1);
    }
    ScreenSetCursor(STATUS_LINE-1,++console[con].x);
  }
}


/* ansi simulation */

static void clear_ansi_buffer(con)
int con;
{
  int i = ANSI_BUFFER;

  console[con].ansi_ptr = 0;
  while(i--) console[con].ansi_buffer[i] = 0;
}


static void move_ansi_buffer(con)
int con;
{
  CHAR *ptr;
  for(ptr = console[con].ansi_buffer;*ptr; ptr++, console[con].x++) {
    if(console[con].x >= WIDTH-1) {
      append_buffer(con,console[con].line_buffer);
      clear_line_buffer(con);
    }
    *console[con].line_ptr++ = *ptr | console[con].color;
  }
  if(current == con) ScreenSetCursor(STATUS_LINE-1,console[con].x);
  clear_ansi_buffer(con);
  if(current == con) display_screen(con);
}


static CHAR ansi_type(str)
CHAR *str;
{
  while(*str == ANSI_COLON || IS_INT(*str)) *str++;
  return *str;
}


static void set_ansi(con,ptr)
int con;
CHAR *ptr;
{
  CHAR attrib;

  if(*ptr == ESC && *(ptr+1) == ANSI_BRACKET && IS_INT(*(ptr+2))) {
    switch(ansi_type(ptr+2)) {
      case ANSI_M:
     *ptr++;
     while(*ptr++ != ANSI_M) {
       ptr = my_atoi(&attrib,ptr);
       ansi_color(con,attrib);
     }
      break;
    }
  }
}
   

static void ansi_color(con,attrib) 
int con;
CHAR attrib;
{
    switch(attrib) {
      /* OFF */
      case 0:
     init_display(con);
      break;

      /* BOLD */
      case 1:
     if(getenv("MUD_BOLD") != NULL) {
       console[con].color = select_color(getenv("MUD_BOLD"));
     }
     else {
       console[con].color |= FG_I;
     }
      break;
      
      /* FAINT */
      case 2:
     console[con].color &= ~FG_I;
      break;

      /* underline mono display only */
      case 4:
     console[con].color |= FG_B;
      break;

      /* BLINK */
      case 5:
     console[con].color |= BLINK;
      break;

      /* REVERSE BLINK */
      case 6:
     console[con].color = ~console[con].color;
     console[con].color |= BLINK;
      break;

      /* REVERSE */
      case 7:
     console[con].color = ~console[con].color;
     console[con].color &= ~BLINK;
      break;
     
      /* foreground *
      case 30: /* black */
     console[con].color &= ~FG_R & ~FG_G & ~FG_B;
      break;

      case 31: /* red */
     console[con].color &= ~FG_R & ~FG_G & ~FG_B;
     console[con].color |= FG_R;
      break;

      case 32: /* green */
     console[con].color &= ~FG_R & ~FG_G & ~FG_B;
     console[con].color |= FG_G;
      break;

      case 33: /* yellow */
     console[con].color &= ~FG_R & ~FG_G & ~FG_B;
     console[con].color |= FG_R | FG_G;
      break;

      case 34: /* blue */
     console[con].color &= ~FG_R & ~FG_G & ~FG_B;
     console[con].color |= FG_B;
      break;

      case 35: /* magenta */
     console[con].color &= ~FG_R & ~FG_G & ~FG_B;
     console[con].color |= FG_R | FG_B;
      break;

      case 36: /* cyan */
     console[con].color &= ~FG_R & ~FG_G & ~FG_B;
     console[con].color |= FG_G | FG_B;
      break;

      case 37: /* white */
     console[con].color |= FG_R | FG_G | FG_B;
      break;

      /* Background */
      case 40: /* black */
     console[con].color &= ~BG_R & ~BG_G & ~BG_B;
      break;

      case 41: /* red */
     console[con].color &= ~BG_R & ~BG_G & ~BG_B;
     console[con].color |= BG_R;
      break;

      case 42: /* green */
     console[con].color &= ~BG_R & ~BG_G & ~BG_B;
     console[con].color |= BG_G;
      break;

      case 43: /* yellow */
     console[con].color &= ~BG_R & ~BG_G & ~BG_B;
     console[con].color |= BG_R | BG_G;
      break;

      case 44: /* blue */
     console[con].color &= ~BG_R & ~BG_G & ~BG_B;
     console[con].color |= BG_B;
      break;

      case 45: /* magenta */
     console[con].color &= ~BG_R & ~BG_G & ~BG_B;
     console[con].color |= BG_R | BG_B;
      break;

      case 46: /* cyan */
     console[con].color &= ~BG_R & ~BG_G & ~BG_B;
     console[con].color |= BG_G | BG_B;
      break;

      case 47: /* white */
     console[con].color |= BG_R | BG_G | BG_B;
      break;

    }
}



/* clear screen */

static void clear_display(color)
CHAR color;
{
    int i;
    CHAR buf[PAGE1];

    for (i = 0; i < PAGE1; i++)
        buf[i] = SPACE | color;
    dosmemput(buf, PAGE1*2, video_address);
}
  

/* display buffer */

static void buffer_display(buf, length, screen_offset)
CHAR *buf;
int length;
int screen_offset;
{
    if (length + screen_offset > PAGE)
        length = PAGE - screen_offset;

    dosmemput(buf, length*2, video_address + 2*screen_offset);
}


/***************************************************************************/
/* status bar */

static void status(number, st_name, scroll)
int number,scroll;
short int *st_name;
{
  char msg[WIDTH];
  int walk;
  char mode[7];

  sprintf(msg,
    " #%d  [%s]%s   MSDOS %s Amylaar%s%s %s Driver (Help: Alt-Z) ",
    number+1,
    st_name,
    scroll ? "SCROLLED" : "",
    MSDOS_VERSION,
    GAME_VERSION,
    PATCH_LEVEL,

    /* if X-Comp was written as X-Compat the below would cause
        something to malfunction.
        stack would get overwritten and other nice things */
#ifdef COMPAT_MODE
    strcpy(mode,"Compat"));
#else
#ifdef NATIVE_MODE
    strcpy(mode,"Native"));
#else
    strcpy(mode,"X-Comp"));
#endif /* NATIVE_MODE */
#endif /* COMPAT_MODE */

  for (walk = 0; msg[walk]; walk++) {
      ScreenPutChar(msg[walk],bar_attrib,walk,STATUS_LINE);
  }
  while(walk < WIDTH) { 
    ScreenPutChar(SPACE,bar_attrib,walk++,STATUS_LINE);
  }
}


/**************************************************************************/
/* help */

static void show_help() 
{
  int attrib, i, x_pos, y_pos;
  char help[] = \
  "                          -=[ Help ]=-\n\n"
  "                       Alt-h........Hang Up\n"\
  "                       Alt-s........Spawn\n"\
  "                       Alt-e........Dos Editor\n"\
  "                       Alt-f........Dos File Manager\n"\
  "                       Alt-m........Toggle Video Mode\n"\
  "                       Alt-x........Shutdown Game\n"\
  "                       Alt-z........Help\n"\
  "                       F1-F10.......Virtual Consoles\n"\
  "                       <home>.......Top of Screen Buffer\n"\
  "                       <end>........End of Screen Buffer\n"\
  "                       < ->, <- >...History Buffer\n"\
  "                       <Insert>.....Insert Current History Line\n"\
  "Press Key to Continue.";

  if (getenv("MUD_SCREEN") != NULL) {
    attrib = select_color(getenv("MUD_SCREEN"));
  }
  else {
    attrib = FG;
  }
  clear_display(attrib);
  for(i = 0, x_pos = 0, y_pos = 0; i < strlen(help); i++) {
    if(help[i] == '\n') {
      y_pos++; x_pos = 0;
    }
    else {
      ScreenPutChar(help[i],attrib >> 8,x_pos++,y_pos);
    }
  }
  ScreenSetCursor(y_pos,x_pos++);
  wait_key();
  activate_console(current);
}


/* page movement */

static void page(key)
CHAR key;
{
    CHAR *ptr;

    if (!scroll) scroll = '#';

    ScreenPutChar(scroll, bar_attrib, WIDTH - 2, STATUS_LINE);

    ptr = console[current].screen;
    switch (key) {
        case PgUp:
            if (ptr - PAGE1 < console[current].buffer)
                ptr = console[current].buffer;
            else
                ptr -= PAGE1;
            break;

        case CrsrUp: 
            if (ptr - WIDTH < console[current].buffer)
                ptr = console[current].buffer;
            else
                ptr -= WIDTH;
            break;
    
        case PgDn:
            if (ptr + PAGE1 > console[current].top)
                ptr = console[current].top;
            else
                ptr += PAGE1;
            break;

        case CrsrDn: 
            if (ptr + WIDTH > console[current].top)
                ptr = console[current].top;
            else
                ptr += WIDTH;
            break;

        case End:
            ptr = console[current].top;
            break;

        case Home:
            ptr = console[current].buffer;
            break;
    }

    console[current].screen = ptr;
    buffer_display(ptr, PAGE1, 0);
}


/* reset page */

static void reset_page()
{
    scroll = 0;
    ScreenPutChar(SPACE,bar_attrib,WIDTH-2,STATUS_LINE);

    if (console[current].screen != console[current].top) {
        console[current].screen = console[current].top;
        display_screen(current);
    }
}


/********************************************************************/
/* video mode */

/* from gppconio src */

enum text_modes { LASTMODE=-1, BW40=0, C40, BW80, C80, MONO=7, C4350=64 };

void toggle_video_mode()
{
    union REGS regs;
    static int mode = C4350;
    int mode_to_set;

    regs.h.ah = 0x12;
    regs.h.bl = 0x10;
    regs.h.bh = 0xff;
    int86(0x10, &regs, &regs);
    if(regs.h.bh == 0xff) return; /* mono */

    if(mode == C4350)
      mode = C80;
    else
      mode = C4350;
    mode_to_set = mode;

    if (mode == C4350)
        /* 
         * just set mode 3 and load 8x8 font, idea taken 
         * (and code translated from Assembler to C)
         * form Csaba Biegels stdvga.asm
         */
      mode_to_set = 0x03;  
    regs.h.ah = 0x00; /* set mode */
    regs.h.al = mode_to_set;
    int86(0x10, &regs, &regs);

   /* 
    * enable cursor size emulation, see Ralf Browns
    * interrupt list
    */
    regs.h.ah = 0x12;
    regs.h.bl = 0x34;
    regs.h.al = 0x00; /* 0: enable (1: disable) */
    int86(0x10, &regs, &regs);
    if (mode == C4350) 
    {
      /* load 8x8 font */
      regs.x.ax = 0x1112;         
      regs.x.bx = 0;
      int86(0x10, &regs, &regs);
    }
    display_screen(current);
}    
