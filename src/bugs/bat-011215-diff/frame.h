/*
 * Frame
 *	++Jam
 */

#ifndef _FRAME_H_
#define _FRAME_H_

/* After all dynamic libraries are loaded, before cdroot */
extern void init_bug_handler();
extern void init_signal(int sig, int what);

/* Handlers */
extern void bug_handler();
extern void dump_stack_frame();

/* Define */
/* extern void error(char *text); */

#endif /* _FRAME_H_ */
