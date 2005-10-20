/*
 *  Client for OS/2 lpmud
 */

#include <os2.h>
#include <string.h>
#include <io.h>
#include <stdio.h>

#include "socket.h"

#define BUF_SIZE MAX_SOCKET_PACKET_SIZE

void print_pipe();
void error();

const char *version = "mudclient 1.0";
char in_buffer[BUF_SIZE];
char out_buffer[BUF_SIZE];
int fd;

main (int argc, char *argv[])
{
  int thread_id, ret, ph, len;
  ULONG count;
  char s[50];

  sprintf(s, REQUEST_PIPE, 7680);

  if ((ph = open(s, 2)) < 0)
    error("Connection refused (Can't open request pipe)");
  if ((ret = read(ph, &in_buffer, 1)) != 1)
    error("Invalid data in request pipe");
  close(ph);

  sprintf(s, CLIENT_PIPE, (int) in_buffer[0]);

  if (DosWaitNPipe(s, 10000L))
    error("Connection timed out");

  if ((fd = open(s, 2)) < 0)
    error("Can't open client pipe");

  if (DosCreateThread(&thread_id, print_pipe, 0L, 0L, 8192L))
    error("Can't create second thread");

  printf("Connected.\n");

  for (;;)
    if (gets (in_buffer, BUF_SIZE - 2) != NULL)
    {
      len = strlen (in_buffer);
      in_buffer[len++] = '\n';
      in_buffer[len++] = '\0';
      write(fd, in_buffer, len);
    }

  exit(0);
}

VOID
print_pipe(ULONG ulThreadArg)
{
  int bc;

  for (;;)
    if (bc = read(fd, out_buffer, BUF_SIZE))
      write(1, out_buffer, bc);
    else
      error ("Connection closed by foreign host");
}

void
error (char *msg)
{
  printf("%s: %s.\n",version,msg);
  exit(10);
}

