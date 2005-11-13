/*---------------------------------------------------------------------------
**
**   PlayMud
**
** Copyright © 1992-1996 by Lars Düning  -  All rights reserved.
** Permission granted for non-commercial use.
**
**---------------------------------------------------------------------------
** An simple interface to a running AMud.
**
**  CLI-Usage: PlayMud ?
**             PlayMud [?] [<console> | Current] [Port <portname>] [Noecho]
**                         [Quiet|Verbose|Debug]
**  AmiExpress:
**             PlayMud <nodeno>
**
**  Args   : <console>  : Consolename, e.g. 'CON:0/11/640/220'
**           <portname> : Portname of LPmud to connect, default is "8888"
**           Current    : use the current console
**           <nodeno>   : AmiExpress-Node to use.
**  Options: ?       : prints some help
**           Noecho  : lines input won't be echoed if output came in between
**           Verbose : more messages from the pgm
**           Debug   : even more messages from the pgm
**
**    Result:  0: ok
**            20: illegal arg or something similar deadly
**
**  WB-Usage: FRONTEND= [DOS|STANDARD|AMIEXPRESS]
**            TYPE=     [NOECHO] [QUIET|VERBOSE|DEBUG]
**            CON=      [<console>]
**            PORT=     [<portname>]
**
**  Icons may be of type TOOL or (fileless) PROJECT.
**  Errors will cause a DisplayBeep.
**---------------------------------------------------------------------------
**  C: DICE 3.0
**  The source was reluctantly converted from the original Oberon sources,
**  so don't expect high aesthetic quality here.
**---------------------------------------------------------------------------
**  09-Apr-93 [lars] reluctantly converted from Oberon
**  03-May-93 [lars] Merged in patches from John Fehr
**                   CNN: is no longer mandatory (no requesters when
**                   not found).
**  06-Sep-93 [lars] The current console can be opened explicitely.
**  04-Oct-93 [lars] On shell start, .info-Files are evaluated before
**                   parsing the cli args.
**  16-Oct-93 [lars] Source reorganisation.
**  10-Nov-93 [lars] AmiExpress arguments are parsed.
**  12-Nov-93 [lars] Prepared Frontend part for AmiExpress.
**  15-Nov-93 [lars] Synced with PlayMud 1.7
**  13-Feb-94 [lars] IAC GA is now properly ignored.
**  25-Feb-94 [lars] DosHndlFront didn't write asynchronously, now it does.
**  17-Jul-94 [lars] Updated for DICE 3.0: some type casts added.
**  19-Nov-94 [lars] Close the readPort as fast as possible on shutdown.
**  02-Jan-96 [lars] Shutdown prompt "Press RETURN" is not printed if current
**                   console is used.
**                   echo-off is properly undone on premature ends.
**  01-May-96 [lars] CNN: is no longer tried.
**                   Added attribute "/CLOSE" to default CON: console.
**---------------------------------------------------------------------------
*/

#include <sys/types.h>

#include <exec/types.h>
#include <exec/io.h>
#include <exec/libraries.h>
#include <exec/execbase.h>
#include <exec/ports.h>
#include <intuition/intuition.h>
#include <workbench/icon.h>
#include <workbench/startup.h>
#include <workbench/workbench.h>
#ifdef INCLUDE_VERSION
#include <clib/alib_protos.h>
#include <clib/dos_protos.h>
#include <clib/exec_protos.h>
#include <clib/icon_protos.h>
#include <clib/intuition_protos.h>
#include <dos/dos.h>
#include <dos/dosextens.h>
/* isn't C wonderful? :-( */
#else
#include <libraries/dos.h>
#include <libraries/dosextens.h>

  /* APTR -> BPTR */
#define MKBADDR(x) (((LONG)(x)) >> 2)
  /* 1.3-CloseWindow in 2.0-Notation */
#define IDCMP_CLOSEWINDOW CLOSEWINDOW
#endif

#include <stdlib.h>
#include <stdio.h>
#include <strings.h>

#include "mudmsgs.h"
#include "telnet.h"

/*-------------------------------------------------------------------------*/

struct Library *IconBase;
struct IntuitionBase *IntuitionBase;
extern struct ExecBase *SysBase;     /* DICE will open this for us */

#ifdef INCLUDE_VERSION
char ver[] = "\0$VER: PlayMud 1.9c (01.05.96) - OS 2.0";
#else
char ver[] = "\0$VER: PlayMud 1.9c (01.05.96) - OS 1.3";
#endif

#define VERSION "1.9c"
#define REVISION "[ld-960501]"

/*=========================================================================*/

/* Global */

#define MaxLineLen 256  /* Max length of line read */

int             frontend;
int             verbosity;            /* Level of verbosity */
BOOL            noEcho;
BOOL            curCon;               /* True if current console was requested */
BOOL            currentCon;           /* True if current console is used */
char            conName[MaxLineLen];
char            mudName[MaxLineLen];  /* Portname of the LPmud, default is 8888 */
int             aeNode;
int             result;
char            line[MaxLineLen];     /* Line read */
struct MsgPort *creadPort;
signed char     readSig;

#define QUIET   0
#define NORMAL  1
#define VERBOSE 2
#define DEBUG   3

#define DOSHANDLER 0
#define AEXPRESS 1

/* all frontends */

char               dhLine[MaxLineLen];  /* Line buffer */
int                pos;
BOOL               writeOccured;        /* did output clutter our input ? */

/* DosHndlFront */

struct FileHandle *con = NULL;
struct Window     *window;              /* Pointer to Console Window if any */
signed char        ConWinSig;           /* Signal returned by window */
struct MsgPort    *ConReplyPort;        /* Replyport for writes to handler */


/* AmiExpressFront */

char              AePortName[MaxLineLen];  /* Name of our AExpress port */
struct MsgPort   *AeReplyPort;             /* Replyport for synchronous messages to AExpress */
signed char       AefrontendSig;           /* Signal number for 'frontend signals' */
struct JHMessage *AeJhmsg;                 /* Message used for sync io */
BOOL              AeGotCR;

/* FrontEnd */

/* PlayMud */

BOOL help, illArg, connected;
BOOL invis = FALSE;
char *arg;

struct MsgPort *readPort = NULL, *replyPort = NULL;
char   portName[MaxLineLen], hostPort[MaxLineLen];

/*=========================================================================*/

/* Global */

/*-------------------------------------------------------------------------*/
BOOL SafePutMsg (struct Message *msg, char *portName)

/* Safely put a message to a named global port.
** Return success.
*/

{
  struct MsgPort *port;

  Forbid();
  port = (struct MsgPort *) FindPort (portName);
  if (port != NULL) PutMsg (port, msg);
  Permit();
  return (port != NULL);
}

/*-------------------------------------------------------------------------*/

BOOL GlobalIntro (void) {
  static int callMeOnce = 0;

  if (callMeOnce++) return TRUE;
  frontend = DOSHANDLER;
  verbosity = NORMAL;
  curCon = FALSE;
  currentCon = FALSE;
  *conName = '\0';
  strcpy (mudName, "8888");
  noEcho = FALSE;
  connected = FALSE;
  result = 0;
  creadPort = (struct MsgPort *) CreatePort (NULL, 0);
  if (creadPort == NULL) {
    puts ("Can't open port for ConsoleStream.");
    return FALSE;
  }
  readSig = creadPort->mp_SigBit;
  pos = -1; dhLine[0] = 0; dhLine[MaxLineLen-1] = 0;
  writeOccured = FALSE;
  return TRUE;
}

void GlobalOutro (void) {
  static int callMeOnce = 0;

  if (callMeOnce++) return;
  if (creadPort != NULL) { DeletePort (creadPort); creadPort = NULL; }
}

/*=========================================================================*/

/* DosHndlFront */

/*-------------------------------------------------------------------------*/

typedef struct WritePacket {
  struct StandardPacket pkt;
  char   txt[0];
} WritePacket;

/*-------------------------------------------------------------------------*/
void ReUseStdPacket ( struct StandardPacket *packet
                    , struct MsgPort        *port
                    , long                   type
                    , long                   arg1
                    , long                   arg2
                    , long                   arg3
                    )
{
  if (packet != NULL) {
    packet->sp_Msg.mn_Node.ln_Name = (char *)&(packet->sp_Pkt);
    packet->sp_Msg.mn_ReplyPort = port;
    packet->sp_Pkt.dp_Link = (struct Message *) packet;
    packet->sp_Pkt.dp_Port = packet->sp_Msg.mn_ReplyPort;
    packet->sp_Pkt.dp_Type = type;
    packet->sp_Pkt.dp_Arg1 = arg1;
    packet->sp_Pkt.dp_Arg2 = arg2;
    packet->sp_Pkt.dp_Arg3 = arg3;
  }
}

/*-------------------------------------------------------------------------*/
struct StandardPacket *CreateStdPacket ( struct MsgPort *port
                                       , long            type
                                       , long            arg1
                                       , long            arg2
                                       , long            arg3
                                       )
{
  struct StandardPacket *packet;

  packet = (struct StandardPacket *) malloc (sizeof (struct StandardPacket));
  ReUseStdPacket (packet, port, type, arg1, arg2, arg3);
  return packet;
}

/*-------------------------------------------------------------------------*/
void GetConInfo ()

/* Get the conUnit info for the opened console stream.
*/

{
  struct Message *reply;
  struct StandardPacket *packet;
  struct InfoData *iData;
  BPTR iDataB;

  window = NULL;
  if (con->fh_Type != NULL) {
    iData = (struct InfoData *) malloc (sizeof (struct InfoData));
    if (iData != NULL) {
      iDataB = MKBADDR(iData);
      packet = CreateStdPacket (creadPort, ACTION_DISK_INFO, iDataB, 0, 0);
      if (packet != NULL) {
        PutMsg (con->fh_Type, (struct Message*)packet);
        WaitPort (creadPort);
        reply = (struct Message *) GetMsg(creadPort);
        if (packet->sp_Pkt.dp_Res1 != DOSFALSE) {
          window = (struct Window *) iData->id_VolumeNode;
        }
        free (packet);
      }
      else {
        puts ("Warning: Not enough mem for console info.");
      }
      free (iData);
    }
    else {
      puts ("Warning: Not enough mem for console info.");
    }
  }
}

/*-------------------------------------------------------------------------*/
BOOL ConOpenCurrent ()

/* Open the current console (either Input() or Output()) for I/O.
** Success is returned.
*/

{
  if (con != NULL) {
    puts ("Error: Open console already exists.");
    return FALSE;
  }

  ConReplyPort = (struct MsgPort *)CreatePort(NULL, 0);
  if (ConReplyPort == NULL) {
    puts("Error: Can't open replyport for console messages.");
    return FALSE;
  }

  if (Input() != NULL) {
    con = BADDR(Input());
    if (verbosity > QUIET)
      puts("  Console is current input");
  }
  else if (Output() != NULL) {
    con = BADDR(Output());
    if (verbosity > QUIET)
      puts("  Console is current output");
  }
  else {
    puts ("Error: No current console.");
    DeletePort(ConReplyPort); ConReplyPort = NULL;
    return FALSE;
  }
  if (!IsInteractive(MKBADDR(con))) {
    puts("Error: Console is not interactive.");
    con = NULL;
    DeletePort(ConReplyPort); ConReplyPort = NULL;
    return FALSE;
  }
  currentCon = TRUE;
  GetConInfo();
  return TRUE;
}

/*-------------------------------------------------------------------------*/
BOOL ConOpen (char *stream)

/* Open the named stream for I/O. If no name is given, following defaults
** are tried in given order:
**   CON:0/11/640/225/A-LPmud/CLOSE
** Success is returned.
*/

{
  long len;
  struct Process *me;
  APTR oldWin;

  /* When trying optional console handlers, use this sequence to suppress
  * requesters:
  *   oldWin = me->pr_WindowPtr;
  *   me->pr_WindowPtr = (APTR) -1L;
  *   con = BADDR(Open ("CNN:/11///AmigaLPmud/c", MODE_NEWFILE));
  *   me->pr_WindowPtr = oldWin;
  */

  if (con != NULL) {
    puts ("Error: Open console already exists.");
    return FALSE;
  }

  ConReplyPort = (struct MsgPort *)CreatePort(NULL, 0);
  if (ConReplyPort == NULL) {
    puts("Error: Can't open replyport for console messages.");
    return FALSE;
  }

  len = strlen(stream);
  con = NULL; currentCon = FALSE;
  if (!len) {
    con = BADDR(Open ("CON:/11///AmigaLPmud/CLOSE", MODE_NEWFILE));
    if (con != NULL) {
      if (verbosity)
        puts ("  Console CON:/11///AmigaLPmud/CLOSE ");
    }
    else {
      puts ("Error: Can't open any console.");
      DeletePort(ConReplyPort); ConReplyPort = NULL;
      return FALSE;
    }
  }
  else {
    con = BADDR(Open (stream, MODE_NEWFILE));
    if (con == NULL) {
      fputs ("Error: Can't open ", stdout); puts (stream);
      DeletePort(ConReplyPort); ConReplyPort = NULL;
      return FALSE;
    }
    if (verbosity > QUIET) {
      fputs ("  Console", stdout); puts (stream);
    }
  }

  if (!IsInteractive(MKBADDR(con))) {
    puts("Error: Console is not interactive.");
    Close(MKBADDR(con));
    con = NULL;
    DeletePort(ConReplyPort); ConReplyPort = NULL;
    return FALSE;
  }

  /* Stream opened, now get info about it if ConsoleHandler */
  GetConInfo();
  return TRUE;
}

/*-------------------------------------------------------------------------*/
void ConClose ()

/* Closes any open console stream */

{
  struct Message *msg;

  if (con == NULL) return;
  if (!currentCon) Close (MKBADDR(con));
  con = NULL; window = NULL; currentCon = FALSE; ConWinSig = -1;
  if (verbosity > QUIET) puts ("Console stream closed.");
  if (creadPort == NULL) return;
  if (verbosity > QUIET) { fputs ("Collecting left packets...", stdout); fflush (stdout); }
  while ((msg = (struct Message *) GetMsg(ConReplyPort)) != NULL);
  DeletePort(ConReplyPort); ConReplyPort = NULL;
  while ((msg = (struct Message *) GetMsg(creadPort)) != NULL);
  /* the mem or our msgs will be automagically freed on programs exit */
  if (verbosity > QUIET) puts ("done.");
}

/*-------------------------------------------------------------------------*/
ULONG ConFrontendSig (void)

/* Return the signal which is set if a message arrives from the user.
** Return -1 for no signal.
** This also sets the console window to accept closeWindow events.
*/

{
  ULONG signals;

  ConWinSig = -1;
  if (con != NULL && window != NULL) {
    if (window->UserPort == NULL) signals = IDCMP_CLOSEWINDOW;
    else signals = window->IDCMPFlags | IDCMP_CLOSEWINDOW;
    ModifyIDCMP (window, signals);
    if (window->UserPort != NULL) {
      ConWinSig = window->UserPort->mp_SigBit;
    }
  }
  signals = 0;
  if (ConWinSig != -1)
    signals = 1<<ConWinSig;
  if (ConReplyPort != NULL)
    signals |= 1<<(ConReplyPort->mp_SigBit);
  return signals;
}

/*-------------------------------------------------------------------------*/
void ConHandleYourSig (ULONG signals, BOOL *abort)

/* If PlayMud got a 'signals' (= a signal from the console window),
** it calls this functions and leaves it to us to handle the signal.
** If an aborting condition is detected, 'abort' is to be set to TRUE,
** else is mustn't be changed.
*/

{
  struct IntuiMessage *winMsg;
  WritePacket         *pkt;

  if (con != NULL && window != NULL && ConWinSig != -1 && (1<<ConWinSig) & signals) {
    winMsg = (struct IntuiMessage *) GetMsg (window->UserPort);
    while (winMsg != NULL) {
      if (IDCMP_CLOSEWINDOW & winMsg->Class) *abort = TRUE;
      ReplyMsg((struct Message *)winMsg);
      winMsg = (struct IntuiMessage *) GetMsg (window->UserPort);
    }
  }
  if (ConReplyPort != NULL && (1<<(ConReplyPort->mp_SigBit)) & signals)
    while ((pkt = (WritePacket *)GetMsg(ConReplyPort))) {
      if (verbosity > VERBOSE) fputs(" ConReply ", stdout);
      free(pkt);
    }
}

/*-------------------------------------------------------------------------*/
void ConWrite (char ch)

/* Write a single character to console */

{
  WritePacket *packet;

  if (con == NULL) return;
  packet = (WritePacket *)malloc(sizeof(WritePacket)+2);
  if (packet == NULL) {
    puts("Error: Not enough mem for packet - character discarded.");
    return;
  }
  ReUseStdPacket(packet, ConReplyPort, ACTION_WRITE, con->fh_Arg1, (long)(packet->txt), 1);
  packet->txt[0] = ch;
  packet->txt[1] = '\0';
  PutMsg(con->fh_Type, (struct Message *)packet);
  if (verbosity > VERBOSE) fputs("ToCon ", stdout);
  if (pos > 0) writeOccured = TRUE;
}

/*-------------------------------------------------------------------------*/
void ConPutString (char *text, long len)

/* Write a string of given length to console */

{
  WritePacket *packet;

  if (con == NULL) return;
  packet = (WritePacket *)malloc(sizeof(WritePacket)+len+1);
  if (packet == NULL) {
    puts("Error: Not enough mem for packet - text discarded.");
    return;
  }
  ReUseStdPacket(packet, ConReplyPort, ACTION_WRITE, con->fh_Arg1, (long)(packet->txt), len);
  strncpy(packet->txt, text, len);
  packet->txt[len] = '\0';
  PutMsg(con->fh_Type, (struct Message *)packet);
  if (verbosity > VERBOSE) fputs("ToCon ", stdout);
  if (pos > 0) writeOccured = TRUE;
}

/*-------------------------------------------------------------------------*/
void ConWriteString (char *text)

/* Write a null-terminated string to console */

{
  ConPutString(text, strlen(text));
}

/*-------------------------------------------------------------------------*/
void ConQueueRead (struct StandardPacket *packet)

/* Sends one read request for one character to console.
** If a packet is to be reused, it may be given, else NULL.
** As buffer, dhLine[++pos] is given.
** This fun must be called directly after ConOpen() once to initiate reads !
*/

{
  if (con == NULL) return;
  if (pos < MaxLineLen-3) pos++;
  if (packet != NULL) {
    ReUseStdPacket (packet, creadPort, ACTION_READ, con->fh_Arg1, (long) &(dhLine[pos]), 1);
  }
  else {
    packet = CreateStdPacket (creadPort, ACTION_READ, con->fh_Arg1, (long) &(dhLine[pos]), 1);
  }
  if (packet != NULL) PutMsg (con->fh_Type, (struct Message *)packet);
  else puts ("Error: Not enough mem for packet.");
}

/*-------------------------------------------------------------------------*/
BOOL ConReadChar ( void )

/* Reads in a received ReadRequest and queues up a new one.
** If the request completed a line, it will be copied in to line.
** If necessary, this line will be echoed.
** Returns condition "line completed".
*/

{
  struct StandardPacket *packet;
  BOOL                   rc;

  if (con == NULL) return FALSE;
  rc = FALSE;
  packet = (struct StandardPacket *) GetMsg(creadPort);
  if (packet == NULL) {
    if (verbosity > VERBOSE) puts ("Warning: No packet arrived.");
    return FALSE;
  }

  if (packet->sp_Pkt.dp_Res1 == 1) {
    switch (dhLine[pos]) {
      case '\n':
      case 0x1c:
      case '\0':
                 dhLine[pos+1] = 0;
                 if (writeOccured && !noEcho) {
                   ConWriteString ("\n"); ConWriteString (dhLine);
                 }
                 strcpy (line, dhLine); pos = -1;
                 writeOccured = FALSE;
                 rc = TRUE;
                 break;
    }
  }
  ConQueueRead(packet);
  return rc;
}

/*------------------------------------------------------------------------*/
void ConOutro (void)
{
  GlobalOutro();
  if (ConReplyPort != NULL) { DeletePort(ConReplyPort); ConReplyPort = NULL; }
}

BOOL ConIntro (void)
{
  static int callMeOnce = 0;
  if (callMeOnce++) return;
  window = NULL;
  ConWinSig = -1;
  ConReplyPort = NULL;
  if (!GlobalIntro())
    return FALSE;
  return TRUE;
}

/*=========================================================================*/

/* AmiExpressFront */

/*-------------------------------------------------------------------------*/

#define AEMaxChars 200

struct JHMessage {
  struct Message msg;
  char           string[AEMaxChars];  /* info buffer */
  long           data;                /* read/write, result indicator */
  long           command;             /* Command code sent */
  long           nodeID;              /* reserved */
  long           lineNum;             /* reserved */
  ULONG          signal;              /* reserved */
  struct Process *task;               /* current nodes task address;
                                      ** used just by the BB_GETTASK command */
};

#define JH_REGISTER    1   /* Register with AmiExpress node */
#define JH_SHUTDOWN    2   /* Unlink from AmiExpress node */
#define JH_WRITE       3   /* Write message to user */
#define JH_PM          5   /* Request input from user */
#define JH_HK          6   /* Request input from user */
#define DT_TIMEOUT   125   /* Set/Query the door timeout */
#define BB_STATUS    129   /* Query the node status */

/*-------------------------------------------------------------------------*/
void AeReuseMessage ( struct JHMessage *msg, struct MsgPort *port, long command)
{
  if (msg != NULL) {
    msg->msg.mn_Node.ln_Type = NT_MESSAGE;
    msg->msg.mn_Length = sizeof(struct JHMessage);
    msg->msg.mn_ReplyPort = port;
    msg->command = command;
    msg->string[0] = 0;
  }
}

/*-------------------------------------------------------------------------*/
struct JHMessage *AeCreateMessage ( struct MsgPort *port, long command)
{
  struct JHMessage *msg;

  msg = (struct JHMessage *) malloc(sizeof(struct JHMessage));
  AeReuseMessage(msg, port, command);
  return msg;
}

/*-------------------------------------------------------------------------*/
BOOL AeSyncIO ( struct JHMessage *msg )

/* Send the given message to AmiExpress and wait for its reply.
** If the PutMsg() failed, FALSE is returned, else TRUE.
*/

{
  struct Message *rmsg;

  if (!SafePutMsg((struct Message *)msg, AePortName)) { return FALSE; }
  while (1) {
    WaitPort(AeReplyPort);
    do {
      rmsg = GetMsg(AeReplyPort);
      if (rmsg == (struct Message *)msg) { return TRUE; }
    } while (rmsg != NULL);
  }
}

/*-------------------------------------------------------------------------*/
BOOL AeOpen (long number)

/* Link Playmud as door with AmiExpress, node <number>.
** Success is returned.
*/

{
  if (AeReplyPort != NULL) {
    puts("Error: AeOpen link to AmiExpress already exists.");
    return FALSE;
  }

  AeReplyPort = (struct MsgPort *)CreatePort(NULL, 0);
  if (AeReplyPort == NULL) {
    puts("Error: Can't open replyport for AmiExpress messages.");
    return FALSE;
  }

  AefrontendSig = AllocSignal(-1);

  if (AefrontendSig == -1) {
    puts("Error: Can't allocate frontend signal.");
    DeletePort(AeReplyPort); AeReplyPort = NULL;
    return FALSE;
  }

  /* Compute the portname we have to talk to */
  sprintf(AePortName, "AEDoorPort%d", number);

  /* Register with AmiExpress */
  if (AeJhmsg != NULL) {
    AeReuseMessage(AeJhmsg, AeReplyPort, JH_REGISTER);
  } else {
    AeJhmsg = AeCreateMessage(AeReplyPort, JH_REGISTER);
  }
  if (AeJhmsg == NULL) {
    puts("Error: Not enough mem for message.");
    DeletePort(AeReplyPort); AeReplyPort = NULL;
    FreeSignal(AefrontendSig); AefrontendSig = -1;
    return FALSE;
  }
  if (!AeSyncIO(AeJhmsg)) {
    fputs("Error: AmiExpress port '", stdout);
    fputs(AePortName, stdout);
    puts("' not found.");
    free(AeJhmsg); AeJhmsg = NULL;
    DeletePort(AeReplyPort); AeReplyPort = NULL;
    FreeSignal(AefrontendSig); AefrontendSig = -1;
    return FALSE;
  }

  /* Set timeout to one second. */
  AeReuseMessage(AeJhmsg, AeReplyPort, DT_TIMEOUT);
  *(long *)(AeJhmsg->string) = 1;
  if (!AeSyncIO(AeJhmsg)) {
    fputs("Error: AmiExpress port '", stdout);
    fputs(AePortName, stdout);
    puts("' not found.");
    free(AeJhmsg); AeJhmsg = NULL;
    DeletePort(AeReplyPort); AeReplyPort = NULL;
    FreeSignal(AefrontendSig); AefrontendSig = -1;
    return FALSE;
  }

  if (verbosity != QUIET) {
    fputs("  AmiExpress ", stdout);
    puts(AePortName);
  }

  AeGotCR = FALSE;
  return TRUE;
}

/*-------------------------------------------------------------------------*/
void AeClose ( void )

/* Closes any open AmiExpress link */

{
  struct Message *msg;

  if (AefrontendSig != -1) { FreeSignal(AefrontendSig); AefrontendSig = -1; }
  if (AeReplyPort == NULL) { return; }

  /* Unlink from AmiExpress */
  msg = (struct Message *)AeCreateMessage(AeReplyPort, JH_SHUTDOWN);
  if (msg == NULL) {
    puts("Error: Not enough mem for shutdown message.");
  }
  else if (!AeSyncIO((struct JHMessage *)msg)) {
    fputs("Error: AmiExpress port '", stdout);
    fputs(AePortName, stdout);
    puts("' not found.");
  }
  free(msg);
  if (verbosity != QUIET) { puts("Unlinked from AmiExpress."); }

  if (verbosity != QUIET) { fputs("Collecting left messages...", stdout); }

  msg = GetMsg(AeReplyPort);
  while (msg != NULL)
    msg = GetMsg(AeReplyPort);
  DeletePort(AeReplyPort); AeReplyPort = NULL;

  if (creadPort != NULL) {
    msg = GetMsg(creadPort);
    while (msg != NULL)
      msg = GetMsg(creadPort);
  }
  /* the mem of our msgs will be automagically freed on programs exit */
  if (verbosity != QUIET) { puts("done."); }
  if (AeJhmsg != NULL) { free(AeJhmsg); AeJhmsg = NULL; }
}

/*-------------------------------------------------------------------------*/
ULONG AeFrontendSig ( void )

/* Return the signal which is set if a message arrives from the user.
** For AmiExpress, the IO routines set the signal if a loss of carrier
** is detected.
** Return 0 for no signal.
*/

{
  if (AefrontendSig != -1)
    return 1 << AefrontendSig;
  return 0;
}

/*-------------------------------------------------------------------------*/
void AeHandleYourSig (ULONG signals, BOOL *abort)

/* If PlayMud got 'signals' (== a signal from the console window),
** it calls this functions and leaves it to us to handle the signal.
** If an aborting condition is detected, 'abort' is to be set to TRUE,
** else is mustn't be changed.
** For AmiExpress a set signal always signals a loss of carrier, and
** thus aborts the session.
*/

{
  if ((AefrontendSig != -1) && ((1<<AefrontendSig) & signals)) {
    SetSignal(0, 1<<AefrontendSig); /* Just in case */
    *abort = TRUE;
  }
}

/*-------------------------------------------------------------------------*/
void AePutString (char *text, long len)

/* Write a string of given length to user */

{
  long start, actlen;

  if (AeReplyPort == NULL) { return; }
  start = 0;
  while (start < len) {
    actlen = len-start;
    if (actlen >= AEMaxChars) {
      actlen = AEMaxChars-1;
    }
    AeReuseMessage(AeJhmsg, AeReplyPort, JH_WRITE);
    strncpy(AeJhmsg->string, text+start, actlen);
    AeJhmsg->string[actlen] = 0;
    if (!AeSyncIO(AeJhmsg) && (AefrontendSig != -1)) {  /* Uh-oh! */
      Signal(FindTask(NULL), 1<<AefrontendSig);
      return;
    }
    start += actlen;
  }
  if ((len > 0) && (pos > 0)) { writeOccured = TRUE; }
}

/*-------------------------------------------------------------------------*/
void AeWrite (char ch, BOOL echo)

/* AeWrite a single character to console
** If <echo> is TRUE, the output is just the echo of a key input, so
** writeOccured is not changed.
*/

{
  if ((AeReplyPort == NULL) || (ch == 0)) { return; }
  AeReuseMessage(AeJhmsg, AeReplyPort, JH_WRITE);
  AeJhmsg->string[0] = ch;
  AeJhmsg->string[1] = 0;;
  if (!AeSyncIO(AeJhmsg) && (AefrontendSig != -1)) {  /* Uh-oh! */
    Signal(FindTask(NULL), 1<<AefrontendSig);
  }
  if (!echo && (pos > 0)) { writeOccured = TRUE; }
}

/*-------------------------------------------------------------------------*/
void AeWriteString (char *text)

/* AeWrite a null-terminated string to console */

{
  AePutString(text, strlen(text));
}

/*-------------------------------------------------------------------------*/
void AeQueueRead (struct JHMessage *msg)

/* Sends one read request for one character to console.
** If a message is to be reused, it may be given, else NULL.
** This fun must be called directly after AeOpen() once to initiate reads !
*/

{
  if (AeReplyPort == NULL) { return; }
  if (msg != NULL) {
    AeReuseMessage(msg, creadPort, JH_HK);
  } else {
    msg = AeCreateMessage(creadPort, JH_HK);
  }
  if (msg != NULL) {
    msg->data = AEMaxChars-2;
    /* We have to clear the string field by hand since AExpress does not
    ** always put a trailing 0 after the string returned. :-(
    */
    msg->string[0] = 0;
    CopyMem(msg->string, msg->string+1, AEMaxChars-1);
    if (!SafePutMsg((struct Message *)msg, AePortName)) {  /* Uh-oh! */
      if (AefrontendSig != -1) {
        Signal(FindTask(NULL), 1<<AefrontendSig);
      }
      free(msg);
    }
  } else {
    puts("Error: Not enough mem for message.");
  }
}

/*-------------------------------------------------------------------------*/
BOOL AeReadChar ( void )

/* Reads in a received ReadRequest and queues up a new one.
** If the request completed a dhLine, it will be copied in to g.dhLine.
** If necessary, this dhLine will be echoed.
** Returns condition "dhLine completed".
*/

{
  struct JHMessage *msg;
  BOOL   rc, timeout, wrOccTmp;

  if (AeReplyPort == NULL) { return FALSE; }
  rc = FALSE;
  timeout = FALSE;

  msg = (struct JHMessage *)GetMsg(creadPort);
  if (msg == NULL) {
    if (verbosity > VERBOSE) {
      puts("Warning: No message arrived.\n");
    }
    return FALSE;
  }

  if (msg->data == -1) { /* User maybe got lost */
    AeReuseMessage(AeJhmsg, AeReplyPort, BB_STATUS);
    if (  !AeSyncIO(AeJhmsg)
       || ( (AeJhmsg->string[0] == 'O') && (AeJhmsg->string[1] == 'F'))
   ) {
      if (verbosity > NORMAL) {
        puts("Lost carrier.\n");
      }
      if (AefrontendSig != -1) {
        Signal(FindTask(NULL), 1<<AefrontendSig);
      }
      free(msg);
      return FALSE;
    }
    msg->string[0] = 0;
    timeout = TRUE;
  }

  if (!timeout && AeGotCR && (msg->string[0] == '\n')) {
    msg->string[0] = 0;
  }

  if (msg->string[0] != 0) {
    AeGotCR = FALSE;
    switch (msg->string[0]) {
    case '\n':
    case '\r':
    case 0x1c:
      AeWrite('\n', TRUE);
      dhLine[pos] = '\n';
      dhLine[pos+1] = 0;
      if (writeOccured && !noEcho) {
        AeWriteString("\n"); AeWriteString(dhLine);
      }
      strcpy(line, dhLine);
      pos = 0;
      dhLine[0] = 0;
      writeOccured = FALSE;
      AeGotCR = msg->string[0] == '\r';
      rc = TRUE;
      break;
    case '\b':
      if (pos > 0) {
        pos--;
        wrOccTmp = writeOccured;
        AeWriteString("\b \b");
        if (pos == 0)
          writeOccured = FALSE;
        else
          writeOccured = wrOccTmp;
      }
      break;
    default:
      AeWrite(msg->string[0], TRUE);
      dhLine[pos] = msg->string[0];
      if (pos < MaxLineLen-2)
        pos++;
    }
  }

  AeQueueRead(msg);
  return rc;
}

/*------------------------------------------------------------------------*/
void AeOutro (void)
{
  GlobalOutro();
  if (AeReplyPort != NULL) { DeletePort(AeReplyPort); AeReplyPort = NULL; }
  if (AefrontendSig != -1) { FreeSignal(AefrontendSig); AefrontendSig = -1; }
  if (AeJhmsg != NULL) { free(AeJhmsg); AeJhmsg = NULL; }
}

BOOL AeIntro (void)
{
  static int callMeOnce = 0;
  if (callMeOnce++) return;
  AeReplyPort = NULL; AefrontendSig = -1;
  strcpy(AePortName, "");
  AeJhmsg = NULL;
  if (!GlobalIntro())
    return FALSE;
  return TRUE;
}

/*=========================================================================*/

/* FrontEnd */

/*-------------------------------------------------------------------------*/
BOOL FeOpen ()

/* Open the frontend to the user */

{
  switch (frontend) {
  case DOSHANDLER:
    if (curCon) {
      if (!ConOpenCurrent())
        return FALSE;
    }
    else if (!ConOpen(conName))
      return FALSE;
    return TRUE;
    break;
  case AEXPRESS:
    return AeOpen(aeNode);
    break;
  }
  return FALSE;
}

/*-------------------------------------------------------------------------*/
void FeClose ()

/* Closes any open console stream */

{
  switch (frontend) {
  case DOSHANDLER:
    ConClose();
    break;
  case AEXPRESS:
    AeClose();
    break;
  }
}

/*-------------------------------------------------------------------------*/
ULONG FrontendSig (void)

/* Return the signal which is set if a message arrives from the user.
** Return 0 for no signal.
*/

{
  switch (frontend) {
  case DOSHANDLER:
    return ConFrontendSig();
    break;
  case AEXPRESS:
    return AeFrontendSig();
    break;
  }
  return 0;
}

/*-------------------------------------------------------------------------*/
void HandleYourSig (ULONG signals, BOOL *abort)

/* If PlayMud got 'signals' from the frontend, it calls this functions and
** leaves it to us to handle the signal.
** If an aborting condition is detected, 'abort' is to be set to TRUE,
** else is mustn't be changed.
*/

{
  switch (frontend) {
  case DOSHANDLER:
    ConHandleYourSig(signals, abort);
    break;
  case AEXPRESS:
    AeHandleYourSig(signals, abort);
    break;
  }
}

/*-------------------------------------------------------------------------*/
void FeWrite (char ch)

/* Write a single character to the user */

{
  switch (frontend) {
  case DOSHANDLER:
    ConWrite(ch);
    break;
  case AEXPRESS:
    AeWrite(ch, FALSE);
    break;
  }
}

/*-------------------------------------------------------------------------*/
void FeWriteString (char *text)

/* Write a null-terminated string to user */

{
  switch (frontend) {
  case DOSHANDLER:
    ConWriteString(text);
    break;
  case AEXPRESS:
    AeWriteString(text);
    break;
  }
}

/*-------------------------------------------------------------------------*/
void FePutString (char *text, long len)

/* Write a string of given length to user */

{
  switch (frontend) {
  case DOSHANDLER:
    ConPutString(text, len);
    break;
  case AEXPRESS:
    AePutString(text, len);
    break;
  }
}

/*-------------------------------------------------------------------------*/
void QueueRead ( void )

/* Sound out one read request to initiate asynchronous reads.
** Upon return of that request to readPort, readSig has to be set.
*/

{
  switch (frontend) {
  case DOSHANDLER:
    ConQueueRead(NULL);
    break;
  case AEXPRESS:
    AeQueueRead(NULL);
    break;
  }
}

/*-------------------------------------------------------------------------*/
BOOL ReadChar ( void )

/* Reads in a received ReadRequest and queues up a new one.
** If the request completed a line, it will be copied in to line.
** If necessary, this line will be echoed.
** Returns condition "line completed".
*/

{
  switch (frontend) {
  case DOSHANDLER:
    return ConReadChar();
    break;
  case AEXPRESS:
    return AeReadChar();
    break;
  }
  return FALSE;
}

/*------------------------------------------------------------------------*/
void FeOutro (void)
{
  static int callMeOnce = 0;
  if (callMeOnce++) return;
  FeClose();
  ConOutro();
  AeOutro();
  GlobalOutro();
}

BOOL FeIntro (void)
{
  static int callMeOnce = 0;
  if (callMeOnce++) return;
  if (!GlobalIntro())
    return FALSE;
  if (!AeIntro())
    return FALSE;
  if (!ConIntro())
    return FALSE;
  return TRUE;
}

/*=========================================================================*/

/* PlayMud */

/*-------------------------------------------------------------------------*/
void PrintTitle (BOOL tofront)

{
  if (!tofront) {
    putchar((char) '\n');
    putchar((char) 0x9b);
    fputs("0;32mPlayMud v", stdout);
    fputs(VERSION, stdout);
    putchar((char) 0x9b);
    fputs("0;33m  ", stdout);
    fputs(REVISION, stdout);
    putchar((char) 0x9b);
    puts("0;39m  --  (C)opyright 1992-1996 by Lars Düning. Freeware.\n");
  }
  else if (!currentCon) {
    FeWrite((char) 0x9b);
    FeWriteString("0;32mPlayMud v");
    FeWriteString(VERSION);
    FeWrite((char) 0x9b);
    FeWriteString("0;33m  ");
    FeWriteString(REVISION);
    FeWrite((char) 0x9b);
    FeWriteString("0;39m  --  (C)opyright 1992-1996 by Lars Düning. Freeware.\n");
  }
}

/*-------------------------------------------------------------------------*/
BOOL SetupAll ( void )

/* SetupAll: creates e.g. our replyPort, etc
**
** Resultat == FALSE: something went wrong
**          == TRUE : ok
*/

{
  if (!FeOpen())
    return FALSE;

  replyPort = (struct MsgPort *) CreatePort (NULL, 0);
  if (replyPort == NULL) return FALSE;
  sprintf (portName, "PlayMud%lx", FindTask(NULL));
  readPort = (struct MsgPort *) CreatePort (portName, 0);
  if (readPort == NULL) return FALSE;

  if (verbosity > QUIET) putchar ('\n');
  if (verbosity > VERBOSE) {
    fputs ("Our port: ", stdout);
    puts (portName);
  }
  return TRUE;
}

/*-------------------------------------------------------------------------*/
void ShutdownAll ( void )

/* ShutdownAll: macht alles, was SetupAll reservierte, rückgängig
*/

{
  if (readPort != NULL) { DeletePort (readPort); readPort = NULL; }
  FeClose();
  if (replyPort != NULL) { DeletePort (replyPort); replyPort = NULL; }
}

/*-------------------------------------------------------------------------*/
BOOL Connect (char *port, char *portName)

/* Tries to connect to host at port, telling him our readPortName.
** Host replies with his port, which is stored in hostPort.
** Returns success.
*/

{
  struct connect_message *cmsg, *reply;
  BYTE sig;
  ULONG signal;

  cmsg = (struct connect_message *) malloc (sizeof (struct connect_message));
  if (cmsg == NULL) {
    FeWriteString ("Not enough mem for ConnectMsg.");
    if (verbosity != QUIET) puts ("Not enough mem for ConnectMsg.");
    return FALSE;
  }
  cmsg->Msg.mn_Node.ln_Type = NT_MESSAGE;
  cmsg->Msg.mn_Length = sizeof (struct connect_message);
  cmsg->Msg.mn_ReplyPort = replyPort;
  cmsg->port_name = portName;
  if (!SafePutMsg ((struct Message *)cmsg, port)) {
    FeWriteString ("Host not found.\n");
    free (cmsg);
    return FALSE;
  }
  sig = replyPort->mp_SigBit;
  while (1) {
    signal = Wait (SIGBREAKF_CTRL_E | (1L << sig));
    if ((1L << sig) & signal) {
      reply = (struct connect_message *) GetMsg (replyPort);
      if (reply != NULL) {
        CopyMem (reply->port_name, hostPort, strlen(reply->port_name));
        free (cmsg);
        if (verbosity > VERBOSE) {
          fputs ("LPmud port: ", stdout);
          puts (hostPort);
        }
        return TRUE;
      }
    }
    if (SIGBREAKF_CTRL_E & signal) {
      if (!curCon) FeWriteString ("** User Abort\n");
      puts ("** User Abort");
      return FALSE;
    }
  }
  return FALSE;
}

/*-------------------------------------------------------------------------*/
void PrintLPmsg (char *text, long len, BOOL *abort)

/* Prints a text received from LPmud to the console.
** Handles also echo on/off and connection close.
*/

{

#define BufLen 10240

  char ntext[BufLen];
  char *i;
  int j;

  if (invis) { FeWrite ((char)0x9b); FeWriteString ("0m"); }
  i = text; j = 0;
  while (len > 0) {
    if ((*i == (char)IAC) && (i[1] == (char)IP)) { /* Prelude to connection close */
      if (verbosity > VERBOSE) { fputs ("[ip] ", stdout); fflush (stdout); }
      if (invis) {
        invis = FALSE;
        FeWrite ((char)0x9b); FeWriteString ("0m");
      }
      i++;
      j--; len--;
    }
    else if ((*i == (char)IAC) && (i[1] == (char)GA)) { /* FABRICATI DIEM, PVNC! */
      if (verbosity > VERBOSE) { fputs ("[ga] ", stdout); fflush (stdout); }
      i++;
      j--; len--;
    }
    else if ((*i == (char)IAC) && (i[2] == 0x01)) {
      if (i[1] == (char)WILL) {  /* No local echo */
        if (verbosity > VERBOSE) { fputs ("[invis] ", stdout); fflush (stdout); }
        invis = TRUE;
      }
      else if (i[1] == (char)WONT) { /* Local echo */
        if (verbosity > VERBOSE){ fputs ("[vis] ", stdout); fflush (stdout); }
        FeWrite ((char)0x9b); FeWriteString ("0m");
        invis = FALSE;
      }
      else if (i[1] == (char)DO) { /* Close connection */
        if (verbosity > 1) { fputs ("[abort] ", stdout); fflush (stdout); }
        *abort = TRUE;
      }
      else {
        if (verbosity > VERBOSE) { fputs ("[] ", stdout); fflush (stdout); }
      }
      i += 2;
      j--; len -= 2;
    }
    else if ((*i == (char)IAC) && (i[1] == (char)WONT)) { /* partial "local echo" */
      if (verbosity > VERBOSE) { fputs ("[vis] ", stdout); fflush (stdout); }
      FeWrite ((char)0x9b); FeWriteString ("0m");
      invis = FALSE;
      i++;
      j--; len--;
    }
    else ntext[j] = *i;
    i++; len--; j++;
    if (j >= BufLen) {
      FePutString (ntext, BufLen);
      j = 0;
    }
  }
  if (verbosity > VERBOSE) { putchar (' '); fflush (stdout); }
  if (j > 0) FePutString (ntext, j);
  if (*abort) { FeWriteString ("\nConnection closed by host.\n"); connected = FALSE; }
  if (invis) { FeWrite ((char) 0x9b); FeWriteString ("8m"); }

#undef BufLen
}

/*-------------------------------------------------------------------------*/
void TellLPinput (char *text)

/* Tell LPmud a line of text, return success */

{
  long len;
  struct data_message *dmsg;

  len = strlen(text)+1;
  if ((dmsg = (struct data_message *)malloc (sizeof (struct data_message))) != NULL) {
    if ((dmsg->buffer = (char *) malloc (len)) == NULL) {
      free (dmsg); dmsg = NULL;
    }
  }
  if (dmsg == NULL) {
    FeWriteString ("Failed to send text: not enough mem.\n");
    if (verbosity != QUIET) puts ("Not enough mem for DataMsg.");
    return;
  }
  dmsg->Msg.mn_Node.ln_Type = NT_MESSAGE;
  dmsg->Msg.mn_Length = sizeof (struct data_message);
  dmsg->Msg.mn_ReplyPort = replyPort;
  CopyMem (text, dmsg->buffer, len-1);
  dmsg->buffer[len-1] = 0;
  dmsg->length = len-1;
#if 0 /* Much too much output */
  if (verbosity > VERBOSE)
    printf ("[%ld]'%s' ", dmsg->length, dmsg->buffer);
#endif
  if (!SafePutMsg ((struct Message *)dmsg, hostPort)) {
    FeWriteString ("Failed to send text: Host disappeared.\n");
    if (verbosity != QUIET) puts ("Host disappeared.");
    return;
  }
}

/*-------------------------------------------------------------------------*/
void TellLPshutdown ( void )

/* Send the mud the 'link shutdown' codes. */

{
  char shutmsg[] = { IAC, IP, IAC, DO, 0x01, '\0' };
  if (connected) TellLPinput (shutmsg);
}

/*-------------------------------------------------------------------------*/
void Action ( void )

/* The real main procedure */

{
  BOOL abort, prompt;
  BYTE rdSig, replySig;
  ULONG signals, frontendSig;
  struct data_message *dmsg;

  frontendSig = FrontendSig();

  PrintTitle(TRUE);

  FeWriteString ("Trying LPmud at port ");
  FeWriteString (mudName);
  FeWriteString ("...\n");
  connected = Connect (mudName, portName);
  if (!connected) {
    if (!curCon) FeWriteString ("Connection failed.\n");
    if (verbosity != QUIET) puts ("Connection failed.");
    return;
  }
  FeWriteString ("Connected.\n");
  rdSig = readPort->mp_SigBit;
  replySig = replyPort->mp_SigBit;
  QueueRead();
  abort = FALSE; prompt = FALSE;
  while (!abort) {
    signals = Wait (  SIGBREAKF_CTRL_E | (1L << readSig) | (1L << rdSig)
                    | (1L << replySig) | frontendSig);
    if ((1L << rdSig) & signals) { /* LPmud tells us something */
      dmsg = (struct data_message *) GetMsg (readPort);
      while (dmsg != NULL) {
        if (verbosity > VERBOSE) { fputs ("From ", stdout); fflush (stdout); }
        PrintLPmsg (dmsg->buffer, dmsg->length, &abort);
        ReplyMsg ((struct Message *)dmsg);
        dmsg = (struct data_message *) GetMsg (readPort);
      }
    }
    if ((1L << readSig) & signals) { /* We tell LPmud something - maybe */
      if (ReadChar()) {
        if (verbosity > VERBOSE) { fputs ("To ", stdout); fflush (stdout); }
        TellLPinput (line);
        if (invis) { FeWrite ((char)0x9b); FeWriteString ("0m"); invis = FALSE; }
      }
    }
      /* If abort is true at this place, the mud driver shut down.
      ** To keep the last messages readable we have to wait for a keypress.
      */
    if (abort) { prompt = TRUE; }
    if ((1L << replySig) & signals) {  /* LPmud replied our message */
      dmsg = (struct data_message *) GetMsg (replyPort);
      while (dmsg != NULL) {
        if (verbosity > VERBOSE) { fputs ("Reply ", stdout); fflush (stdout); }
        free (dmsg->buffer);
        free (dmsg);
        dmsg = (struct data_message *) GetMsg (replyPort);
      }
    }
    if (frontendSig & signals)
      HandleYourSig(signals, &abort);

    if (SIGBREAKF_CTRL_E & signals) abort = TRUE;
  }

    /* Undo pending echo-off-mode, just in case */
  if (invis) { FeWrite ((char)0x9b); FeWriteString ("0m"); invis = FALSE; }

    /* The mud driver shut down, so prompt for a keypress to keep
    ** the last messages readable. Closing the window also suffices.
    */
  if (prompt && !currentCon) {
    abort = FALSE;
    FeWriteString ("\nPress RETURN...");
    while (!abort) {
      signals = Wait (SIGBREAKF_CTRL_E | (1L << readSig) | frontendSig);
      if ((1L << readSig) & signals) { /* some keys had been pressed */
        if (ReadChar()) abort = TRUE;
      }
      if (frontendSig & signals)
        HandleYourSig(signals, &abort);
      if (SIGBREAKF_CTRL_E & signals) abort = TRUE;
    }
  }

  if (verbosity > VERBOSE) putchar ('\n');
}

/*-------------------------------------------------------------------------*/
void ParseIcon ( struct DiskObject *myIcon )

/* ParseIcon: analyzes to the tools of one single icon
*/

{
  char *type;

  if (myIcon != NULL) {
    type = FindToolType(myIcon->do_ToolTypes,"FRONTEND");
    if (type != NULL) {
      if (MatchToolValue (type, "STANDARD"))   { frontend = DOSHANDLER; }
      if (MatchToolValue (type, "DOS"))        { frontend = DOSHANDLER; }
      if (MatchToolValue (type, "AMIEXPRESS")) { frontend = AEXPRESS; }
    }
    type = FindToolType(myIcon->do_ToolTypes,"TYPE");
    if (type != NULL) {
      noEcho = MatchToolValue (type, "NOECHO");
      if (MatchToolValue (type, "QUIET"))   { verbosity = QUIET; }
      if (MatchToolValue (type, "VERBOSE")) { verbosity = VERBOSE; }
      if (MatchToolValue (type, "DEBUG")  ) { verbosity = DEBUG; }
    }
    type = FindToolType(myIcon->do_ToolTypes,"CON");
    if (type != NULL) strcpy (conName, type);
    type = FindToolType(myIcon->do_ToolTypes,"PORT");
    if (type != NULL) strcpy (mudName, type);
  }
}

/*-------------------------------------------------------------------------*/
void ParseWBArgs ( struct WBStartup *wbm )

/* ParseWBArgs: analyzes the args given in icons on start from WB
*/

{
  char *type;
  int   argNr;
  struct Process *me;
  struct DiskObject *myIcon;
  BPTR oldCurrentDir;

  strcpy(arg, "");

  me = (struct Process *)FindTask(NULL);

  if (wbm->sm_NumArgs > 2) { illArg = TRUE; return; }
  argNr = wbm->sm_NumArgs - 1;

  type = wbm->sm_ArgList[argNr].wa_Name;

  oldCurrentDir = me->pr_CurrentDir;
  CurrentDir(wbm->sm_ArgList[argNr].wa_Lock);
  myIcon = GetDiskObject(type);
  CurrentDir(oldCurrentDir);

  ParseIcon(myIcon);
  if (myIcon != NULL)
    FreeDiskObject(myIcon);

  if (frontend == AEXPRESS)
  {
    strcpy(arg, "FRONTEND=AMIEXPRESS");
    illArg = TRUE;
  }
}

/*------------------------------------------------------------------------*/
void PrintHelp ( void )

/* PrintHelp : print the help
*/

{
  puts ("  CLI-Usage: PlayMud ?");
  puts ("             PlayMud [<console> | Current] [Port <portname>] [Noecho]");
  puts ("                     [Quiet|Verbose|Debug]");
  putchar ('\n');
  puts ("  Args   : <console>  : Consolename, e.g. 'CON:0/11/640/220'");
  puts ("           <portname> : Portname of LPmud to connect, default is '8888'");
  puts ("           Current    : use the current console");
  puts ("  Options: ?       : prints this help");
  puts ("           Noecho  : lines input won't be echoed if output came in between");
  puts ("           Quiet   : no messages from the pgm");
  puts ("           Verbose : more messages from the pgm");
  puts ("           Debug   : even more messages from the pgm");
  putchar ('\n');
  puts ("  WB-Usage: FRONTEND= [DOS|STANDARD|AMIEXPRESS]");
  puts ("            TYPE=     [NOECHO] [QUIET|VERBOSE|DEBUG]");
  puts ("            CON=      [<console>]");
  puts ("            PORT=     [<portname>]");
  putchar ('\n');
  puts ("  Icons may be of type TOOL or (fileless) PROJECT.");
  puts ("  Errors will cause a DisplayBeep.");
  putchar ('\n');
  puts("  Iconfiles will also be evaluated when running from shell, prior to parsing");
  puts("  the commandline arguments.");
  putchar ('\n');
}

/*-------------------------------------------------------------------------*/

void Outro ( void )
{
  static int callMeOnce = 0;
  if (callMeOnce++) return;
  TellLPshutdown();
  ShutdownAll ();
  FeOutro();
  GlobalOutro();
}


int Main ( void )
{
  if (illArg) {
    result = 20;
    fputs ("PlayMud: Illegal argument '", stdout);
    fputs (arg, stdout);
    puts ("'");
  }
  else {
    if (verbosity != QUIET)
      PrintTitle(FALSE);
    if (help)
      PrintHelp();
    else {
      if (!SetupAll()) {
        result = 20;
        puts ("PlayMud: Can't perform setup.");
      }
      Action();
    }
  }

  arg = ver; /* Just touch ver[] */

  return result;
}


int wbmain ( struct WBStartup *msg )
{
  IntuitionBase = (struct IntuitionBase *) OpenLibrary ("intuition.library", 33);
  if (IntuitionBase == NULL) exit (20);
  if ((IconBase = (struct Library *) OpenLibrary (ICONNAME, 33)) == NULL) {
    DisplayBeep (NULL);
    CloseLibrary (IntuitionBase);
    exit (20);
  }

  atexit (Outro);

  if (!GlobalIntro() || !FeIntro()) {
    CloseLibrary (IconBase);
    CloseLibrary (IntuitionBase);
    exit (20);
  }

  connected = FALSE;
  help = FALSE; illArg = FALSE;

  ParseWBArgs (msg);

  Main();
  Outro();

  if (result >= 5) DisplayBeep (NULL);

  CloseLibrary (IconBase);
  CloseLibrary (IntuitionBase);
  exit (result);
}


int main (int argc, char **argv )
{
  int i;
  struct Process *me;
  APTR oldWin;
  struct DiskObject *myIcon;
  BPTR oldCurrentDir, newDir;
  char *filepart;

  IntuitionBase = (struct IntuitionBase *) OpenLibrary ("intuition.library", 33);
  if (IntuitionBase == NULL) exit (20);
  if ((IconBase = (struct Library *) OpenLibrary (ICONNAME, 33)) == NULL) {
    DisplayBeep (NULL);
    CloseLibrary (IntuitionBase);
    exit (20);
  }

  atexit (Outro);

  if (!GlobalIntro() || !FeIntro()) {
    CloseLibrary (IconBase);
    CloseLibrary (IntuitionBase);
    exit (20);
  }

  connected = FALSE;
  help = FALSE; illArg = FALSE;

  /* Parse CLI args if help was requested */
  if (argc > 1)
  {
    if (!stricmp ("?", argv[1]))
      help = TRUE;
  }

  if (!help)
  {
#ifdef INCLUDE_VERSION
    if (SysBase->LibNode.lib_Version >= 37) {
      filepart = FilePart(*argv);
    }
    else
#endif
    {
      i = strlen(*argv);
      filepart = (*argv)+i-1;
      while ((i > 1) && (*filepart != '/') && (*filepart != ':')) {
        filepart--;
        i--;
      }
    }

    me = (struct Process *) FindTask (NULL);
    oldWin = me->pr_WindowPtr;
    me->pr_WindowPtr = (APTR) -1L;  /* suppress requesters */

    oldCurrentDir = me->pr_CurrentDir;

#ifdef INCLUDE_VERSION
    /* Try PROGDIR:PlayMud.info */
    if (   (SysBase->LibNode.lib_Version >= 37)
        && me->pr_HomeDir
        && LOCK_SAME == SameLock(me->pr_HomeDir, oldCurrentDir)
       )
    {
      CurrentDir(me->pr_HomeDir);
      myIcon = GetDiskObject(filepart);
      CurrentDir(oldCurrentDir);

      ParseIcon(myIcon);
      if (myIcon != NULL)
        FreeDiskObject(myIcon);
    }
#endif

    /* Try MUDBIN:PlayMud.info */
    newDir = Lock("MUDBIN:", SHARED_LOCK);
    if (newDir != NULL)
    {
      CurrentDir(newDir);
      myIcon = GetDiskObject(filepart);
      CurrentDir(oldCurrentDir);

      ParseIcon(myIcon);
      if (myIcon != NULL)
        FreeDiskObject(myIcon);
      UnLock(newDir);
    }

    /* Try PlayMud.info */
    myIcon = GetDiskObject(filepart);
    ParseIcon(myIcon);
    if (myIcon != NULL)
      FreeDiskObject(myIcon);

    me->pr_WindowPtr = oldWin;  /* reenable requester */

    /* Parse CLI args.
    ** When running for AmiExpress, just one number is allowed.
    */
    if (frontend != AEXPRESS)
    {
      if (argc > 1) {
        i = 1;
        do {
          arg = argv[i];
          illArg = TRUE;
          if (!stricmp ("C", arg) || !stricmp ("CURRENT", arg)) {
            curCon = TRUE; illArg = FALSE;
          }
          if (!stricmp ("N", arg) || !stricmp ("NOECHO", arg)) {
            noEcho = TRUE; illArg = FALSE;
          }
          if (!stricmp ("Q", arg) || !stricmp ("QUIET", arg)) {
            verbosity = QUIET; illArg = FALSE;
          }
          if (!stricmp ("V", arg) || !stricmp ("VERBOSE", arg)) {
            verbosity = VERBOSE; illArg = FALSE;
          }
          if (!stricmp ("D", arg) || !stricmp ("DEBUG"  , arg)) {
            verbosity = DEBUG; illArg = FALSE;
          }
          if (!stricmp ("P", arg) || !stricmp ("PORT"   , arg)) {
            if (++i < argc) {
            strcpy (mudName, argv[i]); illArg = FALSE; }
          }

          if (illArg) { strcpy (conName, arg); illArg = FALSE; }
        }
        while (++i < argc);
      }
    }
    else /* frontend == AEXPRESS */
    {
      switch (argc-1)
      {
        case 0:
          strcpy(arg, "<no arg given>");
          illArg = TRUE;
          break;
        case 1:
          {
            char *tail;
            aeNode = strtol(argv[1], &tail, 10);
            illArg = (tail == argv[1]) || (*tail != 0);
            break;
          }
        default:
          strcpy(arg, "<too much args given>");
          illArg = TRUE;
          break;
      }
    }
  } /* if (!help) */

  Main();
  Outro();

  CloseLibrary (IconBase);
  CloseLibrary (IntuitionBase);
  exit (result);
}

/***************************************************************************/
