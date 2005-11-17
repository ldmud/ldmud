//
// Wunderland Mudlib
//
// telnet.h -- Standard telnet definitions
//
// $Log: telnet.h,v $
// Revision 1.10  2002/12/02 17:31:56  Fiona
// sb_ttype und sb_xdisp
//

#ifndef __TELNET_H__
#define __TELNET_H__

// ******************** Telnet State Machine ********************

// Each option could be in one of the following four states. There
// is also a one element queue for each option. Mind that each option
// could have different states for the lokal and the remote system.
// Six bits of TS_STATE are used.
#define NO           0x00 // option is deactivated
#define YES          0x01 // option is activated
#define WANT_NO      0x02 // option is activated, negotiating to switch off
#define WANT_YES     0x03 // option is deactivated, negotiating to switch on
#define Q_EMPTY      0x00 // no entry in queue
#define Q_OPPOSITE   0x04 // request to toggle state in queue
#define REJECTED     0x08 // option denied, don't retry

// State and queue on the remote side (DO + DONT) (bits 0-3)
#define Q_REMOTE(x)       ((x)  &  0x03)
#define S_REMOTE(x, y)    (((x) & ~0x03) | (y))
#define Q_REMOTEQ(x)      ((x)  &  0x04)
#define S_REMOTEQ(x, y)   (((x) & ~0x04) | (y))
#define Q_REMOTER(x)      ((x)  &  0x08)
#define S_REMOTER(x, y)   (((x) & ~0x08) | (y))
// State and queue on this side (WILL + WONT) (bits 4-7)
#define Q_LOCAL(x)        (((x) &  0x30) >> 4)
#define S_LOCAL(x,y)      (((x) & ~0x30) | ((y) << 4))
#define Q_LOCALQ(x)       (((x) &  0x40) >> 4)
#define S_LOCALQ(x, y)    (((x) & ~0x40) | ((y) << 4))
#define Q_LOCALR(x)       (((x) &  0x80) >> 4)
#define S_LOCALR(x, y)    (((x) & ~0x80) | ((y) << 4))

// Access to mapping ts
#define TS_STATE        0 // option state (yes, no, want yes, want no, Q)
#define TS_SB           1 // sb infos (option specific)
#define TS_R_AGREE      2 // preference or decision callback (remote state)
#define TS_L_AGREE      3 // preference or decision callback (local state)
#define TS_CB           4 // option state change callback (yes, no)
#define TS_SBCB         5 // option sb callback
#define TS_SIZE         6

// To have everything in one place we have one special key in ts
#define TS_EXTRA       -1 // key
#define TSE_STATE       0 // input_to's INPUT_NOECHO and/or INPUT_CHARMODE
#define TSE_TELNETNEG   1 // client answered a negotiation
#define TSE_LOG         2 // negotiation log

// Bits used for the charmode and noecho state of the connection
// Bits 0 + 1 used for TSE_NOECHO (set for noecho mode)
// Bits 2 + 3 used for TSE_SGA_CHAR (set for charmode using SGA)
// Bits 4 + 5 used for TSE_LM_CHAR (set for charmode using LINEMODE)
// each representing the state with NO, YES, WANT_NO and WANT_YES
#define Q_TSE_NOECHO      ((ts[TS_EXTRA, TSE_STATE])  &  0x03)
#define S_TSE_NOECHO(y)   (ts[TS_EXTRA, TSE_STATE] = \
                            ((ts[TS_EXTRA, TSE_STATE]) & ~0x03) | (y))
#define Q_TSE_SGA_CHAR    (((ts[TS_EXTRA, TSE_STATE]) &  0x0c) >> 2)
#define S_TSE_SGA_CHAR(y) (ts[TS_EXTRA, TSE_STATE] = \
                            ((ts[TS_EXTRA, TSE_STATE]) & ~0x0c) | ((y) << 2))
#define Q_TSE_LM_CHAR     (((ts[TS_EXTRA, TSE_STATE]) &  0x30) >> 4)
#define S_TSE_LM_CHAR(y)  (ts[TS_EXTRA, TSE_STATE] = \
                            ((ts[TS_EXTRA, TSE_STATE]) & ~0x30) | ((y) << 4))

#ifdef NEED_PRIVATE_PROTOTYPES
#ifndef __TELNET_H_P_PROTO__
#define __TELNET_H_P_PROTO__
private int send(int* x);
private void tel_error(string err);
private void start_telnetneg();
private string telnet_to_text(int command, int option, int* args);
private void sb_ttype(int command, int option, int* optargs);
private void sb_xdisp(int command, int option, int* optargs);
private void sb_tspeed(int command, int option, int* optargs);
private void sb_env(int command, int option, int* optargs);
private void sb_naws(int command, int option, int* optargs);
private void sb_status(int command, int option, int* optargs);
private void sb_line(int command, int option, int* optargs);
private int neg_sga(int command, int option);
private int neg_echo(int command, int option);
private int neg_bin(int command, int option);
private int neg_tm(int command, int option);
private void start_sb(int command, int option);
private void start_eor(int command, int option);
private void start_lm(int command, int option);
private void cb_echo(int command, int option);
private void cb_sga(int command, int option);
static void modify_prompt(); // std/player/prompt.c
#endif
#endif // NEED_PRIVATE_PROTOTYPES


// ************ Definitions for the TELNET protocol *************
#define IAC     255             /* interpret as command: */
#define DONT    254             /* you are not to use option */
#define DO      253             /* please, you use option */
#define WONT    252             /* I won't use option */
#define WILL    251             /* I will use option */
#define SB      250             /* interpret as subnegotiation */
#define SE      240             /* end sub negotiation */
#define EOR     239             /* end of record (transparent mode) */

#define TELCMDS ({\
        "EOR", "SE", "NOP", "DMARK", "BRK", "IP", "AO", "AYT", "EC",\
        "EL", "GA", "SB", "WILL", "WONT", "DO", "DONT", "IAC",\
})
/* backward starting with IAC == 255 */
#define TELCMD2STRING(x) (((256-x)<sizeof(TELCMDS))?TELCMDS[<(256-x)]:(""+x))

/* telnet options */
#define TELOPT_BINARY   0       /* 8-bit data path */
#define TELOPT_ECHO     1       /* echo */
#define TELOPT_RCP      2       /* prepare to reconnect */
#define TELOPT_SGA      3       /* suppress go ahead */
#define TELOPT_NAMS     4       /* approximate message size */
#define TELOPT_STATUS   5       /* give status */
#define TELOPT_TM       6       /* timing mark */
#define TELOPT_RCTE     7       /* remote controlled transmission and echo */
#define TELOPT_NAOL     8       /* negotiate about output line width */
#define TELOPT_NAOP     9       /* negotiate about output page size */
#define TELOPT_NAOCRD   10      /* negotiate about CR disposition */
#define TELOPT_NAOHTS   11      /* negotiate about horizontal tabstops */
#define TELOPT_NAOHTD   12      /* negotiate about horizontal tab disposition */
#define TELOPT_NAOFFD   13      /* negotiate about formfeed disposition */
#define TELOPT_NAOVTS   14      /* negotiate about vertical tab stops */
#define TELOPT_NAOVTD   15      /* negotiate about vertical tab disposition */
#define TELOPT_NAOLFD   16      /* negotiate about output LF disposition */
#define TELOPT_XASCII   17      /* extended ascic character set */
#define TELOPT_LOGOUT   18      /* force logout */
#define TELOPT_BM       19      /* byte macro */
#define TELOPT_DET      20      /* data entry terminal */
#define TELOPT_SUPDUP   21      /* supdup protocol */
#define TELOPT_SUPDUPOUTPUT 22  /* supdup output */
#define TELOPT_SNDLOC   23      /* send location */
#define TELOPT_TTYPE    24      /* terminal type */
#define TELOPT_EOR      25      /* end or record */
#define TELOPT_TUID     26      /* TACACS user identification */
#define TELOPT_OUTMRK   27      /* output marking */
#define TELOPT_TTYLOC   28      /* terminal location number */
#define TELOPT_3270REGIME 29    /* 3270 regime */
#define TELOPT_X3PAD    30      /* X.3 PAD */
#define TELOPT_NAWS     31      /* window size */
#define TELOPT_TSPEED   32      /* terminal speed */
#define TELOPT_LFLOW    33      /* remote flow control */
#define TELOPT_LINEMODE 34      /* linemode negotiations */
#define TELOPT_XDISPLOC 35      /* X Display Location */
#define TELOPT_ENVIRON  36      /* Environment opt for Port ID */
#define TELOPT_AUTHENTICATION 37/* authentication */
#define TELOPT_ENCRYPT  38      /* authentication */
#define TELOPT_NEWENV   39      /* Environment opt for Port ID */

/* Inofficial, mud specific telnet options */
#define TELOPT_COMPRESS  85     /* Mud Compression Protocol, v.1 */
#define TELOPT_COMPRESS2 86     /* Mud Compression Protocol, v.2 */
#define TELOPT_MSP       90     /* Mud Sound Protocol */
#define TELOPT_MXP       91     /* Mud Extension Protocol */

#define TELOPT_EXOPL    255     /* extended-options-list */

#define NTELOPTS        (1+TELOPT_NEWENV)
#define TELOPTS ({\
        "BINARY", "ECHO", "RCP", "SGA", "NAME",\
        "STATUS", "TM", "RCTE", "NAOL", "NAOP",\
        "NAOCRD", "NAOHTS", "NAOHTD", "NAOFFD", "NAOVTS",\
        "NAOVTD", "NAOLFD", "XASCII", "LOGOUT", "BM",\
        "DET", "SUPDUP", "SUPDUP OUTPUT",\
        "SENDLOC", "TTYPE", "EOR", \
        "TACACS UID", "OUTPUT MARKING", "TTYLOC",\
        "3270 REGIME", "X.3 PAD", "NAWS","TSPEED","LFLOW","LINEMODE",\
        "XDISPLOC","ENVIRON","AUTHENTICATION","ENCRYPT","NEWENV",\
        "TELOPT 40", "TELOPT 41", "TELOPT 42", "TELOPT 43",\
        "TELOPT 44", "TELOPT 45", "TELOPT 46", "TELOPT 47",\
        "TELOPT 48", "TELOPT 49", "TELOPT 50", "TELOPT 51",\
        "TELOPT 52", "TELOPT 53", "TELOPT 54", "TELOPT 55",\
        "TELOPT 56", "TELOPT 57", "TELOPT 58", "TELOPT 59",\
        "TELOPT 60", "TELOPT 61", "TELOPT 62", "TELOPT 63",\
        "TELOPT 64", "TELOPT 65", "TELOPT 66", "TELOPT 67",\
        "TELOPT 68", "TELOPT 69", "TELOPT 70", "TELOPT 71",\
        "TELOPT 72", "TELOPT 73", "TELOPT 74", "TELOPT 75",\
        "TELOPT 76", "TELOPT 77", "TELOPT 78", "TELOPT 79",\
        "TELOPT 80", "TELOPT 81", "TELOPT 82", "TELOPT 83",\
        "TELOPT 84", "MCCP1", "MCCP2", "TELOPT 87",\
        "TELOPT 88", "TELOPT 89", "MSP", "MEP",\
})

#define TELOPT2STRING(x) ((x<sizeof(TELOPTS))?TELOPTS[x]:(""+x))

/* sub-option qualifiers */
#define TELQUAL_IS      0       /* option is... */
#define TELQUAL_SEND    1       /* send option */
#define TELQUAL_INFO    2

#define TELQUAL2STRING(x) (x<3?({"IS","SEND","INFO"})[x]:""+x)

/*
 * LINEMODE suboptions
 */

#define LM_MODE         1
#define LM_FORWARDMASK  2
#define LM_SLC          3

#define MODE_EDIT       0x01
#define MODE_TRAPSIG    0x02
#define MODE_ACK        0x04
#define MODE_SOFT_TAB   0x08
#define MODE_LIT_ECHO   0x10

#define MODE_MASK       (MODE_EDIT|MODE_TRAPSIG|MODE_ACK|MODE_SOFT_TAB|MODE_LIT_ECHO)

#define SLC_SYNCH       1
#define SLC_BRK         2
#define SLC_IP          3
#define SLC_AO          4
#define SLC_AYT         5
#define SLC_EOR         6
#define SLC_ABORT       7
#define SLC_EOF         8
#define SLC_SUSP        9
#define SLC_EC          10
#define SLC_EL          11
#define SLC_EW          12
#define SLC_RP          13
#define SLC_LNEXT       14
#define SLC_XON         15
#define SLC_XOFF        16
#define SLC_FORW1       17
#define SLC_FORW2       18
#define SLC_MCL         19
#define SLC_MCR         20
#define SLC_MCWL        21
#define SLC_MCWR        22
#define SLC_MCBOL       23
#define SLC_MCEOL       24
#define SLC_INSRT       25
#define SLC_OVER        26
#define SLC_ECR         27
#define SLC_EWR         28
#define SLC_EBOL        29
#define SLC_EEOL        30

#define SLC_NAMES       ({"0", "SYNCH", "BRK", "IP", "AO", "AYT", "EOR", \
                        "ABORT", "EOF", "SUSP", "EC", "EL", "EW", "RP", \
                        "LNEXT", "XON", "XOFF", "FORW1", "FORW2", \
                        "MCL", "MCR", "MCWL", "MCWR", "MCBOL", "MCEOL", \
                        "INSRT", "OVER", "ECR", "EWR", "EBOL", "EEOL"})

#define SLC2STRING(x) ((x)<sizeof(SLC_NAMES)?SLC_NAMES[x]:sprintf("%02x",x))

#define SLC_NOSUPPORT   0
#define SLC_CANTCHANGE  1
#define SLC_VARIABLE    2
#define SLC_DEFAULT     3
#define SLC_ACK         0x80
#define SLC_FLUSHIN     0x40
#define SLC_FLUSHOUT    0x20
#define SLC_LEVELBITS   0x03

#define SLC_FLAGNAME    ({ "NOSUPPORT", "CANTCHANGE", "VARIABLE", "DEFAULT" })

#define ENV_VAR         0
#define ENV_VALUE       1
#define ENV_ESC         2
#define ENV_USERVAR     3

#endif
