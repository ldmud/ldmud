#ifndef TELNET_H__
#define TELNET_H__ 1

/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *        @(#)telnet.h        5.7 (Berkeley) 11/14/89
 */

/*
 * Definitions for the TELNET protocol.
 */
#define  IAC     255      /* interpret as command: */
#define  DONT    254      /* you are not to use option */
#define  DO      253      /* please, you use option */
#define  WONT    252      /* I won't use option */
#define  WILL    251      /* I will use option */
#define  SB      250      /* interpret as subnegotiation */
#define  GA      249      /* you may reverse the line */
#define  EL      248      /* erase the current line */
#define  EC      247      /* erase the current character */
#define  AYT     246      /* are you there */
#define  AO      245      /* abort output--but let prog finish */
#define  IP      244      /* interrupt process--permanently */
#define  BREAK   243      /* break */
#define  DM      242      /* data mark--for connect. cleaning */
#define  NOP     241      /* nop */
#define  SE      240      /* end sub negotiation */
#define  EOR     239      /* end of record (transparent mode) */
#define  ABORT   238      /* Abort process */
#define  SUSP    237      /* Suspend process */
#define  xEOF    236      /* End of file: EOF is already used... */

#define  SYNCH   242      /* for telfunc calls */

#ifdef __DRIVER_SOURCE__

#ifdef TELCMDS
char *telcmds[] = {
        "EOF", "SUSP", "ABORT", "EOR",
        "SE", "NOP", "DMARK", "BRK", "IP", "AO", "AYT", "EC",
        "EL", "GA", "SB", "WILL", "WONT", "DO", "DONT", "IAC",
};
#define  TELCMD_FIRST   xEOF
#define  TELCMD_LAST    IAC
#define  TELCMD_OK(x)   ((x) <= TELCMD_LAST && (x) >= TELCMD_FIRST)
#define  TELCMD(x)      telcmds[(x)-TELCMD_FIRST]
#endif

#endif /* __DRIVER_SOURCE__ */

/* telnet options */
#define  TELOPT_BINARY         0        /* 8-bit data path */
#define  TELOPT_ECHO           1        /* echo */
#define  TELOPT_RCP            2        /* prepare to reconnect */
#define  TELOPT_SGA            3        /* suppress go ahead */
#define  TELOPT_NAMS           4        /* approximate message size */
#define  TELOPT_STATUS         5        /* give status */
#define  TELOPT_TM             6        /* timing mark */
#define  TELOPT_RCTE           7        /* remote controlled transmission and echo */
#define  TELOPT_NAOL           8        /* negotiate about output line width */
#define  TELOPT_NAOP           9        /* negotiate about output page size */
#define  TELOPT_NAOCRD        10        /* negotiate about CR disposition */
#define  TELOPT_NAOHTS        11        /* negotiate about horizontal tabstops */
#define  TELOPT_NAOHTD        12        /* negotiate about horizontal tab disposition */
#define  TELOPT_NAOFFD        13        /* negotiate about formfeed disposition */
#define  TELOPT_NAOVTS        14        /* negotiate about vertical tab stops */
#define  TELOPT_NAOVTD        15        /* negotiate about vertical tab disposition */
#define  TELOPT_NAOLFD        16        /* negotiate about output LF disposition */
#define  TELOPT_XASCII        17        /* extended ascic character set */
#define  TELOPT_LOGOUT        18        /* force logout */
#define  TELOPT_BM            19        /* byte macro */
#define  TELOPT_DET           20        /* data entry terminal */
#define  TELOPT_SUPDUP        21        /* supdup protocol */
#define  TELOPT_SUPDUPOUTPUT  22        /* supdup output */
#define  TELOPT_SNDLOC        23        /* send location */
#define  TELOPT_TTYPE         24        /* terminal type */
#define  TELOPT_EOR           25        /* end or record */
#define  TELOPT_TUID          26        /* TACACS user identification */
#define  TELOPT_OUTMRK        27        /* output marking */
#define  TELOPT_TTYLOC        28        /* terminal location number */
#define  TELOPT_3270REGIME    29        /* 3270 regime */
#define  TELOPT_X3PAD         30        /* X.3 PAD */
#define  TELOPT_NAWS          31        /* window size */
#define  TELOPT_TSPEED        32        /* terminal speed */
#define  TELOPT_LFLOW         33        /* remote flow control */
#define  TELOPT_LINEMODE      34        /* Linemode option */
#define  TELOPT_XDISPLOC      35        /* X Display Location */
#define	 TELOPT_ENVIRON	      36        /* Environment opt for Port ID */
#define  TELOPT_AUTHENTICATION 37       /* authentication */
#define  TELOPT_ENCRYPT       38        /* authentication */
#define	 TELOPT_NEWENV        39        /* Environment opt for Port ID */
#define  TELOPT_STARTTLS      46        /* Transport Layer Security */

/* Inofficial, mud specific telnet options */
#define  TELOPT_MSSP          70        /* Mud Server Status Protocol */
#define  TELOPT_COMPRESS      85        /* Mud Compression Protocol, v.1 */
#define  TELOPT_COMPRESS2     86        /* Mud Compression Protocol, v.2 */
#define  TELOPT_MSP           90        /* Mud Sound Protocol */
#define  TELOPT_MXP           91        /* Mud Extension Protocol */
#define  TELOPT_ZMP           93        /* Zenith Mud Protocol */
#define  TELOPT_MUSHCLIENT   102        /* Mushclient/Aardwolf Protocol */
#define  TELOPT_ATCP         200        /* Achaea Telnet Client Protocol */
#define  TELOPT_EXOPL        255        /* extended-options-list */

#define  NTELOPTS            256        /* was: (1+TELOPT_NEWENV) */

#ifdef __DRIVER_SOURCE__

#ifdef TELOPTS
char *telopts[NTELOPTS]
 = { "BINARY", "ECHO", "RCP", "SUPPRESS GO AHEAD"
   , "NAME", "STATUS", "TIMING MARK", "RCTE"
   , "NAOL", "NAOP", "NAOCRD", "NAOHTS"
   , "NAOHTD", "NAOFFD", "NAOVTS", "NAOVTD"
   , "NAOLFD", "EXTEND ASCII", "LOGOUT", "BYTE MACRO"
   , "DATA ENTRY TERMINAL", "SUPDUP", "SUPDUP OUTPUT", "SEND LOCATION"
   , "TERMINAL TYPE", "END OF RECORD", "TACACS UID", "OUTPUT MARKING"
   , "TTYLOC", "3270 REGIME", "X.3 PAD", "NAWS"
   , "TSPEED", "LFLOW", "LINEMODE", "XDISPLOC"
   , "ENVIRON", "AUTH", "ENCRYPT", "NEWENV"
   , "TELOPT 40", "TELOPT 41", "TELOPT 42", "TELOPT 43"
   , "TELOPT 44", "TELOPT 45", "STARTTLS", "TELOPT 47"
   , "TELOPT 48", "TELOPT 49", "TELOPT 50", "TELOPT 51"
   , "TELOPT 52", "TELOPT 53", "TELOPT 54", "TELOPT 55"
   , "TELOPT 56", "TELOPT 57", "TELOPT 58", "TELOPT 59"
   , "TELOPT 60", "TELOPT 61", "TELOPT 62", "TELOPT 63"
   , "TELOPT 64", "TELOPT 65", "TELOPT 66", "TELOPT 67"
   , "TELOPT 68", "TELOPT 69", "MSSP", "TELOPT 71"
   , "TELOPT 72", "TELOPT 73", "TELOPT 74", "TELOPT 75"
   , "TELOPT 76", "TELOPT 77", "TELOPT 78", "TELOPT 79"
   , "TELOPT 80", "TELOPT 81", "TELOPT 82", "TELOPT 83"
   , "TELOPT 84", "MUD COMPRESS", "MUD COMPRESS2", "TELOPT 87"
   , "TELOPT 88", "TELOPT 89", "MUD SOUND", "MUD EXTENSION"
   , "TELOPT 92", "ZMP", "TELOPT 94", "TELOPT 95"
   , "TELOPT 96", "TELOPT 97", "TELOPT 98", "TELOPT 99"
   , "TELOPT 100", "TELOPT 101", "MUSHCLIENT", "TELOPT 103"
   , "TELOPT 104", "TELOPT 105", "TELOPT 106", "TELOPT 107"
   , "TELOPT 108", "TELOPT 109", "TELOPT 110", "TELOPT 111"
   , "TELOPT 112", "TELOPT 113", "TELOPT 114", "TELOPT 115"
   , "TELOPT 116", "TELOPT 117", "TELOPT 118", "TELOPT 119"
   , "TELOPT 120", "TELOPT 121", "TELOPT 122", "TELOPT 123"
   , "TELOPT 124", "TELOPT 125", "TELOPT 126", "TELOPT 127"
   , "TELOPT 128", "TELOPT 129", "TELOPT 130", "TELOPT 131"
   , "TELOPT 132", "TELOPT 133", "TELOPT 134", "TELOPT 135"
   , "TELOPT 136", "TELOPT 137", "TELOPT 138", "TELOPT 139"
   , "TELOPT 140", "TELOPT 141", "TELOPT 142", "TELOPT 143"
   , "TELOPT 144", "TELOPT 145", "TELOPT 146", "TELOPT 147"
   , "TELOPT 148", "TELOPT 149", "TELOPT 150", "TELOPT 151"
   , "TELOPT 152", "TELOPT 153", "TELOPT 154", "TELOPT 155"
   , "TELOPT 156", "TELOPT 157", "TELOPT 158", "TELOPT 159"
   , "TELOPT 160", "TELOPT 161", "TELOPT 162", "TELOPT 163"
   , "TELOPT 164", "TELOPT 165", "TELOPT 166", "TELOPT 167"
   , "TELOPT 168", "TELOPT 169", "TELOPT 170", "TELOPT 171"
   , "TELOPT 172", "TELOPT 173", "TELOPT 174", "TELOPT 175"
   , "TELOPT 176", "TELOPT 177", "TELOPT 178", "TELOPT 179"
   , "TELOPT 180", "TELOPT 181", "TELOPT 182", "TELOPT 183"
   , "TELOPT 184", "TELOPT 185", "TELOPT 186", "TELOPT 187"
   , "TELOPT 188", "TELOPT 189", "TELOPT 190", "TELOPT 191"
   , "TELOPT 192", "TELOPT 193", "TELOPT 194", "TELOPT 195"
   , "TELOPT 196", "TELOPT 197", "TELOPT 198", "TELOPT 199"
   , "ATCP", "TELOPT 201", "TELOPT 202", "TELOPT 203"
   , "TELOPT 204", "TELOPT 205", "TELOPT 206", "TELOPT 207"
   , "TELOPT 208", "TELOPT 209", "TELOPT 210", "TELOPT 211"
   , "TELOPT 212", "TELOPT 213", "TELOPT 214", "TELOPT 215"
   , "TELOPT 216", "TELOPT 217", "TELOPT 218", "TELOPT 219"
   , "TELOPT 220", "TELOPT 221", "TELOPT 222", "TELOPT 223"
   , "TELOPT 224", "TELOPT 225", "TELOPT 226", "TELOPT 227"
   , "TELOPT 228", "TELOPT 229", "TELOPT 230", "TELOPT 231"
   , "TELOPT 232", "TELOPT 233", "TELOPT 234", "TELOPT 235"
   , "TELOPT 236", "TELOPT 237", "TELOPT 238", "TELOPT 239"
   , "TELOPT 240", "TELOPT 241", "TELOPT 242", "TELOPT 243"
   , "TELOPT 244", "TELOPT 245", "TELOPT 246", "TELOPT 247"
   , "TELOPT 248", "TELOPT 249", "TELOPT 250", "TELOPT 251"
   , "TELOPT 252", "TELOPT 253", "TELOPT 254", "EXOPL"
};
#endif

#endif /* __DRIVER_SOURCE__ */

/* sub-option qualifiers */
#define  TELQUAL_IS     0        /* option is... */
#define  TELQUAL_SEND   1        /* send option */

/*
 * LINEMODE suboptions
 */

#define  LM_MODE            1
#define  LM_FORWARDMASK     2
#define  LM_SLC             3

#define  MODE_EDIT       0x01
#define  MODE_TRAPSIG    0x02
#define  MODE_ACK        0x04

#define  MODE_MASK       (MODE_EDIT|MODE_TRAPSIG|MODE_ACK)

/* Not part of protocol, but needed to simplify things... */
#define  MODE_FLOW       0x0100
#define  MODE_ECHO       0x0200
#define  MODE_INBIN      0x0400
#define  MODE_OUTBIN     0x0800
#define  MODE_FORCE      0x1000

#define  SLC_SYNCH   1
#define  SLC_BRK     2
#define  SLC_IP      3
#define  SLC_AO      4
#define  SLC_AYT     5
#define  SLC_EOR     6
#define  SLC_ABORT   7
#define  SLC_EOF     8
#define  SLC_SUSP    9
#define  SLC_EC     10
#define  SLC_EL     11
#define  SLC_EW     12
#define  SLC_RP     13
#define  SLC_LNEXT  14
#define  SLC_XON    15
#define  SLC_XOFF   16
#define  SLC_FORW1  17
#define  SLC_FORW2  18

#define  NSLC       18

#define  SLC_NAMES  "0", "SYNCH", "BRK", "IP", "AO", "AYT", "EOR", \
                    "ABORT", "EOF", "SUSP", "EC", "EL", "EW", "RP", \
                    "LNEXT", "XON", "XOFF", "FORW1", "FORW2"

#define  SLC_NOSUPPORT     0
#define  SLC_CANTCHANGE    1
#define  SLC_VARIABLE      2
#define  SLC_DEFAULT       3
#define  SLC_LEVELBITS    0x03

#define  SLC_FUNC          0
#define  SLC_FLAGS         1
#define  SLC_VALUE         2

#define  SLC_ACK          0x80
#define  SLC_FLUSHIN      0x40
#define  SLC_FLUSHOUT     0x20

#endif  /* TELNET_H__ */
