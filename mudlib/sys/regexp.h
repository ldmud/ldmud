#ifndef LPC_REGEXP_H_
#define LPC_REGEXP_H_ 1

/* Definitions of regexp option values */

/* Not really an RE option, but used in conjunction with some RE functions: */

#define RE_GLOBAL          0x0001  /* Apply RE globally (if possible) */

/* Options supported by the old regexp package: */

#define RE_EXCOMPATIBLE    0x0002  /* RE is compatible with ex */

/* Options supported by the PCRE regexp package: */

#define RE_CASELESS        0x0004
#define RE_MULTILINE       0x0008
#define RE_DOTALL          0x0010
#define RE_EXTENDED        0x0020
#define RE_ANCHORED        0x0040
#define RE_DOLLAR_ENDONLY  0x0080
#define RE_NOTBOL          0x0100
#define RE_NOTEOL          0x0200
#define RE_UNGREEDY        0x0400
#define RE_NOTEMPTY        0x0800

/* Options specific for regmatch(): */

#define RE_MATCH_SUBS      0x1000  /* Return matched subexpressions */

/* Options specific for regexplode(): */

#define RE_OMIT_DELIM      0x1000  /* Omit the delimiters */

/* Regexp package selection options: */

#define RE_TRADITIONAL     0x04000000
#define RE_PCRE            0x02000000

#define RE_PACKAGE_MASK    (RE_TRADITIONAL | RE_PCRE)

#endif /* LPC_REGEXP_H_ */
