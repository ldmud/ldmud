#ifndef LPC_IDN_H_
#define LPC_IDN_H_ 1

/* --- IDNA Constants ---
 */

/* idna_stringprep() profiles. */

#define STRINGPREP_NAMEPREP 1
#define STRINGPREP_SASLPREP 2
#define STRINGPREP_PLAIN 3
#define STRINGPREP_TRACE 4
#define STRINGPREP_KERBEROS5 5
#define STRINGPREP_XMPP_NODEPREP 6
#define STRINGPREP_XMPP_RESOURCEPREP 7 
#define STRINGPREP_ISCSI 8

/* idna_stringprep() flags */

#define STRINGPREP_NO_NFKC_FLAG        (1<<0)
#define STRINGPREP_NO_BIDI_FLAG        (1<<1)
#define STRINGPREP_NO_UNASSIGNED_FLAG  (1<<2)

#define STRINGPREP_FLAG_MAX  (1<<2)

#endif
