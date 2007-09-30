#ifndef LPC_TLS_H
#define LPC_TLS_H

/* Field indices for the result of tls_query_connection_info() */

#define TLS_CIPHER  0
#define TLS_COMP    1
#define TLS_KX      2
#define TLS_MAC     3
#define TLS_PROT    4

#define TLS_INFO_MAX 5  /* Total number of result fields */

/* Interpretation of the cipher information */

#define TLS_CIPHER_TABLE ({ \
"TLS_CIPHER_NONE", \
"TLS_CIPHER_NULL", \
"TLS_CIPHER_ARCFOUR_128", \
"TLS_CIPHER_3DES_CBC", \
"TLS_CIPHER_RIJNDAEL_128_CBC", \
"TLS_CIPHER_RIJNDAEL_256_CBC", \
"TLS_CIPHER_ARCFOUR_40" \
})

#define TLS_CIPHER_NAME(x) TLS_CIPHER_TABLE[(x)]

/* Interpretation of the key-exchange information */

#define TLS_KX_TABLE ({ \
"TLS_KX_NONE", \
"TLS_KX_RSA", \
"TLS_KX_DHE_DSS", \
"TLS_KX_DHE_RSA", \
"TLS_KX_ANON_DH", \
"TLS_KX_SRP", \
"TLS_KX_RSA_EXPORT", \
"TLS_KX_SRP_RSA", \
"TLS_KX_SRP_DSS" \
})

#define TLS_KX_NAME(x) TLS_KX_TABLE[(x)]

/* Interpretation of the MAC information */

#define TLS_MAC_TABLE ({ \
"TLS_MAC_NONE", \
"TLS_MAC_NULL", \
"TLS_MAC_MD5", \
"TLS_MAC_SHA" \
})

#define TLS_MAC_NAME(x) TLS_MAC_TABLE[(x)]

/* Interpretation of the compression information */

#define TLS_COMP_TABLE ({ \
"TLS_COMP_NONE", \
"TLS_COMP_NULL", \
"TLS_COMP_ZLIB", \
"TLS_COMP_LZO" \
})

#define TLS_COMP_NAME(x) TLS_COMP_TABLE[(x)]

/* Interpretation of the protocol information */

#define TLS_PROT_TABLE ({ \
"TLS_PROT_NONE", \
"TLS_PROT_SSL3", \
"TLS_PROT_TLS1" \
})

#define TLS_PROT_NAME(x) TLS_PROT_TABLE[(x)]

/* Recognized hash() algorithms (not all may be supported at runtime) */

#define TLS_HASH_SHA1      (1)
#define TLS_HASH_SHA224    (2)
#define TLS_HASH_SHA256    (3)
#define TLS_HASH_SHA384    (4)
#define TLS_HASH_SHA512    (5)
#define TLS_HASH_MD5       (6)
#define TLS_HASH_RIPEMD160 (7)

#endif /* LPC_TLS_H */
