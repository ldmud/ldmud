#ifndef _STRALLOC_H_
#define _STRALLOC_H_ 1

#include "driver.h"

#if defined(HAS_INLINE) && defined(STRALLOC)
#define STRALLOC_INLINE inline
#else
#define STRALLOC_INLINE
#endif

#define SHSTR_NEXT(str)	(*(char **)((char *) (str) - sizeof(unsigned short)\
						   - sizeof(char *)))
#define SHSTR_REFS(str)	(*(unsigned short *)((char *) (str)\
						   - sizeof(unsigned short)))
#define SHSTR_BLOCK(str) ((char *)(str) - sizeof(unsigned short)\
					- sizeof(char *))

#ifdef MALLOC_smalloc
#include "smalloc.h"
extern int malloc_size_mask PROT((void));
#define shstr_malloced_size(str) ( *( \
	(p_uint *)(str-sizeof(char*)-sizeof(unsigned short))\
	- SMALLOC_OVERHEAD) )
#else
#define malloc_size_mask() (~0)
#define shstr_malloced_size(str) (\
	(sizeof(char*) + sizeof(char *) + sizeof(short) +\
	strlen(str) + 1 + sizeof(char *) - 1) / sizeof(char *))
#endif

extern mp_int stralloc_allocd_strings;
extern mp_int stralloc_allocd_bytes;

#if !defined(NO_INCREMENT_STRING_REF) && !defined(STRALLOC)
static INLINE void increment_string_ref PROT((char * str)) /* TODO: UNUSED */;

static INLINE
void increment_string_ref(str)
char *str;
{
        stralloc_allocd_strings++;
        stralloc_allocd_bytes += shstr_malloced_size(str);
        if (SHSTR_REFS(str))
            SHSTR_REFS(str)++;
}
#endif

/* --- Prototypes --- */
extern void init_shared_strings PROT((void));
extern void clear_shared_string_refs PROT((void));

extern char * findstring PROT((char *s));
extern char * make_shared_string PROT((char *str));
STRALLOC_INLINE void decrement_string_ref PROT((char *str));
extern void free_string PROT((char *str));
extern int add_string_status PROT((int verbose));

#ifdef MALLOC_smalloc
extern void note_shared_string_table_ref PROT((void));
extern void walk_shared_strings PROT((void (*func)(char *, char *) ));
#endif /* MALLOC_smalloc */

/* --- Common used shared strings. --- */
/* The most common strings, including all the predefined applies,
 * are kept in shstrings[] for faster usage.
 * The indices are SHX_xxx, the defines STR_xxx expand to shstring[SHX_xxx]
 */

enum StandardStrings {
    /* Generic game strings */
    SHX_DEFAULT = 0  /* Default string used when running out of mem */
  , SHX_EMPTY        /* "" */

    /* Object lfuns */
  , SHX_CATCH_TELL      /* "catch_tell" */
  , SHX_CATCH_MSG       /* "catch_msg" */
  , SHX_ID              /* "id" */
  , SHX_VARINIT         /* "__INIT" */

    /* Master lfuns */
  , SHX_ABS_PATH        /* "make_path_absolute" */
  , SHX_COMP_OBJ        /* "compile_object" */
  , SHX_CONNECT         /* "connect" */
  , SHX_DISCONNECT      /* "disconnect" */
  , SHX_EPILOG          /* "epilog" */
  , SHX_EXT_RELOAD      /* "external_master_reload" */
  , SHX_FLAG            /* "flag" */
  , SHX_GET_BB_UID      /* "get_bb_uid" */
  , SHX_GET_ED_FNAME    /* "get_ed_buffer_save_file_name" */
  , SHX_GET_M_UID       /* "get_master_uid" */
  , SHX_GET_SEFUN       /* "get_simul_efun" */
  , SHX_GET_WNAME       /* "get_wiz_name" */
  , SHX_HEART_ERROR     /* "heart_beat_error" */
  , SHX_INAUGURATE      /* "inaugurate_master" */
  , SHX_LOG_ERROR       /* "log_error" */
  , SHX_LOGON           /* "logon" */
  , SHX_OBJ_NAME        /* "object_name" */
  , SHX_PLAYER_LEVEL    /* "query_player_level" */
  , SHX_PRELOAD         /* "preload" */
  , SHX_PREP_DEST       /* "prepare_destruct" */
  , SHX_PRIVILEGE       /* "privilege_violation" */
  , SHX_QUERY_SHADOW    /* "query_allow_shadow" */
  , SHX_QUOTA_DEMON     /* "quota_demon" */
  , SHX_RETR_ED         /* "retrieve_ed_setup" */
  , SHX_REACTIVATE      /* "reactivate_destructed_master" */
  , SHX_RECEIVE_IMP     /* "receive_imp" */
  , SHX_REMOVE_PL       /* "remove_player" */
  , SHX_RUNTIME         /* "runtime_error" */
  , SHX_SAVE_ED         /* "save_ed_setup" */
  , SHX_SHUTDOWN        /* "notify_shutdown" */
  , SHX_SLOW_SHUT       /* "slow_shut_down" */
  , SHX_STALE_ERQ       /* "stale_erq" */
  , SHX_VALID_EXEC      /* "valid_exec" */
  , SHX_VALID_QSNOOP    /* "valid_query_snoop" */
  , SHX_VALID_READ      /* "valid_read" */
  , SHX_VALID_SETEUID   /* "valid_seteuid" */
  , SHX_VALID_SNOOP     /* "valid_snoop" */
  , SHX_VALID_WRITE     /* "valid_write" */

    /* Compat mode lfuns */
  , SHX_ADD_WEIGHT      /* "add_weight" */
  , SHX_CANPUTGET       /* "can_put_and_get" */
  , SHX_DROP            /* "drop" */
  , SHX_GET             /* "get" */
  , SHX_QUERY_WEIGHT    /* "query_weight" */
  , SHX_PREVENT_INSERT  /* "prevent_insert" */

  , SHSTR_NOSTRINGS  /* The number of strings */
};

extern char *shstring[SHSTR_NOSTRINGS];

    /* Generic game strings */
#define STR_DEFAULT  shstring[SHX_DEFAULT]
#define STR_EMPTY    shstring[SHX_EMPTY]

    /* Object lfuns */
#define STR_CATCH_TELL  shstring[SHX_CATCH_TELL]
#define STR_CATCH_MSG   shstring[SHX_CATCH_MSG]
#define STR_ID          shstring[SHX_ID]
#define STR_VARINIT     shstring[SHX_VARINIT]

    /* Master lfuns */
#define STR_ABS_PATH       shstring[SHX_ABS_PATH]
#define STR_COMP_OBJ       shstring[SHX_COMP_OBJ]
#define STR_CONNECT        shstring[SHX_CONNECT]
#define STR_DISCONNECT     shstring[SHX_DISCONNECT]
#define STR_EPILOG         shstring[SHX_EPILOG]
#define STR_EXT_RELOAD     shstring[SHX_EXT_RELOAD]
#define STR_FLAG           shstring[SHX_FLAG]
#define STR_GET_BB_UID     shstring[SHX_GET_BB_UID]
#define STR_GET_ED_FNAME   shstring[SHX_GET_ED_FNAME]
#define STR_GET_M_UID      shstring[SHX_GET_M_UID]
#define STR_GET_SEFUN      shstring[SHX_GET_SEFUN]
#define STR_GET_WNAME      shstring[SHX_GET_WNAME]
#define STR_HEART_ERROR    shstring[SHX_HEART_ERROR]
#define STR_INAUGURATE     shstring[SHX_INAUGURATE]
#define STR_LOG_ERROR      shstring[SHX_LOG_ERROR]
#define STR_LOGON          shstring[SHX_LOGON]
#define STR_OBJ_NAME       shstring[SHX_OBJ_NAME]
#define STR_PLAYER_LEVEL   shstring[SHX_PLAYER_LEVEL]
#define STR_PRELOAD        shstring[SHX_PRELOAD]
#define STR_PREP_DEST      shstring[SHX_PREP_DEST]
#define STR_PRIVILEGE      shstring[SHX_PRIVILEGE]
#define STR_QUERY_SHADOW   shstring[SHX_QUERY_SHADOW]
#define STR_QUOTA_DEMON    shstring[SHX_QUOTA_DEMON]
#define STR_RETR_ED        shstring[SHX_RETR_ED]
#define STR_REACTIVATE     shstring[SHX_REACTIVATE]
#define STR_RECEIVE_IMP    shstring[SHX_RECEIVE_IMP]
#define STR_REMOVE_PL      shstring[SHX_REMOVE_PL]
#define STR_RUNTIME        shstring[SHX_RUNTIME]
#define STR_SAVE_ED        shstring[SHX_SAVE_ED]
#define STR_SHUTDOWN       shstring[SHX_SHUTDOWN]
#define STR_SLOW_SHUT      shstring[SHX_SLOW_SHUT]
#define STR_STALE_ERQ      shstring[SHX_STALE_ERQ]
#define STR_VALID_EXEC     shstring[SHX_VALID_EXEC]
#define STR_VALID_QSNOOP   shstring[SHX_VALID_QSNOOP]
#define STR_VALID_READ     shstring[SHX_VALID_READ]
#define STR_VALID_SETEUID  shstring[SHX_VALID_SETEUID]
#define STR_VALID_SNOOP    shstring[SHX_VALID_SNOOP]
#define STR_VALID_WRITE    shstring[SHX_VALID_WRITE]

    /* Compat mode lfuns */
#define STR_ADD_WEIGHT      shstring[SHX_ADD_WEIGHT]
#define STR_CANPUTGET       shstring[SHX_CANPUTGET]
#define STR_DROP            shstring[SHX_DROP]
#define STR_GET             shstring[SHX_GET]
#define STR_QUERY_WEIGHT    shstring[SHX_QUERY_WEIGHT]
#define STR_PREVENT_INSERT  shstring[SHX_PREVENT_INSERT]

#endif /* _STRALLOC_H_ */
