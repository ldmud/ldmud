#ifndef TYPEDEFS_H__
#define TYPEDEFS_H__ 1

/*---------------------------------------------------------------------------
 * Type definitions for opaque uses.
 *
 *---------------------------------------------------------------------------
 * This file defines several types whose opaque definitions need to be known
 * in many places. Main purpose is to decouple the header files.
 */

#include "driver.h"

typedef struct action_s           action_t;           /* sent.h */
// NOTE: mk_bytecode_gen.sh assumes that sizeof(bytecode_t) == 1
typedef unsigned char             bytecode_t;         /* bytecode.h */
typedef bytecode_t              * bytecode_p;         /* bytecode.h */
typedef struct callback_s         callback_t;         /* simulate.h */
typedef struct case_list_entry_s  case_list_entry_t;  /* switch.h */
typedef struct case_state_s       case_state_t;       /* switch.h */
typedef struct error_handler_s    error_handler_t;    /* interpret.h */
typedef struct function_s         function_t;         /* exec.h */
typedef struct ident_s            ident_t;            /* lex.h */
typedef struct include_s          include_t;          /* exec.h */
typedef struct inherit_s          inherit_t;          /* exec.h */
typedef struct interactive_s      interactive_t;      /* comm.h */
typedef struct input_s            input_t;            /* comm.h */
typedef struct instr_s            instr_t;            /* exec.h */
typedef struct lambda_s           lambda_t;           /* closure.h */
typedef struct linenumbers_s      linenumbers_t;      /* exec.h */
typedef struct mapping_s          mapping_t;          /* mapping.h */
typedef struct object_s           object_t;           /* object.h */
typedef struct program_s          program_t;          /* exec.h */
typedef struct pointer_table      ptrtable_t;         /* ptrtable.h */
typedef struct regexp_s           regexp_t;           /* mregex.c */
typedef struct replace_ob_s       replace_ob_t;       /* object.h */
typedef struct rt_context_s       rt_context_t;       /* backend.h */
typedef struct sentence_s         sentence_t;         /* sent.h */
typedef struct shadow_s           shadow_t;           /* sent.h */
typedef struct simul_efun_table_s simul_efun_table_t; /* simul_efun.h */
typedef struct statistic_s        statistic_t;        /* backend.h */
typedef struct string_s           string_t;           /* mstrings.h */
typedef struct strbuf_s           strbuf_t;           /* strfuns.h */
#ifdef USE_STRUCTS
typedef struct struct_s           struct_t;           /* structs.h */
typedef struct struct_def_s       struct_def_t;       /* exec.h */
typedef struct struct_member_s    struct_member_t;    /* structs.h */
typedef struct struct_type_s      struct_type_t;      /* structs.h */
#endif /* USE_STRUCTS */
typedef struct svalue_s           svalue_t;           /* svalue.h */
typedef struct variable_s         variable_t;         /* exec.h */
typedef struct vector_s           vector_t;           /* array.h */
typedef struct wiz_list_s         wiz_list_t;         /* wiz_list.h */

#endif /* TYPEDEFS_H__ */
