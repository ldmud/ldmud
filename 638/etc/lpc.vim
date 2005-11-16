" Vim syntax file
" Language:	LPC
" Maintainer:	Shizhu Pan <poet@mudbuilder.net>
" URL:		http://poet.tomud.com/pub/lpc.vim.bz2
" Last Change:	2003 May 11
" Comments:	If you are using Vim 6.2 or later, see :h lpc.vim for
"		file type recognizing, if not, you had to use modeline.
"               Changed for LDMud 3.2 and 3.3, Coogan@Tubmud, 02-Sep-2003


" Nodule: This is the start nodule. {{{1

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" Nodule: Keywords {{{1

" LPC keywords
" keywords should always be highlighted so "contained" is not used.
syn cluster	lpcKeywdGrp	contains=lpcConditional,lpcLabel,lpcOperator,lpcRepeat,lpcStatement,lpcModifier,lpcReserved

syn keyword	lpcConditional	if else switch
syn keyword	lpcLabel	case default
syn keyword	lpcOperator	catch efun in inherit nolog publish
syn keyword	lpcRepeat	do for foreach while
syn keyword	lpcStatement	break continue return function

syn match	lpcEfunError	/efun[^:]/ display

" Illegal to use keyword as function
" It's not working, maybe in the next version.
syn keyword	lpcKeywdError	contained if for foreach return switch while

" These are keywords only because they take lvalue or type as parameter,
" so these keywords should only be used as function but cannot be names of
" user-defined functions.
" syn keyword	lpcKeywdFunc	new parse_command sscanf time_expression

" Nodule: Type and modifiers {{{1

" Type names list.

" Special types
syn keyword	lpcType		void mixed unknown
" Scalar/Value types.
syn keyword	lpcType		int float string status
" Pointer types.
syn keyword	lpcType		array function mapping object struct
" Other types.
syn keyword     lpcType	        closure funcall


" Type modifier.
syn keyword	lpcModifier	nomask private public
syn keyword	lpcModifier	varargs virtual

" sensible modifiers
syn keyword	lpcModifier	nosave protected static


" Nodule: Applies {{{1

" Match a function declaration or function pointer
syn match	lpcApplyDecl	excludenl /->\h\w*(/me=e-1 contains=lpcApplies transparent display

" We should note that in func_spec.c the efun definition syntax is so
" complicated that I use such a long regular expression to describe.
syn match	lpcLongDecl	excludenl /\(\s\|\*\)\h\+\s\h\+(/me=e-1 contains=@lpcEfunGroup,lpcType,@lpcKeywdGrp transparent display

" this is form for all functions
" ->foo() form had been excluded
syn match	lpcFuncDecl	excludenl /\h\w*(/me=e-1 contains=lpcApplies,@lpcEfunGroup,lpcKeywdError transparent display

" The (: :) parenthesis or $() forms a function pointer
syn match	lpcFuncName	/(:\s*\h\+\s*:)/me=e-1 contains=lpcApplies,@lpcEfunGroup transparent display contained
syn match	lpcFuncName	/(:\s*\h\+,/ contains=lpcApplies,@lpcEfunGroup transparent display contained
syn match	lpcFuncName	/\$(\h\+)/ contains=lpcApplies,@lpcEfunGroup transparent display contained

" Applies list.
"       system applies for compat mode
syn keyword     lpcApplies      contained add_weight can_put_and_get drop get prevent_insert query_weight 
"       system applies for native mode
syn keyword     lpcApplies      contained create
"       system applies
syn keyword     lpcApplies      contained __INIT catch_msg clean_up exit heart_beat id init reset 
"       interactive
syn keyword     lpcApplies      contained catch_tell logon modify_command 
"       master applies
syn keyword     lpcApplies      contained compile_object connect creator_file dangling_lfun_closure disconnect epilog external_master_reload flag get_bb_uid get_ed_buffer_save_file_name get_master_uid get_simul_efun get_wiz_name heart_beat_error inaugurate_master include_file inherit_file log_error make_path_absolute notify_shutdown preload prepare_destruct printf_obj_name privilege_violation query_allow_shadow quota_demon reactivate_destructed_master receive_imp receive_udp remove_player retrieve_ed_setup runtime_error save_ed_setup slow_shutdown stale_erq
syn keyword     lpcApplies      contained valid_exec valid_query_snoop valid_read valid_seteuid valid_snoop valid_trace valid_write
"       parsing
syn keyword     lpcApplies      contained parse_command_adjectiv_id_list parse_command_all_word parse_command_id_list parse_command_plural_id_list parse_command_prepos_list


" Nodule: Efuns {{{1

syn cluster	lpcEfunGroup	contains=lpc_efuns,lpcOldEfuns,lpcNewEfuns,lpcKeywdFunc,lpcCompatEfuns,lpcNativeEfuns,lpcSimulEfuns

"    syn match   lpcErrFunc	/#`\h\w*/
" Shell compatible first line comment.
"    syn region	lpcCommentFunc	start=/^#!/ end=/$/

" efuns which are [to be] removed in newer versions of LDMud.
syn keyword     lpcOldEfuns     contained add_verb add_xverb allocate_mapping copy_mapping efun308 file_name filter_array filter_mapping m_sizeof map_array map_mapping mapping_contains member_array parse_command query_imp_port send_imp set_auto_include_string transfer

" new efuns of LDMud 3.3
syn keyword     lpcNewEfuns     contained baseof call_direct call_direct_resolved match_command md5_crypt regmatch
syn keyword     lpcNewEfuns     contained start_mccp_compress end_mccp_compress query_mccp query_mccp_stats
syn keyword     lpcNewEfuns     contained pg_connect pg_query pg_pending pg_close
syn keyword     lpcNewEfuns     contained struct_info structp to_struct
syn keyword     lpcNewEfuns     contained tls_query_connection_state tls_query_connection_info tls_init_connection tls_deinit_connection tls_error

syn keyword     lpcCompatEfuns  contained creator 
syn keyword     lpcNativeEfuns  contained export_uid geteuid getuid seteuid

" simul efuns, add your simul efuns here
syn keyword     lpcSimulEfuns   contained transfer

" LPC efuns list.
" Efuns list {{{2
syn keyword     lpc_efuns       contained abs acos add_action all_environment all_inventory allocate allocate_mapping and_bits apply asin assoc atan atan2 attach_erq_demon
syn keyword     lpc_efuns       contained binary_message bind_lambda blueprint break_point
syn keyword     lpc_efuns       contained call_other call_out call_out_info call_resolved caller_stack caller_stack_depth capitalize cat catch ceil clear_bit clone_object clonep clones closurep command command_stack command_stack_depth copy copy_bits copy_file cos count_bits crypt ctime
syn keyword     lpc_efuns       contained db_affected_rows db_close db_coldefs db_connect db_conv_string db_error db_exec db_fetch db_handles db_insert_id debug_info debug_message deep_copy deep_inventory destruct disable_commands
syn keyword     lpc_efuns       contained ed enable_commands environment exec execute_command exp expand_define explode extern_call extract
syn keyword     lpc_efuns       contained file_size filter filter_indices filter_objects find_call_out find_input_to find_object first_inventory floatp floor funcall function_exists functionlist
syn keyword     lpc_efuns       contained garbage_collection get_dir get_error_file get_eval_cost get_extra_wizinfo get_type_info gmtime
syn keyword     lpc_efuns       contained heart_beat_info
syn keyword     lpc_efuns       contained implode include_list inherit_list input_to input_to_info insert_alist interactive intersect_alist intp invert_bits
syn keyword     lpc_efuns       contained lambda last_bit last_instructions limited living load_name load_object localtime log lower_case
syn keyword     lpc_efuns       contained m_add m_allocate m_contains m_delete m_entry m_indices m_reallocate m_values make_shared_string map map_indices map_objects mappingp max md5 member min mkdir mkmapping move_object
syn keyword     lpc_efuns       contained negate next_bit next_inventory notify_fail
syn keyword     lpc_efuns       contained object_info object_name object_time objectp or_bits order_alist
syn keyword     lpc_efuns       contained pointerp pow present present_clone previous_object printf process_string program_name program_time
syn keyword     lpc_efuns       contained query_actions query_command query_editing query_idle query_input_pending query_ip_name query_ip_number query_limits query_load_average query_mud_port query_notify_fail query_once_interactive query_shadowing query_snoop query_udp_port query_verb quote
syn keyword     lpc_efuns       contained raise_error random read_bytes read_file referencep regexp regexplode regmatch regreplace remove_action remove_call_out remove_input_to remove_interactive rename rename_object replace_program restore_object restore_value rm rmdir rmember rusage
syn keyword     lpc_efuns       contained save_object save_value say send_erq send_udp set_bit set_buffer_size set_combine_charset set_connection_charset set_driver_hook set_environment set_extra_wizinfo set_extra_wizinfo_size set_heart_beat set_is_wizard set_light set_limits set_modify_command set_next_reset set_prompt set_this_object set_this_player sgn shadow shutdown sin sizeof slice_array snoop sort_array sprintf sqrt sscanf stringp strlen strrstr strstr swap symbol_function symbol_variable symbolp
syn keyword     lpc_efuns       contained tail tan tell_object tell_room terminal_colour test_bit this_interactive this_object this_player throw time to_array to_float to_int to_object to_string trace traceprefix transpose_array trim typeof
syn keyword     lpc_efuns       contained unbound_lambda unique_array unmkmapping unquote unshadow upper_case users utime
syn keyword     lpc_efuns       contained variable_exists variable_list
syn keyword     lpc_efuns       contained walk_mapping widthof wizlist_info write write_bytes write_file
syn keyword     lpc_efuns       contained xor_bits

" Nodule: Constants {{{1

" LPC Constants.
" like keywords, constants are always highlighted, be careful to choose only
" the constants we used to add to this list.
syn keyword     lpcConstant    LPC3 __LDMUD__ __EUIDS__ COMPAT_FLAG __COMPAT_MODE__ __STRICT_EUIDS__ __FILENAME_SPACES__ __MASTER_OBJECT__ __FILE__ __LINE__ __DIR__ __PATH__ __VERSION__ __VERSION_MAJOR__ __VERSION_MINOR__ __VERSION_MICRO__ __VERSION_PATCH__ __DOMAIN_NAME__ __HOST_IP_NUMBER__ __HOST_NAME__ __MAX_RECURSION__ __MAX_EVAL_COST__ __CATCH_EVAL_COST__ __MASTER_EVAL_COST__ __RESET_TIME__ __CLEANUP_TIME__ __EFUN_DEFINED__ __DRIVER_LOG__ __WIZLIST__ __INT_MAX__ __INT_MIN__ __FLOAT_MAX__ __FLOAT_MIN__ __MAX_MALLOC__
 

" Nodule: Todo for this file.  {{{1

" TODO : need to check for LPC4 syntax and other series of LPC besides
" v22, b21 and l32, if you had a good idea, contact me at poet@mudbuilder.net
" and I will be appreciated about that.

" Notes about some FAQ:
"
" About variables : We adopts the same behavior for C because almost all the
" LPC programmers are also C programmers, so we don't need separate settings
" for C and LPC. That is the reason why I don't change variables like
" "c_no_utf"s to "lpc_no_utf"s.
"
" Copy : Some of the following seems to be copied from c.vim but not quite
" the same in details because the syntax for C and LPC is different.
"
" Color scheme : this syntax file had been thouroughly tested to work well
" for all of the dark-backgrounded color schemes Vim has provided officially,
" and it should be quite Ok for all of the bright-backgrounded color schemes,
" of course it works best for the color scheme that I am using, download it
" from http://poet.tomud.com/pub/ps_color.vim.bz2 if you want to try it.
"

" Nodule: String and Character {{{1


" String and Character constants
" Highlight special characters (those which have a backslash) differently
syn match	lpcSpecial	display contained "\\\(x\x\+\|\o\{1,3}\|.\|$\)"
if !exists("c_no_utf")
  syn match	lpcSpecial	display contained "\\\(u\x\{4}\|U\x\{8}\)"
endif

" LPC version of sprintf() format,
syn match	lpcFormat	display "%\(\d\+\)\=[-+ |=#@:.]*\(\d\+\)\=\('\I\+'\|'\I*\\'\I*'\)\=[OsdicoxXf]" contained
syn match	lpcFormat	display "%%" contained
syn region	lpcString	start=+L\="+ skip=+\\\\\|\\"+ end=+"+ contains=lpcSpecial,lpcFormat
" lpcCppString: same as lpcString, but ends at end of line
syn region	lpcCppString	start=+L\="+ skip=+\\\\\|\\"\|\\$+ excludenl end=+"+ end='$' contains=lpcSpecial,lpcFormat

" LPC preprocessor for the text formatting short cuts
" Thanks to Dr. Charles E. Campbell <cec@gryphon.gsfc.nasa.gov>
"	he suggests the best way to do this.
syn region	lpcTextString	start=/@\z(\h\w*\)$/ end=/^\z1/ contains=lpcSpecial
syn region	lpcArrayString	start=/@@\z(\h\w*\)$/ end=/^\z1/ contains=lpcSpecial

" Character
syn match	lpcCharacter	"L\='[^\\]'"
syn match	lpcCharacter	"L'[^']*'" contains=lpcSpecial
syn match	lpcSpecialError	"L\='\\[^'\"?\\abefnrtv]'"
syn match	lpcSpecialCharacter "L\='\\['\"?\\abefnrtv]'"
syn match	lpcSpecialCharacter display "L\='\\\o\{1,3}'"
syn match	lpcSpecialCharacter display "'\\x\x\{1,2}'"
syn match	lpcSpecialCharacter display "L'\\x\x\+'"

" Nodule: White space {{{1

" when wanted, highlight trailing white space
if exists("c_space_errors")
  if !exists("c_no_trail_space_error")
    syn match	lpcSpaceError	display excludenl "\s\+$"
  endif
  if !exists("c_no_tab_space_error")
    syn match	lpcSpaceError	display " \+\t"me=e-1
  endif
endif

" Nodule: Parenthesis and brackets {{{1

" catch errors caused by wrong parenthesis and brackets
syn cluster	lpcParenGroup	contains=lpcParenError,lpcIncluded,lpcSpecial,lpcCommentSkip,lpcCommentString,lpcComment2String,@lpcCommentGroup,lpcCommentStartError,lpcUserCont,lpcUserLabel,lpcBitField,lpcCommentSkip,lpcOctalZero,lpcCppOut,lpcCppOut2,lpcCppSkip,lpcFormat,lpcNumber,lpcFloat,lpcOctal,lpcOctalError,lpcNumbersCom
syn region	lpcParen	transparent start='(' end=')' contains=ALLBUT,@lpcParenGroup,lpcCppParen,lpcErrInBracket,lpcCppBracket,lpcCppString,@lpcEfunGroup,lpcApplies,lpcKeywdError
" lpcCppParen: same as lpcParen but ends at end-of-line; used in lpcDefine
syn region	lpcCppParen	transparent start='(' skip='\\$' excludenl end=')' end='$' contained contains=ALLBUT,@lpcParenGroup,lpcErrInBracket,lpcParen,lpcBracket,lpcString,@lpcEfunGroup,lpcApplies,lpcKeywdError
syn match	lpcParenError	display ")"
syn match	lpcParenError	display "\]"
" for LPC:
" Here we should consider the array ({ }) parenthesis and mapping ([ ])
" parenthesis and multiset (< >) parenthesis.
syn match	lpcErrInParen	display contained "[^^]{"ms=s+1
syn match	lpcErrInParen	display contained "\(}\|\]\)[^)]"me=e-1
syn region	lpcBracket	transparent start='\[' end=']' contains=ALLBUT,@lpcParenGroup,lpcErrInParen,lpcCppParen,lpcCppBracket,lpcCppString,@lpcEfunGroup,lpcApplies,lpcFuncName,lpcKeywdError
" lpcCppBracket: same as lpcParen but ends at end-of-line; used in lpcDefine
syn region	lpcCppBracket	transparent start='\[' skip='\\$' excludenl end=']' end='$' contained contains=ALLBUT,@lpcParenGroup,lpcErrInParen,lpcParen,lpcBracket,lpcString,@lpcEfunGroup,lpcApplies,lpcFuncName,lpcKeywdError
syn match	lpcErrInBracket	display contained "[);{}]"

" Nodule: Numbers {{{1

" integer number, or floating point number without a dot and with "f".
syn case ignore
syn match	lpcNumbers	display transparent "\<\d\|\.\d" contains=lpcNumber,lpcFloat,lpcOctalError,lpcOctal
" Same, but without octal error (for comments)
syn match	lpcNumbersCom	display contained transparent "\<\d\|\.\d" contains=lpcNumber,lpcFloat,lpcOctal
syn match	lpcNumber	display contained "\d\+\(u\=l\{0,2}\|ll\=u\)\>"
" hex number
syn match	lpcNumber	display contained "0x\x\+\(u\=l\{0,2}\|ll\=u\)\>"
" Flag the first zero of an octal number as something special
syn match	lpcOctal	display contained "0\o\+\(u\=l\{0,2}\|ll\=u\)\>" contains=lpcOctalZero
syn match	lpcOctalZero	display contained "\<0"
syn match	lpcFloat	display contained "\d\+f"
" floating point number, with dot, optional exponent
syn match	lpcFloat	display contained "\d\+\.\d*\(e[-+]\=\d\+\)\=[fl]\="
" floating point number, starting with a dot, optional exponent
syn match	lpcFloat	display contained "\.\d\+\(e[-+]\=\d\+\)\=[fl]\=\>"
" floating point number, without dot, with exponent
syn match	lpcFloat	display contained "\d\+e[-+]\=\d\+[fl]\=\>"
" flag an octal number with wrong digits
syn match	lpcOctalError	display contained "0\o*[89]\d*"
syn case match

" Nodule: Comment string {{{1

" lpcCommentGroup allows adding matches for special things in comments
syn keyword	lpcTodo		contained TODO FIXME XXX
syn cluster	lpcCommentGroup	contains=lpcTodo

if exists("c_comment_strings")
  " A comment can contain lpcString, lpcCharacter and lpcNumber.
  syntax match	lpcCommentSkip	contained "^\s*\*\($\|\s\+\)"
  syntax region lpcCommentString	contained start=+L\=\\\@<!"+ skip=+\\\\\|\\"+ end=+"+ end=+\*/+me=s-1 contains=lpcSpecial,lpcCommentSkip
  syntax region lpcComment2String	contained start=+L\=\\\@<!"+ skip=+\\\\\|\\"+ end=+"+ end="$" contains=lpcSpecial
  syntax region  lpcCommentL	start="//" skip="\\$" end="$" keepend contains=@lpcCommentGroup,lpcComment2String,lpcCharacter,lpcNumbersCom,lpcSpaceError
  syntax region lpcComment	matchgroup=lpcCommentStart start="/\*" matchgroup=NONE end="\*/" contains=@lpcCommentGroup,lpcCommentStartError,lpcCommentString,lpcCharacter,lpcNumbersCom,lpcSpaceError
else
  syn region	lpcCommentL	start="//" skip="\\$" end="$" keepend contains=@lpcCommentGroup,lpcSpaceError
  syn region	lpcComment	matchgroup=lpcCommentStart start="/\*" matchgroup=NONE end="\*/" contains=@lpcCommentGroup,lpcCommentStartError,lpcSpaceError
endif
" keep a // comment separately, it terminates a preproc. conditional
syntax match	lpcCommentError	display "\*/"
syntax match	lpcCommentStartError display "/\*"me=e-1 contained

" Nodule: Pre-processor {{{1

syn region	lpcPreCondit	start="^\s*#\s*\(if\|ifdef\|ifndef\|elif\)\>" skip="\\$" end="$" end="//"me=s-1 contains=lpcComment,lpcCppString,lpcCharacter,lpcCppParen,lpcParenError,lpcNumbers,lpcCommentError,lpcSpaceError
syn match	lpcPreCondit	display "^\s*#\s*\(else\|endif\)\>"
if !exists("c_no_if0")
  syn region	lpcCppOut		start="^\s*#\s*if\s\+0\+\>" end=".\|$" contains=lpcCppOut2
  syn region	lpcCppOut2	contained start="0" end="^\s*#\s*\(endif\>\|else\>\|elif\>\)" contains=lpcSpaceError,lpcCppSkip
  syn region	lpcCppSkip	contained start="^\s*#\s*\(if\>\|ifdef\>\|ifndef\>\)" skip="\\$" end="^\s*#\s*endif\>" contains=lpcSpaceError,lpcCppSkip
endif
syn region	lpcIncluded	display contained start=+"+ skip=+\\\\\|\\"+ end=+"+
syn match	lpcIncluded	display contained "<[^>]*>"
syn match	lpcInclude	display "^\s*#\s*include\>\s*["<]" contains=lpcIncluded
syn match lpcLineSkip	"\\$"
syn cluster	lpcPreProcGroup	contains=lpcPreCondit,lpcIncluded,lpcInclude,lpcDefine,lpcErrInParen,lpcErrInBracket,lpcUserLabel,lpcSpecial,lpcOctalZero,lpcCppOut,lpcCppOut2,lpcCppSkip,lpcFormat,lpcNumber,lpcFloat,lpcOctal,lpcOctalError,lpcNumbersCom,lpcString,lpcCommentSkip,lpcCommentString,lpcComment2String,@lpcCommentGroup,lpcCommentStartError,lpcParen,lpcBracket,lpcMulti,lpcKeywdError
syn region	lpcDefine	start="^\s*#\s*\(define\|undef\)\>" skip="\\$" end="$" end="//"me=s-1 contains=ALLBUT,@lpcPreProcGroup

if exists("lpc_pre_v22")
    syn region	lpcPreProc	start="^\s*#\s*\(pragma\>\|echo\>\)" skip="\\$" end="$" keepend contains=ALLBUT,@lpcPreProcGroup
else
    syn region	lpcPreProc	start="^\s*#\s*\(pragma\>\|echo\>\|warn\>\|error\>\)" skip="\\$" end="$" keepend contains=ALLBUT,@lpcPreProcGroup
endif

" Nodule: User labels {{{1

" Highlight Labels
" User labels in LPC is not allowed, only "case x" and "default" is supported
syn cluster	lpcMultiGroup	contains=lpcIncluded,lpcSpecial,lpcCommentSkip,lpcCommentString,lpcComment2String,@lpcCommentGroup,lpcCommentStartError,lpcUserCont,lpcUserLabel,lpcBitField,lpcOctalZero,lpcCppOut,lpcCppOut2,lpcCppSkip,lpcFormat,lpcNumber,lpcFloat,lpcOctal,lpcOctalError,lpcNumbersCom,lpcCppParen,lpcCppBracket,lpcCppString,lpcKeywdError
syn region	lpcMulti		transparent start='\(case\|default\|public\|protected\|private\)' skip='::' end=':' contains=ALLBUT,@lpcMultiGroup

syn cluster	lpcLabelGroup	contains=lpcUserLabel
syn match	lpcUserCont	display "^\s*lpc:$" contains=@lpcLabelGroup

" Don't want to match anything
syn match	lpcUserLabel	display "lpc" contained

" Nodule: Initializations {{{1

if exists("c_minlines")
  let b:c_minlines = c_minlines
else
  if !exists("c_no_if0")
    let b:c_minlines = 50	" #if 0 constructs can be long
  else
    let b:c_minlines = 15	" mostly for () constructs
  endif
endif
exec "syn sync ccomment lpcComment minlines=" . b:c_minlines

" Make sure these options take place since we no longer depend on file type
" plugin for C
setlocal cindent
setlocal fo-=t fo+=croql
setlocal comments=sO:*\ -,mO:*\ \ ,exO:*/,s1:/*,mb:*,ex:*/,://
set cpo-=C

" Win32 can filter files in the browse dialog
if has("gui_win32") && !exists("b:browsefilter")
    let b:browsefilter = "LPC Source Files (*.c *.d *.h)\t*.c;*.d;*.h\n" .
	\ "LPC Data Files (*.scr *.o *.dat)\t*.scr;*.o;*.dat\n" .
	\ "Text Documentation (*.txt)\t*.txt\n" .
	\ "All Files (*.*)\t*.*\n"
endif

" Nodule: Highlight links {{{1

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_lpc_syn_inits")
  if version < 508
    let did_lpc_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink lpcModifier		lpcStorageClass

  HiLink lpcQuotedFmt		lpcFormat
  HiLink lpcFormat		lpcSpecial
  HiLink lpcCppString		lpcString	" Cpp means
						" C Pre-Processor
  HiLink lpcCommentL		lpcComment
  HiLink lpcCommentStart	lpcComment
  HiLink lpcUserLabel		lpcLabel
  HiLink lpcSpecialCharacter	lpcSpecial
  HiLink lpcOctal		lpcPreProc
  HiLink lpcOctalZero		lpcSpecial  " LPC will treat octal numbers
					    " as decimals, programmers should
					    " be aware of that.
  HiLink lpcEfunError		lpcError
  HiLink lpcKeywdError		lpcError
  HiLink lpcOctalError		lpcError
  HiLink lpcParenError		lpcError
  HiLink lpcErrInParen		lpcError
  HiLink lpcErrInBracket	lpcError
  HiLink lpcCommentError	lpcError
  HiLink lpcCommentStartError	lpcError
  HiLink lpcSpaceError		lpcError
  HiLink lpcSpecialError	lpcError
  HiLink lpcErrFunc		lpcError

  " HiLink lpcOldEfuns	lpc_efuns
  HiLink lpcOldEfuns	        lpcReserved
  HiLink lpcNewEfuns	        lpcFunction
  HiLink lpcCompatEfuns         lpcFunction
  HiLink lpcNativeEfuns         lpcFunction
  HiLink lpcSimulEfuns          lpcFunction
  HiLink lpc_efuns		lpcFunction

  HiLink lpcReserved		lpcPreProc
  HiLink lpcTextString		lpcString   " This should be preprocessors, but
  HiLink lpcArrayString		lpcPreProc  " let's make some difference
					    " between text and array

  HiLink lpcIncluded		lpcString
  HiLink lpcCommentString	lpcString
  HiLink lpcComment2String	lpcString
  HiLink lpcCommentSkip		lpcComment
  HiLink lpcCommentFunc		lpcComment

  HiLink lpcCppSkip		lpcCppOut
  HiLink lpcCppOut2		lpcCppOut
  HiLink lpcCppOut		lpcComment

  " Standard type below
  HiLink lpcApplies		Special
  HiLink lpcCharacter		Character
  HiLink lpcComment		Comment
  HiLink lpcConditional		Conditional
  HiLink lpcConstant		Constant
  HiLink lpcDefine		Macro
  HiLink lpcError		Error
  HiLink lpcFloat		Float
  HiLink lpcFunction		Function
  HiLink lpcIdentifier		Identifier
  HiLink lpcInclude		Include
  HiLink lpcLabel		Label
  HiLink lpcNumber		Number
  HiLink lpcOperator		Operator
  HiLink lpcPreCondit		PreCondit
  HiLink lpcPreProc		PreProc
  HiLink lpcRepeat		Repeat
  HiLink lpcStatement		Statement
  HiLink lpcStorageClass	StorageClass
  HiLink lpcString		String
  HiLink lpcStructure		Structure
  HiLink lpcSpecial		LineNr
  HiLink lpcTodo		Todo
  HiLink lpcType		Type

  delcommand HiLink
endif

" Nodule: This is the end nodule. {{{1

let b:current_syntax = "lpc"

" vim:ts=8:nosta:sw=2:ai:si:
" vim600:set fdm=marker: }}}1
