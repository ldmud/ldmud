
#include "config.h"

#ifdef USE_STRING_COMPILER

#include "closure.h"
#include "svalue.h"
#include "simulate.h"
#include "interpret.h"
#include "xalloc.h"
#include "array.h"
#include "mapping.h"
#include "mstrings.h"
#include "object.h"
#include "lex.h"
#include "prolang.h"
#include "simul_efun.h"
#include "stdstrings.h"
#include "backend.h"
#include "swap.h"
#include "lang.h"
#include "instrs.h"

#include <string.h>
#include <stdarg.h>
#include <stdio.h>
#include <ctype.h>

/* pointer to current lex position */
char *sc_p;
char *sc_start_p;

/* string with custom expression-terminators */
char *terminators;

/* current bracelevel, terminators only affect level 0 */
int bracelevel;

/* additional info passed to callbacks */
svalue_t *sc_add_info;

/* oldstyle determines interpretation
   0: LPCish, strings have to be quoted, closures are closures
   1: strings in args need not to be quoted, no variables possible,
      closures are calls
   2: like 1, but indicates that we are inside a function call
 */
int oldstyle;

/* the callbacks */
callback_t *funfun;
callback_t *varfun;
callback_t *primfun;
callback_t *noargsfun;
svalue_t *funfunmap;
svalue_t *primfunmap;
svalue_t *varfunmap;
svalue_t *noargsfunmap;
int primfunlen;
int flags;
object_t *sc_object;

struct sc_state_s
{
   char *sc_p;
   char *sc_start_p;
   char *terminators;

   int bracelevel;
   svalue_t *sc_add_info;

   int oldstyle;

   callback_t *funfun;
   callback_t *varfun;
   callback_t *primfun;
   callback_t *noargsfun;
   svalue_t *funfunmap;
   svalue_t *primfunmap;
   svalue_t *varfunmap;
   svalue_t *noargsfunmap;
   int primfunlen;
   int flags;
   object_t *sc_object;
};

typedef struct sc_state_s sc_state_t;

int recursion_depth=0;

static void sc_errorf(char *format, ...);
static char *sc_strndup(char *str, int n);
static string_t *sc_strndup_shared(char *str, char *end);
static void make_array(svalue_t *ret);
static void copy_array(svalue_t *ret, int i);
static svalue_t *make_array_ofs(svalue_t *ret, int reserve);
static void put_efun(svalue_t *sp, int op);
static void push_efun(int op);
static int synth_closure_lex(char *yyp, char **ret, svalue_t *sp);
static int lex_resword(char *yyp, char **ret);
static void skip_space();
static int parse_part();
static int parse_code();
static int parse_statement();
static int parse_comma_expr();
static int parse_one_comma_expr();
static int see_skip(char *s);
/*static int is_terminator();*/
static int parse_expression(int level);
static int match_funcall(char *str, char **funend, char **ret);
static int match_oldstyle(char *str, char **ret);
static int match_variable(char *str, char **ret);
static int parse_primitive();
static int parse_arglist();
static int parse_m_arglist();
svalue_t *v_string_compiler(svalue_t *sp);
svalue_t *v_string_compiler_one(svalue_t *sp);
svalue_t *v_string_compiler_arglist(svalue_t *sp);
svalue_t *v_string_compiler_term(svalue_t *sp);
static svalue_t *sc_partial(svalue_t *sp, int mode);

static void sc_save_state(sc_state_t* s);
static void sc_restore_state(sc_state_t* s);
void postprocess (svalue_t *v);

static void
sc_save_state(sc_state_t* s)
{
   /* no need to save on recursion depth 0 */
   if (!recursion_depth++) return;
   if (recursion_depth>16)
      sc_errorf("too deep recursion in string_compiler");
   s->sc_p=sc_p;
   s->sc_start_p=sc_start_p;
   s->terminators=terminators;
   s->bracelevel=bracelevel;
   s->sc_add_info=sc_add_info;
   s->oldstyle=oldstyle;
   s->funfun=funfun;
   s->varfun=varfun;
   s->primfun=primfun;
   s->noargsfun=noargsfun;
   s->funfunmap=funfunmap;
   s->varfunmap=varfunmap;
   s->primfunmap=primfunmap;
   s->noargsfunmap=noargsfunmap;
   s->primfunlen=primfunlen;
   s->flags=flags;
   s->sc_object=sc_object;
}

static void
sc_restore_state(sc_state_t* s)
{
   if (!--recursion_depth) return;
   sc_p=s->sc_p;
   sc_start_p=s->sc_start_p;
   terminators=s->terminators;
   bracelevel=s->bracelevel;
   sc_add_info=s->sc_add_info;
   oldstyle=s->oldstyle;
   funfun=s->funfun;
   varfun=s->varfun;
   primfun=s->primfun;
   noargsfun=s->noargsfun;
   funfunmap=s->funfunmap;
   varfunmap=s->varfunmap;
   primfunmap=s->primfunmap;
   noargsfunmap=s->noargsfunmap;
   primfunlen=s->primfunlen;
   flags=s->flags;
   sc_object=s->sc_object;
}

/* the first error counts... */
static void
sc_errorf(char *format, ...)
{
    va_list va;
    static char buff[16384];
    char c;
    char *ptr;
    int len;
    
    va_start(va, format);
    vsprintf(buff, format, va);
    va_end(va);

    ptr=buff+strlen(buff)+1;

    len=sizeof(buff)-(ptr-buff)-1;
    
    c=*sc_p;
    *sc_p=0;
    if (strlen(sc_start_p)>=len) len=strlen(sc_start_p)-len;
    else len=0;
    
    strcpy(ptr,sc_start_p+len);
    *sc_p=c;
    recursion_depth = 0;
    error("%s\nafter:\n%s\n",buff,ptr);
}

static char *
sc_strndup(char *str, int n)
{
   char *ret=(char*)xalloc(n+1);
   if (!ret)
   {
      sc_errorf("Out of memory");
      return ret;
   }
   strncpy(ret,str,n);
   ret[n]=0;
   return ret;
}

static string_t *
sc_strndup_shared(char *str, char *end)
{
   char c;
   string_t *ret;

   c=*end;
   *end=0;
   ret=new_n_tabled(str, strlen(str));
   *end=c;
   return ret;
}

static void
make_array(svalue_t *ret)
{
   int i=inter_sp-ret+1;
   vector_t *res=allocate_uninit_array(i);
         
   if (!res)
   {
      sc_errorf("Out of memory");
      return;
   }

   for(;i--;)
      transfer_svalue_no_free(res->item+i,ret+i);
   
   put_array(ret,res);
   inter_sp=ret;
}

static void
copy_array(svalue_t *ret, int i)
{
   vector_t *res=allocate_uninit_array(i);
         
   if (!res)
   {
      sc_errorf("Out of memory");
      return;
   }

   for(;i--;)
      assign_svalue_no_free(res->item+i,ret+i);
   
   push_array(inter_sp, res);
}

static svalue_t *
make_array_ofs(svalue_t *ret, int reserve)
{
   int i=inter_sp-ret+1;
   vector_t *res=allocate_uninit_array(i+reserve);
         
   if (!res)
   {
      sc_errorf("Out of memory");
      return NULL;
   }

   for(;i--;)
      transfer_svalue_no_free(res->item+i+reserve,ret+i);

   for(;reserve--;) res->item[reserve].type=T_INVALID;
   
   put_array(ret,res);
   inter_sp=ret;
   return res->item;
}

static void
put_efun(svalue_t *sp, int op)
{
   sp->type = T_CLOSURE;
         
   if (instrs[op].Default == -1) {
      sp->x.closure_type = (short)(op + CLOSURE_OPERATOR);
   } else {
      sp->x.closure_type = (short)(op + CLOSURE_EFUN);
   }
   sp->u.ob = ref_object(sc_object, "put_efun");
}

static void
push_efun(int op)
{
   put_efun(++inter_sp, op);
}


/* returns closure-number of closure at *yyp */
static int
synth_closure_lex(char *yyp, char **ret, svalue_t *sp)
{
   char *outp;
   ident_t *p;
   char *wordstart = yyp;
   Bool efun_override;
   char c;
   
   /* True if 'efun::' is specified. */

   /* Set yyp to the last character of the functionname
    * after the #'.
    */
   do
      c = *yyp++;
   while (isalunum(c));
   c = *--yyp;
   /* the assignment is good for the data flow analysis :-} */

   /* Just one character? It must be an operator */
   if (yyp == wordstart)
   {
      int i;

      if ((i = symbol_operator(yyp, &outp)) < 0)
         sc_errorf("Missing function name after #'");
      if (ret) *ret=outp;
      put_efun(sp,i);
      return i + CLOSURE_EFUN_OFFS;
   }

   /* Test for the 'efun::' override. If it is there,
    * reset wordstart/yyp to point to the name after the '::'.
    */
   efun_override = MY_FALSE;
   if (yyp - wordstart == 4
       && !strncmp(wordstart, "efun::", 6))
   {
      efun_override = MY_TRUE;
      wordstart = yyp += 2;
      do
         c = *yyp++;
      while (isalunum(c));
      c = *--yyp;
   }

   outp = yyp;

   /* Lookup the name parsed from the text */
   *yyp = '\0'; /* c holds the char at this place */
   p = make_shared_identifier(wordstart, I_TYPE_GLOBAL, 0);
   *yyp = c;
   if (!p) {
      sc_errorf("Out of memory");
      return 0;
   }

   /* modified copy of symbol_efun */

   /* Loop through the possible multiple definitions.
         */
   while (p->type > I_TYPE_GLOBAL)
   {
      /* Is it a reserved word? */
      if (p->type == I_TYPE_RESWORD)
      {
         int code;

         switch(code = p->u.code)
         {
          default:
            /* Unimplemented reserved word */
            if ( NULL != (p = p->inferior) )
               continue;
            if (ret) *ret=NULL;
            put_number(sp, 0);
            return 0; 

          case L_IF:
            code = F_BRANCH_WHEN_ZERO;
            break;

          case L_DO:
            code = F_BBRANCH_WHEN_NON_ZERO;
            break;

          case L_WHILE:
            /* the politically correct code   /
               /  was already taken, see above. */
            code = F_BBRANCH_WHEN_ZERO;
            break;

          case L_FOREACH:
            code = F_FOREACH;
            break;

          case L_CONTINUE:
            code = F_BRANCH;
            break;

          case L_DEFAULT:
            code = F_CSTRING0;
            break;

          case L_SWITCH:
            code = F_SWITCH;
            break;
          case L_BREAK:
            code = F_BREAK;
            break;
          case L_RETURN:
            code = F_RETURN;
            break;
          case L_SSCANF:
            code = F_SSCANF;
            break;
#ifdef SUPPLY_PARSE_COMMAND
          case L_PARSE_COMMAND:
            code = F_PARSE_COMMAND;
            break;
#endif
          case L_CATCH:
            code = F_CATCH;
            break;
         }

         /* Got the reserved word: return the closure value */
                
         sp->type = T_CLOSURE;
         sp->x.closure_type = (short)(code + CLOSURE_OPERATOR);
         sp->u.ob = ref_object(sc_object, "symbol_efun");

         if (ret) *ret=outp;
         return code;
      }
      if ( !(p = p->inferior) )
         break; /* Found a valid definition */
   }

   /* It is a real identifier */
        
   {
      program_t *prog;
      int i;
      string_t *shname;
      
      /* But it's a (new) local identifier or a non-existing function */
      if (p && p->type == I_TYPE_UNKNOWN)
         free_shared_identifier(p);

      /* modified copy of F_SYMBOL_FUNCTION */

      /* We need the object's program */
      if (O_PROG_SWAPPED(sc_object))
      {
         sc_object->time_of_ref = current_time;
         if (load_ob_from_swap(sc_object) < 0)
         {
            error("Out of memory\n");
         }
      }

      /* Find the function in the program */
      prog = sc_object->prog;

      *yyp = '\0'; /* c holds the char at this place */
      shname = find_tabled_str(wordstart);
      *yyp = c;

      /* if the string is not shared, it can't be a valid function */
      i = -1;
      if (shname)
	i = find_function(shname, prog);

        /* If the function exists and is visible, create the closure
         */
      if ( i >= 0
           && ( !(prog->functions[i] & (TYPE_MOD_STATIC|TYPE_MOD_PROTECTED|TYPE_MOD_PRIVATE) )
                || (    !(prog->functions[i] & TYPE_MOD_PRIVATE)
                        && current_object == sc_object)
                )
           )
      {
         lambda_t *l;
         ph_int closure_type;

         l = xalloc(sizeof *l);
         if (!l)
         {
            error("Out of memory.\n");
            /* NOTREACHED */
            return 0;
         }

         /* Set the closure */
         l->function.lfun.index = (unsigned short)i;
         l->function.lfun.context_size = 0;
	 l->function.lfun.ob = sc_object;
         closure_type = CLOSURE_LFUN;
         ref_object(sc_object, "symbol_function");
         ref_object(sc_object, "symbol_function");

         l->ref = 1;
         l->ob = sc_object; /* see above regarding the ref count */

         /* push the result */
         sp->type = T_CLOSURE;
         sp->x.closure_type = closure_type;
         sp->u.lambda = l;

         /* A last thing: take care of a pending replace_program */
         if ( !(prog->flags & P_REPLACE_ACTIVE)
              ||  !lambda_ref_replace_program(l, closure_type, 0, 0, 0) )
         {
            sc_object->flags |= O_LAMBDA_REFERENCED;
         }

         if (ret) *ret=outp;
         return i;
      }

   }
   
   if (!p || (p->type < I_TYPE_GLOBAL)) {
      if (ret) *ret=NULL;
      put_number(sp, 0);
      return 0; 
   }
   /* Attempting to override a 'nomask' simul efun?
         * Check it with a privilege violation.
         */
   if (efun_override && p->u.global.sim_efun >= 0
       && simul_efunp[p->u.global.sim_efun].flags & TYPE_MOD_NO_MASK)
   {
      svalue_t *res;

      push_ref_string(inter_sp, STR_NOMASK_SIMUL_EFUN);
      inter_sp++;
      if (current_object && !(current_object->flags & O_DESTRUCTED))
	put_object(inter_sp, current_object);
      else
	put_number(inter_sp, 0);
      push_ref_string(inter_sp, p->name);
      res = apply_master(STR_PRIVILEGE, 3);
            
      if (!res || res->type != T_NUMBER || res->u.number < 0)
      {
         /* Override attempt is fatal */
         sc_errorf(
            "Privilege violation: nomask simul_efun %s\n",
            p->name->str->txt
            );
      }
      else if (!res->u.number)
      {
         /* Override attempt not fatal, but rejected nevertheless */
         efun_override = MY_FALSE;
      }
   }

   /* Symbol is ok - create the closure value */
        
   sp->type = T_CLOSURE;
   if (!efun_override && p->u.global.sim_efun >= 0)
   {
      /* Handle non-overridden simul efuns */
          
      sp->x.closure_type = (short)(p->u.global.sim_efun + CLOSURE_SIMUL_EFUN);
      sp->u.ob = ref_object(sc_object, "symbol_efun");
   }
   else
   {
      /* Handle efuns (possibly aliased).
       * We know that p->u.global.efun >= 0 here.
             */
      sp->x.closure_type = (short)(p->u.global.efun + CLOSURE_EFUN);
      if (sp->x.closure_type > LAST_INSTRUCTION_CODE + CLOSURE_EFUN)
         sp->x.closure_type = (short)(CLOSURE_EFUN +
                                      efun_aliases[
					  sp->x.closure_type - CLOSURE_EFUN - LAST_INSTRUCTION_CODE - 1]);
      sp->u.ob = ref_object(sc_object, "symbol_efun");
   }
   if (ret) *ret=outp;
   return sp->x.closure_type;
}

/* returns closure-number of closure at *yyp */
static int
lex_resword(char *yyp, char **ret)
{
   char *outp;
   ident_t *p;
   char *wordstart = yyp;
   char c;
   
   /* True if 'efun::' is specified. */

   /* Set yyp to the last character of the functionname
    * after the #'.
    */
   do
      c = *yyp++;
   while (isalunum(c));
   c = *--yyp;
   /* the assignment is good for the data flow analysis :-} */

   /* Just one character? It must be an operator */
   if (yyp == wordstart)
   {
      if (ret) *ret=NULL;
      return 0;
   }

   outp = yyp;

   /* Lookup the name parsed from the text */
   *yyp = '\0'; /* c holds the char at this place */
   p = make_shared_identifier(wordstart, I_TYPE_GLOBAL, 0);
   *yyp = c;
   if (!p) {
      sc_errorf("Out of memory");
      return 0;
   }

   if (p->type == I_TYPE_RESWORD)
   {
      if (ret) *ret=outp;
      return p->u.code;
   }
   if (p && p->type == I_TYPE_UNKNOWN)
     free_shared_identifier(p);
   if (ret) *ret=0;
   return 0;
}

static void
skip_space()
{
   while(isspace(*sc_p)) sc_p++;
}

/*
static int
is_terminator()
{
   return strchr(terminators,*sc_p)!=NULL;
}
*/

#define is_terminator() (strchr(terminators,*sc_p)!=NULL)


/* parse_XXX

   precondition:
     sc_p is in string to parse
     sc_p[0] may be 0
   postcondition: returncode=parse_XXX()
     returncode:
       1: [ returnvalue is at top of stack ]
       0: [ stack is unchanged, no returnvalue,
            no error, but can not parse this, try something else ]
     does NOT return on error          
*/

/* parse_part

   precondition:
   postcondition:
     sc_p[0]=0
   | sc_p[0]='$'
     *ret=Array(Closure)
   | *ret=String
*/

static int
parse_part()
{
   char *start=sc_p;
   char *part;

   if (!*sc_p || (terminators && is_terminator())) return 0;
   
   if (*sc_p=='$')
   {
      sc_p++;
      if (sc_p[0]=='{') return parse_code();
      else if (isalpha(sc_p[0])) return parse_primitive();
   }
   while(*sc_p && *sc_p!='$' && (!terminators || !is_terminator())) sc_p++;
   part=sc_strndup(start,sc_p-start);
   push_c_string(inter_sp,part);
   xfree(part);
   return 1;
}

#define set_ret svalue_t *ret=inter_sp+1

/* parse_code
   
   precondition:
     sc_p[0]='{'
   postcondition:
     [ sc_p[-1]='}', 
       [   *ret=Number(0)
         | *ret=statement[0]
         | *ret=({#', }) + statement[0..]
       ]
     ]
*/
static int
parse_code()
{
   svalue_t *sp;
   set_ret;
   int oldstyle_save=oldstyle;
   int expectbrace;
   
   oldstyle=0;
   bracelevel++;
   
   /* skip '{' */
   sc_p++;

   skip_space();
   expectbrace=0;
   while(1)
   {
      if (sc_p[0]=='}')
         break;

      if (expectbrace)
         sc_errorf("; expected");
      
      if (!parse_statement())
         sc_errorf("statement expected"); /* does not return */
   
      skip_space();
      if (sc_p[0]==';')
      {
         sc_p++;
         skip_space();
      }
      else expectbrace=1;
   }

   if (sc_p[0]=='}') sc_p++;
   else sc_errorf("unterminated block");

   switch(inter_sp-ret)
   {
    case -1:
      push_number(inter_sp, 0);
      break;
    case 0:
      break;
    default:
      sp=make_array_ofs(ret,1);
      put_efun(sp,F_POP_VALUE);
   }
   bracelevel--;
   oldstyle=oldstyle_save;
   return 1;
}

      
   
/* parse_statement
   
   precondition:
     isspace(sc_p[0]) may be true
   postcondition:
     [   sc_p in '}',';','else/elif/elsif/elseif' ]
     [   *ret=control-structure
       | *ret=expression
     ]
*/
   
static int
parse_statement()
{
   skip_space();

   if (sc_p[0]=='{') return parse_code();
   if (!parse_comma_expr()) return 0;
   return 1;
}

static int
parse_comma_expr()
{
   svalue_t *sp;
   set_ret;

   while(1)
   {
      if (!parse_one_comma_expr())
      {
         sc_errorf("expression expected");
         return 0;
      }
      skip_space();
      
      if (sc_p[0]==',') 
      {
         sc_p++;
         skip_space();
      }
      else break;
   }

   if (ret!=inter_sp)
   {
      sp=make_array_ofs(ret,1);
      put_efun(sp,F_POP_VALUE);
   }
   return 1;
}


static int
parse_one_comma_expr()
{
   set_ret;
   int instr;
   char *new_p;
   char *revert=NULL;
   
   instr=lex_resword(sc_p,&new_p);

   /* if a resword was found */
   if (instr)
   {
      sc_p=new_p;
      skip_space();
      switch(instr)
      {
       case L_IF:
         push_efun(F_BRANCH_WHEN_ZERO);
         if (!parse_expression(12) || !parse_statement())
         {
            sc_errorf("expression and statement expected");
            return 0;
         }
         if (*sc_p==';')
         {
            revert=sc_p;
            sc_p++;
            skip_space();
         }
         while(1)
         {
            int cnt;
            new_p=sc_p;
            while(isalnum(*new_p) || *new_p=='_') new_p++;
            cnt=new_p-sc_p;
            
            if (cnt==4 && strncmp(sc_p,"else",4)==0)
            {
               sc_p=new_p;
               skip_space();
               new_p=sc_p;
               while(isalnum(*new_p) || *new_p=='_') new_p++;
               cnt=new_p-sc_p;
               if (cnt!=2 || strncmp(sc_p,"if",2)!=0)
               {
                  /* "else" path */
                  if (!parse_statement())
                  {
                     sc_errorf("statement expected");
                     return 0;
                  }
                  if (*sc_p==';')
                  {
                     revert=sc_p;
                     sc_p++;
                     skip_space();
                  }
                  else revert=NULL;
                  break;
               }
               /* "else if" path */
            }
            else if (
               !(cnt==4 && strncmp(sc_p,"elif",4))
               && !(cnt==5 && strncmp(sc_p,"elsif",5))
               && !(cnt==6 && strncmp(sc_p,"elseif",6)))
            {
               /* no "else if" path */
               break;
            }
            /* "else if" path */
            if (!parse_expression(12))
            {
               sc_errorf("expression expected");
               return 0;
            }
            if (!parse_statement())
            {
               sc_errorf("statement expected");
               return 0;
            }
            if (*sc_p==';')
            {
               revert=sc_p;
               sc_p++;
               skip_space();
            }
            else revert=NULL;
         }
         make_array(ret);
         break;
       case L_WHILE:
	 push_efun(F_BBRANCH_WHEN_ZERO);
         if (!parse_expression(12))
         {
            sc_errorf("expression expected");
            return 0;
         }
	 push_number(inter_sp, 0);
         if (!parse_statement())
         {
            sc_errorf("statement expected");
            return 0;
         }
	 make_array(ret);
	 break;
       default:
         sc_errorf("keyword not implemented: %d", instr);
         return 0;
      }
      if (revert) sc_p=revert;
      return 1;
   }
   else /* parse for expression */
   {
      if (!parse_expression(12)) return 0;
   }
   
   return 1;
}

static int
see_skip(char *s)
{
   char *loc=sc_p;

   while(*s && *s==*loc) 
   {
      s++;
      loc++;
   }
   if (!*s)
   {
      sc_p=loc;
      skip_space();
      return 1;
   }
   return 0;
}


static int
parse_expression(int level)
{
   set_ret;
   svalue_t *sp, *tmp;
   char *p;
   int symop;
   static int ssymop;
   static int symlevel;
   static char *after_op;
   
   skip_space();
   if (!bracelevel && terminators && is_terminator())
      return 0;

   if (level==14) level=12;
   
   switch(level)
   {
    case_1:
    case 1:
      if (!parse_primitive()) return 0;
      while(1)
      {
         skip_space();
         if (!bracelevel && terminators && is_terminator())
         {
            symlevel=-1;
            return 1;
         }
         
         if (see_skip("->"))
         {
            p=sc_p;
            if (isdigit(*p))
            {
               sc_errorf("digit after -> not allowed");
               return 0;
            }
            while(isalnum(*sc_p) || *sc_p=='_') sc_p++;
            if (p==sc_p)
            {
               sc_errorf("name expected");
               return 0;
            }
            push_string(inter_sp, sc_strndup_shared(p, sc_p));
            skip_space();
            if (*sc_p!='(')
            {
               sc_errorf("( expected");
               return 0;
            }
            sc_p++;
            if (!parse_arglist())
            {
               sc_errorf("argument list expected");
               return 0;
            }
            if (*sc_p!=')')
            {
               sc_errorf(") expected");
               return 0;
            }
            sc_p++;
            sp=make_array_ofs(ret,1);
            put_efun(sp,F_CALL_OTHER);
            continue;
         }
         else if (see_skip("["))
         {
            char op[10];
            
            op[0]='[';
            bzero(op+1,9);
            
            if (see_skip("<")) op[1]='<';
            if (!parse_expression(12))
            {
               sc_errorf("expression expected");
               return 0;
            }
            skip_space();

            if (see_skip(".."))
            {
               if (*sc_p==']')
               {
                  if (op[1]=='<')
                  {
                     strcpy(op+2,"..<]");
                     push_number(inter_sp, 1);
                  }
                  else strcpy(op+1,"..");
               }
               else if (see_skip("<"))
               {
                  strcat(op+1,"..<]");
                  if (!parse_expression(12))
                  {
                     sc_errorf("expression expected");
                     return 0;
                  }
               }
               else /* just .. */
               {
                  strcat(op+1,"..]");
                  if (!parse_expression(12))
                  {
                     sc_errorf("expression expected");
                     return 0;
                  }
               }
            }
            else if (see_skip(","))
            {
               strcat(op+1,",]");
               if (!parse_expression(12))
               {
                  sc_errorf("expression expected");
                  return 0;
               }
            }
            if (!see_skip("]"))
            {
               sc_errorf("] expected");
               return 0;
            }
            symop=symbol_operator(op,&p);
            if (symop==-1 || *p!=0)
            {
               sc_errorf("unknown index operater '%s'",op);
               return 0;
            }
            sp=make_array_ofs(ret,1);
            put_efun(sp,symop);
            continue;
         }
         else if (see_skip("++"))
         {
	    if (ret->type != T_SYMBOL || !ret->x.quotes)
	      sc_errorf("Postincrement only with symbols");
            sp=make_array_ofs(ret,1);
            put_efun(sp,F_POST_INC);
            continue;
         }
         else if (see_skip("--"))
         {
	    if (ret->type != T_SYMBOL || !ret->x.quotes)
	      sc_errorf("Postdecrement only with symbols");
            sp=make_array_ofs(ret,1);
            put_efun(sp,F_POST_DEC);
            continue;
         }

         /* now we will return to level 3 (or 2 and then immediately to 3).
            there we will need the next operator on the input line */
         ssymop=symbol_operator(sc_p,&after_op);
         switch(ssymop)
         {
          case F_MULTIPLY:
          case F_DIVIDE:
          case F_MOD:
            symlevel=3;
            break;
          case F_ADD:
          case F_SUBTRACT:
            symlevel=4;
            break;
          case F_LSH:
          case F_RSHL:
          case F_RSH:
            symlevel=5;
            break;
          case F_GE:
          case F_GT:
          case F_LE:
          case F_LT:
            symlevel=6;
            break;
          case F_EQ:
          case F_NE:
            symlevel=7;
            break;
          case F_AND:
            symlevel=8;
            break;
          case F_XOR:
            symlevel=9;
            break;
          case F_OR:
            symlevel=10;
            break;
          case F_LAND:
            symlevel=11;
            break;
          case F_LOR:
            symlevel=12;
            break;
          case F_BRANCH_WHEN_NON_ZERO:
          case F_BRANCH_WHEN_ZERO:
            symlevel=13;
            break;
          case F_ASSIGN:
          case F_ADD_EQ:
          case F_SUB_EQ:
          case F_MULT_EQ:
          case F_DIV_EQ:
          case F_MOD_EQ:
          case F_XOR_EQ:
          case F_OR_EQ:
          case F_AND_EQ:
          case F_LSH_EQ:
          case F_RSH_EQ:
          case F_RSHL_EQ:
            symlevel=14;
            break;
          default:
            symlevel=-1;
         }
         break;
      }
      return 1;

    case 2:
      {
         int operands=1;

         symop=symbol_operator(sc_p,&p);
         switch(symop)
         {
          case F_POST_INC:
            symop=F_ADD_EQ;
            operands=2;
            goto do_prefix_op;
          case F_POST_DEC:
            symop=F_SUB_EQ;
            operands=2;
            goto do_prefix_op;
          case F_SUBTRACT: // -
            symop=F_NEGATE;
          case F_NOT: // !
          case F_COMPL: // ~
          do_prefix_op: 
            sc_p=p;
            if (!parse_expression(2))
            {
               sc_errorf("expression expected");
               return 0;
            }
            if (operands==2) push_number(inter_sp, 1);
            sp=make_array_ofs(ret,1);
            put_efun(sp,symop);
            return 1;
         }
         goto case_1;
      }
      
    case 3:
    case 4:
    case 5:
    case 6:
    case 7:
    case 8:
    case 9:
    case 10:
    case 11:
    case 12:
      /*
    case 13:  13 and 14 follow
    case 14:
      */
      if (!parse_expression(level-1)) return 0;
      while(level==symlevel)
      {
         symop=ssymop;
         sc_p=after_op;
         if (!parse_expression(level-1))
         {
            sc_errorf("expression expected");
            return 0;
         }
         sp=make_array_ofs(ret,1);
         put_efun(sp,symop);
      }
      if (level!=12) return 1;
      
      /* case_13: */
      if (symlevel==13)
      {
         sc_p=after_op;
         bracelevel++;
         symop=ssymop;
         
         if (!parse_expression(12))
         {
            sc_errorf("expression expected");
            return 0;
         }
         if (!see_skip(":"))
         {
            sc_errorf("':' expected");
            return 0;
         }
         bracelevel--;
         if (!parse_expression(12))
         {
            sc_errorf("expression expected");
            return 0;
         }
         sp=make_array_ofs(ret,1);
         put_efun(sp,symop);
      }

      /* case_14: */
      {
         int newop=-1;

         if (symlevel==14)
         {
            symop=ssymop;
            switch(symop)
            {
             case F_ASSIGN:
             case F_ADD_EQ:
               newop=F_ADD;
               goto do_assign;
             case F_SUB_EQ:
               newop=F_SUBTRACT;
               goto do_assign;
             case F_MULT_EQ:
               newop=F_MULTIPLY;
               goto do_assign;
             case F_DIV_EQ:
               newop=F_DIVIDE;
               goto do_assign;
             case F_MOD_EQ:
               newop=F_MOD;
               goto do_assign;
             case F_XOR_EQ:
               newop=F_XOR;
               goto do_assign;
             case F_OR_EQ:
               newop=F_OR;
               goto do_assign;
             case F_AND_EQ:
               newop=F_AND;
               goto do_assign;
             case F_LSH_EQ:
               newop=F_LSH;
               goto do_assign;
             case F_RSH_EQ:
               newop=F_RSH;
               goto do_assign;
             case F_RSHL_EQ:
               newop=F_RSHL;
             do_assign:
               sc_p=after_op;
               if (!parse_expression(12))
               {
                  sc_errorf("expression expected");
                  return 0;
               }
               /* let lambda care about errors */
	       sp=make_array_ofs(ret,1);
	       if ((symop != F_ASSIGN) && (sp[1].type == T_SYMBOL) &&
		   !sp[1].x.quotes) {
		  tmp = sp+1;
		  put_efun(sp,newop);
		  sp=make_array_ofs(ret,2);
		  transfer_svalue_no_free(sp+1, tmp);
		  put_efun(sp,F_ASSIGN);
	       } else {
		  put_efun(sp,symop);
	       }
               return 1;

            }
         }
         /* no assignment, so just return */
         return 1;
      }
   }
   /* we only get here, if a level outside 1-14 is used */
   sc_errorf("huh?");
   return 0;
}

static int
match_funcall(char *str, char **funend, char **ret)
{
   if (strncmp(str,"efun::",6)==0) str+=6;
   
   if (isalpha(*str) || *str=='_')
   {
      str++;
      while(isalnum(*str) || *str=='_') str++;
      if (funend) *funend=str;
      while(isspace(*str)) str++;
      if (*str=='(')
      {
         if (ret) *ret=str+1;
         return 1;
      }
   }
   return 0;
}

static int
match_oldstyle(char *str, char **ret)
{
   char *ptr;
   
   while(1)
   {
      switch(*str)
      {
       case ')':
         ptr = str-1;
         while (*ptr == ' ')
	   ptr--;
         if (*ptr == '(')
	    return 0;   // Empty call, use default.
       case ']':
       case ':':
       case ';':
       case ',':
       case '}':
       case 0:
         if (ret) *ret=str;
         return 1;
      }
      str++;
   }
}

static int
match_variable(char *str, char **ret)
{
   if (isalpha(*str) || *str=='_')
   {
      str++;
      while(isalnum(*str) || *str=='_' || *str=='.') str++;
      if (ret) *ret=str;
      return 1;
   }
   return 0;
}

static void
count_len(svalue_t *key, svalue_t *data UNUSED, void *extra UNUSED)
{
   if (key->type==T_STRING)
   {
      int l=strlen(key->u.str->str->txt);
      if (l>primfunlen) primfunlen=l;
   }
}

static int
parse_primitive()
{
   set_ret;
   svalue_t *sp;
   char *p;
   char *endptr;
   
   skip_space();
   if (!*sc_p)
   {
      /* sc_errorf("unexpected end of line"); */
      /* this may or may not be an error. let the caller decide */
      return 0;
   }

   if (*sc_p=='(')
   {
      bracelevel++;
      if (sc_p[1]=='{')
      {
         sc_p+=2;
         skip_space();
         if (!parse_arglist())
         {
            sc_errorf("arglist expected");
            return 0;
         }
         if (!see_skip("})"))
         {
            sc_errorf("}) expected");
            return 0;
         }
         sp=make_array_ofs(ret,1);
         put_efun(sp,F_AGGREGATE);
         bracelevel--;
         return 1;
      }
      if (sc_p[1]=='<')
      {
         sc_p+=2;
         skip_space();
         if (!parse_arglist())
         {
            sc_errorf("arglist expected");
            return 0;
         }
         if (!see_skip(">)"))
         {
            sc_errorf(">) expected");
            return 0;
         }
         sp=make_array_ofs(ret,1);
         put_efun(sp,F_S_AGGREGATE);
         bracelevel--;
         return 1;
      }
      if (sc_p[1]=='[')
      {
         sc_p+=2;
         skip_space();
         if (!parse_m_arglist())
         {
            sc_errorf("mapping arglist expected");
            return 0;
         }
         if (!see_skip("])"))
         {
            sc_errorf("]) expected");
            return 0;
         }
         sp=make_array_ofs(ret,1);
         put_efun(sp,F_M_CAGGREGATE);
         bracelevel--;
         return 1;
      }
      sc_p++;
      if (!parse_comma_expr())
      {
         sc_errorf("expression expected");
         return 0;
      }
      skip_space();
      if (!see_skip(")"))
      {
         sc_errorf(") expected");
         return 0;
      }
      bracelevel--;
      return 1;
   }
   else if (isdigit(*sc_p))
   {
      int num;
      char c;
      
      p=sc_p++;
      while(isdigit(*sc_p)) sc_p++;
      if (*sc_p=='.' && sc_p[1]!='.')
      {
         STORE_DOUBLE_USED
         double d;
         
         sc_p++;
         while(isdigit(*sc_p)) sc_p++;
         c=*sc_p;
         *sc_p=0;
         d=atof(p);
         *sc_p=c;

         inter_sp=ret;
         ret->type=T_FLOAT;
         STORE_DOUBLE(ret,d);
         return 1;
      }
      c=*sc_p;
      *sc_p=0;
      num=atoi(p);
      *sc_p=c;
      push_number(inter_sp, num);
      return 1;
   }
   else if (*sc_p=='\'')
   {
      char c;
      
      if (!sc_p[1])
      {
         sc_errorf("unterminated char value");
         return 0;
      }
      else if (sc_p[1]=='\\')
      {
         if (!sc_p[2] || sc_p[3]!='\'')
         {
            sc_errorf("unterminated char value");
            return 0;
         }
         push_number(inter_sp, sc_p[2]);
         sc_p+=4;
         return 1;
      }
      else if (sc_p[1]!='\'' && sc_p[2]=='\'')
      {
         push_number(inter_sp, sc_p[1]);
         sc_p+=3;
         return 1;
      }
      p=sc_p;
      while(*sc_p=='\'') sc_p++;
      ret->type = T_SYMBOL;
      ret->x.quotes = sc_p-p;
      p=sc_p;
      while(isalnum(*sc_p) || *sc_p=='_') sc_p++;
      if (p==sc_p)
      {
         sc_errorf("missing symbol after '");
         return 0;
      }
      c=*sc_p;
      *sc_p=0;
      ret->u.str = new_tabled(p);
      *sc_p=c;
      if (!ret->u.str)
      {
         sc_errorf("out of memory");
         return 0;
       }
      inter_sp=ret;
      return 1;
   }
   else if (*sc_p=='#' && sc_p[1]=='\'')
   {
      synth_closure_lex(sc_p+2,&p,ret);

      if (!p)
      {
         sc_errorf("unknown closure");
         return 0;
      }

      inter_sp=ret;
      sc_p=p;

      if (oldstyle)
      {
         make_array(ret);
      }
      return 1;
   }
   else if (*sc_p=='`' || *sc_p=='\"')
   {
      char t=*sc_p;
      char *str;
      int ofs=0;
      p=++sc_p;
      while(1)
      {
         if (!*sc_p)
         {
            sc_p=p-1;
            sc_errorf("unterminated string");
            return 0;
         }
         if (*sc_p==t)
         {
            if (sc_p[1]==t) ofs++,sc_p++;
            else break;
         }
         sc_p++;
      }
      str=(char*)xalloc(sc_p-p-ofs+1);
      if (!str)
      {
         sc_errorf("out of memory");
         return 0;
      }
      sc_p=p;
      ofs=0;
      while(1)
      {
         if (*sc_p==t)
         {
            if (sc_p[1]==t) ofs++,sc_p++;
            else break;
         }
         str[sc_p-p-ofs]=*sc_p;
         sc_p++;
      }
      str[sc_p-p-ofs]=0;
      sc_p++;
      push_c_string(inter_sp, str);
      xfree(str);
      return 1;
   }
   else if (match_funcall(sc_p,&endptr,&p))
   {
      char *fname;
      int len;

      len=endptr-sc_p;
      
      fname=alloca(len+1);
      strncpy(fname,sc_p,len);
      fname[len]=0;
      
      sc_p=p;
      skip_space();

      bracelevel++;

      if (oldstyle==1)
      {
         oldstyle=2;
         if (!parse_arglist())
         {
            sc_errorf("arglist expected");
            return 0;
         }
         oldstyle=1;
      }
      else
      {
         if (!parse_arglist())
         {
            sc_errorf("arglist expected");
            return 0;
         }
      }
      if (*sc_p!=')')
      {
         sc_errorf("arglist not terminated");
         return 0;
      }
      sc_p++;

      
      bracelevel--;
      if (funfun)
      {
         svalue_t *data;
         svalue_t *args;

         if (!callback_object(funfun))
         {
            sc_errorf("object for 'query_parser_function' destructed");
            return 0;
         }
         
         push_string(inter_sp, new_tabled(fname));
         args=inter_sp;

         /* replicate arglist */
         copy_array(ret,args-ret);
         push_number(inter_sp, oldstyle);
         push_svalue(sc_add_info);

         /* call funfun(fname, ({args[0],...}), oldstyle, add_info ) */
         data=apply_callback(funfun,inter_sp-args+1);

         if (!data)
         {
            sc_errorf("can't call query_parser_function");
            return 0;
         }
         
         if (data->type!=T_NUMBER || data->u.number!=0)
         {
            /* free original arglist */
            args=inter_sp;
            while(args>=ret)
               free_svalue(args--);
            transfer_svalue_no_free(ret,data);
            data->type=T_INVALID;
            inter_sp=ret;
            return 1;
         }
         /* data is NUMBER(0) */
         data->type=T_INVALID;
      }
      else if (funfunmap)
      {
         svalue_t *v;
         svalue_t str;

         put_string(&str,new_tabled(fname));

         v=get_map_value(funfunmap->u.map,&str);

         free_svalue(&str);
         
         if (v->type==T_CLOSURE)
         {
            sp=make_array_ofs(ret,1);
            assign_svalue_no_free(sp,v);
            return 1;
         }
         if (v->type==T_POINTER)
         {
            vector_t *vec=v->u.vec;
            int cnt=VEC_SIZE(vec);
            
            sp=make_array_ofs(ret,cnt);
            for(;cnt--;)
               assign_svalue_no_free(sp+cnt,vec->item+cnt);
            return 1;
         }
         if (v!=&const0)
         {
            sc_errorf("illegal return value in func-mapping");
         }
      }

      if (oldstyle==1 && inter_sp-ret+1==0)
      {
         if (noargsfun)
         {
            svalue_t *data;

            if (!callback_object(noargsfun))
            {
               sc_errorf("object for 'noargs' destructed");
               return 0;
            }
         
            push_string(inter_sp, new_tabled(fname));
            push_svalue(sc_add_info);

            /* call noargsfun(fname, add_info) */
            data=apply_callback(funfun,2);

            if (!data)
            {
               sc_errorf("can't call noargs");
               return 0;
            }
         
            if (data->type!=T_NUMBER || data->u.number!=0)
            {
               push_svalue(data);
               free_svalue(data);
            }
            data->type=T_INVALID;
         }
         else if (noargsfunmap)
         {
            push_svalue(noargsfunmap);
         }
      }
      
      /* ok. now we have to come up with a ({#'fname,args...})-thing */

      sp=make_array_ofs(ret,1);
      
      synth_closure_lex(fname,&p,sp);

      if (!p || *p)
      {
         sc_errorf("unknown closure: %s",fname);
         return 0;
      }
      return 1;
   }
   else if ((oldstyle && match_oldstyle(sc_p,&p))
            || (!oldstyle && match_variable(sc_p,&p)))
   {
      char *str;
      int len;
      
      len=p-sc_p;

      str=alloca(len+1);
      strncpy(str,sc_p,len);
      str[len]=0;

      if (varfun)
      {
         svalue_t *data;
         
         if (!callback_object(varfun))
         {
            sc_errorf("object for 'query_parser_variable' destructed");
            return 0;
         }

         push_number(inter_sp, 0);
         
         push_string(inter_sp, new_tabled(str));
         data=++inter_sp;
         data->type=T_LVALUE;
         data->u.lvalue=ret;
         push_number(inter_sp, oldstyle);
         push_svalue(sc_add_info);

         /* call varfun(str,&ret,oldstyle,add_info) */
         data=apply_callback(varfun,4);

         if (!data)
         {
            sc_errorf("can't call query_parser_variable");
            return 0;
         }

         /* we expect the returncode in data, and the returnvalue in *ret */
         if (data->type==T_NUMBER && data->u.number>0)
         {
            /* this should already be true:  inter_sp=ret; */

	    if (ret->type == T_SYMBOL)
	      ret->x.quotes--;

            data->type=T_INVALID;
            sc_p=p;
            return 1;
         }
         free_svalue(data);
         data->type=T_INVALID;

         /* else we have to free the value */
         pop_stack();
         /* stack is empty now (relative to ret) */
      }
      else if (varfunmap)
      {
         svalue_t *v;
         svalue_t vstr;

         put_string(&vstr,new_tabled(str));

         v=get_map_value(varfunmap->u.map,&vstr);

         free_svalue(&vstr);

         if (v!=&const0)
         {
            push_svalue(v);
            return 1;
         }
      }
      
      if (oldstyle)
      {
         /* no variable, but just a string */
            
         push_string(inter_sp, new_tabled(str));
         sc_p=p;
         return 1;
      }

      /* we should better have a good excuse now */
      sc_errorf("unknown variable: %s",str);
      return 0;
   }
   else if (primfun)
   {
      svalue_t *data;

      if (!callback_object(primfun))
      {
         sc_errorf("object for 'query_parser_primitive' destructed");
         return 0;
      }

      push_number(inter_sp, 0);
         
      push_string(inter_sp, new_tabled(sc_p));
      data=++inter_sp;
      data->type=T_LVALUE;
      data->u.lvalue=ret;
      push_svalue(sc_add_info);
         
      /* call primfun(str,&ret,add_info) */
      data=apply_callback(primfun,3);

      if (!data)
      {
         sc_errorf("can't call query_parser_primitive");
         return 0;
      }

      /* we expect the returncode in data, and the returnvalue in *ret */
      if (data->type==T_NUMBER && data->u.number>0)
      {
         /* this should already be true:  inter_sp=ret; */
         /* we advance sc_p for data->u.number or to the end */
         int len=strlen(sc_p);
         if (data->u.number<len) sc_p+=data->u.number;
         else sc_p+=len;
         data->type=T_INVALID;
         return 1;
      }
      free_svalue(data);
      data->type=T_INVALID;

      /* else we have to free the value */
      pop_stack();
      /* stack is empty now (relative to ret) */
   }
   else if (primfunmap)
   {
      svalue_t *v;
      svalue_t vstr;
      mapping_t *m=primfunmap->u.map;
      int len;
      int tlen;
      
      /* now... this is a bit complicated.
         we have to check, if the beginning of the string is contained
         in the mapping. therefore we start with the longest string
         len=max(strlen(indices(primfunmap)...))
         and check all prefixes
         for(;len--;) if (primfunmap[sc_p[0..len]]) found
      */

      if (!primfunlen)
         walk_mapping(m, count_len, NULL);

      tlen=strlen(sc_p);
      if (primfunlen<tlen) len=primfunlen;
      else len=tlen;

      for(;len;len--)
      {
         char c=sc_p[len];
         sc_p[len]=0;
         
         put_string(&vstr,new_tabled(sc_p));
         sc_p[len]=c;
         v=get_map_value(m,&vstr);
         free_svalue(&vstr);

         if (v!=&const0)
         {
            push_svalue(v);
            sc_p+=len;
            return 1;
         }
      }
   }
      

   /* ok, we can't find a primitive.
      but let the caller decide if this is bad */
   return 0;
}

static int
parse_arglist()
{
   skip_space();

   while(parse_expression(12))
   {
      if (*sc_p==',')
      {
         sc_p++;
         skip_space();
      }
      else break;
   }
   return 1;
}

static int
parse_m_arglist()
{
   skip_space();

   while(parse_expression(12))
   {
      svalue_t *start=inter_sp;
      
      if (*sc_p==':')
      {
         do
         {
            sc_p++;
            skip_space();
            if (!parse_expression(12))
            {
               sc_errorf("expression expected");
               return 0;
            }
         } while(*sc_p==';');
      }
      make_array(start);
      if (*sc_p==',')
      {
         sc_p++;
         skip_space();
      }
      else break;
   }
   return 1;
}

#define CL_CHECK(arg,pos) \
   if (sp[arg].type!=T_CLOSURE && \
       sp[arg].type!=T_MAPPING && \
       (sp[arg].type!=T_NUMBER || sp[arg].u.number!=0)) \
      vefun_bad_arg(pos,sp)

#define SET_CB(arg,fun) \
   if (sp[arg].type==T_CLOSURE)\
   {\
      fun=&_ ## fun;\
      setup_closure_callback(fun, sp+arg, 0, NULL, MY_TRUE);\
      put_callback(sp+arg, fun);\
      fun##map=NULL;\
   }\
   else if (sp[arg].type==T_MAPPING)\
   {\
      fun##map=sp+arg;\
      fun=NULL;\
   }\
   else fun=NULL,fun##map=NULL


/*
   mixed string_compiler(object where
                         string str,
                         closure|mapping func,
                         closure|mapping vars,
                         closure|mapping prim,
                         mixed noargs,
                         mixed info)
*/
svalue_t *
v_string_compiler(svalue_t *sp)
{
   set_ret;
   svalue_t *last;

   callback_t _funfun;
   callback_t _varfun;
   callback_t _primfun;
   callback_t _noargsfun;

   sc_state_t state;

   if (sp[-6].type!=T_OBJECT)
      vefun_bad_arg(1,sp);

   if (sp[-6].u.ob!=current_object
       && !privilege_violation(STR_STRING_COMPILER,sp-6,sp))
   {
      inter_sp=pop_n_elems(7,sp);
      push_number(inter_sp, 0);
      return sp-6;
   }
   
   if (sp[-5].type!=T_STRING)
      vefun_bad_arg(2,sp);

   CL_CHECK(-4,3);
   CL_CHECK(-3,4);
   CL_CHECK(-2,5);

   if (!strchr(sp[-5].u.str->str->txt,'$'))
   {
      assign_svalue(sp-6,sp-5);
      inter_sp=pop_n_elems(6,inter_sp);
      return inter_sp;
   }
   
   sc_save_state(&state);
   
   SET_CB(-4,funfun);
   SET_CB(-3,varfun);
   SET_CB(-2,primfun);
   SET_CB(-1,noargsfun);

   if (!noargsfun /*&& sp[-1].type!=T_NUMBER*/)
      noargsfunmap=sp-1;
   primfunlen=0;
   oldstyle=1;
   terminators=NULL;
   bracelevel=0;
   sc_start_p=sp[-5].u.str->str->txt;
   sc_p=sc_start_p;
   sc_add_info=sp;
   sc_object=sp[-6].u.ob;

   last=NULL;
   
   while(parse_part())
   {
      if (last) {
	 last = make_array_ofs(ret, 1);
	 put_efun(last, F_ADD);
	 last+=2;
      } else
	last = ret;
   }

   if (*sc_p)
   {
      sc_errorf("parse error");
      return NULL;
   }
   /* since the string is not empty, there IS an element on the stack */

   /* copy result to element, which will be top of stack */
   postprocess(ret);
   transfer_svalue(sp-6,ret);

   /* drop on element (the recently transfered result) and
    free 6 (string+4 callbacks+info) */
   inter_sp=pop_n_elems(6,inter_sp-1);

   sc_restore_state(&state);
   
   return sp-6;
}

/*
   int string_compiler_one(object where,
                       string str,
                       mixed &result,
                       string terminators,
                       closure|mapping func,
                       closure|mapping vars,
                       closure|mapping prim,
                       mixed noargs,
                       mixed info)
*/
svalue_t *
v_string_compiler_one(svalue_t *sp)
{
   return sc_partial(sp,0);
}

/*
   int string_compiler_arglist(object where,
                       string str,
                       mixed &result,
                       string terminators,
                       closure|mapping func,
                       closure|mapping vars,
                       closure|mapping prim,
                       mixed noargs,
                       mixed info)
*/
svalue_t *
v_string_compiler_arglist(svalue_t *sp)
{
   return sc_partial(sp,1);
}

/*
   int string_compiler_term(object where,
                       string str,
                       mixed &result,
                       string terminators,
                       closure|mapping func,
                       closure|mapping vars,
                       closure|mapping prim,
                       mixed noargs,
                       mixed info)
*/
svalue_t *
v_string_compiler_term(svalue_t *sp)
{
   return sc_partial(sp,2);
}

static svalue_t *
sc_partial(svalue_t *sp, int mode)
{
   set_ret;
   int len;
   svalue_t *last;
   
   callback_t _funfun;
   callback_t _varfun;
   callback_t _primfun;
   callback_t _noargsfun;

   sc_state_t state;
   
   if (sp[-8].type!=T_OBJECT)
      vefun_bad_arg(1,sp);

   if (sp[-8].u.ob!=current_object
       && !privilege_violation(STR_STRING_COMPILER,sp-8,sp))
   {
      inter_sp=pop_n_elems(9,sp);
      push_number(inter_sp, 0);
      return sp-8;
   }
   
   if (sp[-7].type!=T_STRING)
      vefun_bad_arg(2,sp);

   if (sp[-5].type!=T_STRING &&
       (sp[-5].type!=T_NUMBER || sp[-5].u.number!=0))
      vefun_bad_arg(4,sp);

   CL_CHECK(-4,5);
   CL_CHECK(-3,6);
   CL_CHECK(-2,7);

   if (sp[-7].u.str->str->txt[0]==0)
   {
      assign_svalue(sp-6,sp-7);
      inter_sp=pop_n_elems(9,inter_sp);
      push_number(inter_sp, 0);
      return inter_sp;
   }

   sc_save_state(&state);
   
   SET_CB(-4,funfun);
   SET_CB(-3,varfun);
   SET_CB(-2,primfun);
   SET_CB(-1,noargsfun);

   if (!noargsfun && sp[-1].type!=T_NUMBER)
      noargsfunmap=sp-1;

   primfunlen=0;
   oldstyle=0;
   bracelevel=0;
   sc_add_info=sp;
   sc_start_p=sp[-7].u.str->str->txt;
   sc_p=sc_start_p;
   sc_object=sp[-8].u.ob;
   
   if (sp[-5].type==T_STRING) terminators=sp[-5].u.str->str->txt;
   else terminators=NULL;
      
   skip_space();

   if (mode==2 && !terminators) mode=3;
   
   switch(mode)
   {
    case 0:
      if (*sc_p=='{') parse_code();
      else parse_expression(12);
      break;
    case 1:
      parse_arglist();
      make_array(ret);
      break;
    case 2:
      last=NULL;
      while(parse_part())
      {
         /* can join strings ? */
         if (last && inter_sp->type==T_STRING
             && last->type==T_STRING)
         {
            int llen=strlen(last->u.str->str->txt);
            char *buf=alloca(strlen(inter_sp->u.str->str->txt)+llen+1);
            strcpy(buf,last->u.str->str->txt);
            strcpy(buf+llen,inter_sp->u.str->str->txt);
            free_string_svalue(last);
            put_string(last,new_tabled(buf));
         }
         else if (inter_sp!=ret)
         {
            last=make_array_ofs(ret,1);
            put_efun(last,F_ADD);
            last+=2;
         }
         else last=ret;
      }
      break;
    case 3:
      parse_part();
      break;
   }
   
   len=sc_p-sc_start_p;

   if (inter_sp==ret) transfer_svalue(sp-6,ret);
   inter_sp=pop_n_elems(9,ret-1);
   push_number(inter_sp, len);

   sc_restore_state(&state);
   
   return inter_sp;
}

void pp_replace(svalue_t *v) {
   string_t *sym;
   svalue_t *sp;
   
   if (v->x.quotes)
     return;
   sym = new_mstring(v->u.str->str->txt);
   free_svalue(v);
   put_array(v, allocate_uninit_array(2));
   sp = &v->u.vec->item[0];
   put_efun(sp, F_QUERY_VARIABLE);
   sp++;
   put_string(sp, sym);
}

void postprocess (svalue_t *v) {
   int size, i;
   svalue_t *sp;
   
   if (v->type == T_SYMBOL)
     pp_replace(v);
   else if (v->type == T_POINTER) {
      size = VEC_SIZE(v->u.vec);
      for (i = 0; i < size; i++) {
	 sp = &v->u.vec->item[i];
	 if ((i == 0) && (size > 1) &&
	     (sp[0].type == T_CLOSURE) &&
	     (sp[0].x.closure_type == (F_ASSIGN + CLOSURE_OPERATOR)) &&
	     (sp[1].type == T_SYMBOL) && !sp[1].x.quotes) {
	    put_efun(sp, F_SET_VARIABLE);
	    sp[1].type = T_STRING;
	 }
	 if (sp->type == T_POINTER)
	   postprocess(sp);
	 else if (sp->type == T_SYMBOL)
	   pp_replace(sp);
      }
   }
}

svalue_t *f_query_variable (svalue_t *sp) {
   svalue_t *res;
   string_t *qv;
   
   transfer_svalue_no_free(++inter_sp, sp);
   qv = new_mstring("query_variable");
   res = sapply_int(qv, simul_efun_object, 1, MY_TRUE, MY_FALSE);
   mstring_free(qv);
   inter_sp--;
   assign_svalue_no_free(sp, res);

   return sp;
}

svalue_t *f_set_variable (svalue_t *sp) {
   string_t *sv;
   svalue_t *res;
   
   transfer_svalue_no_free(++inter_sp, sp-1);
   transfer_svalue_no_free(++inter_sp, sp--);
   sv = new_mstring("set_variable");
   res = sapply_int(sv, simul_efun_object, 2, 
		    MY_TRUE, MY_FALSE);
   mstring_free(sv);
   inter_sp--;
   assign_svalue_no_free(sp, res);
   return sp;
}
   
#endif /* USE_STRING_COMPILER */
