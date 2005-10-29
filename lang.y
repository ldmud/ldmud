%{
#include <string.h>
#include "stdio.h"
#include "lnode.h"
#include "config.h"

#define YYMAXDEPTH	10000

struct lnode *prog, *heart_beat;
int variable_count;
void yyerror(), free_all_local_names(), add_local_name();
extern void free(), error();
extern int yylex(), check_deklared();

char *argument_name;	/* The name of the current argument. */

extern int current_line;
extern char *current_file;

char *local_names[MAX_LOCAL];
int current_number_of_locals = 0;
%}

%token F_THIS_PLAYER F_IF F_IDENTIFIER F_AND F_OR F_STATUS F_SET_TRUE
%token F_SET_FALSE F_CONS F_RETURN F_NOT F_WRITE F_STRING F_ADD_VERB
%token F_ADD_ADJ F_ADD_SUBST F_ADD_ACTION F_MOVE_OBJECT F_PRESENT
%token F_NUMBER F_ASSIGN F_INT F_CALL_OTHER F_ADD F_SUBTRACT F_MULTIPLY
%token F_DIVIDE F_MOD F_LT F_GT F_EQ F_GE F_LE F_ARGUMENT F_FUNCTION
%token F_CLONE_OBJECT F_THIS_OBJECT F_SAVE_OBJECT F_RESTORE_OBJECT F_NE
%token F_ENVIRONMENT F_ADD_EQ F_SUB_EQ F_DIV_EQ F_MULT_EQ F_MOD_EQ
%token F_COMMAND F_SET_LIGHT F_DESTRUCT F_CREATE_WIZARD F_NEGATE F_SAY
%token F_STRLEN F_SUBSCRIPT F_WHILE F_BREAK F_SHUTDOWN F_LOG_FILE
%token F_SSCANF F_SHOUT F_STRING_DECL F_LOCAL_NAME F_FIRST_INVENTORY
%token F_NEXT_INVENTORY F_ENABLE_COMMANDS F_RANDOM F_INPUT_TO F_CRYPT
%token F_LS F_CAT F_FIND_LIVING F_FIND_PLAYER F_TELL_OBJECT F_PEOPLE
%token F_ED F_LIVING F_LOWER_CASE F_ELSE F_CAPITALIZE
%token F_SET_HEART_BEAT F_SNOOP F_TELL_ROOM F_FIND_OBJECT F_WIZLIST
%token F_RM F_CONST0 F_CONST1 F_BLOCK F_TRANSFER F_REGCOMP F_REGEXEC
%token F_LOCALCMD F_SWAP F_CONTINUE F_ADD_WORTH F_TIME

%union
{
	struct lnode *lnode;
	int number;
	char *string;
}

%type <lnode> cond expr1 expr_list expr2 expr3 function_call variable
%type <lnode> statements statement program def return expr4
%type <lnode> string expr0 F_NUMBER expr25 expr27 expr23 expr22 while
%type <lnode> F_LOCAL_NAME block number F_CONST0 F_CONST1

%type <number> name F_SET_FALSE F_SET_TRUE F_THIS_PLAYER F_WRITE F_THIS_OBJECT
%type <number> assign

%type <string> F_IDENTIFIER F_STRING F_ARGUMENT

%%

all: program { prog = $1; };

program: def possible_semi_colon program
	{ if ($1 != 0) {
	    struct lnode_def *p = (struct lnode_def *)$1;
	    p->next = (struct lnode_def *)$3;
	    $$ = (struct lnode *)p;
	  } else
	    $$ = $3;
        }
       |	 /* empty */ { $$ = 0; };

possible_semi_colon: /* empty */
                   | ';' { yyerror("Extra ';'. Ignored."); };

pre_pro_info: '#' number F_STRING
                { free(current_file); current_file = $3;
		  current_line = num_val((struct lnode_number *)$2) - 1;
		  free_lnode($2, 1);
		}
	      possible_extra_number;

number:  F_NUMBER | F_CONST0 | F_CONST1;

possible_extra_number: /* empty */ | number { free_lnode($1, 1); };

def: F_IDENTIFIER '(' argument ')' block
	{ struct lnode_def *p;
	  p = alloc_lnode_def(F_IDENTIFIER,$1,$5,
					       current_number_of_locals);
	  $$ = (struct lnode *)p;
	  if (strcmp($1, "heart_beat") == 0) heart_beat = $$;
	  if (argument_name) free(argument_name);
	  argument_name = 0; free_all_local_names(); }
   | name '(' argument ')' block
	{ yyerror("Predefined function name.\n"); $$ = 0; }
   | type name_list ';' { $$ = 0; }
   | pre_pro_info { $$ = 0; };

req_semi: ';'
    	       | /* empty */ { yyerror("Missing ';'"); };

argument: /* empty */
        | F_IDENTIFIER { argument_name = $1; };

type: F_STATUS | F_INT | F_STRING_DECL;

name_list: new_name
	 | new_name ',' name_list;

new_name: F_IDENTIFIER
	{ alloc_lnode_var_def(F_STATUS, $1, variable_count++); };

block: '{' local_declarations statements '}'
	{ $$ = (struct lnode *)alloc_lnode_block($3); };

local_declarations: /* empty */
		  | local_declarations type local_list req_semi ;

local_list: F_IDENTIFIER { add_local_name($1); }
	  | local_list ',' F_IDENTIFIER { add_local_name($3); }
	  | F_ARGUMENT { yyerror("Illegal to redeclare argument"); }
	  | F_LOCAL_NAME { yyerror("Illegal to redeclare local name"); };

statements: /* empty */ { $$ = 0; }
	  | statement statements
		{ if ($1 == 0)
		      $$ = $2;
		  else
		      $$ = (struct lnode *)alloc_lnode_2(F_CONS, $1, $2);
	        }
	  | error ';' { yyerror("Illegal statement"); $$ = 0; }
	  | pre_pro_info { $$ = 0; };

statement: expr0 ';' { $$ = $1; } | cond | while | return ';' { $$ = $1; }
	 | block
	 | F_BREAK req_semi { $$ = (struct lnode *)alloc_lnode_single(F_BREAK);
			    };
	 | F_CONTINUE req_semi { $$ = (struct lnode *)alloc_lnode_single(F_CONTINUE);
			    };

while: F_WHILE '(' expr0 ')' statement
     { $$ = (struct lnode *)alloc_lnode_2(F_WHILE, $3, $5); };

expr0: expr1
     | variable assign expr1
     { $$ = (struct lnode *)alloc_lnode_2($2, $1, $3); }
     | error assign expr1 { yyerror("Illegal LHS"); $$ = 0; }

assign: '=' { $$ = F_ASSIGN; }
      | F_ADD_EQ { $$ = F_ADD_EQ; }
      | F_SUB_EQ { $$ = F_SUB_EQ; }
      | F_MULT_EQ { $$ = F_MULT_EQ; }
      | F_MOD_EQ { $$ = F_MOD_EQ; }
      | F_DIV_EQ { $$ = F_DIV_EQ; };

return: F_RETURN { $$ = (struct lnode *)alloc_lnode_1(F_RETURN, 0); }
      | F_RETURN expr0 { $$ = (struct lnode *)alloc_lnode_1(F_RETURN, $2); };

expr_list: /* empty */ { $$ = 0; }
	 | expr1
	{ $$ = (struct lnode *)alloc_lnode_2(F_CONS, $1, 0); }
         | expr1 ',' expr_list
	{ $$ = (struct lnode *)alloc_lnode_2(F_CONS, $1, $3); };

expr1: expr2
     | expr2 F_OR expr1
	{ $$ = (struct lnode *)alloc_lnode_2(F_OR, $1, $3); };

expr2: expr22
     | expr22 F_AND expr2
	{ $$ = (struct lnode *)alloc_lnode_2(F_AND, $1, $3); };

expr22: expr23
      | expr25 F_EQ expr25
	{ $$ = (struct lnode *)alloc_lnode_2(F_EQ, $1, $3); }
      | expr25 F_NE expr25
	{ $$ = (struct lnode *)alloc_lnode_2(F_NE, $1, $3); };

expr23: expr25
      | expr25 '>' expr25
	{ $$ = (struct lnode *)alloc_lnode_2(F_GT, $1, $3); }
      | expr25 F_GE expr25
	{ $$ = (struct lnode *)alloc_lnode_2(F_GE, $1, $3); }
      | expr25 '<' expr25
	{ $$ = (struct lnode *)alloc_lnode_2(F_LT, $1, $3); }
      | expr25 F_LE expr25
	{ $$ = (struct lnode *)alloc_lnode_2(F_LE, $1, $3); };

expr25: expr27
      | expr25 '+' expr27
	{ $$ = (struct lnode *)alloc_lnode_2(F_ADD, $1, $3); }
      | expr25 '-' expr27
	{ $$ = (struct lnode *)alloc_lnode_2(F_SUBTRACT, $1, $3); };

expr27: expr3
      | expr27 '*' expr3
	{ $$ = (struct lnode *)alloc_lnode_2(F_MULTIPLY, $1, $3); }
      | expr27 '%' expr3
	{ $$ = (struct lnode *)alloc_lnode_2(F_MOD, $1, $3); }
      | expr27 '/' expr3
	{ $$ = (struct lnode *)alloc_lnode_2(F_DIVIDE, $1, $3); };

expr3: expr4
     | F_NOT expr3
	{ $$ = (struct lnode *)alloc_lnode_1(F_NOT, $2); };
     | '-' expr3
	{ $$ = (struct lnode *)alloc_lnode_1(F_NEGATE, $2); };

expr4: function_call | variable | string | number
     | '(' expr0 ')' { $$ = $2; }
     | expr4 '[' expr0 ']'
      { $$ = (struct lnode *)alloc_lnode_2(F_SUBSCRIPT, $1, $3); };

string: F_STRING
	{ $$ = (struct lnode *)alloc_lnode_name(F_STRING, $1); };

function_call: name '(' expr_list ')'
		{ $$ = (struct lnode *)alloc_lnode_1($1, $3); }
	     | F_IDENTIFIER '(' ')'
		{ $$ = (struct lnode *)alloc_lnode_funcall(F_FUNCTION, $1, 0); }
	     | F_IDENTIFIER '(' expr0 ')'
		{ $$ = (struct lnode *)alloc_lnode_funcall(F_FUNCTION, $1, $3); };

variable: F_IDENTIFIER
	{ $$ = (struct lnode *)alloc_lnode_number(F_IDENTIFIER,
						    check_deklared($1));
						    free($1); }
	| F_ARGUMENT { $$ = (struct lnode *)alloc_lnode_single(F_ARGUMENT); }
        | F_LOCAL_NAME ;

name: F_THIS_PLAYER { $$ = F_THIS_PLAYER; }
    | F_THIS_OBJECT { $$ = F_THIS_OBJECT; }
    | F_SET_FALSE { $$ = F_SET_FALSE; }
    | F_WRITE { $$ = F_WRITE; }
    | F_ADD_VERB { $$ = F_ADD_VERB; }
    | F_ADD_SUBST { $$ = F_ADD_SUBST; }
    | F_ED { $$ = F_ED; }
    | F_ADD_ADJ { $$ = F_ADD_ADJ; }
    | F_SHUTDOWN { $$ = F_SHUTDOWN; }
    | F_CALL_OTHER { $$ = F_CALL_OTHER; }
    | F_CLONE_OBJECT { $$ = F_CLONE_OBJECT; }
    | F_ADD_ACTION { $$ = F_ADD_ACTION; }
    | F_MOVE_OBJECT { $$ = F_MOVE_OBJECT; }
    | F_LOG_FILE { $$ = F_LOG_FILE; }
    | F_SET_TRUE { $$ = F_SET_TRUE; }
    | F_SAVE_OBJECT { $$ = F_SAVE_OBJECT; }
    | F_ENVIRONMENT { $$ = F_ENVIRONMENT; }
    | F_PRESENT { $$ = F_PRESENT; }
    | F_SET_HEART_BEAT { $$ = F_SET_HEART_BEAT; }
    | F_COMMAND { $$ = F_COMMAND; }
    | F_CRYPT { $$ = F_CRYPT; }
    | F_CAT { $$ = F_CAT; }
    | F_LS { $$ = F_LS; }
    | F_RM { $$ = F_RM; }
    | F_FIND_LIVING { $$ = F_FIND_LIVING; }
    | F_FIND_PLAYER { $$ = F_FIND_PLAYER; }
    | F_LOWER_CASE { $$ = F_LOWER_CASE; }
    | F_TELL_OBJECT { $$ = F_TELL_OBJECT; }
    | F_FIND_OBJECT { $$ = F_FIND_OBJECT; }
    | F_WIZLIST { $$ = F_WIZLIST; }
    | F_TRANSFER { $$ = F_TRANSFER; }
    | F_REGCOMP { $$ = F_REGCOMP; }
    | F_REGEXEC { $$ = F_REGEXEC; }
    | F_LOCALCMD { $$ = F_LOCALCMD; }
    | F_SWAP { $$ = F_SWAP; }
    | F_ADD_WORTH { $$ = F_ADD_WORTH; }
    | F_TIME { $$ = F_TIME; }
    | F_PEOPLE { $$ = F_PEOPLE; }
    | F_INPUT_TO { $$ = F_INPUT_TO; }
    | F_ENABLE_COMMANDS { $$ = F_ENABLE_COMMANDS; }
    | F_CAPITALIZE { $$ = F_CAPITALIZE; }
    | F_LIVING { $$ = F_LIVING; }
    | F_RANDOM { $$ = F_RANDOM; }
    | F_SNOOP { $$ = F_SNOOP; }
    | F_FIRST_INVENTORY { $$ = F_FIRST_INVENTORY; }
    | F_NEXT_INVENTORY { $$ = F_NEXT_INVENTORY; }
    | F_SET_LIGHT { $$ = F_SET_LIGHT; }
    | F_DESTRUCT { $$ = F_DESTRUCT; }
    | F_CREATE_WIZARD { $$ = F_CREATE_WIZARD; }
    | F_SAY { $$ = F_SAY; }
    | F_TELL_ROOM { $$ = F_TELL_ROOM; }
    | F_SHOUT { $$ = F_SHOUT; }
    | F_RESTORE_OBJECT { $$ = F_RESTORE_OBJECT; }
    | F_SSCANF { $$ = F_SSCANF; }
    | F_STRLEN { $$ = F_STRLEN; };

cond: F_IF '(' expr1 ')' statement { $$ = (struct lnode *)alloc_lnode_3(F_IF, $3, $5, 0); }
    | F_IF '(' expr1 ')' statement F_ELSE statement
	{ $$ = (struct lnode *)alloc_lnode_3(F_IF, $3, $5, $7); };
%%

void yyerror(str)
char *str;
{
    extern int num_parse_error;

    if (num_parse_error > 5)
	return;
    (void)fprintf(stderr, "%s: %s line %d\n", current_file, str,
		  current_line);
    smart_log(current_file, current_line, str);
    num_parse_error++;
}

int check_deklared(str)
    char *str;
{
    struct lnode_var_def *p;
    char buff[100];

    for (p = prog_status; p; p = p->next) {
	if (strcmp(p->name, str) == 0)
	    return p->num_var;
    }
    (void)sprintf(buff, "Variable %s not declared !", str);
    yyerror(buff);
    return 0;
}

void free_all_local_names()
{
    int i;

    for (i=0; i<current_number_of_locals; i++) {
	free(local_names[i]);
	local_names[i] = 0;
    }
    current_number_of_locals = 0;
}

void add_local_name(str)
    char *str;
{
    if (current_number_of_locals == MAX_LOCAL)
	error("Too many local variables (max %d)\n", MAX_LOCAL);
    local_names[current_number_of_locals++] = str;
}

num_val(p)
    struct lnode_number *p;
{
    switch(p->type) {
    case F_NUMBER:
        return p->number;
    case F_CONST0:
	return 0;
    case F_CONST1:
	return 1;
    default:
	fatal("Illegal type of number.\n");
    }
}
