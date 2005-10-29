%{
#include "y.tab.h"
#include "string.h"
#include "lnode.h"
#include "config.h"

int current_line;
char *current_file;
extern char *argument_name;
int number(), ident(), string(), atoi();

#ifdef FLEX
#define yywrap() 1
#endif

extern char *local_names[];
extern int current_number_of_locals;

extern char *string_copy();
%}
%p 4000
%%
";"	{ return ';'; }
"("	{ return '('; }
")"	{ return ')'; }
","	{ return ','; }
"{"	{ return '{'; }
"}"	{ return '}'; }
"!"	{ return F_NOT; }
"&&"	{ return F_AND; }
"||"	{ return F_OR; }
"="	{ return '='; }
"-"	{ return '-'; }
"-="	{ return F_SUB_EQ; }
"+"	{ return '+'; }
"+="	{ return F_ADD_EQ; }
"*"	{ return '*'; }
"*="	{ return F_MULT_EQ; }
"%"	{ return '%'; }
"%="	{ return F_MOD_EQ; }
"/"	{ return '/'; }
"/="	{ return F_DIV_EQ; }
"<"	{ return '<'; }
">"	{ return '>'; }
">="	{ return F_GE; }
"<="	{ return F_LE; }
"=="	{ return F_EQ; }
"!="	{ return F_NE; }
"#"	{ return '#'; }
"["	{ return '['; }
"]"	{ return ']'; }
[0-9]+	{ return number(yytext); }
'.'	{ yylval.lnode = (struct lnode *)alloc_lnode_number(F_NUMBER,
							    yytext[1]);
          return F_NUMBER; }
"ls"	{ return F_LS; }
"cat"	{ return F_CAT; }
"rm"	{ return F_RM; }
"log_file" { return F_LOG_FILE; }
"else"	{ return F_ELSE; }
"find_living" { return F_FIND_LIVING; }
"find_player" { return F_FIND_PLAYER; }
"tell_object" { return F_TELL_OBJECT; }
"people" { return F_PEOPLE; }
"snoop" { return F_SNOOP; }
"input_to" { return F_INPUT_TO; }
"wizlist" { return F_WIZLIST; }
"crypt" { return F_CRYPT; }
"first_inventory" { return F_FIRST_INVENTORY; }
"regcomp"	 { return F_REGCOMP; }
"regexec"	 { return F_REGEXEC; }
"transfer"	 { return F_TRANSFER; }
"next_inventory" { return F_NEXT_INVENTORY; }
"string" { return F_STRING_DECL; }
"shout"	{ return F_SHOUT; }
"sscanf" { return F_SSCANF; }
"ed"	{ return F_ED; }
"localcmd" { return F_LOCALCMD; }
"swap"	{ return F_SWAP; }
"break"	{ return F_BREAK; }
"continue" { return F_CONTINUE; }
"while"	{ return F_WHILE; }
"capitalize" { return F_CAPITALIZE; }
"living" { return F_LIVING; }
"lower_case" { return F_LOWER_CASE; }
"set_heart_beat" { return F_SET_HEART_BEAT; }
"strlen" { return F_STRLEN; }
"say"	{ return F_SAY; }
"tell_room"	{ return F_TELL_ROOM; }
"find_object"   { return F_FIND_OBJECT; }
"add_worth"	{ return F_ADD_WORTH; }
"time"		{ return F_TIME; }
"create_wizard" { return F_CREATE_WIZARD; }
"destruct" { return F_DESTRUCT; }
"environment" { return F_ENVIRONMENT; }
"save_object" { return F_SAVE_OBJECT; }
"enable_commands" { return F_ENABLE_COMMANDS; }
"random" { return F_RANDOM; }
"restore_object" { return F_RESTORE_OBJECT; }
"this_object" { return F_THIS_OBJECT; }
"clone_object" { return F_CLONE_OBJECT; }
"this_player" { return F_THIS_PLAYER; }
"if"	{ return F_IF; }
"status" { return F_STATUS; }
"object" { return F_STATUS; }
"return" { return F_RETURN; }
"write" { return F_WRITE; }
"add_verb" { return F_ADD_VERB; }
"add_adj" { return F_ADD_ADJ; }
"add_subst" { return F_ADD_SUBST; }
"add_action" { return F_ADD_ACTION; }
"move_object" { return F_MOVE_OBJECT; }
"present" { return F_PRESENT; }
"command" { return F_COMMAND; }
"set_light" { return F_SET_LIGHT; }
"int"	{ return F_INT; }
"call_other" { return F_CALL_OTHER; }
"shutdown" { return F_SHUTDOWN; }
[_a-zA-Z][_a-zA-Z0-9]* { return ident(yytext); }
\"([^"\n]|\\\")*\"    { return string(yytext); }
[\t ]	{ break; }
"\n"	{ current_line++; }
.	{ char buff[100]; sprintf(buff, "Illegal character '%c'", yytext[0]);
	  yyerror(buff); }
%%

char *xalloc();

extern YYSTYPE yylval;

#ifdef LEX
int yywrap() { return 1; }
#endif

int ident(str)
    char *str;
{
    int i;

    for (i=current_number_of_locals-1; i>=0; i--) {
	if (strcmp(local_names[i], str) == 0) {
	    yylval.lnode = (struct lnode *)alloc_lnode_number(F_LOCAL_NAME, i);
	    return F_LOCAL_NAME;
	}
    }
    if (argument_name && strcmp(str, argument_name) == 0)
	return F_ARGUMENT;
    yylval.string = string_copy(str);
    return F_IDENTIFIER;
}

int string(str)
    char *str;
{
    char *p;

    p = xalloc(strlen(str) - 1);
    yylval.string = p;
    for (str++; str[1] != '\0'; str++, p++) {
	if (str[0] == '\\' && str[1] == 'n') {
	    *p = '\n';
	    str++;
	} else if (str[0] == '\\' && str[1] == 't') {
	    *p = '\t';
	    str++;
	} else if (str[0] == '\\' && str[1] == '"') {	/* LA */
	    *p = '"';					/* LA */
	    str++;					/* LA */
 	    if (!str[1]) {
 		yyerror("Unterminated string");
 		break;
 	    }

	} else
	    *p = *str;
    }
    *p = '\0';
    return F_STRING;
}

int number(str)
    char *str;
{
    int i;

    i = atoi(str);
    if (i == 0) {
        yylval.lnode = (struct lnode *)alloc_lnode_single(F_CONST0);
	return F_CONST0;
    } else if (i == 1) {
	yylval.lnode = (struct lnode *)alloc_lnode_single(F_CONST1);
	return F_CONST1;
    }
    yylval.lnode =(struct lnode *)alloc_lnode_number(F_NUMBER, atoi(str));
    return F_NUMBER;
}

void start_new_file(f)
    FILE *f;
{
#ifdef FLEX
    yy_init = 1;
#endif
    yyin = f;
}
