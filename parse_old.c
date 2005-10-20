/*

  Pattern Parser package for LPmud, JnA 1990

  Ver 2.0

  If you have questions or complaints about this code please refer them
  to jna@cd.chalmers.se

  This is the old version of parse_command() used in compatibility mode.

*/

#include "driver.h"

#include <stdio.h>
#include <ctype.h>
#include <time.h>

#include "parse.h"

#include "array.h"
#include "interpret.h"
#include "main.h"
#include "object.h"
#include "random.h"
#include "simulate.h"
#include "wiz_list.h"

#define VALUE struct svalue
#define OBJECT struct object
#define VECTOR struct vector
#define SVALUE struct svalue
#define LVALUE struct svalue


#if defined(COMPAT_MODE) && defined(SUPPLY_PARSE_COMMAND)
/*****************************************************

  This is the parser


  Left to do 910723:

  Fix so alternatives possible after %s in a pattern 

*/
/*
  Doc for LPC function

int parse_command(string,ob/arr,string,destargs...)

	parse		Returns 1 if pattern matches

	string		Given command
	ob/arr		if arr 
				array holding the accessible objects
			if ob
				object from which to recurse and create
				the list of accessible objects, normally
				ob = environment(this_player())
	string		Parsepattern as list of words and formats:
			Example string = " 'get' / 'take' %i "
			Syntax:
				'word' 		obligatory text
				[word]		optional text
				/		Alternative marker
				%o		Single item, object
				%l		Single living object
				%s		Any text
				%w              Any word
				%p		Preposition
				%i		Any items
				%d              Number 0- or tx(0-99)

	destargs	This is the list of result variables as in sscanf
			One variable is needed for each %_
			The return types of different %_ is:
			%o	Returns an object
			%l	Returns an object
			%s	Returns a string of words
			%w      Returns a string of one word
			%p	Can on entry hold a list of word in array
				or an empty variable
				Returns:
				   if empty variable: a string
				   if array: array[0]=matched word
			%i	Returns a special array on the form:
				[0] = (int) +(wanted) -(order) 0(all)
				[1..n] (object) Objectpointers
			%d      Returns a number
			
Example:

 parse_command("take apple",environment(this_player()),
 " 'get' / 'take' %i ",items);


*/ 

/* Hardcoded function names to call in LPC objects
*/
#define QIDFUNC "id"                /* Func to query in objects to ack name */
#define QPLURIDFUNC "plural_id"     /* As Id but pluralform of name */
#define QADJFUNC "adjectiv_id"      /* As Id but ack adjectiv not name */
#define QSHORTFUNC "short"          /* Objects shortdescription */

#define EQ(x,y) (strcmp(x,y)==0)
#define EQN(x,y) (strncmp(x,y,strlen(x))==0)
#define EMPTY(x) (strcmp(x,"")==0)
#define COPY(x) (strcpy((char*)xalloc(strlen(x)+1),x))

#define KLUDGELEN   200            /* How long should strings be */

struct altern_objects {
  OBJECT *ao_obj;
  struct altern_objects *next;
};

static int gDebug=0;
static char gMword[KLUDGELEN];            /* Text inside '' or [] */
static char gFword[KLUDGELEN];            /* Temp word gotten by getfirst() */
static struct altern_objects *gOblist;    /* List of accessible objects */
static char gAdjective[4*KLUDGELEN];      /* all adjectives before objname */
static LVALUE *gCarg;                     /* Current argument to %_ */
static int gWantnum;                      /* Number of wanted items 0 = all */
static struct altern_objects *gPobjects;  /* List of parsed objects */
static LVALUE *gTxarg;                    /* Argument of LPCvariable to store %s  */
static LVALUE *gForprepos;                /* Save arg* here for findprepos */
static LVALUE *gTopStack;                 /* arg* to arg after my last */
static struct svalue sv_tmp;

#define EP 0          /* End Parse marker */
#define SI 1          /* %o single item */
#define IT 2          /* %i  items */
#define US 3          /* %l  single living object */
#define PP 4          /* %p  prepositions */
#define TX 5          /* %s  string of words */
#define DTX 6         /* 'word' */
#define OTX 7         /* [word] */
#define ALT 8         /* /   alternates */
#define W1 9          /* %w  string of one word */
#define NUM 10        /* %d  integer */

/* Search string in reverse order, I should replace it with strrchr I know :)
*/
static char *backstrchr (apa, ch)
    char *apa;
    char ch;
{
  char *sp;

  sp = &apa[strlen(apa)];
  while (sp>apa) {
    if (*sp==ch) return sp+1;
    sp--;
  }
  return 0;
}

/* Convert string to lowercase
*/
static char *lowercase (apa)
    char *apa;
{
  char *bepa;

  bepa = apa;
  while (*bepa) {
    if ((*bepa>'A') && (*bepa<='Z')) (*bepa)+='a'-'A';
    bepa++;
  }
  return apa;
}

/* Add all objects in the inventory of 'first' and itself to the list 'parent'
   antal holds the number of elements in the 'parent' list.
*/
static void fixlist (first, parent, antal)
    OBJECT *first;
    struct altern_objects **parent;
    int *antal;
{
  OBJECT *cur;
  struct altern_objects *this,*taill;
  cur=first; this= *parent;
  if (!cur) return;
  while (cur) {
    if (cur->contains) fixlist(cur->contains,&this,antal);
    taill=this;
    this=(struct altern_objects *) xalloc(sizeof(struct altern_objects));
    this->ao_obj=cur; (*antal)++;
    add_ref(cur,"parse->fixlist");
    if (gDebug) fprintf(stderr," Obj:%s ,",cur->name);
    this->next=taill; cur=cur->next_inv;
  }
  *parent=this;
}

/* Create a list of the theoretically accessible objects from 'src'
*/
static int makeobjlist (alist, src)
    struct altern_objects **alist;
    OBJECT *src;
{
  OBJECT *env,*cur;
  struct altern_objects *this;
  int cnt;
  *alist=0;
  if (!src) env=command_giver->super;
  else env=src;
  if (!env) return 0;
  if (gDebug) fprintf(stderr,"Make list: %s\n",env->name);
  if (!(env->contains)) return 0;
  cur=env->contains;
  this=0; cnt=0;
  fixlist(cur,&this,&cnt); *alist=this;
  return cnt;
}

/* Routines to use for alternate filters
*/

/* Return number of objects in the list
*/
static int itnumalt()
{ 
  int ant;
  struct altern_objects *ao;

  ant=0; ao=gPobjects;
  while (ao) {
    ant++;
    ao=ao->next;
  }
  return ant;
}

/* Return a pointer to the numbered element of the list
*/ 
static OBJECT *italt (a)
    int a;
{ 
  int ant;
  struct altern_objects *ao;

  ant=0; ao=gPobjects;
  while (ao) {
    ant++;
    if (ant==a) return ao->ao_obj;
    ao=ao->next;
  }
  return 0;
}

/* Free the list, parameter bas is obsolete
*/
static void italt_new ()
{

  /* Free p} altern_objects listan */
  struct altern_objects *ao;
  struct altern_objects *ao2;

  if (gDebug) fprintf(stderr,"Clearing italt\n");
  ao2=ao=gPobjects; gPobjects=0;
  while (ao) {
    ao=ao->next;
    free_object(ao2->ao_obj,"parse->italt_new()");
    xfree((char *)ao2); 
    ao2=ao;
  }
}

/* Create the list as all the theoretically accessible objects
*/
static void italt_loadall()
{
  if (gPobjects) italt_new();
  makeobjlist(&gPobjects,0);
}

/* Put an object at the end of the list
*/
static void italt_put (obj)
    OBJECT *obj;
{
  struct altern_objects *ao,*old;

  if (gDebug) fprintf(stderr,"Putting: %s",obj->name);
  ao=gPobjects; old=ao;
  while (ao) {
    old=ao; ao=ao->next;
  }
  ao=(struct altern_objects *) xalloc(sizeof(struct altern_objects));
  if (old) old->next=ao; 
  else gPobjects=ao;
  ao->ao_obj=obj; ao->next=0;
  add_ref(obj,"parse->italt_put()");
  if (gDebug) fprintf(stderr,"..done\n");
}


/* Put first word of cmd in gFword and point to next word
*/
static char *getfirst (cmd)
    char **cmd;
{
  int pos,inqoute;
  char ch,*st;

  st= *cmd; strcpy(gFword,""); if (st[0]==0) return gFword;
  ch=' '; pos=0; inqoute=0; 
  while ((st[0]<=' ') && (st[0])) st++; /* Skip leading spaces */
  if ((st[0]=='\'') || (st[0]=='[')) {
    inqoute=1; ch=st[0]; gFword[0]=st[0]; pos=1; st++;
  }
  if (ch=='[') ch=']';
  while ((pos<(KLUDGELEN-1)) && (st[0]) && (st[0]!=ch)) {
    gFword[pos++]=st[0]; st++;
    if ((st[0]) && (st[0]<' ')) st[0]=' ';  /* Replace ctrl chars */
  }
  if ((inqoute) && (pos<(KLUDGELEN-1))) {
    gFword[pos++]=ch; if (st[0]) st++;
  }
  gFword[pos]=0;  /* fprintf(stderr,"{%s}",gFword); */
  *cmd=st;
  return gFword;
}

/* Put first word of cmd in gFword
   without incrementing the callers pointer in the command string
*/
static char *lookfirst (cmd)
    char *cmd;
{
  return getfirst(&cmd);
}     

/* Call object function in LPC
*/
static int call_obj (fnamn, on, apa)
    char *fnamn;
    OBJECT *on;
    char *apa;
{
  struct svalue *ret;
  
  if (gDebug) fprintf(stderr,"Calling %s in %s with %s\n",fnamn,on->name,apa);
  push_volatile_string(apa);
  ret = apply(fnamn,on,1);
  if (!ret) return 0;
  if ( ret->type == T_NUMBER) return ret->u.number;
  return 0;
}

/* Change a noun in pluralform to a noun in singularform
*/
static char *singfix (str)
    char *str;
{
  static char sing[KLUDGELEN];
  char *sp;
  int sl;

  sl=strlen(str); if (sl<2) return str;
  sp= &str[sl-3]; strcpy(sing,str); sing[sl-3]=0;
  if (gDebug) fprintf(stderr,"To singular (%s) end(%s) begin(%s)\n",str,sp,sing);
  if (EQ(str,"corpses")) return "corpse";
  if (EQ(sp,"ses")) return strcat(sing,"s");
  else if (EQ(sp,"xes")) return strcat(sing,"x");
  else if (EQ(sp,"hes")) return strcat(sing,"h");
  else if (EQ(sp,"ies")) return strcat(sing,"y");
  else if (EQ(sp,"ves")) return strcat(sing,"fe");
  else if (sp[2]=='s') {
    strcat(sing,sp); sing[sl-1]=0;
    return sing;
  }
  if (EQ(str,"teeth")) return "tooth";
  if (EQ(str,"feet")) return "foot";
  if (EQ(str,"men")) return "man";
  if (EQ(str,"women")) return "woman";
  if (EQ(str,"children")) return "child";
  if (EQ(str,"sheep")) return "sheep";
  return str;
}
    
/* Checks with obj in gOblist to see if it accepts all gAdjectives in adjs
   Used when name of object not known, i.e cmds like: 'get all the red ones'
   if func QADJFUNC doesn't exist in obj last word of QSHORTFUNC is used
   as object name and set as suffix to the adjective in call to id()
*/
static int matchadjective (adjs)
    char *adjs;
{

  char ad[KLUDGELEN],tot[2*KLUDGELEN],*sp,*sp2;
  struct altern_objects *ao;
  OBJECT *on;
  struct svalue *ret;

  ao=gOblist;
  while (ao) {
    on=ao->ao_obj;
    sp=adjs;
    while ((on) && (sp) && (*sp)) {
      while (*sp==' ') sp++;
      sp2=strchr(sp,' ');  
      if (sp2) {
	*sp2=0;
	strcpy(ad,sp); *sp2=' '; sp=sp2;
	if (!call_obj(QADJFUNC,on,ad)) on=0; /* Not ok */
      }
      sp=sp2;
    }
    if (on) italt_put(on);
    else {
      on=ao->ao_obj;
      ret = apply(QSHORTFUNC,on,0);
      if (ret && ret->type==T_STRING) {
	sp=ret->u.string; sp2=backstrchr(sp,' ');
	if (sp2) sp=sp2;
	sprintf(tot,"%s%s",adjs,sp); lowercase(tot);
	if (!call_obj(QIDFUNC,on,tot)) on=0;
      }
      else on=0;
    }
    if (on) italt_put(on);
    ao=ao->next;
  }
  return itnumalt();
}

/* Decide if a word is a general word of type: those, ones
*/
static int check_for_general (onam, plur)
    char *onam;
    int plur;
{

  static char* plurpron[] = {"those","them","these","ones","$"};
  static char* singpron[] = {"this","one","it","$"};
  int ilop;
  
  if (EMPTY(onam)) return 0;
  if (plur) {
    ilop=0; 
    while (!EQ(plurpron[ilop],"$")) if (EQ(plurpron[ilop++],onam)) return 1;
  }
  else {
    ilop=0; 
    while (!EQ(singpron[ilop],"$")) if (EQ(singpron[ilop++],onam)) return 1;
  }
  return 0;
}

static int order_num (wd)
    char *wd;
{ /* Only positive numbers and zero */

  static char *onums[] = {"first","second","third","fourth","fifth","sixth",
		  "seventh","eighth","nineth","tenth",
		  "eleventh","twelfth","thirteenth","fourteenth","fifteenth","sixteenth",
		  "seventeenth","eighteenth","nineteenth","dummy"};
  static char *onumt[] = {"twenty","thirty","forty","fifty","sixty","seventy",
		  "eighty","ninety"};
  static char *onumta[] = {"twentieth","thirtieth","fortieth","fiftieth","sixtieth","seventieth",
		  "eightieth","ninetieth"};

  char ns[KLUDGELEN];

  int ilop,nm;

  if (EMPTY(wd)) return -1;
  for (ilop=1;ilop<20;ilop++) {
    if (EQ(onums[ilop-1],wd)) return ilop;
  }
  for (nm=0;nm<8;nm++) for (ilop=0;ilop<9;ilop++) {
    if (ilop>0) {
	sprintf(ns,"%s%s",onumt[nm],onums[ilop-1]);
	if (EQ(ns,wd)) return 20+nm*10+(ilop);
    } else
	if (EQ(onumta[nm],wd)) return 20+nm*10+(ilop);
  }
  return -1;
}

static int numeric (wd)
    char *wd;
{ /* Only positive numbers and zero */

  static char *nums[] = {"one","two","three","four","five","six",
		  "seven","eight","nine","ten",
		  "eleven","twelve","thirteen","fourteen","fifteen","sixteen",
		  "seventeen","eighteen","nineteen"};
  static char *numt[] = {"twenty","thirty","forty","fifty","sixty","seventy",
		  "eighty","ninety"};

  char ns[KLUDGELEN];

  int ilop,nm;

  if (EMPTY(wd)) return -1;
  if (sscanf(wd,"%d",&nm)) return (nm>=0) ? nm : -1;
  if ((EQ(wd,"a")) || (EQ(wd,"an"))) return 1;
  for (ilop=1;ilop<20;ilop++) if (EQ(nums[ilop-1],wd)) return ilop;
  for (nm=0;nm<8;nm++) for (ilop=0;ilop<9;ilop++) {
    if (ilop>0) {
	sprintf(ns,"%s%s",numt[nm],nums[ilop-1]);
	if (EQ(ns,wd)) return 20+nm*10+(ilop);
    }
    else
	if (EQ(numt[nm],wd)) return 20+nm*10+(ilop);
  }
  return -1;
}

/* Searches commandstring for "adj1 adj2 ... adjN objectname"
   Stores words before name in string: gAdjective, as "adj1 adj2 adj3 "
*/
static OBJECT *matchobject2 (cmd, plur)
    char **cmd;
    int plur;
{ 
  OBJECT *on;
  struct altern_objects *ao;
  char *st,*ocmd,totname[2*KLUDGELEN],tot2[2*KLUDGELEN];

  ocmd= *cmd; strcpy(gAdjective,"");
  st=lowercase(getfirst(cmd)); 
  while (st[0]) {
    if (check_for_general(st,plur)) {
      if (matchadjective(gAdjective)) return italt(1);
    }
    sprintf(totname,"%s%s",gAdjective,st);
    if (plur) sprintf(tot2,"%s%s",gAdjective,singfix(st));
    ao=gOblist; 
    while (ao) {
      on=0;
      if (plur) {
	if (call_obj(QPLURIDFUNC,ao->ao_obj,totname)) on=ao->ao_obj;
	else if (call_obj(QIDFUNC,ao->ao_obj,tot2)) on=ao->ao_obj;
      }
      else if (call_obj(QIDFUNC,ao->ao_obj,totname)) on=ao->ao_obj;
      if (on) italt_put(on);
      ao=ao->next;
    }
    if (!itnumalt()) {
      strcat(gAdjective,st); strcat(gAdjective," ");
      st=lowercase(getfirst(cmd)); 
    }
    else st[0]=0;
  }
  if (itnumalt()) return italt(1);
  *cmd=ocmd; return 0;
}

/* Search the command for valid object description
*/
static OBJECT *finditem (cmd, plur)
    char **cmd;
    int plur;
{ 
  int nm;
  OBJECT *pn;
  char *ocmd,w1[KLUDGELEN];
  
  ocmd= *cmd; strcpy(w1,lowercase(getfirst(cmd)));
  if (gDebug) fprintf(stderr,"FO (plur=%d): %s\n",plur,ocmd);
  italt_new(); /* Clear alternate list */
  gWantnum=1; /* Antal objekt som anv{ndaren vill ta */
  if (EMPTY(w1)) {
    *cmd=ocmd;
    return 0;
  }

  /* Must be hardcode skipped for recursive with plural
   */
  if (EQ("the",w1)) {
    if (gDebug) fprintf(stderr,"Skip 'the'\n");
    *cmd=ocmd; getfirst(cmd); 
    if ((pn=finditem(cmd,plur))) return pn;
    else { *cmd=ocmd; return 0; }
  }
  
  /* Check things like: some <objname>
   */
  else if ((EQ("some",w1)) && (plur==0)) {
    *cmd=ocmd; getfirst(cmd);
    /* Skip of in : some of the bottles */
    if (EQ(lowercase(lookfirst(*cmd)),"of")) getfirst(cmd);

    nm = random_number(6) + 2; /* some objects are random:2-7 */
    if (gDebug) fprintf(stderr,"some rnd = %d\n",nm);
    if ((pn=finditem(cmd,1))) { 
      gWantnum=nm; 
      return pn; 
    }
  }
  
  /* Check things like: three <objname>
   */
  else if ( (plur==0) && ((nm=numeric(w1))>0) ) {
    if (gDebug) fprintf(stderr,"Numeral: %d (%s)\n",nm,w1);
    *cmd=ocmd; getfirst(cmd);
    /* Skip of in : two of the bottles */
    if (EQ(lowercase(lookfirst(*cmd)),"of")) getfirst(cmd);
    if ((pn=finditem(cmd,nm-1))) {
      gWantnum=nm; return pn;
    }
  }
  
  /* Check things like: third <objname>
   */
  else if ( (plur==0) && ((nm=order_num(w1))>0) ) {
    if (gDebug) fprintf(stderr,"Order: %d (%s)\n",nm,w1);
    *cmd=ocmd; getfirst(cmd);
    /* Skip of in : second of the bottles */
    if (EQ(lowercase(lookfirst(*cmd)),"of")) {
      getfirst(cmd);
      if ((pn=finditem(cmd,1))) { /* Fix second of the bottles */
	gWantnum= -nm; return pn;
      }
    }
    else {
      if ((pn=finditem(cmd,0))) { /* Fix second bottle */
	gWantnum = -nm; return pn;
      }
    }
  }
  
  
  /* Check things like: all <objname>
   */
  else if ((EQ("all",w1)) && (plur==0)) {
    if (gDebug) fprintf(stderr,"All found\n");
    *cmd=ocmd; getfirst(cmd);
    /* Skip of in : all of the bottles */
    if (EQ(lowercase(lookfirst(*cmd)),"of")) { getfirst(cmd); }
    if ((pn=finditem(cmd,1))) { /* Handle: get all apples */
      gWantnum=0; /* Antal objekt som anvandaren vill ta 0 == all */
      return pn;
    }
    else { /* Handle: get all */
      italt_loadall(); pn=(OBJECT *) italt(1); 
      gWantnum=0; /* Antal objekt som anvandaren vill ta 0 == all */
    }
  }
  
  /* Search for: adj1 adj2 ... adjN objectname
   */
  else {
    *cmd=ocmd; if (gDebug) fprintf(stderr,"Objtry: %s\n",ocmd);
    if (plur) pn=matchobject2(cmd,plur);
    else { /* Standard singular */
      pn=matchobject2(cmd,0); 
      if (!pn) { /* Handle things of type: get apples */
	pn=matchobject2(cmd,1); 
	if (pn) gWantnum = 0; /* Default this to all */
      }
    }
    if (gDebug) fprintf(stderr,"Done matchobject in finditem.\n");
  }
  
  if (pn == 0) *cmd=ocmd;
  if (gDebug) fprintf(stderr,"Done finditem.\n");
  return pn;
}

/* Find match to a %i in pattern
*/
static int findobject (cmd)
    char **cmd;
{
  int nm,s;
  OBJECT *ob;
  
  if (finditem(cmd,0)) {
    if (gCarg) {
      VECTOR *p;
      if (gDebug) fprintf(stderr,"Assign array\n");
      nm=itnumalt();
      p=(VECTOR *)allocate_array(nm+1);
      p->item[0].type = T_NUMBER;
      p->item[0].u.number = gWantnum;

      /* Make array in reverse order from italt() because
         makeobjlist() has reversed the order on entry
      */
      for (s=1;s<=nm;s++) {
	p->item[s].type = T_OBJECT;
	p->item[s].u.ob = ob = italt(nm+1-s);
	if (!ob) if (gDebug) fprintf(stderr,"No object from italt %d\n",s);
	if (ob) add_ref(ob,"parse_command");
      }
      sv_tmp.type = T_POINTER;
      sv_tmp.u.vec = p;
      transfer_svalue(gCarg->u.lvalue, &sv_tmp);
#if 0 /* amylaar: the ref count is already set by allocate_array */
      gCarg->u.lvalue->u.vec->ref++;
#endif

      if (gDebug) fprintf(stderr,"Assign array2\n");
    }
    return 1;
  }
  return 0;
}

/* Find match to a %l in pattern
*/
static OBJECT *findplay (cmd)
    char **cmd;
{ 
  OBJECT *pn;
  char w1[KLUDGELEN];

  strcpy(w1,lowercase(lookfirst(*cmd)));
  /* can be fixed later with call to LPC command_giver->query_real_name() 
  if (EQ(w1,"me")) strcpy(w1,getmyname_jp());
  if (EQ(w1,"myself")) strcpy(w1,getmyname_jp());
  */
  pn=(OBJECT *) find_living_object(w1, 1);  /* Find player by name */
  if (pn) {
    getfirst(cmd); 
    if (gCarg) {
      sv_tmp.type = T_OBJECT;
      sv_tmp.u.ob = pn;
      add_ref(pn, "parse_command(%l)");
      transfer_svalue(gCarg->u.lvalue, &sv_tmp);
    }
  }
  return pn;
}

/* Find match to %p in pattern when a list of words has been supplied
*/
static int findword (cmd, v)
    char **cmd;
    VALUE *v;
{
  char *w;
  VECTOR *p;
  struct svalue sv;
  int cnt,m,f;

  w = lookfirst(*cmd); lowercase(w); p=v->u.vec;
  cnt=0; m=VEC_SIZE(p); f= -1;
  while (cnt<m) {
    if (p->item[cnt].type == T_STRING) {
      if (strcmp(p->item[cnt].u.string,w)==0) {
	f=cnt; cnt=m;
      }
    }
    cnt+=1;
  }
  if (f<0) return 0;
  getfirst(cmd);
  if (!f) return 1; /* Match and word in first element */
  /*
    Swap element 0 and f in array
  */
  sv=p->item[0]; p->item[0]=p->item[f]; p->item[f]=sv;
  return 1;
}

/* Find match to %p in pattern
*/
static int findprepos (cmd)
    char **cmd;
{
  char *w;
  static char *hard_prep[] = {"on","in","under","from","behind","beside",0};
  SVALUE *v;
  int cnt;

  if (gForprepos) {
    v = gForprepos->u.lvalue;
    if ((v != 0) && (v->type == T_POINTER)) return findword(cmd,v);
  }

  /* No wordlist sent, use hard coded prepositions and return a string
  */
  w = lookfirst(*cmd); lowercase(w);
  cnt=0; while (hard_prep[cnt]) {
    if (strcmp(w,hard_prep[cnt])==0) {
      getfirst(cmd); /* Skip this word */
      if (gCarg) {
	sv_tmp.type = T_STRING;
	sv_tmp.x.string_type = STRING_MALLOC;
	sv_tmp.u.string = string_copy(w);
	transfer_svalue(gCarg->u.lvalue, &sv_tmp);
	return 1;
      }
    }
    cnt++;
  }
  return 0;
}

/* Find match to %o in pattern
*/
static int findsingle (cmd)
    char **cmd;
{
  if (finditem(cmd,0)) {
    if ((itnumalt()==1) && (gCarg)) {
      sv_tmp.type = T_OBJECT;
      add_ref( sv_tmp.u.ob=italt(1), "parse_command(%o)" );
      transfer_svalue(gCarg->u.lvalue, &sv_tmp);
    }
    return 1;
  }
  return 0;
}

/* Get the first parsetype of the pattern
*/
static int get1ps (parsep, lin, skip)
    char **parsep;
    LVALUE **lin;
    int skip;
{
  char *cod,ch;
  int pt;
  LVALUE *l;

  pt=EP; cod=getfirst(parsep); l= *lin;
  ch=cod[0]; if (ch=='%') { ch=tolower(cod[1]); ch=cod[1]; }
  switch (ch) {
  case 'i':pt=IT; break;
  case 'l':pt=US; break;
  case 's':pt=TX; break;
  case 'w':pt=W1; break;
  case 'o':pt=SI; break;
  case 'p':pt=PP; break;
  case 'd':pt=NUM; break;
  case '\'':sscanf(cod,"'%[^\']",gMword); pt=DTX; break;
#if 0
  case '[':sscanf(cod,"\[%[^\135]",gMword); pt=OTX; /* 135 is oct for ] */
#else
  case '[':sscanf(cod,"\\[%[^]]",gMword); pt=OTX;
#endif
    if (gMword[strlen(gMword)-1]==']') gMword[strlen(gMword)-1]=0;
    break;
  case '/':pt=ALT; break;
  }
  if (gDebug) fprintf(stderr,"(%s,%s,%d,%c)",cod,gMword,pt,ch);
  gCarg=0;
  if ((skip) || (pt==DTX) || (pt==OTX) || (pt==ALT)) return pt;
  if (l != gTopStack) { gForprepos=l; gCarg=l; l++; }
  else { gCarg = 0; gForprepos=0; }
  *lin=l;
  return pt;
}

/* Add word to the string that matches the latest %s
*/
static void addword (d, s)
    char *d;
    char *s;
{
  if ((strlen(d)+strlen(s))<(KLUDGELEN-2)) {
    if (!EMPTY(d)) strcat(d," "); 
    strcat(d,s);
  }
  if (gTxarg) {
    sv_tmp.type = T_STRING;
    sv_tmp.x.string_type = STRING_MALLOC;
    sv_tmp.u.string = string_copy(d);
    transfer_svalue(gTxarg->u.lvalue, &sv_tmp);
  }
}

/* Main function, called from interpret.c
*/
int parse (cs, ob_or_array, ps, dest_args, num_arg)
    char *cs, *ps;
    struct svalue *ob_or_array, *dest_args;
    int num_arg;
{
  /* Arguments:
     string      Command
     ob/arr      List of accessible objects or recurse start object
     ps		 parsepattern
     destargs	 lvalues.
     num_arg	 Number of lvalues.
     */
  
  int altflag,         /* =1 (true) i altkedja, match har intr{ffat */
  txflag,              /* = 1 om vi adderar text till str{ng */
  ptyp;                /* Parsetype %o %i %l %p %s 'word' [word] / %w */
  
  char *parsep;            /* Parsepattern */
  char *cmd;               /* Command to be parsed */
  char *ops,*ocs;          /* Temporary parse and command */
  char tx_jp[KLUDGELEN];   /* Fill up string for %s */
  char *tx_save_parsep = NULL; /* Where to continue parsing when a word was added
			        * to a string
			        */
  char *tx_end_first_pattern = NULL;
  LVALUE *l;               /* Argument pointer in dest_args */

  if (ob_or_array->type == T_OBJECT &&
      (ob_or_array->u.ob->flags & O_DESTRUCTED))
    return 0;
  gDebug = 0;
  if (gDebug) {
    fprintf(stderr,"Hold on to your toes, entering JnAparse. ");
  }


  /* In mudlib 3.0 we will not have this
  */
  if (cs[0]=='@')
        error("Unsupported @ construct.\n");

  ocs = cmd = (char *) string_copy(cs);
  
  /* Get the accessible objects
   */
  if (ob_or_array->type == T_OBJECT) makeobjlist(&gOblist,ob_or_array->u.ob);
  else { /* Copy list of given accessible objects in reverse order */
    VECTOR *v;
    SVALUE *vv;
    int cnt;
    v=ob_or_array->u.vec; italt_new();
    for (cnt=0;cnt<(int)VEC_SIZE(v);cnt++) {
      vv = &(v->item[VEC_SIZE(v)-1-cnt]);
      if (vv->type==T_OBJECT) italt_put(vv->u.ob);
    }
    gOblist=gPobjects; gPobjects=0;
  }
  
  ops = parsep = (char *) string_copy(ps);
  gPobjects=gOblist;
  if (gDebug) fprintf(stderr,"Has made list of Acsobjects. %d \n",itnumalt());
  if (gDebug) fprintf(stderr,"Parsing: %s, with pattern: %s\n",cmd,parsep);
  gPobjects=0;

  /* Start parsing
   */
  txflag = -1; gCarg = 0; gTxarg = 0; gTopStack = dest_args + num_arg;
  l = dest_args;

  ptyp=get1ps(&parsep,&l,0); /* Get first pattern from parse */

  while (ptyp != EP) {
    if (gDebug) fprintf(stderr,"Pattern: %d\n",ptyp);
    altflag=0;
    switch(ptyp) { /* See which pattern type to search for */

    case EP: break;
      
    case SI: if (findsingle(&cmd)) altflag=1; /* %o */ break;
    case IT: if (findobject(&cmd)) altflag=1; /* %i */ break;
    case US: if (findplay(&cmd)) altflag=1;   /* %l */ break;
    case PP: if (findprepos(&cmd)) altflag=1; /* %p */ break;

    case TX: /* %s */
      txflag=1; strcpy(tx_jp,""); gTxarg=gCarg; altflag=1; 
      tx_save_parsep = parsep;
      break;
      
    case DTX: /* 'word' */
      if (EQ(lowercase(gMword),lowercase(lookfirst(cmd)))) {
	getfirst(&cmd); altflag=1;
      }
      break;

    case OTX: /* [word] */
      if (EQ(lowercase(gMword),lowercase(lookfirst(cmd)))) {
	getfirst(&cmd); 
      }
      altflag=1; /* Always take next parsepattern type [word] is opt */
      break;

    case ALT: /* / */
      altflag=1; /* Should not be found here, if so skip it */

    case W1: /* %w */
      gTxarg=gCarg; strcpy(tx_jp,""); altflag=1;
      addword(tx_jp,getfirst(&cmd));
      break;

    case NUM: /* %d */
      if ((altflag=numeric(lookfirst(cmd)))>0) {
	if (gCarg) {
	  sv_tmp.type = T_NUMBER;
	  sv_tmp.u.number = altflag;
	  transfer_svalue(gCarg->u.lvalue, &sv_tmp);
	}
	altflag=1; getfirst(&cmd);
      }
      break;
    }

    /* Pattern checked altflag==1 indicates match
    */
    if (altflag) { /* Pattern matched, fetch next and end string input */
      if (ptyp == TX) {
        ptyp=get1ps(&parsep,&l,0);
        tx_end_first_pattern = parsep;
      } else {
        ptyp=get1ps(&parsep,&l,0);
        txflag = -1; /* End string input if not just started */
      }
      while (ptyp == ALT) { /* Skip left over alternatives */
	ptyp=get1ps(&parsep,&l,1); /* Skip this pattern */
	ptyp=get1ps(&parsep,&l,0); /* Next real pattern or ALT */
      }
    }
    else { /* Pattern did not match */
      char *a;
      LVALUE *try;
      int tmp;
      a=parsep; try=l;
      tmp=get1ps(&a,&try,0);
      if (tmp == ALT) { 
	parsep = a;		   /* Skip ALT */
	ptyp=get1ps(&parsep,&l,0); /* Next real pattern or ALT */
      }
      else {
	if (txflag>=0 && *getfirst(&cmd)) {
	  /* %s is defined, add word and try pattern again */
	  addword(tx_jp, gFword);
	  if (parsep != tx_end_first_pattern) {
	    parsep = tx_save_parsep;
	    ptyp = get1ps(&parsep,&l,0);
	  }
	}
	else break; /* Impossible to match pattern, exit */
      }
    }
    
    if (gDebug) fprintf(stderr,"Pattern, after: %d cmd: '%s'\n",ptyp, cmd);
  } 

  /* End of pattern reached, what have got? What is left?
  */
  if (gDebug) fprintf(stderr,"Pattern ended\n");

  if (txflag>=0) { /* It ended with a %s, fill up the string */
    while (!EMPTY(cmd)) addword(tx_jp,getfirst(&cmd));
  }

  if (!EMPTY(cmd)) ptyp=ALT; /* No match so set ptyp != EP */

  /* Now clean up our mess, no alloced mem should remain
  */
  xfree(ocs); xfree(ops);
  if (gPobjects) italt_new(); /* Free alternate object list */
  if (gOblist) {
    gPobjects=gOblist; italt_new(); /* Free list of accessible objects */
  }
  if (ptyp==EP) {
    if (gDebug) fprintf(stderr,"Exiting Jnaparse with match\n");
    return 1; /* Successfull match */
  }
  else {
    if (gDebug) fprintf(stderr,"Exiting Jnaparse: No match\n");
    return 0; /* No match */
  }
}

/*

   End of Parser

***************************************************************/

#endif
