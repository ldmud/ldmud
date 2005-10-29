#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>
#ifdef __STDC__
#include <memory.h>
#endif

#include "lint.h"
#include "interpret.h"
#include "object.h"
#include "sent.h"
#include "config.h"
#include "wiz_list.h"
#include "exec.h"

extern int d_flag;
extern int total_num_prog_blocks, total_prog_block_size;

#ifdef USE_TIOCGETP		/* Check if BSD */
extern int getpid();
#else
extern pid_t getpid();
#endif

extern char *xalloc PROT((int)), *string_copy PROT((char *));

void remove_swap_file PROT((struct object *));

extern int atoi();

struct object *previous_ob;
extern struct svalue const0;

int tot_alloc_object, tot_alloc_object_size;

/*
 * Replace newlines in a string with a carriage return, to make the string
 * writeable on one line.
 */

static void replace_newline(str)
    char *str;
{
    for (; *str; str++) {
	if (str[0] == '\n')
#ifndef MSDOS
	    str[0] = '\r';
#else
	    str[0] = 30;
#endif
    }
}

/*
 * Replace carriage return in a string with newlines.
 */

static void restore_newline(str)
    char *str;
{
    for (; *str; str++) {
#ifndef MSDOS
	if (str[0] == '\r')
#else
	if (str[0] == 30)
#endif
	    str[0] = '\n';
    }
}

static int my_strlen(str)
    char *str;
{
    int sz = 0;

    while (*str) {
	sz++; 
	if (*str == '\"' || *str == '\\') sz++;
	str++;
    }
    return sz;
}

/*
 * Similar to strcat(), but escapes all funny characters.
 * Used by save_object().
 * src is modified temporarily, but restored again :-(
 */
static void my_strcat(dest,src)
    char *dest,*src;
{
    char *pt,*pt2,ch[2];

    pt = strchr(src,'\\'); ch[1] = 0;
    pt2 = strchr(src,'\"');
    if (pt2 && pt2<pt || pt == 0)
	pt = pt2;
    while (pt) {
	ch[0] = *pt; *pt = 0; 
	strcat(dest,src); strcat(dest,"\\"); strcat(dest,ch);
	src = pt+1; *pt = ch[0];
	pt = strchr(src,'\\'); 
	pt2 = strchr(src,'\"');
	if (pt2 && pt2<pt || !pt)
	    pt = pt2;
    }
    strcat(dest,src);
}
	

static int save_size(v)
     struct vector *v;
{
  int i,siz;
  char numbuf[100];

  for (i=0, siz = 0; i < v->size; i++) {
    if (v->item[i].type == T_STRING) {
      siz += my_strlen(v->item[i].u.string) + 3; /* my_ */
    }
    else if (v->item[i].type == T_POINTER) {
      siz += 2 + save_size(v->item[i].u.vec) + 2 + 1;
    }
    else if (v->item[i].type == T_NUMBER) {
      sprintf(numbuf,"%d",v->item[i].u.number);
      siz += strlen(numbuf) + 1;
    }
    else siz += 2;
  }
  return siz;
}

/*
 * Encode an array of elements into a contiguous string.
 */
static char *save_array(v)
     struct vector *v;
{
    char *buf,*tbuf,numbuf[100];
    int i;
    
    buf = xalloc(2+save_size(v)+2+1);
    
    strcpy(buf,"({");
    for (i=0; i < v->size; i++) {
	if (v->item[i].type == T_STRING) {
	    strcat(buf,"\""); my_strcat(buf,v->item[i].u.string); /* my_ */
	    strcat(buf,"\","); 
	}
	else if (v->item[i].type == T_POINTER) {
	    tbuf = save_array(v->item[i].u.vec);
	    strcat(buf,tbuf); strcat(buf,",");
	    free(tbuf);
	}
	else if (v->item[i].type == T_NUMBER) {
	    sprintf(numbuf,"%d,",v->item[i].u.number);
	    strcat(buf,numbuf);
	}
	else strcat(buf,"0,");	/* Objects can't be saved. */
    }
    strcat(buf,"})");
    return buf;
}

/*
 * Save an object to a file.
 * The routine checks with the function "valid_write()" in /obj/master.c
 * to assertain that the write is legal.
 */
void save_object(ob, file)
    struct object *ob;
    char *file;
{
    char *name, tmp_name[80];
    int len, i;
    FILE *f;
    int failed = 0;
    /* struct svalue *v; */

    if (ob->flags & O_DESTRUCTED)
	return;
#ifdef COMPAT_MODE
    if (ob->user) {
	strcpy(tmp_name,ob->user->name);strcat(tmp_name,"/");
	if (strncmp(file, "players/", 8) != 0 ||
	    strncmp(file+8, tmp_name, strlen(tmp_name)) != 0 ||
	    strchr(file, '.'))
	    {
		error("Illegal save file name %s\n", file);
	    }
    } else {
	if (strncmp(current_prog->name, "obj/", 4) != 0 &&
	    strncmp(current_prog->name, "room/", 5) != 0 &&
	    strncmp(current_prog->name, "std/", 4) != 0) 
	    {
		error("Illegal use of save_object()\n");
	    }
    }
#else
    file = check_valid_path(file, ob->eff_user, "save_object", 1);
    if (file == 0)
	error("Illegal use of save_object()\n");
#endif
    len = strlen(file);
    name = xalloc(len + 3);
    (void)strcpy(name, file);
#ifndef MSDOS
    (void)strcat(name, ".o");
#endif
    /*
     * Write the save-files to different directories, just in case
     * they are on different file systems.
     */
    sprintf(tmp_name, "%s.tmp", name);
#ifdef MSDOS
    (void)strcat(name, ".o");
#endif
    f = fopen(tmp_name, "w");
    if (f == 0) {
	free(name);
	error("Could not open %s for a save.\n", tmp_name);
    }
    for (i=0; i < ob->prog->num_variables; i++) {
	struct svalue *v = &ob->variables[i];
	char *new_string;

	if (ob->prog->variable_names[i].type & TYPE_MOD_STATIC)
	    continue;
	if (v->type == T_NUMBER) {
	    if (fprintf(f, "%s %d\n", ob->prog->variable_names[i].name,
			v->u.number) == EOF)
		failed = 1;
	} else if (v->type == T_STRING) {
	    new_string = string_copy(v->u.string);
	    replace_newline(new_string);
	    if (fprintf(f, "%s \"%s\"\n", ob->prog->variable_names[i].name,
			new_string) == EOF)
		failed = 1;
	    free(new_string);

/* Saving of arrays: JnA 910520
*/
	} else if (v->type == T_POINTER) {
	    new_string = save_array(v->u.vec);
	    replace_newline(new_string);
	    if (fprintf(f, "%s %s\n", ob->prog->variable_names[i].name,
			new_string) == EOF)
	        failed = 1;
	    free(new_string);
	}
    }
    (void)unlink(name);
#ifndef MSDOS
    if (link(tmp_name, name) == -1)
#else
    (void) fclose(f);
    if (rename(tmp_name,name) < 0)
#endif
    {
	perror(name);
	printf("Failed to link %s to %s\n", tmp_name, name);
	add_message("Failed to save object !\n");
    }
#ifndef MSDOS
    (void)fclose(f);
    unlink(tmp_name);
#endif
    free(name);
    if (failed)
	add_message("Failed to save to file. Disk could be full.\n");
}

static char *my_string_copy(str)
    char *str;
{
    char *apa,*cp;

    cp = apa = xalloc(strlen(str)+1);
    
    while(*str) {
	if (*str == '\\') {
	    *cp = str[1];
	    if (str[1]) str+=2;
	    else str++;   /* String ends with a \\ buggy probably */
	    cp++;
	}
	else { *cp = *str; cp++; str++; }
    }
    *cp=0;
    cp = string_copy(apa);
    free(apa);
    return cp;
}

/*
 * Find the size of an array. Return -1 for failure.
 */
static int restore_size(str)
     char **str;
{
  char *pt,*pt2;
  int siz,tsiz;

  pt = *str; 
  if (strncmp(pt,"({",2)) return -1;
  else pt += 2;
  siz = 0;

  while ((pt) && (*pt)) {
    if (pt[0] == '}') {
      if (pt[1] != ')') return -1;
      *str = &pt[2];
      return siz;
    }
    if (pt[0] == '\"') {
      pt2 = strchr(&pt[1],'\"');
      if (!pt2) return -1;
      pt2--;
      while (pt2[0] == '\\') {
	  pt = pt2;
	  pt2 = strchr(&pt[2],'\"');
	  if (!pt2) return -1;
	  pt2--;
      }
      if (pt2[2] != ',') return -1;
      siz++;
      pt = &pt2[3];
    }
    else if (pt[0] == '(') { 
      tsiz = restore_size(&pt); /* Lazy way of doing it, a bit inefficient */
      if (tsiz < 0)
	  return -1;
      pt++;
      siz++;
    }
    else {
      pt2 = strchr(pt, ',');
      if (!pt2)
	  return -1;
      siz++;
      pt = &pt2[1];
    }
  }
  return -1;
}

static struct vector *restore_array(str)
     char **str;
{
  struct vector *v,*t;
  char *pt,*pt2;
  int i,siz;
  
  pt = *str; 
  if (strncmp(pt,"({",2)) return 0;
  pt2 = pt;
  siz = restore_size(&pt2);
  if (siz < 0) return 0;
  v = allocate_array(siz);
  pt+=2;

  for (i=0;i<siz;i++) {
    if (!*pt) return v;
    if (pt[0] == '\"') {
      pt2 = strchr(&pt[1],'\"');
      if (!pt2) return v;
      pt2--;
      while (pt2[0] == '\\') {
	  pt2 = strchr(&pt2[2],'\"');
	  if (!pt2) return v;
	  pt2--;
      }
      if (pt2[2] != ',') return v;
      pt2[1] = 0;
      v->item[i].type = T_STRING;
      v->item[i].u.string = my_string_copy(pt+1); /* my_ */
      v->item[i].string_type = STRING_MALLOC;
      pt = &pt2[3];
    }
    else if (pt[0] == '(') {
      t = restore_array(&pt);
      if (!t) return v;
      v->item[i].type = T_POINTER;
      v->item[i].u.vec = t;
      /* v->item[i].u.vec->ref++; marion - ref is already 1 (allocate_array) */
      pt++;
    }
    else {
      pt2 = strchr(pt,',');
      if (!pt2) return v;
      pt2[0] = 0;
      v->item[i].type = T_NUMBER;
      sscanf(pt,"%d",&(v->item[i].u.number));
      pt = &pt2[1];
    }
  }
  if (strncmp(pt,"})",2)) return v;
  *str = &pt[2];
  return v;
}

int restore_object(ob, file)
    struct object *ob;
    char *file;
{
    char *name, var[100], *val, *buff, *space;
    int len;
    FILE *f;
    struct object *save = current_object;
    struct stat st;
    struct variable *p;

    if (current_object != ob)
	fatal("Bad argument to restore_object()\n");
    if (ob->flags & O_DESTRUCTED)
	return 0;

#ifndef COMPAT_MODE
    file = check_valid_path(file, ob->eff_user, "restore_object", 0);
    if (file == 0)
	error("Illegal use of restore_object()\n");
#endif

    len = strlen(file);
    name = xalloc(len + 3);
    (void)strcpy(name, file);
    if (name[len-2] == '.' && name[len-1] == 'c')
	name[len-1] = 'o';
    else
	(void)strcat(name, ".o");
    f = fopen(name, "r");
    if (!f || fstat(fileno(f), &st) == -1) {
	free (name);
	if (f) 
	    (void)fclose(f);
	return 0;
    }
    if (st.st_size == 0) {
	(void)fclose(f);
	free (name);
	return 0;
    }
    val = xalloc(st.st_size + 1);
    buff = xalloc(st.st_size + 1);
    current_object = ob;
    while(1) {
	struct svalue *v;

	if (fgets(buff, st.st_size + 1, f) == 0)
	    break;
	/* Remember that we have a newline at end of buff ! */
	space = strchr(buff, ' ');
	if (space == 0 || space - buff >= sizeof (var)) {
	    (void)fclose(f);
	    error("Illegal format when restore %s.\n", name);
	}
	(void)strncpy(var, buff, space - buff);
	var[space - buff] = '\0';
	(void)strcpy(val, space+1);
	p = find_status(var, 0);
	if (p == 0 || (p->type & TYPE_MOD_STATIC))
	    continue;
	v = &ob->variables[p - ob->prog->variable_names];
	if (val[0] == '"') {
	    val[strlen(val) - 2] = '\0';	/* Strip trailing "\n */
	    restore_newline(val+1);
	    free_svalue(v);
	    v->type = T_STRING;
	    v->u.string = string_copy(val+1);
	    v->string_type = STRING_MALLOC;
	    continue;
	}
/* Restore array: JnA 910520
*/
	if (val[0] == '(') {
	    char *pt = val;
	    val[strlen(val) - 1] = '\0';	/* Strip trailing \n */
	    restore_newline(val+1);
	    free_svalue(v);
	    v->type = T_POINTER;
	    v->u.vec = restore_array(&pt);
	    if (!v->u.vec) {
		*v = const0;
		(void)fclose(f);
	       error("Illegal array format when restore %s.\n", name);
	    }
	    continue;
	}

	free_svalue(v);
	v->type = T_NUMBER;
	v->u.number = atoi(val);
    }
    current_object = save;
    if (d_flag > 1)
	debug_message("Object %s restored from %s.\n", ob->name, name);
    free(name);
    free(buff);
    free(val);
    (void)fclose(f);
    return 1;
}

void tell_npc(ob, str)
    struct object *ob;
    char *str;
{
    push_constant_string(str);
    (void)apply("catch_tell", ob, 1);
}

/*
 * Send a message to an object.
 * If it is an interactive object, it will go to his
 * screen. Otherwise, it will go to a local function
 * catch_tell() in that object. This enables communications
 * between players and NPC's, and between other NPC's.
 */
void tell_object(ob, str)
    struct object *ob;
    char *str;
{
    struct object *save_command_giver;

    if (ob->flags & O_DESTRUCTED)
	return;
    if (ob->interactive) {
	save_command_giver = command_giver;
	command_giver = ob;
	add_message("%s", str);
	command_giver = save_command_giver;
	return;
    }
    tell_npc(ob, str);
}

void free_object(ob, from)
    struct object *ob;
    char *from;
{
    struct sentence *s;

    ob->ref--;
    if (d_flag > 1)
	printf("Subtr ref to ob %s: %d (%s)\n", ob->name,
		      ob->ref, from);
    if (ob->ref > 0)
	return;
    if (d_flag)
	printf("free_object: %s.\n", ob->name);
    if (!(ob->flags & O_DESTRUCTED)) {
	/* This is fatal, and should never happen. */
	fatal("FATAL: Object 0x%x %s ref count 0, but not destructed (from %s).\n",
	    ob, ob->name, from);
    }
    if (ob->interactive)
	fatal("Tried to free an interactive object.\n");
    /*
     * If the program is freed, then we can also free the variable
     * declarations.
     */
    if (ob->prog) {
	tot_alloc_object_size -=
	    (ob->prog->num_variables - 1) * sizeof (struct svalue) +
		sizeof (struct object);
	free_prog(ob->prog, 1);
	ob->prog = 0;
    }
    if (ob->swap_num != -1)
	remove_swap_file(ob);
    for (s = ob->sent; s;) {
	struct sentence *next;
	next = s->next;
	free_sentence(s);
	s = next;
    }
    if (ob->name) {
	if (d_flag > 1)
	    debug_message("Free object %s\n", ob->name);
	if (lookup_object_hash(ob->name) == ob)
	    fatal("Freeing object %s but name still in name table", ob->name);
	free(ob->name);
	ob->name = 0;
    }
    tot_alloc_object--;
    free((char *)ob);
}

void add_ref(ob, from)
    struct object *ob;
    char *from;
{
    ob->ref++;
    if (d_flag > 1)
	printf("Add reference to object %s: %d (%s)\n", ob->name,
	       ob->ref, from);
}

/*
 * Allocate an empty object, and set all variables to 0. Note that a
 * 'struct object' already has space for one variable. So, if no variables
 * are needed, we allocate a space that is smaller than 'struct object'. This
 * unused (last) part must of course (and will not) be referenced.
 */
struct object *get_empty_object(num_var)
    int num_var;
{
    static struct object NULL_object;
    struct object *ob;
    int size = sizeof (struct object) +
	(num_var - !!num_var) * sizeof (struct svalue);
    int i;

    tot_alloc_object++;
    tot_alloc_object_size += size;
    ob = (struct object *)xalloc(size);
    /* marion
     * Don't initialize via memset, this is incorrect. E.g. the bull machines
     * have a (char *)0 which is not zero. We have structure assignment, so
     * use it.
     */
    *ob = NULL_object;
    ob->ref = 1;
    ob->swap_num = -1;
    for (i=0; i<num_var; i++)
	ob->variables[i] = const0;
    return ob;
}

void remove_all_objects() {
    struct object *ob;
    struct svalue v;

    v.type = T_OBJECT;
    while(1) {
	if (obj_list == 0)
	    break;
	ob = obj_list;
	v.u.ob = ob;
	destruct_object(&v);
	if (ob == obj_list)
	    break;
    }
    remove_destructed_objects();
}

#if 0
/*
 * For debugging purposes.
 */
void check_ob_ref(ob, from)
    struct object *ob;
    char *from;
{
    struct object *o;
    int i;

    for (o = obj_list, i=0; o; o = o->next_all) {
	if (o->inherit == ob)
	    i++;
    }
    if (i+1 > ob->ref) {
	fatal("FATAL too many references to inherited object %s (%d) from %s.\n",
	      ob->name, ob->ref, from);
	if (current_object)
	    fprintf(stderr, "current_object: %s\n", current_object->name);
	for (o = obj_list; o; o = o->next_all) {
	    if (o->inherit != ob)
		continue;
	    fprintf(stderr, "  %s\n", ob->name);
	}
    }
}
#endif /* 0 */

static struct object *hashed_living[LIVING_HASH_SIZE];

static int num_living_names, num_searches = 1, search_length = 1;

static int hash_living_name(str)
    char *str;
{
#if 1
    return hashstr(str, 100, LIVING_HASH_SIZE);
#else
    unsigned ret = 0;

    while(*str)
	ret = ret * 2 + *str++;
    return ret % LIVING_HASH_SIZE;
#endif
}

struct object *find_living_object(str, player)
    char *str;
    int player;
{
    struct object **obp, *tmp;
    struct object **hl;

    num_searches++;
    hl = &hashed_living[hash_living_name(str)];
    for (obp = hl; *obp; obp = &(*obp)->next_hashed_living) {
	search_length++;
	if (player && !((*obp)->flags & O_ONCE_INTERACTIVE))
	    continue;
	if (!((*obp)->flags & O_ENABLE_COMMANDS))
	    continue;
	if (strcmp((*obp)->living_name, str) == 0)
	    break;
    }
    if (*obp == 0)
	return 0;
    /* Move the found ob first. */
    if (obp == hl)
	return *obp;
    tmp = *obp;
    *obp = tmp->next_hashed_living;
    tmp->next_hashed_living = *hl;
    *hl = tmp;
    return tmp;
}

void set_living_name(ob, str)
    struct object *ob;
    char *str;
{
    struct object **hl;

    if (ob->flags & O_DESTRUCTED)
	return;
    if (ob->living_name) {
	remove_living_name(ob);
    }
    num_living_names++;
    hl = &hashed_living[hash_living_name(str)];
    ob->next_hashed_living = *hl;
    *hl = ob;
    ob->living_name = make_shared_string(str);
#if 0	/* This is not a pretty way to find out if it is a wizard */
    if (ob->interactive) {
	struct svalue *v;
	
	v = apply("query_level", ob, 0);
	if (v && v->type == T_NUMBER && v->u.number >= 21)
	    ob->flags |= O_IS_WIZARD;
    }
#endif
    return;
}

void remove_living_name(ob)
    struct object *ob;
{
    struct object **hl;

#ifdef MUDWHO
    sendmudwhologout(ob);
#endif
    num_living_names--;
    if (!ob->living_name)
	fatal("remove_living_name: no living name set.\n");
    hl = &hashed_living[hash_living_name(ob->living_name)];
    while(*hl) {
	if (*hl == ob)
	    break;
	hl = &(*hl)->next_hashed_living;
    }
    if (*hl == 0)
	fatal("remove_living_name: Object named %s no in hash list.\n",
	      ob->living_name);
    *hl = ob->next_hashed_living;
    free_string(ob->living_name);
    ob->next_hashed_living = 0;
    ob->living_name = 0;
}

void stat_living_objects() {
    add_message("Hash table of living objects:\n");
    add_message("-----------------------------\n");
    add_message("%d living named objects, average search length: %4.2f\n",
		num_living_names, (double)search_length / num_searches);
}

void reference_prog (progp, from)
    struct program *progp;
    char *from;
{
    progp->ref++;
    if (d_flag)
	printf("reference_prog: %s ref %d (%s)\n",
	    progp->name, progp->ref, from);
}

/*
 * Decrement reference count for a program. If it is 0, then free the prgram.
 * The flag free_sub_strings tells if the propgram plus all used strings
 * should be freed. They normally are, except when objects are swapped,
 * as we want to be able to read the program in again from the swap area.
 * That means that strings are not swapped.
 */
void free_prog(progp, free_sub_strings)
    struct program *progp;
    int free_sub_strings;
{
    progp->ref--;
    if (progp->ref > 0)
	return;
    if (d_flag)
	printf("free_prog: %s\n", progp->name);
    if (progp->ref < 0)
	fatal("Negative ref count for prog ref.\n");
    total_prog_block_size -= progp->total_size;
    total_num_prog_blocks -= 1;
    if (free_sub_strings) {
	int i;

	/* Free all function names. */
	for (i=0; i < progp->num_functions; i++)
	    if (progp->functions[i].name)
		free_string(progp->functions[i].name);
	/* Free all strings */
	for (i=0; i < progp->num_strings; i++)
	    free_string(progp->strings[i]);
	/* Free all variable names */
	for (i=0; i < progp->num_variables; i++)
	    free_string(progp->variable_names[i].name);
	/* Free all inherited objects */
	for (i=0; i < progp->num_inherited; i++)
	    free_prog(progp->inherit[i].prog, 1);
	free(progp->name);
    }
    free((char *)progp);
}

void reset_object(ob, arg)
    struct object *ob;
    int arg;
{
    extern int current_time;

    /* Be sure to update time first ! */
    ob->next_reset = current_time + TIME_TO_RESET/2 +
	random_number(TIME_TO_RESET/2);
#ifdef COMPAT_MODE
    push_number(arg);
    (void)apply("reset", ob, 1);
#else
    if (arg == 0) {
	apply("__INIT", ob, 0);
	apply("create", ob, 0);
    } else {
	apply("reset", ob, 0);
    }
#endif    
    ob->flags |= O_RESET_STATE;
}

/*
 * If there is a shadow for this object, then the message should be
 * sent to it. But only if catch_tell() is defined. Beware that one of the
 * shadows may be the originator of the message, which means that we must
 * not send the message to that shadow, or any shadows in the linked list
 * before that shadow.
 */
int shadow_catch_message(ob, str)
    struct object *ob;
    char *str;
{
    if (!ob->shadowed)
	return 0;
    while(ob->shadowed != 0 && ob->shadowed != current_object)
	ob = ob->shadowed;
    while(ob->shadowing) {
	if (function_exists("catch_tell", ob))
	{
	    push_constant_string(str);
	    if (apply("catch_tell", ob, 1)) /* this will work, since we know the */
		/* function is defined */
		return 1;
	}
	ob = ob->shadowing;
    }
    return 0;
}
