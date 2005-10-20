/*
 * Object/File security For lpmud  by mmcg@bruce.cs.monash.edu.au -
 * with help and abuse geoff@bruce.cs.monash.edu.
 *		 (from the Shattered World Lpmud)
 *
 * SECURITY: File security functions.
 * These should generally be called from interpret.c and simulate.c,
 * as the game driver shouldn't have to keep fiddling with
 * current_object.  To check whether a *player* can write a particular
 * file (and resolve its path), call 'valid_read' etc in the player object,
 * or use check_file_name(file, 0/1) - which checks that the object
 * calling the function is interactive, and calls the object's valid_read.
 * From LPC, objects that can write files for players (i.e. wiztoys, etc)
 * should always suid themselves to level 0 and let the wizard trust them;
 * there is thus no need for them to call the player's valid_read (their
 * commands should only be accessible to the player by typing commands,
 * all routines static - cf the wiz_soul stuff).
 */

/* 
 * General philosophy:  We are really only concerned with who can
 * read or write what; we don't really care who *uses* what (with clone,
 * call_other, etc, it would get too restrictive and expensive).  So,
 * what we do is assign everything a level based on where it came from.
 * Things that come from player directories or standard dirs get CREATE
 * and EDIT_STANDARD respectively.  Things used by the game driver - from
 * /secure and /closed, get no power at all - they are made suid by the
 * game driver itself.  This means that only if player.c is cloned from
 * comm1 will it be given god status; player.c can go on to clone a soul,
 * and pass on the appropriate level.  And writing the soul etc will be
 * easier because we dont have to check for spoofing all the time; such
 * attempts (to clone it) will result in an object with no power.
 * Stuff in /closed still needs to be difficult to modify by wizards (because
 * there are functions that depend on wiz_level but not object level - like
 * snoop) but any instance of a soul will never be given a higher level than
 * the cloner, so the file security problem is much simpler.
 */

/*
 * Functions:
 *    o_read_ok(ob, file) - is it ok for ob to read file?
 *    o_save_ok(ob, file) - is it ok to save a savefile here?
 *    o_write_ok(ob, file) - is it ok to do arbitrary writes here?
 *    o_append_ok(ob, file) - ok to append? (only used in loggers)
 *
 * Generally, Write permission requires one level higher than save
 * permission, and all objects not in closed directories get only
 * save permission.
 */

#include <stdio.h>         /* Kickaha */
#include <sys/types.h>
#include <sys/stat.h>
#include "secure/security.h"
#include "lint.h"
#include "interpret.h"
#include "object.h"
#include "config.h"
#include "sent.h"

#define 	NOONE (-1)	/* noone can access this */
#define 	NOTHING (0)	/* level for no wiz powers */

int test_domain(), p_read_ok(), p_write_ok(), p_append_ok(), set_file_su();
char *strcat(), *strncpy();
struct svalue *apply();
int fscanf();

/*
 * Access lists for files and directories.
 */

struct access { char * dir;
	 int read, write, append;
	 }
/* List of levels required to read, write and append files in each dir.
 * %p is replaced by playername.  %n is no slash. %s is anything.
 * other characters match precisely.  Creation and reading of directories
 * follows the same rules as the file access lists; you should append a '/' to
 * the directory name when checking a 'directory' read or write (see
 * secure/wiz_soul for examples...)
 */
FileAccessList[] = {
	{ "/",			APPRENTICE, GOD, GOD },	/* mud root dir */
	{ "",			APPRENTICE, GOD, GOD },	/* mud root dir */
	{ "/%s",		NOONE, NOONE, NOONE },	/* NOPE! */
	{ "secure%s",		70, GOD, GOD },	/* game driver */
	{ "newmud%s", 		NOONE, NOONE, NOONE },  /* us!! */
	{ "RO%s",		APPRENTICE, NOONE, NOONE }, /* read only */
	{ "closed%s",		GOD, GOD, GOD },	/* souls, etc */
	{ "guilds%s",		APPRENTICE, GOD, GOD },
	{ "players/%p/%s",	APPRENTICE, CREATE, CREATE },
	{ "players/%d/%s",      APPRENTICE, CREATE, CREATE },
	{ "players/%p",		APPRENTICE, GOD, GOD },	 /* my savefile */
	{ "players/%p.o",	APPRENTICE, GOD, GOD },	 /* my savefile */
	{ "players/%p.%n",	APPRENTICE, GOD, GOD },	 /* my savefile */
	{ "players/%p/closed%s",	/* my closed stuff (>cre) */
				LOCAL_ED, LOCAL_ED, LOCAL_ED },
	{ "players/%n/closed%s", GOD, GOD, GOD },	/* player only */
	{ "players/%n/open%s",	APPRENTICE, ED_OTHERS, ED_OTHERS },
	{ "players/%n/toys%s",	APPRENTICE, ED_OTHERS, ED_OTHERS },
	{ "players/%n/%s",	APPRENTICE, ED_OTHERS, ED_OTHERS}, /* others */
	{ "%Gplayers/%s",	CREATE, GOD, GOD }, /* savefiles */
	{ "players/%s",		EDIT_STANDARD, GOD, GOD }, /* savefiles */
	{ "log/%p/%s",  		APPRENTICE, CREATE, CREATE },  /* your log */
	{ "log/%d/%s",             APPRENTICE, CREATE, CREATE },  /* your log */
	{ "log/%p.%n/%s", 		APPRENTICE, CREATE, CREATE },  /* your log */
	{ "log/%d.%n/%s",          APPRENTICE, CREATE, CREATE },  /* your log */
	{ "log/.%p/%s",		APPRENTICE, CREATE, CREATE },  /* your log dir */
	{ "log/%s",   		APPRENTICE, ED_LOG, ED_LOG },  /* all logs */
	{ "obj/newsv2/save/groups/council.intern/%s",
				GOD, GOD, GOD },
	{ "obj/newsv2/save/groups/arch.info/%s",
				700, 700, 700 },
	{ "%Gobj/%s",		APPRENTICE, CREATE, CREATE },
	{ "obj/%s",		APPRENTICE, 100, 100 },
	{ "%Groom/post_dir/%s",	CREATE, CREATE, CREATE },
	{ "room/post_dir/%s",	EDIT_STANDARD, EDIT_STANDARD, EDIT_STANDARD },
	{ "%Groom/%s",		APPRENTICE, CREATE, CREATE },
	{ "room/%s",		APPRENTICE, 100, 100 },
	{ "%Gguilds/%s",	APPRENTICE, CREATE, CREATE },
	{ "guilds/%s",		APPRENTICE, EDIT_STANDARD, EDIT_STANDARD },
	{ "%Gstd/%s",		APPRENTICE, CREATE, CREATE },
	{ "std/%s",		APPRENTICE, EDIT_STANDARD, EDIT_STANDARD },
	{ "include/%s",		APPRENTICE, EDIT_STANDARD, EDIT_STANDARD },
	{ "open/%s",		APPRENTICE, CREATE, CREATE },
	{ "ftp/%s",		APPRENTICE, CREATE, CREATE },
	{ "%Gbanish/%s",	APPRENTICE, CREATE, CREATE },
	{ "banish/%s",		APPRENTICE, EDIT_STANDARD, EDIT_STANDARD },
	{ "doc/quests/%s",      40,40,40 },
	{ "doc/%s",		APPRENTICE, EDIT_STANDARD, EDIT_STANDARD },
	{ "%n",			APPRENTICE, GOD, GOD },	/* files in root dir */
	{ "%s",			GOD, GOD, GOD },	/* all other dirs */
	{ 0,			NOONE, NOONE, NOONE }
};

/*
 * Level to (initially) give any object.  The object itself is responsible
 * for its own security; in general, only player.c need worry, as it is
 * the only god-level object (as well as the game driver).
 */
struct ocap { char * dir; int level; }
caplist[] = {
	{ "secure%s",		NOTHING }, /* assigned by driver */
	{ "RO%s",		NOTHING }, /* assigned by driver */
	{ "closed%s",		CREATE }, /* passed on by player */
	{ "guilds%s",		EDIT_STANDARD },
	{ "players/%n/toys%s",	NOTHING }, /* other players must trust to use */
	{ "players/%n/closed%s", LOCAL_ED }, /* was LOCAL_ED, Kickaha */
	{ "players/%n/%s",	CREATE },
	{ "obj/news/%s",	10001 },
	{ "obj/newsv2/%s",	10001 },
	{ "obj/%s",		EDIT_STANDARD },
	{ "room/%s",		EDIT_STANDARD },
	{ "guilds/%s",		CREATE },
	{ "std/%s",		CREATE },
	{ "include/%s",		NOTHING },
	{ "open/%s",		EXPLORE }, /* all other objs - ie /open - */
	{ "%s",			NOTHING }, /* must be nilpotent. Player can */
	{ 0,			NOTHING }  /* trust them if he needs them. */
	};

/*
 * test whether a filename has anything nasty in it - . or .. between
 * slashes, or 2 consecutive slashes.  Will allow a slash at the end of
 * the name, but not the start. '/' is an invalid name.
 */

static char * badstr[] = { "/./" , "/../", "//", 0 };

char *my_index();
/*char *index();*/

int strcmp(), strncmp(), strlen();

int valid_filename(str)
char * str;
{
    char ** bs;
    char * s = str;

    if (!strcmp(str, "/"))
	return 1;	/* root dir */

    for (bs = badstr; *bs; bs++)
	if (!strncmp(str, 1+ *bs, strlen(1+*bs)))
	    return 0;	/* check for leading nasties - without initial '/' */

    while (s && *s) {
	s = my_index(s, '/');
	if (!s) break;

        for (bs = &badstr[0]; *bs; bs++)
            if (!strncmp(*bs, s, strlen(*bs)))
	        return 0;
	s++;
	}

/* check for trailers - allow a single '/' */
    if (str[strlen(str) - 1] == '/')
	return 1;	/* would have matched if preceded by ugliness */

    s = str + strlen(str);
    for (bs = badstr; *bs; bs++)
	 if (!strncmp(s - strlen(*bs) + 1, *bs, strlen(*bs) - 1))
	     return 0;

    return 1;
}

/*
 * test whether a given object filename matches a %s string.
 * pname is the invoking player (or object owner).
 */

static int test_match(str, oname, pname)
char * str, * oname, *pname;
{
    char * s, *o, *t;
    s = str;
    o = oname;

    while (1) {
	if ((!*s) && (!*o))
	    return 1;
	if (*s == '%') {
	    s++;
	    switch(*s) {
		case 'G': if (pname) return 0;	/* Game driver object */
			break;
		case 'c': if (!*o) return 0;
			o++; break;	/* matches a single character */
		case 's': return 1;	/* matches anything */
		case 'p': if (!pname) return 0;
			  if (strncmp(o, pname, strlen(pname)))
				return 0;
			  o += strlen(pname);
			  break;
		case 'n': if (!strlen(o) || o[0] == '/') return 0;
			t = my_index(o, '/');
			if (t) o = t;
			else o += strlen(o);
			break;
                case 'd': if (!strlen(o) || o[0] == '/') return 0;
                          t=my_index(o,'/');
			  if (!t) t=my_index(o,'.');
                          if (t)
                          {
                            if (test_domain(pname,o,t-o)) o=t;
                            else return 0;
                          }
                          else
                          {
                            if (test_domain(pname,o,strlen(o))) o+=strlen(o);
                            else return 0;
                          }
                          break;
		default:
			fatal("Bad specifier in security string\n");
			exit(1);
		}
	    }
	else {
	    if (*s != *o)
		return 0;
	    o++;
	    }
	s++;
	}
}

struct access us;

static struct access * find_acc_entry(file, player, list)
struct access * list;
char * file;
char * player;
{
    while (list->dir) {
	if (test_match(list->dir, file, player)) {
/*		printf("Match!(wow) %s %s %s\n", list->dir, file, player); */
            if (strncmp("newmud", file, 6) == 0) break;
	    return list;
	    }
	list++;
	}
    if (strncmp(file, "newmud", 6)==0)
    {
      if (!player || (player && ((strcmp(player, "kickaha")==0) ||
	  (strcmp(player, "merlin")==0)  ||
	  (strcmp(player, "raistlin")==0) ||
	  (strcmp(player, "beldin")==0))))
      {
	us.dir = "newmud";
	us.read = GOD;
	us.write = GOD;
	us.append = GOD;
	return &us;
      }
    }
    return 0;
}

/*
 * Check that the level of this object is high enough to read this
 * file.
 */

int o_read_ok(ob, file)
char * file;
struct object * ob;
{
    if (!ob) return 1;	/* must be the mud (current_object = 0) */
    return p_read_ok(ob->su_name, ob->level, file);
}
/*
 * an internal one for directories; LPC functions append a slash
 * and call the normal one...
 */
char namebuf[500];
int o_dir_read_ok(ob, file)
char * file;
struct object * ob;
{
    if (strlen(file) == 0 || file[strlen(file) - 1] != '/') {
	strncpy(namebuf, file, 498);
	namebuf[498] = '\0';
	strcat(namebuf, "/");
	file = namebuf;
	}
    return o_read_ok(ob, file);
}
int p_read_ok(suname, olevel, file)
char * suname;
int olevel;
char * file;
{
    struct access *l;

/* could regularise .'s & ..'s here - but why bother?? */
/*    if (file && *file == '/' && strcmp(file, "/")) file = file++; */
    if (!valid_filename(file)) return 0;
    l = find_acc_entry(file, suname, FileAccessList);
    if (!l || l->read == NOONE) return 0;
    if (l->read <= olevel) return 1;
    return 0;
}

/*
 * Write level.
 */

int o_save_ok(ob, file)
char * file;
struct object * ob;
{
    if (!ob) return 1;	/* must be the mud (current_object = 0) */
    return p_write_ok(ob->su_name, ob->level, file);
}

int o_write_ok(ob, file)
char * file;
struct object * ob;
{
    if (!ob) return 1;	/* must be the mud (current_object = 0) */
    return p_write_ok(ob->su_name, ob->level/* -1 */, file);
      /* Removed -1 -- Kickaha */
}

int p_write_ok(suname, olevel, file)
char * suname;
int olevel;
char * file;
{
    struct access *l;

/*    if (file && *file == '/' && strcmp(file, "/")) file = file++; */
    if (!valid_filename(file)) return 0;
    l = find_acc_entry(file, suname, FileAccessList);
    if (!l || l->write == NOONE) return 0;
    if (l->write <= olevel) return 1;
    return 0;
}

/*
 * Append level.
 */

int o_append_ok(ob, file)
char * file;
struct object * ob;
{
    if (!ob) return 1;	/* must be the mud (current_object = 0) */
    return p_append_ok(ob->su_name, ob->level, file);
}

int p_append_ok(suname, olevel, file)
char * suname;
int olevel;
char * file;
{
    struct access *l;

/*    if (file && *file == '/' && strcmp(file, "/")) file = file++; */
    if (!valid_filename(file)) return 0;
    l = find_acc_entry(file, suname, FileAccessList);
    if (!l || l->append == NOONE) return 0;
    if (l->append <= olevel) return 1;
    return 0;
}

/*
 * Assign level to an object.
 * If "add" is < 0, we are loading it for the first time, so
 * do an fstat to see whether it is suid.  Otherwise, add should
 * usually be 0 or 1, and is the number of levels to add to clones
 * of this object.  Caller should ensure that when add is (-1), name
 * is the name of the file as stored on disk (i.e. with a .c or .T).
 */

static int file_was_suid;

int get_object_level(o, add)
char * o;
int add;
{
    struct ocap * c = caplist;
    struct stat sbuf;

    file_was_suid = (add < 0)? 0 : add;

    while (c->dir) {
	if (test_match(c->dir, o, (char *)0)) {
	    if (add < 0)
		if ((-1 != stat(o, &sbuf)) && (sbuf.st_mode & 0100)) {
		    file_was_suid = 1;
		}
            if (test_match("players/%n/closed/%s", o, (char *)0)
	      && current_object /* && current_object->interactive */
              && current_object->level >= 700)   /* Kickaha, 13/4/1994 */
              return current_object->level + file_was_suid;
	    return c->level + file_was_suid;
	    }
	c++;
	}
    return NOTHING;
}

static char wname[100];

char * get_wname(obname)
char * obname;
{
    char *en, *x, *t;

    if (!strncmp(obname, "players/", 8)) {
	en = my_index(obname+8, '/');
	if (en) {
	    t = wname;
	    x = obname+8;
	    while (x < en)
		*(t++) = *(x++);
	    *t = 0;
	    return wname;
	    }
	}
    return 0;
}

void init_object_su(ob, origname, addon)
struct object * ob;
char * origname;
int addon;
{
    char * x;
    ob->level = get_object_level(origname, addon);
    ob->file_was_suid = file_was_suid; /* ugly */
    if (ob->su_name) free_string(ob->su_name);
    ob->su_name = 0;
    if (x = get_wname(origname))
	ob->su_name = make_shared_string(x);
}

/*
 * setuid_me:  make an object into a "trusted" object.
 *
 * Objects with GOD permission can set the target object's name to an
 * arbitrary string; otherwise, the target object's name can only be
 * set to the same as the caller's name (if it's a standard object, the
 * caller's name is usually (char *)0).  This is because even though they
 * have EDIT_STANDARD, /player/closed needs GOD perms to write, so we can't
 * have /std objects suid'ing themselves to a player name and pissing
 * around in his /closed directories.   Objects can't suid on other
 * objects of higher level (i.e. to lower them).
 */

int setuid_me(from, to, level, name)
struct object * from;
struct object * to;
int level;	/* amount of level to pass on, max is from */
char * name;
{
    if (from) { /* 0 = game driver, otherwise current object */
        if (to->level > from->level) {
            return -1; /* it'd be bad to decrease /secure/master :) */
        }
        if (from->level < level)
	    level = from->level;
        if (from->level < GOD) {
	    name = from->su_name;
	    }
	}
    to->level = level;
    if (from == to)
	return level;
    if (to->su_name) {
	free_string(to->su_name);
	to->su_name = 0;
	}
    if (name) {
	to->su_name = make_shared_string(name);
	}
    return level;
}

/*
 * A predicate to compare object levels.  Shouldn't be used for
 * anything *really* important, as it pretends that objects in a wizards
 * directory are not of a lower level than that wizard (and people with
 * EDIT_OTHERS could spoof it).  Used for query_snoop, etc.
 */

int lower_level(o1, o2)
struct object * o1, *o2;
{
    if (!o1 || !o2) return 1;
    if (o1->su_name && o2->su_name) {
	if (!strcmp(o1->su_name, o2->su_name))
	    return 0;
	}
	return (o1->level < o2->level);
}

/*
 * Given a filename, and an operation (in writeflag - 0 for read,
 * 1 for write), and a controlling object (usually a wiz soul), will
 * call "valid_read" or "valid_write" in that object, and return the
 * result.  The routine called should return 0 if the user is not allowed
 * to read/write that file, or a string (being the full pathname of the file
 * to edit) if the user can edit it.  Naturally, the caller (which at this
 * point is only ed()) will have to run o_read_ok (or whatever) over
 * the new filename, to make sure noone's trying to pull a fast one (we
 * don't do this here in case the regularised filename will be useful).
 *
 * This is used so wizards can have working directories, and so wiz_souls
 * can regularise "../", etc (the game driver simply errors), and so things
 * like newsrooms can enforce access to less files than their level indicates.
 */


char           *
check_file_name(file, writeflg, who)
    char           *file;
    int             writeflg;
    struct object * who;
{
    struct svalue    v, *ret;

    if (current_object && !O_GET_INTERACTIVE(current_object)) {
	error("check_file_name called from non-interactive object %s\n",
	      current_object->name);
    } /* hmm - how come this check is never exercised? */
    if (command_giver && !O_GET_INTERACTIVE(command_giver))
      error("check_file-name called from non-interactive object %s\n",
	      command_giver->name);
    v.type = T_STRING;
    v.u.string = file;
    /*
     * We don't have to free the string in ret. This is done by the garbage
     * collection.
     */
    push_constant_string(file);   /* Kickaha */
    if (writeflg)
	ret = apply("valid_write", who, 1);   /* Kickaha */
    else
	ret = apply("valid_read", who, 1);    /* Kickaha */
    if (!ret || ret->type != T_STRING) {
	add_message("Bad file name.\n");
	return 0;
    }
    if (!legal_path(ret->u.string)) {
	add_message("Illegal path: %s\n", ret->u.string);
	return 0;
    }
    return ret->u.string;
}

/*
 * Some stuff to manage the setuid bits on files.  You can only suid
 * a file if you own the file and you have write permission on it (or if
 * you are a god) - checks are in interpret.c.
 */

int force_no_su(fname)
char * fname;
{
    return set_file_su(fname, 0);
}

int set_file_su(fname, val)
char * fname;
int val;
{
    if (val)
	val = 0774;
    else
	val = 0664;
    if (-1 == chmod(fname, val))
	return 0;
    else
	return 1;
}

/* n strlen of domain-name */

int test_domain(w,d,n)
char *w,*d;
int n;
{
  FILE *fil,*fopen();
  char t1[100],t2[100];
  if (w==(char *)0) return 0;
  if (d==(char *)0) return 0;
  fil=fopen(DOMAINS_FILE,"r");
  if (fil==NULL) return 0;
  while (fscanf(fil,"%s : %s\n",t1,t2)!=0 && (strcmp(t1,"slut")!=0))
  {
    if (strcmp(t1,w)==0 && strncmp(t2,d,n)==0)
    {
      fclose(fil);
      return 1;
    }
  }
  fclose(fil);
  return 0;
}

char *my_index(str, c)
char *str;
char c;
{
  char *i;
  int len = strlen(str);

  for (i=str; i<str+len; i++)
    if (*i==c) return i;

  return NULL;
}


