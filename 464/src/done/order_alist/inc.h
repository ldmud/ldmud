/* Foslay's std. include file */

#ifndef _foslay_std_
#define _foslay_std_

#include <wizlevels.h>
#define HOME "players/foslay"
#define LIB "players/foslay/lib/lpc"
#define NAME "foslay"
#define CAPNAME "Foslay"

/*** Configuration ***/

/* Mudlib: ML2, ML3 */
#define ML2
/* Gamedriver: #ifdef LPC3 */
/* Location: */
/* #define MESSYMUD */
#define AMIGA
/* #define TUBMUD */

#ifdef LPC3
#ifndef LIBRARY
inherit LIB;
#endif
#endif

#ifdef MESSYMUD
#define filter_array  filter_objects
break_string(x,y) { return x; }
#endif

/*** Macros ***/

#define TIP		this_player()
#define THIS		this_object()
#define PREV		previous_object()
#define PNAME(x)	capitalize((string) (x)->query_name())
#define RNAME(x)	((string) (x)->query_vis_name())

#define FIRST(x)	first_inventory(x)
#define NEXT(x)		next_inventory(x)
#define ENV(x)		environment(x)
#define INV(x)		all_inventory(x)
#define TELLME(x)	if(find_player(NAME)) tell_object(find_player(NAME),(x));
#define CAP(x)		capitalize(x)
#define OK		write("Ok.\n"); return 1

#define he(x)	(x) -> query_pronoun ()
#define his(x)	(x) -> query_possessive ()
#define him(x)	(x) -> query_objective ()

#define HIS	his (this_player ())
#define HIM	him (this_player ())
#define HE	he (this_player ())

#define FOREACH(arr,i)	for(i=0;i<sizeof(arr);i++)

#define LEVEL(x)	(int) call_other(x,"query_level")
#define HP(x)		(int) call_other(x,"query_hp")
#define SP(x)		(int) call_other(x,"query_spell_points")
#define CLONE(x)	clone_object(x)
#define TELL(x,y)	{ if(x) tell_object(x,y); }
#define MOVE(x,y)	move_object(x,y)

#define IF_INV	if(ENV(THIS) != TIP) return 0;
#define IF_ID	if(!id(str)) return 0;
#define IF_WIZ	if(LEVEL(TIP)<WL_APPRENTICE) return 0;
#define IF_ME	if(RNAME(TIP) != CAPNAME) return 0;
#define IF_LOC	if(PREV && PREV != THIS) return 0;
#define IF_STR	if(!str) return 0;
#define IF_CLONE if(sizeof(explode(file_name(THIS),"#")) == 1) return 0;

/* compat */

#define sure_find_object(x) load_object(x)

#endif /* _foslay_std_ */
