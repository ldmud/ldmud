/*
 *  Yet another dinosaur by foslay 
 *  You can ride on this one.
 */

#include <escoryn.h> // added Escoryn defines, Chameloid, 21-Nov-98

#include "/players/foslay/inc.h"

#define DINO "/players/foslay/obj/dino"
#define DINOPIC ES_PICDIR
#define DINO_ROOM "/players/foslay/room/dino"

#define HEAD 0
#define BACK 1
#define TAIL 2
#define BACK2HEAD 3
#define TAIL2BACK 4
#define TAIL_BUSY 5

#include <wizlevels.h>
#include <prop/ladder.h>

inherit "/obj/monster";
inherit "/basic/orientation";

mixed *dino = ({ 0,0,0,0,0,0 }); /* shared between all 3 dino parts */
int part = HEAD;

string dino_pic;

query_dino() { return dino; }

reset (arg)
{
  ::reset (arg);

  if (!clonep()) {
    if (!dino[HEAD]) {
      clone_object(DINO);
    }
    return;
  }
  if (arg) return;

  set_name ("dino");
  set_race ("dinosaur");
  set_level (20);

  set_property ( P_LADDER_EXIT, ({ DINO_ROOM, 0, ({ "dino", "dinosaur" }), DINO_ROOM }) );

  dino = DINO -> query_dino();

  if (!dino[HEAD])
    create_dino ();
}

init()
{
  if (part != TAIL)
    return;

  add_action ("climb_dino","climb");
}

extra_long () { return dino_pic; }

climb_dino (str)
{
  if (!id(str)) return 0;

  if (this_player () -> query_level () < WL_WIZARD)
  {
    notify_fail ("Sorry, only wizards can climb on ths dinosaur.\n");
    return 0;
  }
  this_player()->move_player("up",DINO_ROOM);
  return 1;
}

#if 1
gimme_exit (object room, int flag)
{
  mixed *exits;
  int i;

  if ((exits = room -> query_exit ()) == 0)
    exits = ({ ({ "down" }), ({ "/dev/null" }) });
  i = random (sizeof (exits[0]));
  if (flag)
    write ("Exit to the " + exits[0][i] + " leads to " + exits[1][i] + "\n");
  return ({ exits[0][i], exits[1][i] });
}
#endif

reset_dino ()
{   
  move_dino ("up", ES_ROADDIR"bushes");
  move_dino ();
  move_dino ();
}
    
notify_destruct () /* can't leave a half dino lying around */
{
  object d;
  int t;

  tell_room (environment (), "The dinosaur disappears in a puff of smoke.\n");

  if (dino[HEAD] || dino[BACK] || dino[TAIL])
  {
    dino[part] = 0;
    for (t=0; t<3; t++)
      if (dino[t])
        destruct (dino[t]);

    if (d = find_object ( DINO ))
      destruct (d);
  }
}

create_dino ()
{
#if 0
  call_out ("move_dino_callout", random (20) + 20);
#endif    

  dino[HEAD] = this_object ();
  dino[BACK] = clone_object (DINO);
  dino[TAIL] = clone_object (DINO);

  configure_dino (HEAD);
  dino[BACK] -> configure_dino (BACK);
  dino[TAIL] -> configure_dino (TAIL);

  reset_dino ();
}

configure_dino (p) { part = p; if (p == BACK) dino_pic = read_file (DINOPIC); }

exit_of (object room, string dir)
{
  mixed *exits, dest;
  int i;

  if ((exits = room -> query_dest_dir ()) != 0)
  {
    i = random (sizeof (exits) / 2) * 2;
    dest = exits[i];
    dir = exits[i+1];  
  }
  else if ((exits = room -> query_exit ()) != 0)
  {
    i = random (sizeof (exits[0]));
    dir = exits[0][i];
    dest = exits[1][i];
  }
  else
  {
    dir = "down";
    dest = "/dev/null";
  }
  return objectp (dest) ? file_name (dest) : dest;
}

move_dino_callout ()
{
  move_dino ();
  call_out ("move_dino_callout", random (20) + 20);
}

dino_goto (string dir, int flag)
{
  mixed *exits;
  int t;

  if ((exits = environment (this_object ()) -> query_exit ()) == 0)
  {
    if (flag)
      write ("Function not supported.\n");
    return 0;
  }
  for (t = 0; t < sizeof (exits[0]); t++)
  {
    if (exits[0][t] == dir)
    {
      if (flag)
	write ("Sending dino to " + exits[1][t] + ".\n");
      move_dino (dir, exits[1][t]);
      return 1;
    }
    else if (flag)
      write ("Direction " + exits[0][t] + " != " + dir + ".\n");
  }
  if (flag)
    write ("No such direction.\n");
  return 0;
}

move_dino (dir, dest)
{
  int mode;

  dest = stringp (dest) ? load_object(dest) : dest;

  if (!dir) {
    dest = exit_of (environment (dino[HEAD]), &dir );
  }
  if (environment(dino[BACK]) == dest)
    move_tight_curve(dir, dest);
  else if (environment(dino[TAIL]) == dest)
    move_curve(dir, dest);
  else
    move_normal(dir, dest);
}

move_tight_curve (string dir, mixed dest)
{
  tell_room (environment (dino[TAIL]),
	"The tail of the dinosaur disappears to the " + dino[TAIL2BACK] + ".\n");

  dino[TAIL] -> move_player(0, environment(dino[BACK]));
  
  tell_room (environment (dino[BACK]),
	"The dinosaur turns around and looks at its tail.\n");

  dino[TAIL2BACK] = dino[BACK2HEAD];
  dino[TAIL] -> set_dino_desc(2);

  dino[BACK] -> move_player(0, environment(dino[HEAD]));
  
  tell_room (environment (dino[HEAD]), "The dinosaur turns around.\n");

  dino[BACK2HEAD] = dir;
  dino[BACK] -> set_dino_desc(2);

  dino[HEAD] -> move_player(0, dest);

  dino[HEAD] -> set_dino_desc(2);
}

move_curve (string dir, mixed dest)
{
  tell_room (environment (dino[TAIL]),
	"The head of the dinosaur disappears from the "
	+ orientation_reverse (dir) + " just in time to see\nits tail vanish to the"
	+ dino[TAIL2BACK] + ".\n");

  dino[TAIL] -> move_player(0, environment(dino[BACK]));
  
  tell_room (environment (dino[BACK]), "The dinosaur leaves "
	+ dino[BACK2HEAD] + ", dragging its huge tail behind it.\n");

  dino[TAIL2BACK] = dino[BACK2HEAD];
  dino[TAIL] -> set_dino_desc(1);

  dino[BACK] -> move_player(0, environment(dino[HEAD]));
  
  tell_room (environment (dino[HEAD]), "The dinosaur turns " + dir + ".\n");

  dino[BACK2HEAD] = dir;
  dino[BACK] -> set_dino_desc(1);

  dino[HEAD] -> move_player(0, dest);

  dino[HEAD] -> set_dino_desc(1);
}

move_normal (string dir, mixed dest)
{
  if (environment (dino[TAIL])) {
    tell_room (environment (dino[TAIL]),
	"The tail of the dinosaur disappears to the " + dino[TAIL2BACK] + ".\n");
  }

  if (environment (dino[BACK]) ) {
    dino[TAIL] -> move_player(0, environment(dino[BACK]));
  
    tell_room (environment (dino[BACK]), "The dinosaur leaves "
	+ dino[BACK2HEAD] + ", dragging its huge tail behind it.\n");

    dino[TAIL2BACK] = dino[BACK2HEAD];
    dino[TAIL] -> set_dino_desc(0);
  }

  if (environment (dino[HEAD]) ) {
    dino[BACK] -> move_player(0, environment(dino[HEAD]));
  
    tell_room (environment (dino[HEAD]), "The dinosaur turns " + dir + ".\n");

    dino[BACK2HEAD] = dir;
    dino[BACK] -> set_dino_desc(0);
  }

  dino[HEAD] -> move_player(0, dest);

  tell_room (environment (dino[HEAD]), "A dinosaur arrives from the "
	+ orientation_reverse (dir) + ".\n");

  dino[HEAD] -> set_dino_desc(0);
}

set_dino_desc (int mode)
{
  switch (part) {
  case HEAD:
    set_alias ("head");
    set_alt_name ("body");
    set_short("A huge dinosaur");
    set_long("\
The huge dinosaur eyes you suspiciously. The giant head looms above you and its\n\
enormous body disappears in the mist to the " + 
orientation_reverse (dino[BACK2HEAD]) + ".\n"
    );
    break;

  case BACK:
    set_alias ("legs");
    set_alt_name ("back");
#if 1
    set_short("Back of a huge dinosaur");
#else
    set_short(dino_pic); /* extra_long() now */
#endif
    set_long("\
The huge hind legs of the dinosaur are mud-caked and have left deep footprints\n\
which slowly fill with mud. To the " + dino[BACK2HEAD] + " you see the giant head\n\
of the dinosaur, its tail lies to the " + orientation_reverse(dino[TAIL2BACK])
 + ".\n");
    break;

  case TAIL:
    set_alias ("tail");
    set_alt_name ("rocks");
    if (mode == 2) {
      set_short("The tail of the dinosaur");
      set_long("\
What looks like a chain of pointy rocks at first sight turns out to be the tail\n\
of the huge dinosaur.\n"
      );
    }
    else {
      set_short("The tail of the dinosaur to the " + dino[TAIL2BACK]);
      set_long("\
What looks like a chain of pointy rocks at first sight turns out to be the tail\n\
of the huge dinosaur to the " + dino[TAIL2BACK] + ".\n"
      );
    }
    break;
  }  
}

private object victim;

attack () {}

hit_player (int hp)
{
  if (!dino[TAIL_BUSY]) {
    dino[TAIL_BUSY] = 1;
    victim = previous_object ();
    call_out ("dino_strikes_back", 2, 1);
  }
  return 1;
}

dino_strikes_back (action)
{
  if (environment (this_player ()) != environment (this_object ())
      && environment (this_player ()) != environment (dino[HEAD])
      && environment (this_player ()) != environment (dino[BACK])
      && environment (this_player ()) != environment (dino[TAIL]))
  {
    dino[TAIL_BUSY] = 0;
    return;
  }

  switch (action) {
  case 1:
    call_out ("dino_strikes_back", 4, ++action);
    write ("The dinosaur glances sharply at you.\n");
    say ("The dinosaur glances sharply at "
	 + this_player () -> query_name () + ".\n", this_player ());
    break;
  case 2:
    call_out ("dino_strikes_back", 4, ++action);
    write ("The dinosaur slowly turns its huge head towards you.\n");
    say ("The dinosaur slowly turns its huge head towards "
	 + this_player () -> query_name () + ".\n", this_player ());
    break;
  case 3:
    call_out ("dino_strikes_back", 4, ++action);
    write ("The dinosaur growls.\n");
    say ("The dinosaur growls.\n", this_player ());
    break;
  case 4:
    call_out ("flying_lesson", 4, 0);
    call_out ("flying_lesson", 8, 1);
    call_out ("player_falls_down", 12);

    write ("The dinosaur deals a mighty blow with its tail.\n");
    say ("The dinosaur deals a blow with its tail to "
	 + this_player () -> query_name () + " and hurls "
         + this_player () -> query_objective () + " up into the air.\n",
	 this_player ());
    this_player () -> move_living (0, "/players/foslay/room/sky");
    write ("You are airborne.\n");
    tell_room (environment (this_player ()),
	       this_player () -> query_name () + " flies past.\n",
	       ({ this_player () }));
    break;
  }
}

player_falls_down ()
{
  string name;

  dino[TAIL_BUSY] = 0;

  name = this_player () -> query_name ();

  log_file ("foslay.dino", (name ? name : file_name (victim))
			   + " had a flying lesson.\n");

  tell_room (environment (this_player ()),
             this_player () -> query_name ()
	     + " drops like a stone.\n", ({ this_player () }) );

  this_player () -> move_living (0, "/room/church");

  say ("SPLUT! " + this_player () -> query_name ()
       + " drops out of the sky and into the mud.\n", this_player ());
  write ("SPLUT! You dive headlong into the mud!\n");
}

#define ESCORYN_DIRS ({ ES_TOWNDIR, ES_ROADDIR })

#define VILLAGE_ROOMS ({ \
"room/church",     "room/hump",       "room/vill_green", \
"room/vill_track", "room/vill_road1", "room/vill_road2", \
"room/vill_shore", "room/vill_jetty", "players/foslay/workroom", \
ES_BAYDIR"lighthouse", ES_ROADDIR"road1", ES_ROADDIR"road2", \
ES_ROADDIR"road3" })

flying_lesson (int flag)
{
  object *players;
  string room, path,
	 escoryn_msg, village_msg, bog_msg;
  int t;

  players = users ();

  if (flag == 0) {
    bog_msg = "You see " + this_player () -> query_name ()
    + " flying high above you.\n";

    escoryn_msg = "You see " + this_player () -> query_name ()
    + " flying high above the Great Bog, the swamp to the east.\n";

    village_msg = "You see " + this_player () -> query_name ()
    + " flying high above the Great Bog, the swamp to the west.\n";
  }
  else {
    bog_msg = escoryn_msg = village_msg = this_player () -> query_name ()
    + " has reached the highest point and is falling down again.\n";
  }

  for (t=0; t < sizeof (players); t++)
  {
    if (players [t] == this_player ())
      continue;

    if (!environment (players [t]))
      continue;

    room = file_name (environment (players [t]));
    path = implode (explode (room, "/") [0..<2], "/");

    if (path == "BOGDIR")
      tell_object (players[t], bog_msg);
    else if (member_array (room, VILLAGE_ROOMS) >= 0)
      tell_object (players[t], village_msg);
    else if (member_array (path, ESCORYN_DIRS) >= 0)
      tell_object (players[t], escoryn_msg);
  }
}
