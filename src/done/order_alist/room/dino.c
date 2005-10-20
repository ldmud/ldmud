/*
   On the dinosaur, foslay
*/

#include "/players/foslay/inc.h"

#include <prop/room.h>

#define DINO "/players/foslay/obj/dino"

#define HEAD 0
#define BACK 1
#define TAIL 2
#define BACK2HEAD 3
#define TAIL2BACK 4
#define TAIL_BUSY 5

#include <wizlevels.h>
#include <prop/ladder.h>

inherit "/complex/room";

int players_allowed = 0;
object holding_reins;
object ladder;

configure ()
{
  set_property ( P_OUTSIDE );

  set_short ("On the dinosaur");

  set_long ("\
You're on the back of the dinosaur. The head of the dinosaur\n\
looms up in front of you and a row of large scales leads down\n\
its back. You can look around from here.\n");

  load_object ( DINO );

  move_object (clone_object ("/players/amylaar/bin"), this_object ());
  move_object (load_object ("/players/foslay/party/book"), this_object ());
  load_object ("/players/foslay/bottle");

#if 0
  set_exits ( ({ "/room/church", "down" }) );
#endif
}

set_players_allowed (int i) { players_allowed = i; }

ladder_exit_hook (flag, item)
{
  switch (flag)
  {
    case 0: /* lean, lower */
      if (ladder && ladder != previous_object ())
      {
	write ("There is already a ladder leaning against the dinosaur.\n");
	return 0;
      }
      else {
	ladder = previous_object ();
      }
      break;
    case 1: /* climb */
      if (this_player () -> query_level () < WL_WIZARD && players_allowed == 0)
      {
        write ("With a swish of its tail the dinosaur pushes you from the ladder.\n");
        tell_room (ENV (TIP), PNAME (TIP) + " tries to climb the ladder but the dinosaur\n\pushes " +
	           HIM + " down with a swish of its tail.\n", ({ TIP }));
        return 0;
      }
      break;
  }
  return 1;
}

init ()
{
  add_action ("move_dino_cmd", "", 1);
  add_action ("look_around", "look");
  add_action ("grab_reins", "grab");
  add_action ("drop_reins", "drop");
  ::init ();
}

static list_people (object room, mixed *dino)
{
  mixed *people;

  people = list_livings (all_inventory (room), 0);
  people -= ({ dino[ HEAD ], dino[ BACK ], dino[ TAIL ] });
  if (sizeof (people))
    write ("You see " + liblpc_enumerate (visible_objects (people)) + " down there.\n");
}

look_around (str)
{
  mixed *dino;
  string long;
  object env;

  if (str != "around")
    return 0;

  dino = DINO -> query_dino ();
  if (dino[ HEAD ] && (env = ENV (dino[ HEAD ])))
  {
    write ("The dinosaur's head is at " + env -> short () + ".\n");
    list_people (env, dino);
  }
  if (dino[ BACK ] && (env = ENV (dino[ BACK ])))
  {
    write ("The dinosaur's body is at " + env -> short () + ".\n");
    list_people (env, dino);
  }
  if (dino[ TAIL ] && (env = ENV (dino[ TAIL ])))
  {
    write ("The dinosaur's tail is at " + env -> short () + ".\n");
    list_people (env, dino);
  }

  if (dino[ HEAD ] && ENV (dino[ HEAD ]))
  {
    if ((long = ENV (dino[ HEAD ]) -> query_exit_long ()) == 0)
      long = "You can't see the exits of this room from up here.\n";
    write (long);
  }
  return 1;
}

grab_reins (str)
{
  if (str != "reins")
    return 0;

  if (holding_reins && ENV (holding_reins) == THIS)
  {
    tell_object (holding_reins, RNAME (TIP) + " grabs the reins out of your hands.\n");
    tell_object (TIP, "You grab the reins out of " + RNAME (holding_reins) + "'s hands.\n");
  }
  holding_reins = TIP;

  write ("You grab the reins of the dinosaur.\n");

  return 1;
}

drop_reins (str)
{
  if (str != "reins")
    return 0;

  if (TIP != holding_reins)
  {
    notify_fail ("You aren't holding any reins.\n");
    return 0;
  }
  holding_reins = 0;
  write ("You drop the reins.\n");
  return 1;
}

move_dino_cmd (str)
{
  mixed *dino, *exits;
  string verb;
  object env;
  mixed dest;
  int t;
  int ladder_down;

  if (holding_reins != TIP || ENV (holding_reins) != THIS)
    return 0;

  dino = DINO -> query_dino ();
  verb = query_verb ();
  if (dino[ HEAD ] && (env = ENV (dino[ HEAD ])))
  {
    if ((exits = env -> query_exit ()) == 0)
    {
      notify_fail ("The dinosaur is stuck.\n");
      return 0;
    }
  }
  else
    return 0;

  for (t = 0; t < sizeof (exits[0]); t++)
  {
    if (exits[0][t] != verb)
      continue;

    dest = load_object (exits[1][t]);
    if (function_exists ("query_exit", dest) == 0)
    {
      tell_room (this_object (), "The dinosaur can't go " + exits[0][t] + " because the destination room has no query_exit ().\n");
      return 1;
    }
    if (ladder && env = ENV (ladder))
    {
      if (ENV (dino [HEAD]) == env || ENV (dino [BACK]) == env || ENV (dino [TAIL]) == env)
      {
	/* Hmm, can't verify if the ladder is still leaning against the dinosaur ... */
        ladder_down = 1;
      }
    }
    if (dino [HEAD] -> dino_goto (exits[0][t], 0))
    {
      tell_room (this_object (), "The dinosaur walks " + exits[0][t] + ".\n");
      look_around ("around");
    }
    if (ladder_down)
    {
      tell_room (ENV (ladder), "The ladder falls down as the dinosaur moves.\n");
      ladder -> place_ladder (0);
      ladder = 0;
    }
    return 1;
  }
  return 0;
}

notify_destruct ()
{
  mixed *dino;

  dino = DINO -> query_dino ();
  if (dino[ HEAD ]) destruct (dino[ HEAD ]);
  if (dino[ BACK ]) destruct (dino[ BACK ]);
  if (dino[ TAIL ]) destruct (dino[ TAIL ]);
}
