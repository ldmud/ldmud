/* death_room.c */
/* Mrpr 901119 */

inherit "room/room";

#define WRITE(x) tell_object(players[j], x)
#define SPEAK(x) tell_object(players[j], "Death says: " + x)

mixed * players;

/*
 * Function name: create_death
 * Description:   Create death.
 */
void create_death()
{

	object death;

	death = clone_object("/room/death/death");
	move_object(death, "/room/death/death_room");
}

/*
 * Function name: remove_death_obj
 * Description:   Remove the "death_mark"-object.
 */
void remove_death_obj(object player)
{
	
	object plobj;

	plobj = present("death_mark", player);

	while (plobj = present("death_mark", player))
	    destruct(plobj);
}

/*
 * Function name: do_remove
 * Description:   Removes players that's finished
 */
void do_remove()
{
	int j, nr;

	if (!players)
		return;

	nr = sizeof(players);

	for (j = 0 ; j < nr ; j += 2)
	{
		if (players[j + 1] == 70)
		{
			remove_death_obj(players[j]);
			move_object(players[j], "/room/church");
			/* Note that move_object() will call exit() */
			/* remove_player(players[j]); */
			do_remove();
			return;
		}
	}
}

/*
 * Function name: heart_beat
 * Description:   Let the actions be governed by time.
 */
void heart_beat()
{

    int align, j, nr;

    if (!players)
    {
	set_heart_beat(0);
	return;
    }

    nr = sizeof(players);

    for (j = 0 ; j < nr ; j += 2)
    {
	
	players[j + 1]++;
	
	if (players[j + 1] == 5)
	{
	
	
	    SPEAK("IT IS TIME\n");
	    WRITE("\nDeath lifts his right arm and makes beckoning motion.\n" +
		  "You feel quite certain that if you had been alive you would have died from\n" +
		  "fear on the spot. Strangely enough you don't feel anything like that at all.\n" +
		  "Just a mild curiosity.\n\n");
	
	}
	
	if (players[j + 1] == 10)
	{
	
	    SPEAK("NO GLANDS, THAT'S WHY.\n");
	    WRITE("\nDeath seems to smile a bit. On the other hand that's a bit hard to tell.\n" +
		  "It might very well be that that is his normal expression....\n\n");
	
	}
	
	if (players[j + 1] == 15)
	{
	
	    SPEAK("WITHOUT GLANDS YOU FEEL NOTHING, NOTHING AT ALL.\n");
	    WRITE("\nWell, he seems to be right. Instead of being mad with fear you're getting\n" +
		  "a little bored. You wish for something to happen real soon.\n\n");
	
	}
	
	if (players[j + 1] == 20)
	{
	
	    SPEAK("COME HERE, I MUST READ YOUR SOUL.\n");
	    WRITE("\nDeath steps closer, reaches out a bony hand straight into your chest,\n" +
		"grabbing something that is within! You feel a strange internal yank as\n" +
		"your very soul is removed for examination...Suddenly Death collects your\n" +
		"bodyless essence with great sweeping motions of his skeletal hands and\n" +
		"puts you in a small glass orb that he inserts into his right eye socket!\n" +
		"You feel a strange blue light from within his eyeless orb penetrate you\n" +

		"as he leans over the chart.\n\n");
	
	}
	
	if (players[j + 1] == 30)
	{
	
	    align = players[j]->query_alignment();
	
	    if(align < -1000)
	    {
		SPEAK("YOUR SINS ARE AS MANY AS THE GRAINS OF SAND IN THE DESERT.\n" +
		      "MAYBE YOU'RE WORSE A MONSTER THAN I! HAHAHAHAHA!\n\n");
		continue;
	    }
	
	    if(align < -500)
	    {
		SPEAK("OH WHAT A DESPISABLE BUG WE HAVE HERE. STEALING CANDY FROM\n" +
		      "BABIES AND BEATING OLD LADIES NO DOUBT. WELL NOW THEY CAN DANCE\n" +
		      "ON YOUR GRAVE. HAHAHA!\n\n");
		continue;
	    }
	
	    if(align < -200)
	    {
		SPEAK("HAVE YOU EVER BEEN TOLD ABOUT REPENTANCE AND ATONEMENT? NO?\n" +
		      "DIDN'T THINK SO EITHER. YOU WILL BE TOLD NOW HOWEVER,\n" +
		      "FOR ETERNITY! HAHAHA!\n\n");
		continue;
	    }
	
	    if(align < 0)
	    {
		SPEAK("SHAME ON YOU MORTAL ONE! STEALING AND KILLING, IS THAT ALL\n" +
		      "YOU CAN THINK OF? WELL NOW YOU WILL BE GIVEN TIME TO REGRET YOUR\n" +
		      "DEEDS. FOR EVER, HAHA!\n\n");
		continue;
	    }
	
	    if(align == 0)
	    {
		SPEAK("WHAT A FENCE-CLIMBER WE HAVE HERE! NEVER MADE UP YOUR MIND\n" +
		      "IN ALL YOUR LIFE, DID YOU? WELL, DON'T WORRY. YOU WON'T HAVE TO\n" +
		      "NOW EITHER! HAHAHA!\n\n");
		continue;
	    }
	
	    if(align < 200)
	    {
		SPEAK("OH WHAT A NICE FELLOW WE HAVE HERE. ALWAYS WALKING THE NARROW\n" +
		      "ROAD, DID YOU? WELL, YOU'LL NEVER KNOW WHAT THE OTHER ROAD IS LIKE\n"  +
		      "NOW! HAHAHA!\n\n");
		continue;
	    }
	
	    if(align < 500)
	    {
		SPEAK("NEVER SAID A DIRTY WORD IN YOUR LIFE DID YOU? WELL, IT'S TOO\n" +
		      "LATE TO CHANGE YOUR MIND ABOUT THAT NOW. HAHAHA! NO MR NICE-GUY YOU\n" +
		      "ARE WHAT YOU WERE. HAHAHA!\n\n");
		continue;
	    }
	
	    if(align < 1000)
	    {
		SPEAK("I HEARD THEY WERE OUT OF ARCHANGELS IN HEAVEN. PERHAPS YOU\n" +
		      "SHOULD APPLY FOR THE JOB? I HOPE YOU KNOW HOW TO PLAY THE HARP,\n" +
		      "OR THEY'LL GIVE THE JOB TO SOMEONE ELSE! HAHAHA!\n\n");
		continue;
	    }
	
	    SPEAK("TRYING TO TAKE THE JOB AWAY FROM GOD, ARE YOU? HAHAHA! LET ME TELL\n" +
		  "YOU A BIT ABOUT IT BEFORE YOU SIGN ANY PAPERS THOUGH. BAD HOURS AND NO\n" +
		  "VACATION. BELIEVE ME, YOU DON'T WANT IT!\n\n");
	
	}
	
	if (players[j + 1] == 35)
	{
	
	    SPEAK("WELL, I GUESS YOU HAVE DONE YOURS FOR THIS TIME, SEE YOU ON\n" +
		  "ARMAGEDDON DAY! HAHAHAHAHAHAHAHAHAHAHAHAHAHAHAHAHAHAHA!\n\n");
	    WRITE("Death removes the orb from his eye and stands up. Suddenly he turns and walks\n" +
		  "out of the room through the nearest wall, still holding you in his skeletal\n" +
		  "hand! He walks rapidly through a dark winding corridor, down a staircase to\n" +
		  "the innermost room in the bottom of the cellar. Finally he stops in front of a\n" +
		  "door bearing the words 'ETERNITY' in black letters. On the door you can see a\n" +
		  "small hatch which Death opens with a flick of his hand. From the open hatch\n" +
		  "you can hear the moaning murmurs of a million souls. Slowly he lifts you to\n" +
		  "the gaping maw of the hatch.\n\n");
	}
	
	if (players[j + 1] == 40)
	    WRITE("Lars arrives through a rift in the fabric of space.\n");
	
	if (players[j + 1] == 45) {
	    WRITE("Lars smiles at you.\n");
	}
	if (players[j + 1] == 47)
	{
	    WRITE("Lars whispers something to Death.\n");
	}
	
	if (players[j + 1] == 54)
	{
	    SPEAK("WHAT? OUT OF THE QUESTION! YOU KNOW THAT!\n\n");
	}
	if (players[j + 1] == 56)
	{
	    WRITE("Lars sighs deeply.\n");
	}
	if (players[j + 1] == 58)
	{
	    WRITE("Lars whispers something to Death.\n");
	}
	
	if (players[j + 1] == 60)
	{
	    SPEAK("REINCARNATION? FOR THIS ONE? HE IS NOT WORTHY OF THAT!\n" +
		  "PLEASE BE SENSIBLE LARS!\n\n");
	}
	if (players[j + 1] == 62) {
	    WRITE("Lars sulks in the corner.\n");
	    WRITE("Lars leaves through a rift in the fabric of space.\n");
	    WRITE("Death looks at you with something that must be disgust even if it's hard to\n" +
		  "say. His face is not the best suited face for showing expressions, but\n" +
		  "you feel fairly confident about this one.\n\n");
	}
	
	if (players[j + 1] == 70)
	{
	
	    SPEAK("OH ALL RIGHT THEN! I CAN WAIT. ONE DAY YOU WILL BE MINE ANYWAY!\n\n");
	    WRITE("Suddenly Death hurles you up in the air, you feel a strange sensation as you\n" +
		  "pass through the very walls of the building, out in the open air, through some\n" +
		  "other walls and fairly surprised horses before you finally stop inside another\n" +
		  "building. It seems vaguely familiar...\n");
	
	}
    }

    do_remove();

}

/*
 * Function name: add_player
 * Description:   Adds a player to the list
 */
void add_player(object plobj)
{

	int i, j;
        mixed * oldlist;

	oldlist = players;

	i = 0;
	if (players)
	{
		i = sizeof(players);
		players = allocate(i + 2);
		for (j = 0 ; j < i ; j ++)
		{
			players[j] = oldlist[j];
		}
	}
	else
		players = allocate(2);

	players[i] = plobj;
	players[i + 1] = 0;

}

/*
 * Function name: remove_player
 * Description:   Removes a player from the list
 */
void remove_player(object plobj)
{

	int i, j, x;
        mixed * oldlist;

	if(!players)
		return;

	i = sizeof(players);

	if (i == 2)
	{
		if (players[0] == plobj)
		{
			players = 0;
			set_heart_beat(0);
			return;
		}
		return;
	}

	x = 0;
	oldlist = players;
	players = allocate(i - 2);

	for (j = 0 ; j < i - 2 ; j += 2)
	{

		if (oldlist[j] == plobj)
			x = 2;

		players[j] = oldlist[j + x];
		players[j + 1] = oldlist[j + x + 1];

	}
}

/*
 * Function name: exit
 * Description:   Remove players if they leave the room prematurly
 */
void exit(object ob)
{

	remove_player(ob);

}

/*
 * Function name: filter
 * Description:   Filter out relevant commands.
 */
int filter(string str)
{

	string verb;

	verb = query_verb();

	if (verb == "quit")
	{
		write("Death says: YOU CAN NOT ESCAPE DEATH!\n");
		return 0;
	}

	if (verb == "look")
		return 0;

	if (verb == "take")
		return 0;

	write("That is impossible in your immaterial state.\n");
	return 1;

}

void long(string str)
{
    int i;
    ::long(str);
    if (str)
	return;
    if (!pointerp(players))
	return;
    for (i=0; i < sizeof(players); i += 2) {
	if (players[i] == this_player()) {
	    if (players[i+1] >= 40 && players[i+1] < 62) {
		write("Lars The Implementor.\n");
		return;
	    }
	}
    }
}

void init()
{

	object death;

	::init();

	death = find_living("moot");

	add_action("filter", "", 1);

	if (this_player() != death)
	{

		if(this_player()->query_ghost() != 1)
		{

			write("Death says: WHAT ARE YOU DOING HERE? YOUR TIME HAS NOT COME YET. BEGONE!\n");
			move_object(this_player(), "/room/church");
			return;
		}

		add_player(this_player());
		set_heart_beat(1);
	}

}


/*
 * Function name: reset
 * Description:   Reset the room
 */
void reset(int arg)
{

	if (arg)
		return;

	create_death();
	players = 0;
	set_light(1);
	no_castle_flag = 1;
	short_desc = "death's workroom";
	long_desc = "A dark room lighted with a dark light that seems to defy darkness not so much\n" +
		"by actually illuminating the room as by being a darkpoint in less dark\n" +
		"surroundings. In the strange light (dark?) you can see a centrally placed desk\n" +
		"covered with books and diagrams. The walls are covered with bookshelves\n" +
		"filled with dark tomes bound in leather that gleam with strange runes.\n\n";
	dest_dir = 0;

}

