/* start_mark.c */
/* Mrpr 901122 */

void  start_death();

/*
 * Function name: init
 * Description:   Init this object
 */
void init()
{

	start_death();

}

/*
 * Function name: get
 * Description:   Don't give it away.
 */
int get()
{
	return 1;
}

/*
 * Function name: id
 * Description:   Identify the object
 */
int id(string str)
{

	return str == "death_mark";

}

/*
 * Function name: start_death
 * Description:   Start the death sequence.
 */
void start_death()
{
	
	object ned, my_host;

	my_host = environment(this_object());

	if (my_host)
	{
		if(living(my_host))
		{
			if(my_host->query_ghost() != 1)
			{
				destruct(this_object());
				return;
			}
		}
		else
			return;
	}
	else
		return;

	say("You see a dark shape gathering some mist... or maybe you're just imagining that.\n");
	write("You can see a dark hooded man standing beside your corpse.\n" +
		"He is wiping the bloody blade of a wicked looking scythe with slow measured\n" +
		"motions. Suddenly he stops and seems to look straight at you with his empty...\n" +
		"no, not empty but.... orbs....\n\n");

	write("Death says: COME WITH ME, MORTAL ONE!\n\n");

	write("He reaches for you and suddenly you find yourself in another place.\n\n");
	move_object(my_host, "/room/death/death_room");

}

/*
 * Function name: query_auto_load
 * Description:   Automatic load of this object
 */
string query_auto_load()
{

	return "room/death/death_mark:";

}

/*
 * Function name: drop
 * Description:   No dropping.
 */
int drop()
{
	return 1;
}
