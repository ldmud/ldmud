/* init_scroll.c */
/* Mrpr 901130 */

/*
   This object will autoload and cannot be dropped. The wizard holding it
   must actually destruct it to get rid of it. This to prevent him from
   losing information he badly needs in the beginning.
*/

int hush_up;

/*
 * Function name: init
 * Description:   Initialize commandverbs
 */
void init()
{

	add_action("read_rules", "rules");
	add_action("read_scroll", "read");
	add_action("hush", "quit");

}

/*
 * Function name: reset
 * Description:   Reset the scroll
 */
void reset(int arg)
{
	if (arg)
		return;

	hush_up = 0;
	
}

/*
 * Function name: id
 * Description:   Return 1 on positive identification.
 */
int id(string str)
{
	return str == "scroll";
}

/*
 * Function name: short
 * Description:   Return a short description
 */
string short()
{
	return "A scroll labeled 'READ ME!'";
}

/*
 * Function name: long
 * Description:   Write a long description
 */
void long()
{

	write("The scroll is held rolled up with an blue and yellow band tied around its\n" +
		"middle. On the band the text 'READ ME!' is written in golden letters.\n");

}

/*
 * Function name: get
 * Description:   Gettable.
 */
int get()
{
	return 1;
}

/*
 * Function name: drop
 * Description:   Don't drop it.
 */
int drop()
{
	if(!hush_up)
		write("Don't drop the scroll, it contains valuable information!\n");
	return 1;
}

/*
 * Function name: query_auto_load
 * Description:   Autoload this object.
 */
string query_auto_load()
{
	return "doc/examples/init_scroll:";
}

/*
 * Function name: hush
 * Description:   Be quiet when quitting.
 */
void hush()
{
	hush_up = 1;
}

/*
 * Function name: read_scroll
 * Description:   Read the text
 */
int read_scroll()
{

	log_file("examples.scroll", capitalize(this_player()->query_real_name()) + " has read the scroll. " + ctime(time()) + "\n");
		
	this_player()->cat_file("/doc/examples/init_text");

	return 1;

}

/*
 * Function name: read_rules
 * Description:   Read the RULES
 */
int read_rules()
{
	log_file("examples.scroll", capitalize(this_player()->query_real_name()) + " has read the RULES. " + ctime(time()) + "\n");
	this_player()->more("/doc/build/RULES");
	return 1;
}
