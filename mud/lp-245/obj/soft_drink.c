/*

   This object is a standard soft_drink object and works
   like /obj/food.c or /obj/armour.c

   To use this you can do:
     inherit "obj/soft_drink";
   ......
   or,
   object ob;
   ob = clone_object("obj/soft_drink");
   ob->set_name("apple juice");

*  For more documentation look at /doc/build/drinks


   These functions are defined:

	   set_name(string)	To set the name of the item. For use in id().

   Two alternative names can be set with the calls:

	   set_alias(string) and set_alt_name(string)

	   set_short(string)	To set the short description.

	   set_long(string)	To set the long description.

	   set_value(int)	To set the value of the item.

	   set_weight(int)	To set the weight of the item.

	   set_strength(int)	To set the healing power of the item. If you
				don't wish the item to have healing powers
				just set this value to 0.

	   set_drinker_mess(string)
				To set the message that is written to the
				player when he drinks the item.

	   set_drinking_mess(string)
				To set the message given to the surrounding
				players when this object is drunk.

	   set_empty_container(string)
				The container of the liquid inside. For
				example "bottle" or "jug".


	For an example of the use of this object, please read:
*	/doc/examples/apple_juice.c

*/

string name, short_desc, long_desc, drinking_mess,
	drinker_mess, alias, alt_name, empty_container;
int value, strength, weight, full;

void init()
{
	add_action("drink", "drink");
}

void reset(int arg)
{
	if (arg)
		return;

	full = 1;
	weight = 1;
	drinker_mess = "Gloink Glurk Glburp.\n";
	empty_container = "bottle";
}

int prevent_insert()
{
	if (empty_container)
		return 0;
	else
	{
		write("You don't want to ruin " + name + ".\n");
		return 1;
	}
}

int id(string str)
{
	if (full)
		return  str == name || str == alt_name || str == alias;
	else
		return str == empty_container;
}

string short()
{
	if (full)
	{
		if (!short_desc)
		    return name;

		return short_desc;
	}
	else
		return "An empty " + empty_container;
}

void long()
{
	if (full)
	{
		if (!long_desc)
			write(short() + ".\n");
		else
			write(long_desc);
	}
	else
		write(short() + "\n");
}

int get()
{
	return 1;
}

int drink(string str)
{
	object tp;
	string p_name;

	tp = this_player();
	p_name = capitalize(tp->query_name());

	if (!full)
		return 0;

	if (!str || !id(str))
		return 0;


	if (tp->query_level() * 8 < strength)
	{
		write("This is much to much for you to drink! You drool most of it on the ground.\n");
		say(p_name + " tries to drink " + short_desc + " but drools most of it on the ground.\n");
		full = 0;
		return 1;
	}

	if (!tp->drink_soft(strength))
		return 1;
	
	full = 0;
	tp->heal_self(strength);
	write(drinker_mess);
	if (drinking_mess)
		say(p_name + drinking_mess);
	else
		say(p_name + " drinks " + short_desc + ".\n");
	return 1;
}

int min_cost()
{
	return 4 * strength + (strength * strength) / 10;
}

void set_name(string n)
{
	name = n;
}

void set_short(string s)
{
	short_desc = s;
}

void set_long(string l)
{
	long_desc = l;
}

void set_value(int v)
{
	value = v;
}

void set_weight(int w)
{
	weight = w;
}

void set_strength(int s)
{
	strength = s;
}

void set_alias(string a)
{
	alias = a;
}

void set_alt_name(string an)
{
	alt_name = an;
}

void set_drinking_mess(string dm)
{
	drinking_mess = dm;
}

void set_drinker_mess(string dm)
{
	drinker_mess = dm;
}

void set_empty_container(string ec)
{
	empty_container = ec;
}

/*
 * Things that other objects might want to know.
 */

int query_value()
{
	if (full)
	{
		if (value)
			return value;
		else
			return min_cost();
	}
	else
		return 10;
}

int query_weight()
{
	return weight;
}
