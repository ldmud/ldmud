/*

   This object is a standard food object and works
   like /obj/alco_drink.c or /obj/armour.c

   To use this you can do:
     inherit "obj/food";
   ......
   or,
   object ob;
   ob = clone_object("obj/food");
   ob->set_name("apple pie");

*  For more documentation look at /doc/build/food


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

	   set_eater_mess(string)
				To set the message that is written to the
				player when he eats the item.

	   set_eating_mess(string)
				To set the message given to the surrounding
				players when this object is eaten.


	For an example of the use of this object, please read:
*	/doc/examples/apple_pie.c

*/

string name, short, long, eating_mess, eater_mess, alias, alt_name;
int value, strength, weight;

void init()
{
	add_action("eat", "eat");
}

void reset(int arg)
{
	if (arg)
		return;

	weight = 1;
	eater_mess = "Yum yum yum.\n";
}

int prevent_insert()
{
  write("You don't want to ruin " + short + ".\n");
  return 1;
}

int id(string str)
{
	return  str == name || str == alt_name || str == alias;
}

string short()
{
	if(!short)
	    return name;

	return short;
}

void long()
{
	if(!long)
		write(short() + ".\n");
	else
		write(long);
}

int get()
{
	return 1;
}

int eat(string str)
{
	object tp;

	tp = this_player();

	if (!str || !id(str))
		return 0;

	if(tp->query_level() * 8 < strength)
	{
		write("You realize even before trying that you'll never be able to eat all this.\n");
		return 1;
	}

	if (!tp->eat_food(strength))
		return 1;

	tp->heal_self(strength);
	write(eater_mess);
	if (eating_mess)
		say(capitalize(this_player()->query_name()) + eating_mess);
	else
		say(capitalize(this_player()->query_name()) + " eats " + short + ".\n");
	destruct(this_object());
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
	short = s;
}

void set_long(string l)
{
	long = l;
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

void set_eating_mess(string em)
{
	eating_mess = em;
}

void set_eater_mess(string em)
{
	eater_mess = em;
}

/*
 * Things that other objects might want to know.
 */

int query_value()
{
	if (value)
		return value;
	else
		return min_cost();
}

int query_weight()
{
	return weight;
}
