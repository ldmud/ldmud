object wyrm;

void extra_reset();

void reset(int started)
{
     if (!started)
	 set_light(0);
     extra_reset();
}

void init()
{
    add_action("up", "up");
}

string short()
{
    return "The bottom of the well";
}

void long()
{
    write("You are standing at the bottom of the well, about thirty feet below the\n" +
	  "surface. Bones lie strwen about in a random fashion, many of them broken\n" +
	  "or shattered.\n" +
	  "\tThe only way out is the way in, back up the ladder.\n");
}

int up()
{
    this_player()->move_player("up the ladder#room/south/sislnd17");
    return 1;
}

void extra_reset()
{
     if (!wyrm || !living(wyrm))
	 {
	     object coins, jem;
	     int jemnum;
	     wyrm = clone_object("obj/monster");
	     wyrm->set_name("wyrm");
	     wyrm->set_level(17 );
	     wyrm->set_hp(350);
	     wyrm->set_al(-900);
	     wyrm->set_short("The Wyrm of Arcanarton");
	     wyrm->set_long(
"The giant undead dragon you see before you is the result of one of\n"+
"Arcanarton's magic experiments.\n");
	     wyrm->set_wc(25);
	     wyrm->set_ac(7);
	     wyrm->set_spell_chance(50);
	     wyrm->set_spell_dam(100);
	     wyrm->set_spell_mesg(
"Arcanarton's wyrm turns his head and breathes death at you.\n");
             coins = clone_object("obj/money");
	     coins->set_amount(random(500));
	     move_object(coins, wyrm);
	     jem = clone_object("obj/treasure");
             jemnum = random(3);
	     if (jemnum == 0)
		 {
		     jem->set_id("diamond");
		     jem->set_short("a diamond");
		 }
	     if (jemnum == 1)
		 {
		     jem->set_id("emerald");
		     jem->set_short("an emerald");
		 }
	     if (jemnum == 3)
		 {
		     jem->set_id("sapphire");
		     jem->set_short("a sapphire");
		 }
 	    jem->set_alias("jem");
	    jem->set_value(random(250) + 300);
	     move_object(jem, wyrm);
	     move_object(wyrm, this_object());
	 }
}
