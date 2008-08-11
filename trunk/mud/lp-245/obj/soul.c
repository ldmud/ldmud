/* Modified for genders: padrone, Oct 22 1990 */

object the_owner;
string cap_name;

int get() {
    the_owner = this_player();
    cap_name = this_player()->query_name();
    return 1;
}

int drop() { return 1; }

int id(string str) { return str == "soul"; }

void long() {
    write("It is transparent.\n");
}

int ghost() {
    the_owner = this_player();
    cap_name = this_player()->query_name();
    return this_player()->query_ghost();
}

void init() {
    the_owner = this_player();
    cap_name = this_player()->query_name();
    add_action("applaud", "applaud");
    add_action("blush", "blush");
    add_action("bounce", "bounce");
    add_action("bow", "bow");
    add_action("burp", "burp");
    add_action("cackle", "cackle");
    add_action("chuckle", "chuckle");
    add_action("clap", "clap");
    add_action("comfort", "comfort");
    add_action("cough", "cough");
    add_action("cry", "cry");
    add_action("cuddle", "cuddle");
    add_action("curtsey", "curtsey");
    add_action("dance", "dance");
    add_action("fart", "fart");
    add_action("flip", "flip");
    add_action("fondle", "fondle");
    add_action("french", "french");
    add_action("frown", "frown");
    add_action("gasp", "gasp");
    add_action("giggle", "giggle");
    add_action("glare", "glare");
    add_action("grin", "grin");
    add_action("groan", "groan");
    add_action("grope", "grope");
    add_action("growl", "growl");
    add_action("hiccup", "hiccup");
    add_action("hug", "hug");
    add_action("kick", "kick");
    add_action("kiss", "kiss");
    add_action("knee", "knee");
    add_action("laugh", "laugh");
    add_action("lick", "lick");
    add_action("love", "love");
    add_action("moan", "moan");
    add_action("nibble", "nibble");
    add_action("nod", "nod");
    add_action("poke", "poke");
    add_action("pout", "pout");
    add_action("puke", "puke");
    add_action("purr", "purr");
    add_action("ruffle", "ruffle");
    add_action("scream", "scream");
    add_action("shake", "shake");
    add_action("shiver", "shiver");
    add_action("shrug", "shrug");
    add_action("sigh", "sigh");
    add_action("sing", "sing");
    add_action("slap", "slap");
    add_action("smirk", "smirk");
    add_action("smile", "smile");
    add_action("snap", "snap");
    add_action("sneeze", "sneeze");
    add_action("snicker", "snicker");
    add_action("sniff", "sniff");
    add_action("snore", "snore");
    add_action("snuggle", "snuggle");
    add_action("spit", "spit");
    add_action("squeeze", "squeeze");
    add_action("stare", "stare");
    add_action("strut", "strut");
    add_action("sulk", "sulk");
    add_action("thank", "thank");
    add_action("twiddle", "twiddle");
    add_action("whistle", "whistle");
    add_action("wiggle", "wiggle");
    add_action("wink", "wink");
    add_action("yawn", "yawn");
}

int applaud() {
    if (ghost())
	return 0;
    write("You applaud wholeheartedly.\n");
    say(cap_name + " gives a round of applause.\n");
    return 1;
}

int blush() {
    if (ghost())
	return 0;
    write("Your cheeks are burning.\n");
    say(cap_name + " blushes.\n");
    return 1;
}

int bounce() {
    if (ghost())
	return 0;
    write("B O I N G !!\n");
    say(cap_name + " bounces around.\n");
    return 1;
}

int bow(string str) {
    object who;
    if (ghost())
	return 0;
    if (!str) {
        write("You bow to your audience.\n");
	say(cap_name + " bows gracefully.\n");
	return 1;
    }
    who = present(lower_case(str), environment(this_player()));
    if (!who || !living(who) || who == this_player())
	return 0;
    tell_object(who, cap_name + " bows before you.\n");
    write("You bow to " + str +".\n");
    say(cap_name + " bows to " + str + ".\n", who);
    return 1;
}

int burp() {
    if (ghost())
	return 0;
    write("Excuse yourself!\n");
    say(cap_name + " burps rudely.\n");
    return 1;
}

int cackle() {
    if (ghost())
	return 0;
    write("You cackle gleefully.\n");
    say(cap_name + " throws " + the_owner->query_possessive() +
	" head back and cackles with glee!\n");
    return 1;
}

int chuckle() {
    if (ghost())
	return 0;
    write("You chuckle politely.\n");
    say(cap_name + " chuckles politely.\n");
    return 1;
}

int clap() {
    if (ghost())
	return 0;
    write("You clap briefly.\n");
    say(cap_name + " claps.\n");
    return 1;
}

int comfort(string str) {
    object who;
    if (ghost())
	return 0;
    if (!str)
	return 0;
    who = present(lower_case(str), environment(this_player()));
    if (!who || !living(who) || who == this_player())
	return 0;
    tell_object(who, cap_name + " comforts you.\n");
    write("You comfort " + str + ".\n");
    say(cap_name + " comforts " + str + ".\n", who);
    return 1;
}

int cough() {
    if (ghost())
	return 0;
    write("Cover your mouth when you do that!\n");
    say(cap_name + " coughs noisily.\n");
    return 1;
}

int cry() {
    if (ghost())
	return 0;
    write("Waaaaah!\n");
    say(cap_name + " bursts into tears.\n");
    return 1;
}

int cuddle(string str) {
    object who;
    if (ghost())
	return 0;
    if (!str)
	return 0;
    who = present(lower_case(str), environment(this_player()));
    if (!who || !living(who) || who == this_player())
	return 0;
    tell_object(who, cap_name + " cuddles you.\n");
    write("You cuddle " + str + ".\n");
    say(cap_name + " cuddles " + str + ".\n", who);
    return 1;
}

int curtsey() {
    if (ghost())
	return 0;
    write("You curtsey gracefully.\n");
    say(cap_name + " curtseys gracefully.\n");
    return 1;
}

int dance(string str) {
    object who;
    if (ghost())
	return 0;
    if (!str) {
        write("Feels silly, doesn't it?\n");
	say(cap_name + " does the disco duck.\n");
	return 1;
    }
    who = present(lower_case(str), environment(this_player()));
    if (!who || !living(who) || who == this_player())
	return 0;
    tell_object(who, cap_name + " sweeps you across the dance floor.\n");
    write("You sweep " + str +" across the dance floor.\n");
    say(cap_name + " sweeps " + str + " across the dance floor.\n", who);
    return 1;
}

int fart() {
    if (ghost())
	return 0;
    write("How rude!\n");
    say(cap_name + " lets off a real rip-roarer.\n");
    return 1;
}

int flip() {
    if (ghost())
	return 0;
    write("You flip head over heels.\n");
    say(cap_name + " flips head over heels.\n");
    return 1;
}

int fondle(string str) {
    object who;
    if (ghost())
	return 0;
    if (!str)
	return 0;
    who = present(lower_case(str), environment(this_player()));
    if (!who || !living(who) || who == this_player())
	return 0;
    tell_object(who, cap_name + " fondles you.\n");
    write("You fondle " + str + ".\n");
    say(cap_name + " fondles " + str + ".\n", who);
    return 1;
}

int french(string str) {
    object who;
    if (ghost())
	return 0;
    if (!str)
	return 0;
    who = present(lower_case(str), environment(this_player()));
    if (!who || !living(who) || who == this_player())
	return 0;
    tell_object(who, cap_name +
       " gives you a deep and passionate kiss..it seems to take forever...\n");
    write("You give " + str + " a REAL kiss..it lasts a long time...\n");
    say(cap_name + " gives " + str +
       " a deep and passionate kiss..it seems to take forever...\n", who);
    return 1;
}

int frown() {
    if (ghost())
	return 0;
    write("Is something wrong?\n");
    say(cap_name + " frowns.\n");
    return 1;
}

int gasp() {
    if (ghost())
	return 0;
    write("You gasp in astonishment.\n");
    say(cap_name + " gasps in astonishment!\n");
    return 1;
}

int giggle() {
    if (ghost())
	return 0;
    write("You giggle inanely.\n");
    say(cap_name + " giggles inanely.\n");
    return 1;
}

int glare(string str) {
    object who;
    if (ghost())
	return 0;
    if (!str)
	return 0;
    who = present(lower_case(str), environment(this_player()));
    if (!who || !living(who) || who == this_player())
	return 0;
    tell_object(who, cap_name + " glares at you.\n");
    write("You glare stonily at " + str + ".\n");
    say(cap_name + " glares at " + str + ".\n", who);
    return 1;
}

int grin() {
    if (ghost())
	return 0;
    write("You grin evilly.\n");
    say(cap_name + " grins evilly.\n");
    return 1;
}

int groan() {
    if (ghost())
	return 0;
    write("You groan.\n");
    say(cap_name + " groans loudly.\n");
    return 1;
}

int grope(string str) {
    object who;
    if (ghost())
	return 0;
    if (!str)
	return 0;
    who = present(lower_case(str), environment(this_player()));
    if (!who || !living(who) || who == this_player())
	return 0;
    tell_object(who, cap_name + " gropes you.\n");
    write("<Well what sort of noise do you expect here?>.\n");
    say(cap_name + " gropes " + str + ".\n", who);
    return 1;
}

int growl(string str) {
    object who;
    if (ghost())
	return 0;
    if (!str) {
        write("You growl.\n");
	say(cap_name + " growls.\n");
	return 1;
    }
    who = present(lower_case(str), environment(this_player()));
    if (!who || !living(who) || who == this_player())
	return 0;
    tell_object(who, cap_name + " growls at you.\n");
    write("You growl at " + str +".\n");
    say(cap_name + " growls at " + str + ".\n", who);
    return 1;
}

int hiccup() {
    if (ghost())
	return 0;
    write("Hic!\n");
    say(cap_name + " hiccups.\n");
    return 1;
}

int hug(string str) {
    object who;
    if (ghost())
	return 0;
    if (str == 0)
	return 0;
    who = present(lower_case(str), environment(this_player()));
    if (!who || !living(who) || who == this_player())
	return 0;
    tell_object(who, cap_name + " hugs you.\n");
    write("You hug " + str + ".\n");
    say(cap_name + " hugs " + str + ".\n", who);
    return 1;
}

int kick(string str) {
    object who;
    if (ghost())
	return 0;
    if (str == 0)
	return 0;
    who = present(str, environment(this_player()));
    if (!who || !living(who) || who == this_player())
	return 0;
    tell_object(who, cap_name + " kicks you.   OUCH!!\n");
    say(cap_name + " kicks " + str + ".\n", who);
    write("You kick " + str + ".\n");
    return 1;
}

int kiss(string str) {
    object who;
    if (ghost())
	return 0;
    if (str == 0)
	return 0;
    who = present(str, environment(this_player()));
    if (!who || !living(who) || who == this_player())
	return 0;
    tell_object(who, cap_name + " kisses you.\n");
    say(cap_name + " kisses " + str + ".\n", who);
    if (who->query_frog()) {
	this_player()->frog_curse(1);
	who->frog_curse(0);
	return 1;
    }
    write("You kiss " + str + ".\n");
    return 1;
}

int knee(string str) {
    object who;
    if (ghost())
	return 0;
    if (str == 0)
	return 0;
    who = present(lower_case(str), environment(this_player()));
    if (!who || !living(who) || who == this_player())
	return 0;
    if (who->query_male()) {
	tell_object(who, cap_name + " hits you with " +
		    the_owner->query_possessive() + " knee below your belt!\n" +
		    "You double over and fall to the ground, writhing in " +
		    "excrutiating pain,\nfeeling like you may throw up " +
		    "everything you have eaten!\n");
	say(cap_name + " suddenly raises " + the_owner->query_possessive() +
	    " knee, sending " + str + " to the floor, writhing in pain!\n",
	    who);
	write("You hit " + str + " with your knee.\n");
    }
    else {
	tell_object(who, cap_name + " tries to knee you, without much effect.\n");
	say(cap_name + " tries to knee " + str + ", without much effect.\n", who);
	write("You try to knee " + str + ". Not very effective though.\n");
    }
    return 1;
}

int laugh() {
    if (ghost())
	return 0;
    write("You fall down laughing.\n");
    say(cap_name + " falls down laughing.\n");
    return 1;
}

int lick(string str) {
    object who;
    if (ghost())
	return 0;
    if (str == 0)
	return 0;
    who = present(str, environment(this_player()));
    if (!who || !living(who) || who == this_player())
	return 0;
    tell_object(who, cap_name + " licks you.\n");
    say(cap_name + " licks " + str + ".\n", who);
    write("You lick " + str + ".\n");
    return 1;
}

int love(string str) {
    object who;
    if (ghost())
	return 0;
    if (str == 0)
	return 0;
    who = present(str, environment(this_player()));
    if (!who || !living(who) || who == this_player())
	return 0;
    tell_object(who, cap_name + " whispers to you sweet words of love.\n");
    say(cap_name + " whispers softly to " + str + ".\n", who);
    write("You tell your true feelings to " + str + ".\n");
    return 1;
}

int moan() {
    if (ghost())
	return 0;
    write("You start to moan.\n");
    say(cap_name + " starts moaning.\n");
    return 1;
}

int nibble(string str) {
    object who;
    if (ghost())
	return 0;
    if (str == 0)
	return 0;
    who = present(str, environment(this_player()));
    if (!who || !living(who) || who == this_player())
	return 0;
    tell_object(who, cap_name + " nibbles on your ear.\n");
    say(cap_name + " nibbles on " + str + "s ear.\n", who);
    write("You nibble " + str + "s ear.\n");
    return 1;
}

int nod() {
    if (ghost())
	return 0;
    write("You nod solemnly.\n");
    say(cap_name + " nods solemnly.\n");
    return 1;
}

int poke(string str) {
    object who;
    if (ghost())
	return 0;
    if (str == 0)
	return 0;
    who = present(str, environment(this_player()));
    if (!who || !living(who) || who == this_player())
	return 0;
    tell_object(who, cap_name + " pokes you in the ribs.\n");
    say(cap_name + " pokes " + str + " in the ribs.\n", who);
    write("You poke " + str + " in the ribs.\n");
    return 1;
}

int pout() {
    if (ghost())
	return 0;
    write("Ah, don't take it so hard.\n");
    say(cap_name + " pouts.\n");
    return 1;
}

int puke(string str) {
    object who;
    if (ghost())
	return 0;
    if(!str) {
        write("You puke on your shoes.\n");
        say(cap_name + " doubles over and puke.\n");
        return 1;
    }
    who = present(lower_case(str), environment(this_player()));
    if (!who || !living(who) || who == this_player())
	return 0;
    tell_object(who, cap_name + " pukes all over you!\n");
    write("You puke on " + str +".\n");
    say(cap_name + " pukes on " + str + ".\n", who);
    return 1;
}

int purr() {
    if (ghost())
	return 0;
    write("MMMMEEEEEEEEOOOOOOOWWWWWWW!\n");
    say(cap_name + " purrs contentedly.\n");
    return 1;
}

int ruffle(string str) {
    object who;
    if (ghost())
	return 0;
    if (!str)
	return 0;
    who = present(lower_case(str), environment(this_player()));
    if (!who || !living(who) || who == this_player())
	return 0;
    tell_object(who, cap_name + " ruffles your hair playfully.\n");
    write("You ruffle " + str + "s hair playfully.\n");
    say(cap_name + " ruffles " + str + "s hair playfully.\n", who);
    return 1;
}

int scream() {
    if (ghost())
	return 0;
    write("ARRGGGGGGHHHHHH!!!!\n");
    say(cap_name + " screams loudly!\n");
    return 1;
}

int shake(string str) {
    object who;
    if (ghost())
	return 0;
    if(!str) {
        write("You're shaking in your boots.\n");
        say(cap_name + " shakes and quivers like a bowlful of jelly.\n");
        return 1;
    }
    who = present(lower_case(str), environment(this_player()));
    if (!who || !living(who) || who == this_player())
	return 0;
    tell_object(who, cap_name + " shakes your hand.\n");
    write("You shake hands with " + str +".\n");
    say(cap_name + " shakes " + str + "s hand.\n", who);
    return 1;
}

int shiver() {
    if (ghost())
	return 0;
    write("Brrrrrr!!!\n");
    say(cap_name + " shivers from the cold.\n");
    return 1;
}

int shrug() {
    if (ghost())
	return 0;
    write("You shrug.\n");
    say(cap_name + " shrugs helplessly.\n");
    return 1;
}

int sigh() {
    if (ghost())
	return 0;
    write("You sigh.\n");
    say(cap_name + " sighs deeply.\n");
    return 1;
}

int sing() {
    if (ghost())
	return 0;
    write("Oh sole mio!\n");
    say(cap_name + " sings in Italian.\n");
    return 1;
}

int slap(string str) {
    object who;
    if (ghost())
	return 0;
    if (str == 0)
	return 0;
    who = present(str, environment(this_player()));
    if (!who || !living(who) || who == this_player())
	return 0;
    write("You slap " + str + ".\n");
    say(cap_name + " slaps " + str + ".\n", who);
    tell_object(who, cap_name + " slaps you!\n");
    return 1;
}

int smirk() {
    if (ghost())
	return 0;
    write("You smirk.\n");
    say(cap_name + " smirks.\n");
    return 1;
}

int smile(string str) {
    object who;
    if (ghost()) {
	write("You smile inwardly.\n");
	return 1;
    }
    if(!str) {
        write("You smile happily.\n");
        say(cap_name + " smiles happily.\n");
        return 1;
    }
    who = present(lower_case(str), environment(this_player()));
    if (!who || !living(who) || who == this_player())
	return 0;
    tell_object(who, cap_name + " smiles at you.\n");
    write("You smile at " + str +".\n");
    say(cap_name + " smiles at " + str + ".\n", who);
    return 1;
}

int snap() {
    if (ghost())
	return 0;
    write("You snap your fingers.\n");
    say(cap_name + " snaps " + the_owner->query_possessive() +
	" fingers.\n");
    return 1;
}

int sneeze() {
    if (ghost())
	return 0;
    write("Gazundheit!\n");
    say(cap_name + " sneezes.\n");
    return 1;
}

int snicker() {
    if (ghost())
	return 0;
    write("You snicker.\n");
    say(cap_name + " snickers.\n");
    return 1;
}

int sniff() {
    if (ghost())
	return 0;
    write("You sniff.\n");
    say(cap_name + " sniffs.\n");
    return 1;
}

int snore() {
    if (ghost())
	return 0;
    write("Zzzzzzzzzz...\n");
    say(cap_name + " snores loudly.\n");
    return 1;
}

int snuggle(string str) {
    object who;
    if (ghost())
	return 0;
    if (str == 0)
	return 0;
    who = present(str, environment(this_player()));
    if (!who || !living(who) || who == this_player())
	return 0;
    write("You snuggle " + str + ".\n");
    say(cap_name + " snuggles up to " + str + ".\n", who);
    tell_object(who, cap_name + " snuggles up to you.\n");
    return 1;
}

int spit(string str) {
    object who;
    if (ghost())
	return 0;
    if(!str) {
        write("You spit.\n");
        say(cap_name + " spits on the ground.\n");
        return 1;
    }
    who = present(lower_case(str), environment(this_player()));
    if (!who || !living(who) || who == this_player())
	return 0;
    tell_object(who, cap_name + " spits on you!\n");
    write("You spit on " + str +".\n");
    say(cap_name + " spits on " + str + ".\n", who);
    return 1;
}

int squeeze(string str) {
    object who;
    if (ghost())
	return 0;
    if (str == 0)
	return 0;
    who = present(str, environment(this_player()));
    if (!who || !living(who) || who == this_player())
	return 0;
    write("You squeeze " + str + " fondly.\n");
    say(cap_name + " squeezes " + str + " fondly.\n", who);
    tell_object(who, cap_name + " squeezes you fondly.\n");
    return 1;
}

int stare(string str) {
    object who;
    if (ghost())
	return 0;
    if(!str) {
        write("You stare into space.\n");
        say(cap_name + " stares into space.\n");
        return 1;
    }
    who = present(lower_case(str), environment(this_player()));
    if (!who || !living(who) || who == this_player())
	return 0;
    tell_object(who, cap_name + " stares deep into your eyes.\n");
    write("You stare dreamily at " + str +".\n");
    say(cap_name + " stares dreamily at " + str + ".\n", who);
    return 1;
}

int strut() {
    if (ghost())
	return 0;
    write("Strut your stuff!\n");
    say(cap_name + " struts proudly.\n");
    return 1;
}

int sulk() {
    if (ghost())
	return 0;
    write("You sulk.\n");
    say(cap_name + " sulks in the corner.\n");
    return 1;
}

int thank(string str) {
    object who;
    if (ghost())
	return 0;
    if (str == 0)
	return 0;
    who = present(str, environment(this_player()));
    if (!who || !living(who) || who == this_player())
	return 0;
    write("You thank " + str + ".\n");
    say(cap_name + " thanks " + str + ".\n", who);
    tell_object(who, cap_name + " thanks you.\n");
    return 1;
}

int twiddle() {
    if (ghost())
	return 0;
    write("You twiddle your thumbs.\n");
    say(cap_name + " twiddles " + the_owner->query_possessive() +
	" thumbs.\n");
    return 1;
}

int whistle(string str) {
    object who;
    if (ghost())
	return 0;
    if(!str) {
        write("You whistle appreciatively.\n");
        say(cap_name + " whistles appreciatively.\n");
        return 1;
    }
    who = present(lower_case(str), environment(this_player()));
    if (!who || !living(who) || who == this_player())
	return 0;
    tell_object(who, cap_name + " whistles appreciatively at you.\n");
    write("You whistle appreciatively at " + str +".\n");
    say(cap_name + " whistles appreciatively at " + str + ".\n", who);
    return 1;
}

int wiggle() {
    if (ghost())
	return 0;
    write("You wiggle your bottom.\n");
    say(cap_name + " wiggles " + the_owner->query_possessive() +
	" bottom.\n");
    return 1;
}

int wink(string str) {
    object who;
    if (ghost())
	return 0;
    if(!str) {
        write("You wink.\n");
        say(cap_name + " winks suggestively.\n");
        return 1;
    }
    who = present(lower_case(str), environment(this_player()));
    if (!who || !living(who) || who == this_player())
	return 0;
    tell_object(who, cap_name + " winks suggestively at you.\n");
    write("You wink at " + str +".\n");
    say(cap_name + " winks suggestively at " + str + ".\n", who);
    return 1;
}

int yawn() {
    if (ghost())
	return 0;
    write("My, what big teeth you have!\n");
    say(cap_name + " yawns.\n");
    return 1;
}
