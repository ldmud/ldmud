soul_init() {
    cap_name = call_other(this_player(), "query_name", 0);
    add_action("applaud"); add_verb("applaud");
    add_action("blush"); add_verb("blush");
    add_action("bounce"); add_verb("bounce");
    add_action("bow"); add_verb("bow");
    add_action("burp"); add_verb("burp");
    add_action("cackle"); add_verb("cackle");
    add_action("chuckle"); add_verb("chuckle");
    add_action("clap"); add_verb("clap");
    add_action("comfort"); add_verb("comfort");
    add_action("cough"); add_verb("cough");
    add_action("cry"); add_verb("cry");
    add_action("cuddle"); add_verb("cuddle");
    add_action("curtsey"); add_verb("curtsey");
    add_action("dance"); add_verb("dance");
    add_action("fart"); add_verb("fart");
    add_action("flip"); add_verb("flip");
    add_action("fondle"); add_verb("fondle");
    add_action("french"); add_verb("french");
    add_action("frown"); add_verb("frown");
    add_action("gasp"); add_verb("gasp");
    add_action("giggle"); add_verb("giggle");
    add_action("glare"); add_verb("glare");
    add_action("grin"); add_verb("grin");
    add_action("groan"); add_verb("groan");
    add_action("grope"); add_verb("grope");
    add_action("growl"); add_verb("growl");
    add_action("hiccup"); add_verb("hiccup");
    add_action("hug"); add_verb("hug");
    add_action("kick"); add_verb("kick");
    add_action("kiss"); add_verb("kiss");
    add_action("laugh"); add_verb("laugh");
    add_action("lick"); add_verb("lick");
    add_action("love"); add_verb("love");
    add_action("moan"); add_verb("moan");
    add_action("nibble"); add_verb("nibble");
    add_action("nod"); add_verb("nod");
    add_action("pat"); add_verb("pat");
    add_action("poke"); add_verb("poke");
    add_action("pout"); add_verb("pout");
    add_action("puke"); add_verb("puke");
    add_action("purr"); add_verb("purr");
    add_action("ruffle"); add_verb("ruffle");
    add_action("scream"); add_verb("scream");
    add_action("shake"); add_verb("shake");
    add_action("shiver"); add_verb("shiver");
    add_action("shrug"); add_verb("shrug");
    add_action("sigh"); add_verb("sigh");
    add_action("sing"); add_verb("sing");
    add_action("slap"); add_verb("slap");
    add_action("smirk"); add_verb("smirk");
    add_action("smile"); add_verb("smile");
    add_action("snap"); add_verb("snap");
    add_action("sneeze"); add_verb("sneeze");
    add_action("snicker"); add_verb("snicker");
    add_action("sniff"); add_verb("sniff");
    add_action("snore"); add_verb("snore");
    add_action("snuggle"); add_verb("snuggle");
    add_action("spit"); add_verb("spit");
    add_action("squeeze"); add_verb("squeeze");
    add_action("stare"); add_verb("stare");
    add_action("strut"); add_verb("strut");
    add_action("sulk"); add_verb("sulk");
    add_action("thank"); add_verb("thank");
    add_action("twiddle"); add_verb("twiddle");
    add_action("whistle"); add_verb("whistle");
    add_action("wiggle"); add_verb("wiggle");
    add_action("wink"); add_verb("wink");
    add_action("yawn"); add_verb("yawn");
}

applaud() {
    if (ghost())
	return 0;
    write("You applaud wholeheartedly.\n");
    say(cap_name + " gives a round of applause.\n");
    return 1;
}

blush() {
    if (ghost())
	return 0;
    write("Your cheeks are burning.\n");
    say(cap_name + " blushes.\n");
    return 1;
}

bounce() {
    if (ghost())
	return 0;
    write("B O I N G !!\n");
    say(cap_name + " bounces around.\n");
    return 1;
}

bow(str) {
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

burp() {
    if (ghost())
	return 0;
    write("Excuse yourself!\n");
    say(cap_name + " burps rudely.\n");
    return 1;
}

cackle() {
    if (ghost())
	return 0;
    write("You cackle gleefully.\n");
    say(cap_name + " throws the head back and cackles with glee!.\n");
    return 1;
}

chuckle() {
    if (ghost())
	return 0;
    write("You chuckle politely.\n");
    say(cap_name + " chuckles politely.\n");
    return 1;
}

clap() {
    if (ghost())
	return 0;
    write("You clap briefly.\n");
    say(cap_name + " claps.\n");
    return 1;
}

comfort(str) {
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

cough() {
    if (ghost())
	return 0;
    write("Cover your mouth when you do that!\n");
    say(cap_name + " coughs noisily.\n");
    return 1;
}

cry() {
    if (ghost())
	return 0;
    write("Waaaaah!\n");
    say(cap_name + " bursts into tears.\n");
    return 1;
}

cuddle(str) {
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

curtsey() {
    if (ghost())
	return 0;
    write("You curtsey gracefully.\n");
    say(cap_name + " curtseys gracefully.\n");
    return 1;
}

dance(str) {
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

fart() {
    if (ghost())
	return 0;
    write("How rude!\n");
    say(cap_name + " lets off a real rip-roarer.\n");
    return 1;
}

flip() {
    if (ghost())
	return 0;
    write("You flip head over heels.\n");
    say(cap_name + " flips head over heels.\n");
    return 1;
}

fondle(str) {
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

french(str) {
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

frown() {
    if (ghost())
	return 0;
    write("Is something wrong?\n");
    say(cap_name + " frowns.\n");
    return 1;
}

gasp() {
    if (ghost())
	return 0;
    write("You gasp in astonishment.\n");
    say(cap_name + " gasps in astonishment!\n");
    return 1;
}

giggle() {
    if (ghost())
	return 0;
    write("You giggle inanely.\n");
    say(cap_name + " giggles inanely.\n");
    return 1;
}

glare(str) {
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

grin() {
    if (ghost())
	return 0;
    write("You grin evilly.\n");
    say(cap_name + " grins evilly.\n");
    return 1;
}

groan() {
    if (ghost())
	return 0;
    write("You groan.\n");
    say(cap_name + " groans loudly.\n");
    return 1;
}

grope(str) {
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

growl(str) {
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

hiccup() {
    if (ghost())
	return 0;
    write("Hic!\n");
    say(cap_name + " hiccups.\n");
    return 1;
}

hug(str) {
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

kick(str) {
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

kiss(str) {
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
    if (call_other(who, "query_frog")) {
	call_other(this_player(), "frog_curse", 1);
	call_other(who, "frog_curse", 0);
	return 1;
    }
    write("You kiss " + str + ".\n");
    return 1;
}

laugh() {
    if (ghost())
	return 0;
    write("You fall down laughing.\n");
    say(cap_name + " falls down laughing.\n");
    return 1;
}

lick(str) {
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

love(str) {
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

moan() {
    if (ghost())
	return 0;
    write("You start to moan.\n");
    say(cap_name + " starts moaning.\n");
    return 1;
}

nibble(str) {
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

nod() {
    if (ghost())
	return 0;
    write("You nod solemnly.\n");
    say(cap_name + " nods solemnly.\n");
    return 1;
}

poke(str) {
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
 
pat(str) {
    object who;
    if (ghost())
	return 0;
    if (str == 0)
	return 0;
    who = present(str, environment(this_player()));
    if (!who || !living(who) || who == this_player())
	return 0;
    tell_object(who, cap_name + " pats you on the head.\n");
    say(cap_name + " pats " + str + " on the head.\n", who);
    write("You pat " + str + " on the head.\n");
    return 1;
}

pout() {
    if (ghost())
	return 0;
    write("Ah, don't take it so hard.\n");
    say(cap_name + " pouts.\n");
    return 1;
}

puke(str) {
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

purr() {
    if (ghost())
	return 0;
    write("MMMMEEEEEEEEOOOOOOOWWWWWWW!\n");
    say(cap_name + " purrs contentedly.\n");
    return 1;
}

ruffle(str) {
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

scream() {
    if (ghost())
	return 0;
    write("ARRGGGGGGHHHHHH!!!!\n");
    say(cap_name + " screams loudly!\n");
    return 1;
}

shake(str) {
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

shiver() {
    if (ghost())
	return 0;
    write("Brrrrrr!!!.\n");
    say(cap_name + " shivers from the cold.\n");
    return 1;
}

shrug() {
    if (ghost())
	return 0;
    write("You shrug.\n");
    say(cap_name + " shrugs helplessly.\n");
    return 1;
}

sigh() {
    if (ghost())
	return 0;
    write("You sigh.\n");
    say(cap_name + " sighs deeply.\n");
    return 1;
}

sing() {
    if (ghost())
	return 0;
    write("Oh sole mio!\n");
    say(cap_name + " sings in Italian.\n");
    return 1;
}

slap(str) {
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

smirk() {
    if (ghost())
	return 0;
    write("You smirk.\n");
    say(cap_name + " smirks.\n");
    return 1;
}

smile(str) {
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

snap() {
    if (ghost())
	return 0;
    write("You snap your fingers.\n");
    say(cap_name + " snaps with the fingers.\n");
    return 1;
}

sneeze() {
    if (ghost())
	return 0;
    write("Gazundheit!\n");
    say(cap_name + " sneezes.\n");
    return 1;
}

snicker() {
    if (ghost())
	return 0;
    write("You snicker.\n");
    say(cap_name + " snickers.\n");
    return 1;
}

sniff() {
    if (ghost())
	return 0;
    write("You sniff.\n");
    say(cap_name + " sniffs.\n");
    return 1;
}

snore() {
    if (ghost())
	return 0;
    write("Zzzzzzzzzz...\n");
    say(cap_name + " snores loudly.\n");
    return 1;
}

snuggle(str) {
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

spit(str) {
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
    tell_object(who, cap_name + " spits on you!.\n");
    write("You spit on " + str +".\n");
    say(cap_name + " spits on " + str + ".\n", who);
    return 1;
}

squeeze(str) {
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

stare(str) {
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

strut() {
    if (ghost())
	return 0;
    write("Strut your stuff!\n");
    say(cap_name + " struts proudly.\n");
    return 1;
}

sulk() {
    if (ghost())
	return 0;
    write("You sulk.\n");
    say(cap_name + " sulks in the corner.\n");
    return 1;
}

thank(str) {
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

twiddle() {
    if (ghost())
	return 0;
    write("You twiddle your thumbs.\n");
    say(cap_name + " twiddles the thumbs.\n");
    return 1;
}

whistle(str) {
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

wiggle() {
    if (ghost())
	return 0;
    write("You wiggle your bottom.\n");
    say(cap_name + " wiggles the bottom.\n");
    return 1;
}

wink(str) {
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

yawn() {
    if (ghost())
	return 0;
    write("My, what big teeth you have!\n");
    say(cap_name + " yawns.\n");
    return 1;
}

