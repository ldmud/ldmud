#include "../../security.h"
#include "handshake.h"
object myself,talkee;
int is_invis,level,alignment,muffled,time_shut;
string msgin,msgout,mmsgin,mmsgout,msghome,name,cap_name,it,title,al_title;
object grantee;
string log,pwd,castle;
reset(arg) {
/*    if (arg && (environment() != myself)) {
        destruct(this_object());
        tell_object(myself, "Due to your soul leaving your body" +
            " it has been destroyed\n");
    }
*/if (arg) return;
    myself = this_player();
    restore_object("players/" + call_other(myself,"query_real_name",0));
    myself = this_player();
}

get() {
    return 1;
}

drop() { return 1; }

id(str) { return str == "wiz_soul" || str == "ND"; }

long() {
    write("Even wizard souls are transparent.\n");
}

init() {
    if (myself != this_player()) return 0; 
    soul_init();

    pwd = "/players/"+name+"/";
    castle = "/players/" + name + "/castle";
    log = "/log/" + name;
    call_other(myself,"set_pwd",pwd);

    if (level >= ECHO) {
        add_action("echo_to"); add_verb("echoto");
        add_action("echo"); add_verb("echo");
        add_action("echo_all"); add_verb("echoall");
    }

    if (level >= EXPLORE) {
        add_action("teleport"); add_verb("goto");
        add_action("list_files"); add_verb("ls");
        add_action("cat_file"); add_verb("cat");
        add_action("origin_object"); add_verb("origin");
        add_action("where"); add_verb("where");
	add_action("castle"); add_verb("castle");
    }

    if (level >= SENIOR) {
        add_action("grant_level"); add_verb("promote");
	add_action("work"); add_verb("work");
    }

    if (level >= CREATE) {
        add_action("stat"); add_verb("stat");
        add_action("clone"); add_verb("clone");
        add_action("destruct_local_object"); add_verb("dest");
        add_action("destruct_local_object"); add_verb("destruct");
        add_action("load"); add_verb("load");
        add_action("reset_object"); add_verb("reset");
        add_action("update_object"); add_verb("update");
        add_action("remove_file"); add_verb("rm");
        add_action("edit"); add_verb("ed");
        add_action("home"); add_verb("home");
        add_action("local_commands"); add_verb("localcmd");
	add_action("ear_muffs"); add_verb("earmuffs");
        add_action("log"); add_verb("log");
	add_action("cd"); add_verb("cd");
    }

    if (level >= OUT_OF_BODY) {
        add_action("in_room"); add_verb("in");
        add_action("at_player"); add_verb("at");
    }

    if (level >= SHUTDOWN) {
        add_action("shut_down_game"); add_verb("shutdown");
        add_action("adjust_time"); add_verb("time");
    }

    /* any wizard can do these for effect */

        add_action("wiz_score_list"); add_verb("wizlist");
        add_action("setmmin"); add_verb("setmmin");
        add_action("setmmout"); add_verb("setmmout");
        add_action("setmin"); add_verb("setmin");
        add_action("setmout"); add_verb("setmout");
        add_action("setmhome"); add_verb("setmhome");
        add_action("review"); add_verb("review");
        add_action("emote"); add_verb("emote");
        add_action("set_alignment"); add_verb("setal");
        add_action("set_alignment_num"); add_verb("setaln");
        add_action("list_peoples"); add_verb("people");
	add_action("light"); add_verb("light");
        add_action("vis"); add_verb("vis");
	add_action("set_wc"); add_verb("hands");
	/* This help can override the player.c help when appropriate. */
	add_action("wizhelp");	add_verb("help");

    /* end any wizard section */

    if (level >= TITLE) {
        add_action("set_title"); add_verb("title");
    }

    if (level >= CONTROL) {
        add_action("force_player"); add_verb("force");
        add_action("trans"); add_verb("trans");
        if (level >= ITEM_OVER) {
	    add_action("power_trans"); add_verb("trans!");
	}
    }

    if (level >= DAMAGE) {
        add_action("spell_zap"); add_verb("zap");
        add_action("heal"); add_verb("heal");
    }

    if (level >= SNOOP) {
        add_action("snoop_on"); add_verb("snoop");
    }

    if (level >= INVIS) {
        add_action("invis"); add_verb("invis");
    }

    if (level >= ITEM_OVER) {
        add_action("enter");  add_verb("climbin");
        add_action("exit");  add_verb("exit");
        add_action("power_get"); add_verb("get!");
	add_action("power_drop"); add_verb("drop!");
	add_action("power_give"); add_verb("give!");
	add_action("power_put"); add_verb("put!");
    }

}

emote(str) {
    if (level < 20) return 0;
    if (!str) {
	write("emote what ?\n");
	return 1;
    }
    call_other(myself,"remote_say",cap_name + " " + str + "\n");
    return 1;
}

set_alignment(al) {
    if (level < 20) return 0;
    al_title = al;
    call_other(myself,"update",8);
}

/* Added by Drax */
set_alignment_num(al) {
    int new_aln;

    if (level < 20) return 0;
    if (sscanf(al,"%d",new_aln) == 1) {
	alignment = new_aln;
	write("Alignment reset.\n");
    }
    else
	write("Alignment specified is not valid.\n");
    call_other(myself,"update",7);
    return 1;
}


list_peoples() {
    if (level < 20) return 0;
    people();
    return 1;
}


heal(name)
{
    object ob;
    if (!name || level < DAMAGE)
	return 0;
    it = lower_case(name);
    ob = find_living(it);
    if (!ob) {
	write("No such person is playing now.\n");
	return 1;
    }
    call_other(ob, "heal_self", 100000);
    if (call_other(myself,"query_invis",0) < INVIS_ACTION)
        tell_object(ob, "You are healed by " + cap_name + ".\n");
    write("Ok.\n");
    return 1;
}


stat(name)
{
    object ob;

    if (level < CREATE) return 0;
    it = lower_case(name);
    ob = find_living(it);
    if (!ob) {
	write("No such person is playing now.\n");
	return 1;
    }
    call_other(ob, "show_stats", 0);
    return 1;
}


shut_down_game(arg)
{
    string str;

    if (level < SHUTDOWN) return 0;
    str = arg;
    if (!str) str = "5";
    if (lower_case(str) == "now") shutdown();
    if (sscanf(str,"%d",time_shut) != 1) {
        write("Not a valid time.\n");
        return 1;
    }
    shout("!Game is being shut down by " + cap_name + " in " + str + " minutes.\n");
    write("Game will shut down in " + time_shut + " minutes.\n");
    log_file("SHUTDOWN","Game is being shut down by " + capitalize(name) + " in " + str + " minutes.\n");
    write("If you have not done so, do an emergency giving the reason.\n");
    time_shut *= 20;
    set_heart_beat(1);
    write("Auto shutdown started.  Type soul off to cancel.\n");
    return 1;
}

power_trans(str) { return trans("! "+str); }
power_get(str) { return call_other(myself,"pick_up","! "+str); }
power_drop(str) { return call_other(myself,"drop_thing","! "+str); }
power_put(str) { return call_other(myself,"put","! "+str); }
power_give(str) { return call_other(myself,"give_object","! "+str); }

trans(str)
{
    object ob;
    string out,power;

    if (!str || level < CONTROL)
	return 0;
    if (level >= ITEM_OVER)
        if (sscanf(str,"! %s",power) == 1)
            str = power;
    ob = find_player(str);
    if (!ob) ob = find_living(str);
    if (!ob) {
	write("No such living thing.\n");
	return 1;
    }
    it = str;
    tell_object(ob, "You are magically transfered somewhere.\n");
    out = call_other(ob,"query_mmsgin",0);
    if (!out)
	out = call_other(ob, "query_name", 0) +
	    " arrives in a puff of smoke.\n";
    else
	out = call_other(ob, "query_name", 0) + " " + out + ".\n";
    tell_room(environment(myself),out);
    if (power) {
        move_object(ob, environment());
    } else {
        move_object(ob, environment(myself));
    }
    return 1;
}


spell_zap(str)
{
    object ob;
    if (level < DAMAGE) return 0;
    if (!str)
	ob = call_other(myself,"query_attack",0);
    else
	ob = present(lower_case(str), environment(myself));
    if (!ob || !living(ob)) {
	write("At whom?\n");
	return 1;
    }
    call_other(myself,"zap_object",ob);
    return 1;
}


force_player(str)
{
    string who, what;
    object ob;
    if (!str || level < CONTROL)
	return 0;
    if (sscanf(str, "%s to %s", who, what) == 2 ||
	sscanf(str, "%s %s", who, what) == 2) {
	ob = find_living(who);
	if (!ob) {
	    write("No such player.\n");
	    return 1;
	}
	call_other(ob, "force_us", what);
	write("Ok.\n");
	return 1;
    }
    return 0;
}

clone(str) {
    object ob;
    if (level < CREATE) return 0;
    if (!str) {
	write("Clone what object?\n");
	return 1;
    }
    str = valid_read(str);
    if (!str) {
	write("Invalid file.\n");
	return 1;
    }
    call_other(myself,"checked_say",cap_name + " fetches something from another dimension.\n");
    ob = clone_object(str);
    if (call_other(ob, "get")) {
	call_other(myself,"add_weight",call_other(ob, "query_weight"));
	move_object(ob, myself);
    } else {
	move_object(ob, environment(this_player()));
    }
    write("Ok.\n");
    return 1;
}

destruct_local_object(str)
{
    object ob;
    if (level < CREATE) return 0;
    if (!str) {
	write("Destruct what?\n");
	return 1;
    }
    str = lower_case(str);
    if (str == "all") {
        destruct_inventory();
	return 1;
    }
    ob = present(str, myself);
    if (!ob)
	ob = present(str, environment(myself));
    if (!ob) {
	write("No " + str + " here.\n");
	return 1;
    }
    call_other(myself,"checked_say",call_other(ob, "short") + " is disintegrated by " + cap_name + ".\n");
    destruct(ob);
    write("Ok.\n");
    return 1;
}

destruct_inventory() {
    object ob,player;
    object next_ob;
    string it;
        ob = first_inventory(myself);
    while(ob) {
        string out;
        next_ob = next_inventory(ob);
        it = call_other(ob, "short", 0);
        if (!call_other(ob,"id","ND")) {
            destruct(ob);
            write("destruct: " + it +".\n");
        }
        ob = next_ob;
    }
}

load(str)
{
    object env;
    if (level < CREATE) return 0;
    if (!str) {
	write("Load what?\n");
	return 1;
    }
    str = valid_read(str);
    if (!str) {
	write("Invalid file name.\n");
	return 1;
    }
    env = environment(myself);
    move_object(myself, str);
    move_object(myself, env);
    write("Ok.\n");
    return 1;
}

snoop_on(str)
{
    if (level < SNOOP) return 0;
    call_other(myself,"remote_snoop",set_handshake(str));
    return 1;
}

invis(str)
{
    int invis;
    
    if (level < INVIS) return 0;
    if (str) {
	sscanf(str,"%d",invis);
    } else {
	invis = 100;
    }
    if (!invis) return 0;
    if (invis >= 100 && level < ALL_POWER)
       invis=100;
    if (invis >= INV7 && level < CINV7) invis = INV7 - 1;
    if (invis >= INV6 && level < CINV6) invis = INV6 - 1;
    if (invis >= INV5 && level < CINV5) invis = INV5 - 1;
    if (invis >= INV4 && level < CINV4) invis = INV4 - 1;
    if (invis >= INV3 && level < CINV3) invis = INV3 - 1;
    if (invis >= INV2 && level < CINV2) invis = INV2 - 1;
    if (invis >= INV1 && level < CINV1) invis = INV1 - 1;
    is_invis = invis;
    call_other(myself,"update",5);
    write("You are now invisible = "+is_invis+".\n");
    if (is_invis < INVIS_ACTION) call_other(myself,"remote_say",cap_name + " disappears.\n");
    if (is_invis >= SOMEONE) cap_name = "Someone";
    return 1;
}

vis()
{
    if (level < INVIS) return 0;
    if (!is_invis) {
	write("You are not invisible.\n");
	return 1;
    }
    is_invis = 0;
    call_other(myself,"update",5);
    write("You are now visible.\n");
    cap_name = capitalize(name);
    call_other(myself,"remote_say",cap_name + " appears in puff of smoke.\n");
    return 1;
}

home() {
    object old_env;
    
    if (level < CREATE) return 0;
    if (call_other(myself,"query_invis",0) < INVIS_TELEPORT)
        call_other(myself,"remote_say",cap_name + " " + msghome + ".\n");
    move_object(myself,"players/" + name + "/workroom");
    call_other(myself,"look",0);
    return 1;
}


wiz_score_list() {
    if (level < 20) return 0;
    wizlist();
    return 1;
}

remove_file(str) {
    if (level < CREATE) return 0;
    if (!str)
        return 0;
    rm(str);
    return 1;
}

local_commands() {
    if (level < CREATE) return 0;
    localcmd();
    write("\n");
    return 1;
}

set_title(t) {
    if (level < TITLE) {
	write("You must be of level " + TITLE + " to do that.\n");
	return 0;
    }
    call_other(myself, "set_title", t);
    return 1;
}

/* Some functions to set moving messages. */

setmin(m) {
    if (level < 20) return 0;
    msgin = m;
    call_other(myself,"update",1);
    return 1;
}

setmmin(m) {
    if (level < 20) return 0;
    mmsgin = m;
    call_other(myself,"update",2);
    return 1;
}

setmout(m) {
    if (level < 20) return 0;
    msgout = m;
    call_other(myself,"update",3);
    return 1;
}

setmmout(m) {
    if (level < 20) return 0;
    mmsgout = m;
    call_other(myself,"update",4);
    return 1;
}

setmhome(m) {
    if (level < 20) return 0;
    msghome = m;
    call_other(myself,"update",9);
    return 1;
}


review() {
    if (level < 20) return 0;
    write("mout:\t" + msgout +
	  "\nmin:\t" + msgin +
	  "\nmmout:\t" + mmsgout +
	  "\nmmin:\t" + mmsgin + 
	  "\nmhome:\t" + msghome + "\n");
    return 1;
}


echo(str) {
    if (level < ECHO) return 0;
    if (!str) {
       write ("Echo what?\n");
       return 1;
    }
    say (str + "\n");
    write ("You echo: " + str + "\n");
    return 1;
}

echo_to(str)
{
    object ob;
    string who;
    string msg;
    if (level < ECHO) return 0;
if (!str || sscanf(str, "%s %s", who, msg) != 2) {
	write("Echoto what ?\n");
	return 1;
    }
    it = lower_case(who);
    ob = find_living(it);
    if (!ob) {
	write("No player with that name.\n");
	return 1;
    }
    tell_object(ob, msg + "\n");
    write("You echo: " + msg + "\n");
    return 1;
}

echo_all(str) {
    if (level < ECHO) return 0;
    if (!str) {
       write("Echoall what?\n");
       return 1;
    }
    shout(str + "\n");
    write("You echo: " + str + "\n");
    return 1;
}


teleport(dest) {
    object ob,old_env;
    if (level < EXPLORE) return 0;
    if (!dest) {
	write("Goto where ?\n");
	return 1;
    }
    ob = find_player(dest);
    if (!ob) ob = find_living(dest);
    if (ob) {
	ob = environment(ob);
        if (call_other(myself,"query_invis",0) < INVIS_TELEPORT)
	    call_other(myself,"remote_say",cap_name + " " + mmsgout + ".\n");
	old_env = environment(myself);
        move_object(myself, ob);
        if (call_other(myself,"query_invis",0) < INVIS_TELEPORT)
            call_other(myself,"remote_say",cap_name + " " + mmsgin + ".\n");
	write(call_other(ob, "short") + ".\n");
	return 1;
    }
    dest = valid_read(dest);
    if (!dest) {
	write("Invalid monster name of file name.\n");
	return 1;
    }
    call_other(myself,"move_player","X#" + dest);
    return 1;
}


in_room(str)
{
    object room;
    object old_room;
    string cmd;
    if (!str || level < OUT_OF_BODY)
	return 0;
    if (sscanf(str, "%s %s", room, cmd) != 2) {
	write("Usage: in ROOM CMD\n");
	return 1;
    }
    room = valid_read(room);
    if (!room) {
	write("Invalid file name.\n");
	return 1;
    }
    old_room = environment(myself);
    move_object(myself, room);
    call_other(myself,"remote_cmd",set_handshake(cmd));
    move_object(myself, old_room);
    return 1;
}

at_player(str)
{
    object other_player;
    object old_room;
    string cmd, who;
    if (!str || level < OUT_OF_BODY)
	return 0;
    if (sscanf(str, "%s %s", who, cmd) != 2) {
	write("Usage: at PLAYER CMD\n");
	return 1;
    }
    other_player = find_living(who);
    if (!other_player) {
      write("There is no living creature named " + who + ".\n");
      return 1;
    }
    old_room = environment(myself);
    move_object(myself, environment(other_player));
    call_other(myself,"remote_cmd",set_handshake(cmd));
    move_object(myself, old_room);
    return 1;
}



reset_object(str) {
    object ob;
    if (level < CREATE) return 0;
    if (!str) {
	write("Update what object ?\n");
	return 1;
    }
    str = valid_read(str);
    if (!str) {
	write("Invalid file name.\n");
	return 1;
    }
    ob = find_object(str);
    if (!ob) {
	write("No such object.\n");
	return 1;
    }
    call_other(ob, "reset", 0);
    return 1;
}

update_object(str) {
    object ob;
    if (level < CREATE) return 0;
    if (!str) {
	write("Update what object ?\n");
	return 1;
    }
    str = valid_read(str);
    if (!str) {
	write("Invalid file name.\n");
	return 1;
    }
    ob = find_object(str);
    if (!ob) {
	write("No such object.\n");
	return 1;
    }
    destruct(ob);
    write(str + " will be reloaded at next reference.\n");
    return 1;
}

edit(file)
{
    if (level < CREATE) return 0;
    if (!file) {
	write("Edit what file ?\n");
	return 1;
    }
    call_other(myself,"remote_ed",set_handshake(file));
    return 1;
}

list_files(path)
{
    if (level < EXPLORE) return 0;
    ls(path);
    return 1;
}

cat_file(path)
{
    if (level < EXPLORE) return 0;
    if (!path)
	return 0;
    cat(path);
    return 1;
}

origin_object(str) {
    object ob;
    
    if (!str || level < EXPLORE) return 0;
    ob = present(str);
    if (!ob) ob = present(str,environment(myself));
    if (!ob) return 0;
    write(ob);
    write("\n");
}

where(str) {
    object ob;

    if (!str || level < EXPLORE) return 0;
    ob = find_player(str);
    if (!ob) ob = find_living(str);
    if (!ob) ob = find_object(str);
    if (!ob) {
        write(str + " not found.\n");
	return 1;
    }
    ob = environment(ob);
    if (!ob) {
        write(str + " is not in a place.\n");
	return 1;
    }
    write(ob);
    write("\n"+call_other(ob,"short",0)+".\n");
}

light(str) {
    if (level < 20) return 0;
     if (!str) return 0;
     if (str == "on") set_light(1);
     if (str == "off") set_light(-1);
     return 1;
}

enter(str) {
    object ob;

    if (level < ITEM_OVER) return 0;
    if (!str) return 0;
    ob = present(str,environment(myself));
    if (!ob) return 0;
    move_object(myself,ob);
    call_other(myself,"look",0);
}

exit() {
    object ob;

    if (level < ITEM_OVER) return 0;
    ob = environment(environment(myself));
    if (ob) {
        move_object(myself,ob);
	call_other(myself,"look",0);
    } else {
        write("Nowhere to exit to.\n");
    }
    return 1;
}

grant_level(str) {
    int rec_lev;
    
    grantee = present(str,environment(myself)) /*find_player(str)  uncomment when command avail*/;
    if (!grantee) {
        write(str+" is not within range.\n");
	return 1;
    }
    rec_lev = call_other(grantee,"query_level",0);
    if (rec_lev >= GOD) {
        write("You can't mess with a god!\n");
	return 1;
    }
    if (rec_lev > level) {
        write("You can't change a more powerful wizard!\n");
	return 1;
    }
    if (level < SENIOR) {
        write("You are still a junior wizard and can't grant power!\n");
	return 1;
    }
    if ((level < GOD && rec_lev >= ELDER) ||
        (level < ELDER && rec_lev >= SENIOR)) {
	    write("You may not grant power to your peers\n");
	    return 1;
    }
    write("What level do you want to grant "+str +"?\n"+
          str + " is level "+rec_lev+" now.\n");
    input_to("grant_level2");
}

grant_level2(str) {
    int rec_lev;
    if (!str) {
        grantee = 0;
	write("Aborted.\n");
	return 1;
    }
    if (!grantee || this_player() != myself) {
        write("Level setting eror.\n");
	illegal_patch("grant_level2");
    }
    sscanf(str,"%d",rec_lev);
    if (!rec_lev) {
        write("Invalid integer.\n");
	return 1;
    }
    if (rec_lev<20 && level < GOD) {
        write("Only a god can take away wizard status");
	return 1;
    }
    if ((level < GOD && rec_lev >= ELDER) ||
        (level < ELDER && rec_lev >= SENIOR)) {
	    write("You may not promote someone to the status of a peer"+
	          " or higher.\n");
 	    return 1;
    }
    call_other(grantee,"set_wiz_level",set_handshake(str));
    write("level "+str+" granted.\n");
}
	    
valid_read(str) { return call_other(myself,"valid_read",str); }

valid_write(str) { return call_other(myself,"valid_write",str); }

update(num){
    if (num == 1) level = call_other(myself,"query_level",0);
}

set_wc(str) {
    int wc,num;

    sscanf(str,"%d",num);
    wc = num;
    if ((num > 10 + level/2) && (level < ALL_POWER)) {
        wc = 10 + level / 2;
        write("weapon class "+num+" too high setting weapon class" + wc);
    }
    call_other(myself,"set_wc",wc);
    return 1;
}

ear_muffs(str) {
    if (level < CREATE) return 0;
     if (!str) return 0;
     if (str == "on") muffled = 1;
     if (str == "off") muffled = 0;
     call_other(myself,"update",6);
     return 1;
}

query_msgin() { return msgin; }
query_msgout() { return msgout; }
query_mmsgin() { return mmsgin; }
query_mmsgout() { return mmsgout; }
query_msghome() { return msghome; }
query_invis() { return is_invis; }
query_alignment() {return alignment;}
query_al_title() {return al_title;}
query_muffled() { return muffled; }


wizhelp(what) {
    if (!what)	{
	write ("Type 'help wizard' for a list of wizard commands.\n");
	return 0;   /* Return normal help */
    }
    if (what == "wizard") {
	write("Do 'cat /doc/w/<topic>' for more information.\n\n");
	write("Do 'help wiz_levels' for a list of levels necessary for certain functions.\n");
	if (level >= EXPLORE) {
	    write("goto <room/player>   -- go to the specified room or player.\n");
	    write("cat <file>           -- look at the specified file.\n");
	    write("ls <path>            -- list the files in the specified directory.\n");
	    write("origin <obj>         -- give the object handle of the specified object.\n");
	    write("where <obj>          -- give the environment of the specified object.\n");
	    write("castle <name>        -- goes to a wizard's castle\n");
	}
	if (level >= ECHO) {
	    write("echo, echoto, echoall-- make the argument appear to the appropriate group.\n");
	}
	if (level >= SENIOR) {
	    write("promote <wizard>     -- change a wizards level.\n");
	    write("work <name>          -- change pwd, castle, and log defaults\n"); 
	}
	if (level >= CREATE) {
	    write("clone <object>       -- create the object in your inventory.\n");
	    write("dest(ruct) <object>  -- destroy the specified object completely.\n");
	    write("load <room>          -- load the specified room (or object)\n");
	    write("reset <object/room>  -- cause the specified object to experience a reset.\n");
	    write("update <object/room> -- remove the specified object and delete the .i file\n");
	    write("rm <file>            -- delete the specified file.\n");
	    write("ed <file>            -- edit the specified file.\n");
	    write("home                 -- take you to your workroom, if you have a workroom.c\n");
	    write("localcmd             -- list all commands you can currently do.\n");
            write("earmuffs             -- makes you not hear shouts by players.\n");
            write("cd <dir>             -- change directory (supports ~ and ~name)\n");
	    write("log <file>           -- cat a log file\n");
	    write("stat <living>        -- give statistics on player or monster.\n");
	}
	if (level >= OUT_OF_BODY) {
	    write("in <room> <action>   -- perform the action in the room.\n");
	    write("at <living> <action> -- perform the action at the player or monster.\n");
	}
	if (level >= SHUTDOWN) {
	    write("shutdown <num>/now   -- kick all players off and kill the game in <num> minutes.\n");
            write("time <num>           -- check or adjust shutdown time.\n");
	}
	if (level >= CONTROL) {
	    write("force <player> <act> -- force a player to do something.\n");
	    write("trans <player>       -- bring the player to you.\n");
	}
	if (level >= DAMAGE) {
	    write("zap/heal <living>    -- kill or heal the player or monster.\n");
	}
	if (level >= SNOOP) {
	    write("snoop <player>       -- see everything the player does.\n");
	}
	if (level >= INVIS) {
	    write("invis <num>/vis      -- turn invisible or visible.\n");
	}
	if (level >= ITEM_OVER) {
	    write("climbin <obj>/exit   -- enter or leave an object.\n");
	    write("get!,drop!,give!,put!-- do action even if you normally couldn't.\n");
	}
        write("wizlist              -- list wizards' rankings.\n");
	write("setmmin, setmmout, setmin, setmout, setmhome, review\n");
	write("                     -- deal with travel messages.\n");
	write("emote <arg>          -- print your name followed by the argument in the room.\n");
	write("setal, setaln        -- set the name and number of your alignment.\n");
	write("people               -- list all players and their location.\n");
	write("light <on/off>       -- make yourself a light source (or not).\n");
	write("hands <num>          -- set your own weapon class.\n");
	return 1;
    }
    if (what == "wiz_levels") {
        write("Beginning Wizards can: wizlist,setmmin, setmmout, setmin,setmout, setmhome, \n");
	write("                         review,emote,setal,setaln,people,light,hands\n");
	if (level >= EXPLORE) {
	    write("Level  "+EXPLORE+" Wizards can: goto,cat,ls,origin,where,castle\n");
	}
	if (level >= CREATE) {
	    write("Level  "+CREATE+" Wizards can: clone,dest,destruct,load,reset,update,rm,\n");
	    write("                         ed,home,localcmd,earmuffs,cd,log,stat\n");
	}
	if (level >= TITLE) {
	    write("Level  "+TITLE+" Wizards can: title\n");
	}
	if (level >= INVIS) {
	    write("Level  "+INVIS+" Wizards can: invis,vis\n");
	}
	if (level >= STAT) {
	    write("Level  "+STAT+" Wizards can: read other wizard's files\n");
	    write("                         get current hp with stat\n");
	}
	if (level >= OUT_OF_BODY) {
	    write("Level  "+OUT_OF_BODY+" Wizards can: in,at\n");
	}
	if (level >= CONTROL) {
	    write("Level  "+CONTROL+" Wizards can: force,trans\n");
	}
	if (level >= SNOOP) {
	    write("Level  "+SNOOP+" Wizards can: snoop\n");
	}
	if (level >= DAMAGE) {
	    write("Level  "+DAMAGE+" Wizards can: zap,heal\n");
	}
	if (level >= ITEM_OVER) {
	    write("Level  "+ITEM_OVER+" Wizards can: climbin,exit,get!,drop!,give!,put!,\n");
	    if (level >= CONTROL) {
	        write("                         trans!\n");
	    }
	}
	if (level >= SENIOR) {
	    write("Level "+SENIOR+" Wizards can: edit other wizard's files\n");
	    write("                         promote,work\n");
	}
	if (level >= ECHO) {
	    write("Level "+ECHO+" Wizards can: echo, echoto, echoall\n");
	}
	if (level >= ELDER) {
	    write("Level "+ELDER+" Wizards can: edit log files\n");
	}
	if (level >= SHUTDOWN) {
	    write("Level "+SHUTDOWN+" Wizards can: shutdown,time\n");
	}
        return 1;
    }
}

heart_beat() {
    int interval;
    time_shut -= 1;
    if (time_shut == 0) {
        log_file("SHUTDOWN","Shutdown complete.\n");
	call_other(myself,"remote_cmd",set_handshake("shutdown now"));
	tell_object(myself,"The shutdown didn't work.\n");
	set_heart_beat(0);
    }
    interval = 20;
    if (time_shut > 200) interval = 100;
    if (time_shut > 600) interval = 200;
    if (time_shut > 1200) interval = 600;
    if (!(time_shut % interval)) shout("!The game will be shut down in "+time_shut/20+" minutes.\n"); 
    return;
}

adjust_time(new_time) {
    string str;
    int temp;
    
    if (level < SHUTDOWN) return 0;
    if (!new_time) {
        write(time_shut/1200 + ":"+(time_shut%1200)/20+":"+(time_shut%20)*3+" left to shutdown.\n");
	return 1;
    }
    str = new_time;
    if (!str) str = "5";
    if (lower_case(str) == "now") shutdown();
    if (sscanf(str,"%d",temp) != 1) {
        write("Not a valid time.\n");
        return 1;
    }
    time_shut = temp;
    write("Shutdown rescheduled for " + time_shut + " minutes.\n");
    shout("!Shutdown rescheduled for "+time_shut+" minutes.\n");
    log_file("SHUTDOWN","Shutdown rescheduled for "+time_shut+" minutes.\n");
    time_shut *= 20;
    return 1;
}

log (str) {
    if (level < CREATE) return 0;
    if (str)
        log = "/log/"+str;
    cat(log);
    return 1;
}

castle (str) {
    if (level < EXPLORE) return 0;
    if (str)
        castle = "/players/"+str+"/castle";
    /* do it this way to be sure that all is loaded */
    teleport(castle);
    exit();
}

work(str) {
    if (level < SENIOR) return 0;
    if (!str) str=name;
        write("Working on the property of "+str+"\n");
    pwd = "/players/"+str+"/";
    castle = "/players/" + str + "/castle";
    log = "/log/"+str;
    call_other(myself,"set_pwd",pwd);
    return 1;
}

cd(str) {
    string temp,junk;
    if (level < CREATE) return 0;
    if (str) {
        if (str == "~") 
	    str = "/players/" + name;
	if (sscanf(str,"~%s",temp) == 1)
	    str = "/players/" + temp;
	temp = call_other(myself,"valid_read",str+"/");
	if (!temp) {
	    write("Illegal path!\n");
	    return 1;
	}
	pwd = temp;
        call_other(myself,"set_pwd",pwd);
    }
    write("pwd: " + pwd + "\n");
    return 1;
}
    

/*-------------------------------------------------------------------*/
/*                           soul                                    */
/*-------------------------------------------------------------------*/

ghost() {
    return call_other(this_player(), "query_ghost");
}

#include "soul_com.c"
