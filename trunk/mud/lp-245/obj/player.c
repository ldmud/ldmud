#include "input_to.h"
#include "log.h"
#include "living.h"

#define WIZ 1
#define ARCH 0

static object myself;                /* Ourselfs. */
string title;                /* Our official title. Wiz's can change it. */
string password;        /* This players crypted password. */
static string password2;        /* Temporary when setting new password */
string al_title;
int intoxicated;        /* How many ticks to stay intoxicated. */
int stuffed;                /* How many ticks to stay stuffed */
int soaked;                /* How many ticks to stay soaked */
int headache, max_headache;
string called_from_ip;        /* IP number was used last time */
string quests;                /* A list of all quests */
static int time_to_save;        /* Time to autosave. */

static mixed saved_where;     /* Temp... */
string mailaddr;        /* Email address of player */
static string it;                /* Last thing referenced. */
int tot_value;                /* Saved values of this player. */
static string current_path;        /* Current directory */
string access_list;        /* What extra directories can be modified */
int stats_is_updated;

#define MAX_SCAR        10
int scar;

static void move_player_to_start(mixed where);
static void move_player_to_start2(mixed where);
static void move_player_to_start3(mixed where);
void save_me(int value_items);
int illegal_patch(string what);
void add_standard_commands();
static void wiz_commands();
static void wiz_commands2();
mixed valid_read(string str, int lvl);
mixed valid_write(string str);
void drop_all(int verbose);
static void get_all(object from);
static int pick_item(string obj);
int drop_one_item(object ob);
void load_auto_obj(string str);
void compute_auto_str();
int vis();
string check_access_list(string top, string dir, string file);

/* Some functions to set moving messages. */

int setmout(string m) { msgout = m; return 1; }
int setmin(string m) { msgin = m; return 1; }
int setmmout(string m) { mmsgout = m; return 1; }
int setmmin(string m) { mmsgin = m; return 1; }

int review() {
    write("mout:\t" + msgout +
          "\nmin:\t" + msgin +
          "\nmmout:\t" + mmsgout +
          "\nmmin:\t" + mmsgin + "\n");
    return 1;
}

string query_msgin() { return msgin; }
string query_msgout() { return msgout; }
string query_mmsgin() { return mmsgin; }
string query_mmsgout() { return mmsgout; }


string version() {
    return "2.04.05";
}

/* logon() is called when the players logges on. */

static int logon() {
    time_to_save = 500;
    /* enable_commands(); */
    cat("/WELCOME");
    write("Version: " + version() + "\n");
    write("What is your name: ");
    input_to("logon2", INPUT_PROMPT, "What is your name: ");
    call_out("time_out", 120);
    return 1;
}

object other_copy;

static void try_throw_out(string str)
{
    object ob;
    if (str == "" || (str[0] != 'y' && str[0] != 'Y')) {
        write("Welcome another time then !\n");
        destruct(this_object());
        return;
    }
    ob = first_inventory(other_copy);
    while(ob) {
        int weight;
        object next_ob;
        weight = ob->query_weight();
        next_ob = next_inventory(ob);
        /*
         * Don't move the soul.
         */
        if (!ob->id("soul") && add_weight(weight)) {
            ob->drop();
            if (ob)
                move_object(ob, this_player());
        }
        ob = next_ob;
    }
    ob = environment(other_copy);
    other_copy->quit();
    if (restore_object("players/" + name))
        write("Points restored from the other object.\n");
    else
        destruct(other_copy);        /* Is this really needed ? */
    other_copy = 0;
    move_player_to_start(ob);
#ifdef LOG_ENTER
    log_file("ENTER", " (throw)\n");
#endif
}

/*
 * Check that a player name is valid. Only allow
 * lowercase letters.
 */
int valid_name(string str)
{
    int i, length;
    if (str == "logon") {
        write("Invalid name");
        return 0;
    }
    length = strlen(str);
    if (length > 11) {
        write("Too long name.\n");
        return 0;
    }
    i=0;
    while(i<length) {
        if (str[i] < 'a' || str[i] > 'z') {
            write("Invalid characters in name:" + str + "\n");
            write("Character number was " + (i+1) + ".\n");
            return 0;
        }
        i += 1;
    }
    return 1;
}

static void logon2(string str) {
    if (!str || str == "") {
        destruct(this_object());
        return;
    }
    if (name != "logon") {
        illegal_patch("logon2 " + name);
        destruct(this_object());
        return;
    }
    str = lower_case(str);
    if (!valid_name(str)) {
        input_to("logon2", INPUT_PROMPT, "Give name again: ");
        return;
    }
    if (restore_object("banish/" + str)) {
        write("That name is reserved.\n");
        destruct(this_object());
        return;
    }
    if (!restore_object("players/" + str)) {
        write("New character.\n");
    }
    time_to_save = age + 500;
    /*
     * Don't do this before the restore !
     */
    name = str;                        /* Must be here for a new player. */
    dead = ghost;
    myself = this_player();
    if (is_invis)
        cap_name = "Someone";
    else
        cap_name = capitalize(name);

    local_weight = 0;
    armour_class = 0;
    name_of_weapon = 0;
    weapon_class = 0;
    /* If this is a new character, we call the adventurers guild to get
     * our first title !
     */
    if (level != -1)
        input_to("check_password", 1);
    else
        input_to("new_password", 1);
    write("Password: ");
    if (name == "guest")
        write("(just CR) ");
    attacker_ob = 0;
    alt_attacker_ob = 0;
    return;
}

/* Called by command 'save' */
int save_character() {
    save_me(1);
    write("Ok.\n");
    return 1;
}

void reset(int arg) {
    if (arg)
        return;
/*
 *   With arg = 0 this function should only be entered once!
 */
    if(myself) return;
    if (creator(this_object())) {
        illegal_patch("Cloned player.c");
        destruct(this_object());
        return;
    }
    level = -1;
    name = "logon";
    cap_name = "Logon";
    msgin = "arrives"; msgout = "leaves";
    mmsgin = "arrives in a puff of smoke";
    mmsgout = "disappears in a puff of smoke";
    title = "the title less";
    al_title = "neutral";
    gender = -1; /* Illegal value, so it will be changed! */
}

/* Enable other objects to query our hit point. */
int query_hit_point() {
    return hit_point;
}

string short() {
    if (is_invis)
        return 0;
    if (ghost)
        return "ghost of " + cap_name;
    if (frog)
        return cap_name + " the frog";
    return cap_name + " " + title + " (" + al_title + ")";
}

void show_scar() {
    int i, j, first, old_value;
    string * scar_desc;

    scar_desc = ({ "left leg", "right leg", "nose", "left arm", "right arm",
                   "left hand", "right hand", "forhead", "left cheek",
                   "right cheek" });
    j = 1;
    first = 1;
    old_value = scar;
    while(i < MAX_SCAR) {
        if (scar & j) {
            old_value &= ~j;
            if (first) {
                write(cap_name + " has a scar on " + query_possessive() +
                      " " + scar_desc[i]);
                first = 0;
            } else if (old_value) {
                write(", " + query_possessive() + " " + scar_desc[i]);
            } else {
                write(" and " + query_possessive() + " " + scar_desc[i]);
            }
        }
        j *= 2;
        i += 1;
    }
    if (!first)
        write(".\n");
}

static void make_scar() {
    if (level < 10)
        return;
    scar |= 1 << random(MAX_SCAR);
}

void long() {
    string cap_pronoun;

    cap_pronoun = capitalize(query_pronoun());
    write(short() + ".\n");
    if (ghost || frog)
        return;
    show_scar();
    if (hit_point < max_hp/10) {
        write(cap_pronoun + " is in very bad shape.\n");
        return;
    }
    if (hit_point < max_hp/5) {
        write(cap_pronoun + " is in bad shape.\n");
        return;
    }
    if (hit_point < max_hp/2) {
        write(cap_pronoun + " is not in a good shape.\n");
        return;
    }
    if (hit_point < max_hp - 20) {
        write(cap_pronoun + " is slightly hurt.\n");
        return;
    }
    write(cap_pronoun + " is in good shape.\n");
}

int score(string arg)
{

    string tmp;

    if (ghost) {
        write("You are in an immaterial state with no scores.\n");
        return 1;
    }

    if (arg)
    {
        write("Str: " + Str + "\n");
        write("Dex: " + Dex + "\n");
        write("Int: " + Int + "\n");
        write("Con: " + Con + "\n");
        return 1;
    }

    write("You have " + experience + " experience points, " +
          money + " gold coins, ");
    write(hit_point + " hit points(" + max_hp + ").\n");
    write(spell_points + " spell points.\n");
    if (hunter)
        write("You are hunted by " + hunter->query_name() + ".\n");
    if (intoxicated || stuffed || soaked)
    {
        tmp = "You are ";

        if (intoxicated)
        {
            tmp += "intoxicated";
            if (stuffed && soaked)
                tmp += ", ";
            else
            {
                if (stuffed || soaked)
                    tmp += " and ";
                else
                    tmp += ".\n";
            }
        }

        if (stuffed)
        {
            tmp += "satiated";

            if (soaked)
                tmp += " and ";
            else
                tmp += ".\n";
        }

        if (soaked)
            tmp += "not thirsty.\n";

        write(tmp);
    }

    if (whimpy)
        write("Wimpy mode.\n");
    show_age();
    return 1;
}

/* Identify ourself. */
int id(string str, int lvl) {
  /*
   *  Some wizzies make invisibility items useable by
   *  players , and this will prevent cheating.
   */
    if(level < 20)
        if(str == name || str == "ghost of " + name)
            return 1;
  /*
   *  I think this looks stupid. When I am invisible it is
   *  because I want to work in PEACE.
   */
    if (is_invis && lvl <= level)
        return 0;
    if (ghost)
        return str == "ghost of " + name;
    if (str == name)
        return 1;
    return 0;
}

string query_title() {
    return title;
}

void set_level(int lev) {
    object scroll;
    if (lev > 21 || lev < level && level >= 20)
    {
        illegal_patch("set_level");                /* NOPE ! */
        return;
    }
    level = lev;
    if (level == 20) {
        scroll = clone_object("doc/examples/init_scroll");
        move_object(scroll, myself);
        tell_object(myself, "You have been given a scroll containing valuable information. Read it now!\n");
        tell_object(myself, "Adding wizard commands...\n");
        wiz_commands();
    }
    if (level == 21) {
        tell_object(myself, "Adding more wizard commands...\n");
        wiz_commands2();
    }
}

int set_title(string t) {
    if (!t) {
        write("Your title is " + title + ".\n");
        return 1;
    }
    title = t;
    return 1;
}

static void wiz_commands2() {
    if (this_object() != this_player())
        return;
    add_action("earmuffs", "earmuffs");
    add_action("makedir", "mkdir");
    add_action("removedir", "rmdir");
    add_action("pwd", "pwd");
    add_action("more", "more");
    add_action("echo_to", "echoto");
    add_action("echo", "echo");
    add_action("echo_all", "echoall");
    add_action("home", "home");
    add_action("remove_file", "rm");
    add_action("list_files", "ls");
    add_action("cat_file", "cat");
    add_action("edit", "ed");
    add_action("clone", "clone");
    add_action("destruct_local_object", "destruct");
    add_action("load", "load");
    add_action("tail_file", "tail");
    add_action("cd", "cd");
}

static void wiz_commands() {
    if (this_object() != this_player())
        return;
    add_action("local_commands", "localcmd");
    add_action("wiz_score_list", "wizlist");
    add_action("force_player", "force");
    add_action("spell_zap", "zap");
    add_action("stat", "stat");
    add_action("heal", "heal");
    add_action("update_object", "update");
    add_action("set_title", "title");
    add_action("teleport", "goto");
    add_action("in_room", "in");
    add_action("emote", "emote");
    add_action("list_peoples", "people");
    add_action("setmmin", "setmmin");
    add_action("setmmout", "setmmout");
    add_action("setmin", "setmin");
    add_action("setmout", "setmout");
    add_action("review", "review");
    add_action("shut_down_game", "shutdown");
    add_action("trans", "trans");
    add_action("snoop_on", "snoop");
    add_action("invis", "invis");
    add_action("vis", "vis");
}

/*
  New shout routines to allow earmuffs via catch_shout()

  Possible future extension:
     filter_objects() returns an array holding the players that accepted
     the shout(), that is those who heard it.
     These could be listed to the shouter if one wishes.
*/

#define SHOUT_OLD(x) shout(x)
#define SHOUT(x) gTellstring=x; filter_objects(users(),"filter_tell",this_object())

static string gTellstring;
static int listen_to_shouts_from_level;

int filter_tell(object ob) {
    if (ob == this_player())
        return 0;
    return ob->catch_shout(gTellstring);
}

/* This is called for every shouted string to this player.
*/
int catch_shout(string str)
{
    if (this_player()->query_level() >= listen_to_shouts_from_level) {
        tell_object(this_object(),str);
        return 1;
    }
    return 0;
}

/* This is the earmuff hook. You can set the level of the players to which
   you want to listen to, to one more than your own.
   This means you can not stop higher level players from shouting to you,
   but you can stop lower levels and your own level.
 */
int listen_shout(int lev)
{
    if (lev && lev <= level+1) listen_to_shouts_from_level=lev;
    return listen_to_shouts_from_level;
}

int earmuffs(string str) {
    int lev;
    if (str && sscanf(str, "%d", lev) == 1)
        listen_shout(lev);
    write("Earmuffs at level " + listen_to_shouts_from_level + ".\n");
    return 1;
}

static int echo_all(string str) {
    if (!str) {
       write("Echoall what?\n");
       return 1;
    }
    SHOUT(str + "\n");
    write("You echo: " + str + "\n");
    return 1;
}

static int echo(string str) {
    if (!str) {
       write ("Echo what?\n");
       return 1;
    }
    say (str + "\n");
    write ("You echo: " + str + "\n");
    return 1;
}

static int echo_to(string str)
{
    object ob;
    string who;
    string msg;
    if (!str || sscanf(str, "%s %s", who, msg) != 2) {
        write("Echoto what ?\n");
        return 1;
    }
    ob = find_living(lower_case(who));
    if (!ob) {
        write("No player with that name.\n");
        return 1;
    }
    tell_object(ob, msg + "\n");
    write("You echo: " + msg + "\n");
    return 1;
}

int teleport(string dest) {
    object ob;
    if (!dest) {
        write("Goto where ?\n");
        return 1;
    }
    ob = find_living(dest);
    if (ob) {
        ob = environment(ob);
        if(!is_invis)
            say(cap_name + " " + mmsgout + ".\n");
        move_object(myself, ob);
        if(!is_invis)
            say(cap_name + " " + mmsgin + ".\n");
        if (brief)
            write(ob->short() + ".\n");
        else
            ob->long();
        ob = first_inventory(ob);
        while(ob) {
            if (ob != this_object()) {
                string short_str;
                short_str = ob->short();
                if (short_str)
                    write(short_str + ".\n");
            }
            ob = next_inventory(ob);
        }
        return 1;
    }
    dest = valid_read(dest, WIZ);
    if (!dest || file_size("/" + dest + ".c") <= 0) {
        write("Invalid monster name or file name: " + dest + "\n");
        return 1;
    }
    move_player("X#" + dest);
    return 1;
}

int quit() {
    save_me(0);
    drop_all(1);
    write("Saving "); write(capitalize(name)); write(".\n");
    if (!is_invis) {
        say(cap_name + " left the game.\n");
    }
    destruct(this_object());
    return 1;
}

int kill(string str) {
    object ob;
    if (ghost)
        return 0;
    if (!str) {
        write("Kill what ?\n");
        return 1;
    }
    ob = present(lower_case(str), environment(this_player()));
    if (!ob) {
        write("No " + str + " here !\n");
        return 1;
    }
    if (!living(ob)) {
        write(str + " is not a living thing !\n");
        say(cap_name + " tries foolishly to attack " + str + ".\n");
        return 1;
    }
    if (ob == this_object()) {
        write("What ? Attack yourself ?\n");
        return 1;
    }
    if (attacker_ob == ob) {
        write("Yes, yes.\n");
        return 1;
    }
    attack_object(ob);
    return 1;
}

int communicate(string str) {
    string verb;

    verb = query_verb();
    if (str == 0)
        str = "";
    if (verb[0] == "'"[0])
        str = verb[1..] + " " + str;
    write("Ok.\n");
    if (ghost) {
        say(short() + " says: " + str + ".\n");
        return 1;
    }
    say(cap_name + " says: " + str + "\n");
    return 1;
}

static void heart_beat() {
    if (ghost)
        return;
    age += 1;
    if (age > time_to_save) {
        if (!brief)
            write("Autosave.\n");
        save_me(1);
        time_to_save = age + 500;
    }
    if (intoxicated && random(40) == 0) {
        int n;
        n = random(7);
        if (n == 0) {
            say(cap_name + " hiccups.\n");
            write("You hiccup.\n");
        }
        if (n == 1) {
            say(cap_name + " seems to fall, but takes a step and recovers.\n");
            write("You stumble.\n");
        }
        if (n == 3) {
            write("You feel drunk.\n");
            say(cap_name + " looks drunk.\n");
        }
        if (n == 5) {
            say(cap_name + " burps.\n");
            write("You burp.\n");
        }
    }

    /* No obvious effects of being stuffed or soaked */

    if (hit_point < max_hp || spell_points < max_sp || intoxicated || headache)
    {
        time_to_heal -= 1;
        if (time_to_heal < 0) {
            if (headache) {
                headache -= 1;
                if (headache == 0)
                    tell_object(myself, "You no longer have a headache.\n");
            }
            if (hit_point < max_hp) {
                hit_point += 1;
                if (intoxicated)
                    hit_point += 3;
                if (hit_point > max_hp)
                    hit_point = max_hp;
            }
            if (spell_points < max_sp) {
                spell_points += 1;
                if (intoxicated)
                    spell_points += 3;
                if (spell_points > max_sp)
                    spell_points = max_sp;
            }
            if (intoxicated) {
                intoxicated -= 1;
                if (intoxicated == 0) {
                    headache = max_headache;
                    max_headache = 0;
                    tell_object(myself,
                       "You suddenly without reason get a bad headache.\n");
                    hit_point -= 3;
                    if (hit_point < 0)
                        hit_point = 0;
                }
            }
            time_to_heal = INTERVAL_BETWEEN_HEALING;
        }
    }

    if (stuffed)
        stuffed--;

    if (soaked)
        soaked--;

    if (attacker_ob)
        attack();
    if (attacker_ob && whimpy && hit_point < max_hp/5)
        run_away();
}

/*
 * Update our aligment.
 */

void add_alignment(int a) {
    if (!intp(a)) {
        write("Bad type argument to add_alignment.\n");
        return;
    }
    alignment = alignment*9/10 + a;
    if (level > 20)
        return;
    if (alignment > KILL_NEUTRAL_ALIGNMENT * 100) {
        al_title = "saintly";
        return;
    }
    if (alignment > KILL_NEUTRAL_ALIGNMENT * 20) {
        al_title = "good";
        return;
    }
    if (alignment > KILL_NEUTRAL_ALIGNMENT * 4) {
        al_title = "nice";
        return;
    }
    if (alignment > - KILL_NEUTRAL_ALIGNMENT * 4) {
        al_title = "neutral";
        return;
    }
    if (alignment > - KILL_NEUTRAL_ALIGNMENT * 20) {
        al_title = "nasty";
        return;
    }
    if (alignment > - KILL_NEUTRAL_ALIGNMENT * 100) {
        al_title = "evil";
        return;
    }
    al_title = "demonic";
}

void set_al(int a) {
    if (!intp(a))
        return;
    alignment = a;
}

void set_alignment(string al) {
    al_title = al;
}

static int test_dark() {
    if (set_light(0) <= 0) {
        write("It is too dark.\n");
        return 1;
    }
    return 0;
}

int put(string str) {
    int i;
    string item;
    string container;
    object item_o;
    object container_o;

    if (!str)
        return 0;
    if (test_dark())
        return 1;
    if (sscanf(str, "%s in %s", item, container) != 2) {
        write("put what ?\n");
        return 1;
    }
    container = lower_case(container);
    container_o = present(container, this_player());
    if (!container_o)
        container_o = present(container, environment(this_player()));
    if (!container_o) {
        write("There are no " + container + "s here!\n");
        return 1;
    }
    if (!container_o->can_put_and_get()) {
        write("You can't do that.\n");
        return 1;
    }
    item = lower_case(item);
    item_o = present(item, this_player());
    if (!item_o) {
        write("You have no " + item + "!\n");
        return 1;
    }
    if (item_o == container_o)
        return 0;
    if (item_o->prevent_insert())
        return 1;
    if (item_o->drop())
        return 1;
    i = item_o->query_weight();
    if (container_o->add_weight(i)) {
        /* Remove the weight from the previous container. */
        environment(item_o)->add_weight(-i);
        move_object(item_o, container_o);
        say(cap_name + " puts the " + item + " in the " + container + ".\n");
        write("Ok.\n");
        it = item;
        return 1;
    }
    write("There is not room for more.\n");
    return 1;
}

int pick_up(string str) {
    string item;
    string container;
    object item_o;
    object container_o;
    int weight;

    if (!str) {
        write("What ?\n");
        return 1;
    }
    if (ghost) {
        write("You fail.\n");
        return 1;
    }
    if (test_dark())
        return 1;
    if (str == "all") {
        get_all(environment());
        return 1;
    }
    if (sscanf(str, "%s from %s", item, container) != 2) {
        pick_item(str);
        return 1;
    }
    container_o = present(lower_case(container));
    if (!container_o) {
        write("There is no " + container + " here.\n");
        return 1;
    }
    if (!container_o->can_put_and_get()) {
        write("You can't do that!\n");
        return 1;
    }
    item_o = present(item, container_o);
    if (!item_o){
        write("There is no " + item + " in the " + container + ".\n");
        return 1;
    }
    if (!item_o->get(item)) {
        write("You can not take " + item + " from " +
              container + ".\n");
        return 1;
    }
    weight = item_o->query_weight();
    if (!add_weight(weight)) {
        write("You can not carry more.\n");
        return 1;
    }
    container_o->add_weight(-weight);
    move_object(item_o, myself);
    write("Ok.\n");
    say(cap_name + " takes " + item + " from " + container + ".\n");
    return 1;
}

static int pick_item(string obj) {
    object ob;
    int i;
    obj = lower_case(obj);
    if (environment(this_player())->id(obj)) {
        write("You can't take that.\n");
        return 1;
    }
    ob = present(obj, environment(this_player()));
    if (!ob) {
        write("That is not here.\n");
        return 1;
    }
    if (ghost) {
        write("You fail.\n");
        return 1;
    }
    if (environment(ob) == myself) {
        write("You already have it!\n");
        return 1;
    }
    if (ob->get() == 0) {
        write("You can not take that!\n");
        return 1;
    }
    i = ob->query_weight();
    if (add_weight(i)) {
        move_object(ob, myself);
        say(cap_name + " takes " + obj + ".\n");
        it = obj;
        write("Ok.\n");
        return 1;
    }
    write("You can't carry that much.\n");
    return 1;
}

int drop_thing(mixed obj) {
    string tmp;
    string tmp2;
    int i;
    if (!obj) {
        write("What ?\n");
        return 1;
    }
    if (obj == "all") {
        drop_all(1);
        return 1;
    }
    if (sscanf(obj, "%s in %s", tmp, tmp2) == 2) {
        put(obj);
        return 1;
    }
    if (obj == "money" || obj == "all money") {
        drop_all_money(1);
        return 1;
    }
    tmp = obj;
    obj = present(lower_case(obj), this_player());
    if (!obj) {
        write("That is not here.\n");
        return 1;
    }
    if (drop_one_item(obj)) {
        it = tmp;
        write("Ok.\n");
        say(cap_name + " drops the " + tmp + ".\n");
    }
    return 1;
}

int add_weight(int w) {
    int max;

    max = level + 10;
    if (frog)
        max = max / 2;
    if (w + local_weight > max)
        return 0;
    if(w + local_weight < 0)
        return 0;
    local_weight += w;
    return 1;
}

/*
 * Temporary move the player to another destination and execute
 * a command.
 */

static int in_room(string str)
{
    string room;
    object old_room;
    string cmd;
    if (!str)
        return 0;
    if (sscanf(str, "%s %s", room, cmd) != 2) {
        write("Usage: in ROOM CMD\n");
        return 1;
    }
    room = valid_read(room, WIZ);
    if (!room) {
        write("Invalid file name.\n");
        return 1;
    }
    old_room = environment();
    move_object(myself, room);
    command(cmd);
    if (old_room)
        move_object(myself, old_room);
    else
        write("Could not go back again.\n");
    return 1;
}

int shout_to_all(string str) {
    if (spell_points < 0) {
        write("You are low on power.\n");
        return 1;
    }
    if (level < 20)
        spell_points -= 30;
    if (!str) {
        write("Shout what ?\n");
        return 1;
    }
    if (ghost) {
        write("You fail.\n");
        return 1;
    }
    if (!frog) { SHOUT(cap_name + " shouts: " + str + "\n"); }
    else {
      SHOUT(cap_name + " the frog shouts: " + "Hriibit! Hrriiibit!" + "\n");
    }
    write("Ok.\n");
    return 1;
}

static int emote(string str) {
    if (!str) {
        write("emote what ?\n");
        return 1;
    }
    say(cap_name + " " + str + "\n");
    write("Ok.\n");
    return 1;
}

int inventory() {
    object ob;
    if (test_dark())
        return 1;
    ob = first_inventory(myself);
    while(ob) {
        string str;
        str = ob->short();
        if (str) {
            write(capitalize(str) + ".\n");
            it = str;
        }
        ob = next_inventory(ob);
    }
    return 1;
}

int look(string str) {
    object ob, ob_tmp;
    string item;
    int max;
    if (test_dark())
        return 1;
    if (!str) {
        environment()->long();
        ob = first_inventory(environment());
        max = MAX_LIST;
        while(ob && max > 0) {
            if (ob != myself) {
                string short_str;
                short_str = ob->short();
                if (short_str) {
                    max -= 1;
                    write(short_str + ".\n");
                    it = short_str;
                }
            }
            ob = next_inventory(ob);
        }
        return 1;
    }
    if (sscanf(str, "at %s", item) == 1 || sscanf(str, "in %s", item) == 1) {
        int weight;
        item = lower_case(item);
        ob = present(item, this_player());
        if (!ob && environment(this_player())->id(item))
            ob = environment(this_player());
        if (!ob)
            ob = present(item, environment(this_player()));
        if (!ob) {
            write("There is no " + item + " here.\n");
            return 1;
        }
        it = item;
        ob->long(item);
        weight = ob->query_weight();
        if (!living(ob)) {
            if (weight >= 5)
                write("It looks very heavy.\n");
            else if (weight >= 3)
                write("It looks heavy.\n");
        }
        if (!ob->can_put_and_get(item))
            return 1;
        if (living(ob)) {
            object special;
            special = first_inventory(ob);
            while(special) {
                string extra_str;
                extra_str = special->extra_look();
                if (extra_str)
                    write(extra_str + ".\n");
                special = next_inventory(special);
            }
        }
        ob_tmp = first_inventory(ob);
        while(ob_tmp && ob_tmp->short() == 0)
            ob_tmp = next_inventory(ob_tmp);
        if (ob_tmp) {
           if (living(ob)) {
                write("\t" + capitalize(item) + " is carrying:\n");
            } else
                write("\t" + capitalize(item) + " contains:\n");
        }
        max = MAX_LIST;
        ob = first_inventory(ob);
        while(ob && max > 0) {
            string sh;
            sh = ob->short();
            if (sh)
                write(sh + ".\n");
            ob = next_inventory(ob);
            max -= 1;
        }
        return 1;
    }
    write("Look AT something, or what ?\n");
    return 1;
}

static int examine(string str) {
    return look("at " + str);
}

static void check_password(string p)
{
    write("\n");
    remove_call_out("time_out");
    if (password == 0)
        write("You have no password ! Set it with the 'password' cmd.\n");
    else if (name != "guest" && crypt(p, password) != password) {
        write("Wrong password!\n");
        destruct(myself);
        return;
    }
    move_player_to_start(0);
#ifdef LOG_ENTER
    log_file("ENTER", cap_name + ", " + ctime(time())[4..15]+ ".\n");
#endif
}

/*
 * Give a new password to a player.
 */
static void new_password(string p)
{
    write("\n");
    if (!p || p == "") {
        write("Try again another time then.\n");
        destruct(myself);
        return;
    }
    if (strlen(p) < 6) {
        write("The password must have at least 6 characters.\n");
        input_to("new_password", 1);
        write("Password: ");
        return;
    }
    if (password == 0) {
        password = p;
        input_to("new_password", 1);
        write("Password: (again) ");
        return;
    }
    remove_call_out("time_out");
    if (password != p) {
        write("You changed !\n");
        destruct(myself);
        return;
    }
    password = crypt(password, 0);        /* Generate new seed. */
    "room/adv_guild"->advance(0);
    set_level(1); set_str(1); set_con(1); set_int(1); set_dex(1);
    hit_point = max_hp;
    move_player_to_start(0);
#ifdef LOG_NEWPLAYER
    log_file("NEWPLAYER", cap_name + ".\n");
#endif
}


static void move_player_to_start(mixed where) {
  if (!mailaddr || mailaddr == "")
  {
    write("Please enter your email address (or 'none'): ");
    saved_where = where;
    input_to("getmailaddr");
    return;
  }

  move_player_to_start2(where);
}

void set_mailaddr(string addr) {
  mailaddr = addr;
}

string query_mailaddr() {
  return mailaddr;
}

static void getmailaddr(string maddr) {
  mailaddr = maddr;

  move_player_to_start2(saved_where);
}

static void move_player_to_start2(mixed where) {
  if (gender == -1) {
    write("Are you, male, female or other: ");
    input_to("getgender", 0);
    return;
  }
  move_player_to_start3(where);
}

/*  This function is called using input_to, and sets the
 *  gender of this player.
 */
static void getgender(string gender_string) {

  gender_string = lower_case(gender_string);
  if (gender_string[0] == 'm') {
      write("Welcome, Sir!\n");
      set_male();
  }
  else if (gender_string[0] == 'f') {
      write("Welcome, Madam!\n");
      set_female();
  }
  else if (gender_string[0] == 'o') {
      write("Welcome, Creature!\n");
      set_neuter();
  }
  else {
      write("Sorry, but that is too weird for this game!\n");
      write("Are you, male, female or other (type m, f or o): ");
      input_to("getgender");
      return;
  }

  move_player_to_start3(saved_where);
}

static void move_player_to_start3(mixed where) {
    object ob;
    string tmp_name;
    /*
     * See if we are already playing.
     * We must blank our own name, or we could find ourselves !
     */
    tmp_name = name;
    name = 0;
    other_copy = find_player(tmp_name);
    name = tmp_name;
    enable_commands();
    if (other_copy) {
        write("You are already playing !\n");
        write("Throw the other copy out ? ");
        input_to("try_throw_out");
        return;
    }
    /*
     * Initilize the character stats, if not already done.
     */
    if (!stats_is_updated) {
        int tmp;
        tmp = level;
        if (tmp > 20)
            tmp = 20;
        set_str(tmp); set_int(tmp); set_con(tmp); set_dex(tmp);
        stats_is_updated = 1;
    }
    /*
     * Now we can enter the game. Check tot_value if the game
     * crashed, and the values of the player was saved.
     */
    set_heart_beat(1);
    add_standard_commands();
    if (level >= 20)
        wiz_commands();
    if (level >= 21)
        wiz_commands2();
    move_object(clone_object("obj/soul"), myself);
    if (tot_value) {
        write("You find " + tot_value + " coins of your lost money!\n");
        money += tot_value;
        tot_value = 0;
    }
    cat("/NEWS");
    if (where)
        move_object(myself, where);
    else {
        move_object(myself, "room/church");
        load_auto_obj(auto_load);
    }
    if (is_invis && level < 20)
        vis();
    if (!is_invis)
        say(cap_name + " enters the game.\n");
    else
        write("YOU ARE INVISIBLE !\n\n");
    if (level >= 21)
        cat("/WIZNEWS");
    "room/post"->query_mail();
    if (query_ip_number() != called_from_ip && called_from_ip)
        write("Your last login was from " + called_from_ip + "\n");
    called_from_ip = query_ip_number();
    ob = first_inventory(environment());
    while(ob) {
        if (ob != this_object()) {
            string sh;
            sh = ob->short();
            if (sh)
                write(sh + ".\n");
        }
        ob = next_inventory(ob);
    }
    current_path = "players/" + name;
    set_living_name(name);
}

static int list_files(string path)
{
    if (!path)
        path = "/" + current_path;
    if (path != "/")
        path = path + "/.";
    ls(path);
    return 1;
}

int tail_file(string path)
{
    if (!path)
        return 0;
    if (!tail(path))
        return 0;
    return 1;
}

int cat_file(string path)
{
    if (!path)
        return 0;
    if (!cat(path))
        write("No such file.\n");
    return 1;
}

static int help(string what) {
    if (what == "wizard" && level >= 20) {
        cat("/doc/wiz_help");
        return 1;
    }
    if (what) {
        cat("/doc/helpdir/" + what);
        return 1;
    }
    cat("/doc/help");
    return 1;
}

static int tell(string str)
{
    object ob;
    string who;
    string msg;
    if (ghost) {
        write("You fail.\n");
        return 1;
    }
    if (spell_points < 0) {
        write("You are low on power.\n");
        return 1;
    }
    if (level < 20)
        spell_points -= 5;
    if (!str || sscanf(str, "%s %s", who, msg) != 2) {
        write("Tell what ?\n");
        return 1;
    }
    it = lower_case(who);
    ob = find_living(it);
    if (!ob) {
        write("No player with that name.\n");
        return 1;
    }
    tell_object(ob, cap_name + " tells you: " + msg + "\n");
    write("Ok.\n");
    return 1;
}

int whisper(string str)
{
    object ob;
    string who;
    string msg;
    if (ghost) {
        write("You fail.\n");
        return 1;
    }
    if (!str || sscanf(str, "%s %s", who, msg) != 2) {
        write("Whisper what ?\n");
        return 1;
    }
    it = lower_case(who);
    ob = find_living(it);
    if (!ob || !present(it, environment(this_player()))) {
        write("No player with that name in this room.\n");
        return 1;
    }
    tell_object(ob, cap_name + " whispers to you: " + msg + "\n");
    write("Ok.\n");
    say(cap_name + " whispers something to " + who + ".\n", ob);
    return 1;
}

int list_peoples() {
    object * list;
    int i, a;

    list = users();
    write("There are now " + sizeof(list) + " players");
    for (i=0, a=0; i < sizeof(list); i++)
        if (query_idle(list[i]) >= 5 * 60)
            a++;
    if (a)
        write(" (" + (sizeof(list) - a) + " active)");
    write(". " + query_load_average() + "\n");
    for(i=0; i<sizeof(list); i++) {
        string name;
        name = list[i]->query_real_name();
        if (!name)
            name = list[i]->query_name();
        if (!name)
            name = "logon";
        name = capitalize(name);
        if (list[i]->short() == 0)
            name = "(" + name + ")";
        if (strlen(name) < 8)
            name = name + "\t";
        write(query_ip_number(list[i]) + "\t" + name + "\t" +
              list[i]->query_level() + "\t");
        a = list[i]->query_age();
        if (a / 43200 > 9)
            write(a / 43200 + " D");
        else if (a / 43200 > 0)
            write(a / 43200 + "  D");
        else if (a / 1800 > 9)
            write(a / 1800 + " h");
        else if (a / 1800 > 0)
            write(a / 1800 + "  h");
        else if (a / 30 > 9)
            write(a / 30 + " m");
        else
            write(a / 30 + "  m");
        if (query_idle(list[i]) >= 5 * 60)
            write(" I\t");
        else
            write("\t");
        if (environment(list[i]))
            write(object_name(environment(list[i])));
        write("\n");
    }
    return 1;
}

static int update_object(string str) {
    object ob;
    if (!str) {
        write("Update what object ?\n");
        return 1;
    }
    str = valid_read(str, WIZ);
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

static int edit(string file)
{
    string tmp_file;
    if (!file) {
        ed();
        return 1;
    }
    file = valid_write(file);
    if (!file) {
        write("You can only edit your own files.\n");
        return 1;
    }
    ed(file);
    return 1;
}

static int heal(string name)
{
    object ob;

    if (!name)
        return 0;
    it = lower_case(name);
    ob = find_living(it);
    if (!ob) {
        write("No such person is playing now.\n");
        return 1;
    }
    ob->heal_self(100000);
    tell_object(ob, "You are healed by " + cap_name + ".\n");
    write("Ok.\n");
    return 1;
}

static int stat(string name)
{
    object ob;

    if (!name)
        return 0;
    it = lower_case(name);
    ob = present(name, environment());
    if (!ob || !living(ob))
        ob = find_living(it);
    if (!ob) {
        write("No such person is playing now.\n");
        return 1;
    }
    ob->show_stats();
    return 1;
}

/*
 * This routine is called from other routines to drop one specified object.
 * We return true if success.
 */

int drop_one_item(object ob)
{
    int weight;

    if (ob->drop())
        return 0;
    weight = ob->query_weight();
    if (!weight)
        weight = 0;
    add_weight(-weight);
    move_object(ob, environment(myself));
    return 1;
}

void drop_all(int verbose)
{
    object ob;
    object next_ob;
    if (!myself || !living(myself))
        return;
    ob = first_inventory(myself);
    while(ob) {
        string out;
        next_ob = next_inventory(ob);
        it = ob->short();
        if (drop_one_item(ob) && verbose) {
            out = it + ".\n";
            say(cap_name + " drops " + out);
            tell_object(myself, "drop: " + out);
        }
        ob = next_ob;
    }
}

static int shut_down_game(string str)
{
    if (!str) {
        write("You must give a shutdown reason as argument.\n");
        return 1;
    }
    shout("Game is shut down by " + capitalize(name) + ".\n");
#ifdef LOG_SHUTDOWN
    log_file("GAME_LOG", ctime(time()) + " Game shutdown by " + name +
             "(" + str + ")\n");
#endif
    shutdown();
    return 1;
}

/*
 * This one is called when the player wants to change his password.
 */
static int change_password(string str)
{
    if (password != 0 && !str) {
        write("Give old password as an argument.\n");
        return 1;
    }
    if (password != 0 && password != crypt(str, password)) {
        write("Wrong old password.\n");
        return 1;
    }
    password2 = 0;
    input_to("change_password2", 1);
    write("New password: ");
    return 1;
}

static void change_password2(string str)
{
    if (!str) {
        write("Password not changed.\n");
        return;
    }
    if (password2 == 0) {
        password2 = str;
        input_to("change_password2", 1);
        write("Again: ");
        return;
    }
    if (password2 != str) {
        write("Wrong! Password not changed.\n");
        return;
    }
    password = crypt(password2, 0);        /* Generate new seed */
    password2 = 0;
    write("Password changed.\n");
}

void smart_report(string str) {
    string who;
    string current_room;

    current_room = object_name(environment(this_object()));
    if (sscanf(current_room, "players/%s/", who) != 1)
        return;
    log_file(who + ".rep", current_room + " " + str + "\n");
}

static int bug(string str)
{
    if (!str) {
        write("Give an argument.\n");
        return 1;
    }
    log_file("BUGS", "\n");
    log_file("BUGS", cap_name + " (" +
             object_name(environment(this_object())) + "):\n");
    log_file("BUGS", str + "\n");
    smart_report("Bug " + cap_name + "\n" + str);
    write("Ok.\n");
    return 1;
}

static int typo(string str)
{
    if (!str) {
        write("Give an argument.\n");
        return 1;
    }
    log_file("TYPO", cap_name + " (" +
             object_name(environment(this_object())) + "):\n");
    log_file("TYPO", str + "\n");
    smart_report("Typo " + cap_name + "\n" + str);
    write("Ok.\n");
    return 1;
}

static int idea(string str)
{
    if (!str) {
        write("Give an argument.\n");
        return 1;
    }
    log_file("IDEA", cap_name + ":\n");
    log_file("IDEA", str + "\n");
    smart_report("Idea " + cap_name + "\n" + str);
    write("Ok.\n");
    return 1;
}

static int converse()
{
    write("Give '**' to stop.\n");
    write("]");
    input_to("converse_more");
    return 1;
}

static void converse_more(string str)
{
    string cmd;
    if (str == "**") {
        write("Ok.\n");
        return;
    }
    if (str[0] == '!') {
        sscanf(str, "!%s", cmd);
        command(cmd);
    } else if (str != "") {
        say(cap_name + " says: " + str + "\n");
    }
    write("]");
    input_to("converse_more");
}

static int toggle_whimpy()
{
    whimpy = !whimpy;
    if (whimpy)
        write("Wimpy mode.\n");
    else
        write("Brave mode.\n");
    return 1;
}

int query_brief() { return brief; }

int toggle_brief()
{
    brief = !brief;
    if (brief)
        write("Brief mode.\n");
    else
        write("Verbose mode.\n");
    return 1;
}

void add_exp(int e) {
#ifdef LOG_EXP
    if (this_player() && this_player() != this_object() &&
      query_ip_number(this_player()) && level < 20 && e >= ROOM_EXP_LIMIT)
        log_file("EXPERIENCE", ctime(time()) + " " + name + "(" + level +
                ") " + e + " exp by " + this_player()->query_real_name() +
                "(" + this_player()->query_level() + ")" +"\n");
#endif
    experience += e;
    if (level <= 19)
        add_worth(e);
}

void add_intoxination(int i) {
    if(i < 0)
    {
        if (-i > intoxicated / 10)
                i = -intoxicated / 10;
    }
    intoxicated += i;
    if(intoxicated < 0)
        intoxicated = 0;
}

void add_stuffed(int i)
{
    if(i < 0)
    {
        if (-i > stuffed / 10)
                i = -stuffed / 10;
    }
    stuffed += i;
    if (stuffed < 0)
        stuffed = 0;
}

void add_soaked(int i)
{
    if(i < 0)
    {
        if (-i > soaked / 10)
                i = -soaked / 10;
    }
    soaked += i;
    if (soaked < 0)
        soaked = 0;
}

int query_intoxination() {
    return intoxicated;
}

int query_stuffed()
{
    return stuffed;
}

int query_soaked()
{
    return soaked;
}

int second_life() {
#if 1
    object death_mark;
#endif
    if (level >= 20)
        return illegal_patch("second_life");
    make_scar();
    ghost = 1;
    if (level > 1)
        level = level - 1;
    if (Str > 1)
        set_str(Str-1);
    if (Con > 1)
        set_con(Con-1);
    if (Dex > 1)
        set_dex(Dex-1);
    if (Int > 1)
        set_int(Int-1);
    msgin = "drifts around";
    msgout = "blows";
    headache = 0;
    intoxicated = 0;
    stuffed = 0;
    soaked = 0;
    hunter = 0;
    hunted = 0;
#ifdef LOG_KILLS
    if (attacker_ob)
        log_file("KILLS", name + "(" + level + ")" + " killed by " +
                 attacker_ob->short() + "(" +
                 attacker_ob->query_real_name() + "), creator: " +
                 creator(attacker_ob) + "\n");
#endif
    attacker_ob = 0;
    alt_attacker_ob = 0;
    tell_object(myself, "\nYou die.\nYou have a strange feeling.\n" +
                "You can see your own dead body from above.\n\n");

#if 1
    death_mark = clone_object("/room/death/death_mark");
    move_object(death_mark, myself);
#endif
    return 1;
}

int remove_ghost() {
    if (!ghost)
        return 0;
    write("You feel a very strong force.\n");
    write("You are sucked away...\n");
    write("You reappear in a more solid form.\n");
    say("Some mist disappears.\n");
    say(cap_name + " appears in a solid form.\n");
    ghost = 0;
    dead = 0;
    msgin = "arrives";
    msgout = "leaves";
    save_me(1);
    return 1;
}

static int trans(string str)
{
    object ob;
    string out;

    if (!str)
        return 0;
    ob = find_living(str);
    if (!ob) {
        write("No such living thing.\n");
        return 1;
    }
    it = str;
    tell_object(ob, "You are magically transfered somewhere.\n");
    out = ob->query_mmsgin();
    if (!out)
        out = ob->query_name() +
            " arrives in a puff of smoke.\n";
    else
        out = ob->query_name() + " " + out + ".\n";
    say(out);
    write(out);
    move_object(ob, environment(this_object()));
    return 1;
}

int stop_hunting_mode()
{
    if (!hunted) {
        write("You are not hunting anyone.\n");
        return 1;
    }
    hunted->stop_hunter();
    hunted = 0;
    write("Ok.\n");
    return 1;
}

int drink_alcohol(int strength)
{
    if (intoxicated > level + 3) {
        write("You fail to reach the drink with your mouth.\n");
        return 0;
    }
    intoxicated += strength;
    if (intoxicated < 0)
        intoxicated = 0;
    if (intoxicated == 0)
        write("You are completely sober.\n");
    if (intoxicated > 0 && headache) {
        headache = 0;
        tell_object(myself, "Your headache disappears.\n");
    }
    if (intoxicated > max_headache)
        max_headache = intoxicated;
    if (max_headache > 8)
        max_headache = 8;
    return 1;
}

int drink_alco(int strength)
{
        if (intoxicated + strength > level * 3)
        {
                write("You fail to reach the drink with your mouth.\n");
                return 0;
        }

        intoxicated += strength / 2;

        if (intoxicated < 0)
                intoxicated = 0;

        if (intoxicated == 0)
                write("You are completely sober.\n");

        if (intoxicated > 0 && headache)
        {
                headache = 0;
                tell_object(myself, "Your headache disappears.\n");
        }

        if (intoxicated > max_headache)
                max_headache = intoxicated;

        if (max_headache > 8)
                max_headache = 8;

        return 1;
}
int drink_soft(int strength)
{
        if (soaked + strength > level * 8)
        {
                write("You can't possibly drink that much right now!\n" +
                        "You feel crosslegged enough as it is.\n");
                return 0;
        }

        soaked += strength * 2;

        if (soaked < 0)
                soaked = 0;

        if (soaked == 0)
                write("You feel a bit dry in the mouth.\n");

        return 1;
}

int eat_food(int strength)
{
        if (stuffed + strength > level * 8)
        {
                write("This is much too rich for you right now! Perhaps something lighter?\n");
                return 0;
        }

        stuffed += strength * 2;

        if (stuffed < 0)
                stuffed = 0;

        if (stuffed == 0)
                write("Your stomach makes a rumbling sound.\n");

        return 1;
}

int spell_missile(string str)
{
    object ob;
    if (test_dark())
        return 1;
    if (level < 5)
        return 0;
    if (!str)
        ob = attacker_ob;
    else
        ob = present(lower_case(str), environment(this_player()));
    if (!ob || !living(ob)) {
        write("At whom ?\n");
        return 1;
    }
    if (ob == myself) {
        write("What ?");
        return 1;
    }
    missile_object(ob);
    return 1;
}

int spell_shock(string str)
{
    object ob;
    if (test_dark())
        return 1;
    if (level < 10)
        return 0;
    if (!str)
        ob = attacker_ob;
    else
        ob = present(lower_case(str), environment(this_player()));
    if (!ob || !living(ob)) {
        write("At whom ?\n");
        return 1;
    }
    if (ob == myself) {
        write("What ?");
        return 1;
    }
    shock_object(ob);
    return 1;
}

int spell_fire_ball(string str)
{
    object ob;
    if (test_dark())
        return 1;
    if (level < 15)
        return 0;
    if (!str)
        ob = attacker_ob;
    else
        ob = present(lower_case(str), environment(this_player()));
    if (!ob || !living(ob)) {
        write("At whom ?\n");
        return 1;
    }
    if (ob == myself) {
        write("What ?");
        return 1;
    }
    fire_ball_object(ob);
    return 1;
}

static int spell_zap(string str)
{
    object ob;
    if (!str)
        ob = attacker_ob;
    else
        ob = present(lower_case(str), environment(this_player()));
    if (!ob || !living(ob)) {
        write("At whom ?\n");
        return 1;
    }
    zap_object(ob);
    return 1;
}

int give_object(string str)
{
    string item, dest;
    object item_ob, dest_ob;
    int weight;
    int coins;

    if (!str)
        return 0;
    if (test_dark())
        return 1;
    if (sscanf(str, "%d coins to %s", coins, dest) == 2)
        item = 0;
    else if ( sscanf(str, "1 coin to %s", dest) == 1)
        coins = 1;
    else if ( sscanf(str, "coin to %s", dest) == 1)
        coins = 1;
    else if (sscanf(str, "one coin to %s", dest) == 1)
        coins = 1;
    else if (sscanf(str, "%s to %s", item, dest) != 2) {
        write("Give what to whom ?\n");
        return 1;
    }
    dest = lower_case(dest);
    if (item) {
        item = lower_case(item);
        item_ob = present(item, this_player());
        if (!item_ob) {
            write("There are no " + item + " here.\n");
            return 1;
        }
        it = item;
        if (environment(item_ob) == this_object() &&
            item_ob->drop() == 1) {
            return 1;
        } else {
            if (!item_ob->get()) {
                write("You can't get that !\n");
                return 1;
            }
        }
    }
    dest_ob = present(dest, environment(this_player()));
    if (!dest_ob) {
        write("There is no " + capitalize(dest) + " here.\n");
        return 1;
    }
    if (!living(dest_ob)) {
        write("You can't do that.\n");
        return 1;
    }
    if (!item) {
        if (coins <= 0 && level < 20)
            return 0;
        if (money < coins) {
            write("You don't have that much money.\n");
            return 1;
        }
        money -= coins;
        /* Checkpoint the character, to prevent cheating */
        if (coins > 1000 && level < 20)
            save_me(1);
        dest_ob->add_money(coins);
        tell_object(dest_ob, cap_name + " gives you " + coins +
            " gold coins.\n");
        write("Ok.\n");
        return 1;
    }
    weight = item_ob->query_weight(0);
    if (!dest_ob->add_weight(weight)) {
        write(capitalize(dest) + " can't carry any more.\n");
        return 1;
    }
    add_weight(-weight);
    move_object(item_ob, dest_ob);
    say(cap_name + " gives " + item + " to " + capitalize(dest) + ".\n");
    write("Ok.\n");
    return 1;
}

/*
 * Get all items here.
 */
static void get_all(object from)
{
    object ob, next_ob;

    ob = first_inventory(from);
    while(ob) {
        string item;
        next_ob = next_inventory(ob);
        item = ob->short();
        if (item && ob->get()) {
            int weight;
            weight = ob->query_weight();
            if (add_weight(weight)) {
                write(item + ": Ok.\n");
                move_object(ob, this_object());
                say(cap_name + " takes: " + item + ".\n");
            } else {
                write(item + ": Too heavy.\n");
            }
            it = item;
        }
        ob = next_ob;
    }
}

static int force_player(string str)
{
    string who, what;
    object ob;
    if (!str)
        return 0;
    if (sscanf(str, "%s to %s", who, what) == 2 ||
        sscanf(str, "%s %s", who, what) == 2) {
        ob = find_living(who);
        if (!ob) {
            write("No such player.\n");
            return 1;
        }
        tell_object(ob, cap_name + " force you to: " + what + "\n");
        command(what, ob);
        write("Ok.\n");
        return 1;
    }
    return 0;
}

int clone(string str) {
    object ob;
    if (!str) {
        write("Clone what object ?\n");
        return 1;
    }
    str = valid_read(str, WIZ);
    if (!str) {
        write("Invalid file.\n");
        return 1;
    }
    ob = clone_object(str);
    say(cap_name + " fetches something from another dimension.\n");
    move_object(ob, environment());
    if (ob->get())
        transfer(ob, myself);
    write("Ok.\n");
    return 1;
}

int pose() {
    if (level >= 15) {
        write("You send a ball of fire into the sky.\n");
        say(cap_name + " makes a magical gesture.\n");
        say("A ball of fire explodes in the sky.\n");
        return 1;
    }
    return 0;
}

static int destruct_local_object(string str)
{
    object ob;
    if (!str) {
        write("Destruct what ?\n");
        return 1;
    }
    str = lower_case(str);
    ob = present(str, this_player());
    if (!ob)
        ob = present(str, environment(this_player()));
    if (!ob) {
        write("No " + str + " here.\n");
        return 1;
    }
    say(ob->short() + " is disintegrated by " + cap_name + ".\n");
    destruct(ob);
    write("Ok.\n");
    return 1;
}

static int load(string str)
{
    object env;
    if (!str) {
        write("Load what ?\n");
        return 1;
    }
    str = valid_read(str, WIZ);
    if (!str) {
        write("Invalid file name.\n");
        return 1;
    }
    env = environment();
    /* We just call a non existing function in the object, and that way
     * force it to be loaded.
     */
    load_object(str);
    write("Ok.\n");
    return 1;
}

static int snoop_on(string str)
{
    object ob;
    int ob_level;

    if (!str) {
        snoop();
        return 1;
    }
    ob = find_player(str);
    if (!ob) {
        write("No such player.\n");
        return 1;
    }
    ob_level = ob->query_level();
    if (ob_level >= level) {
        write("You fail.\n");
        return 1;
    }
    snoop(ob);
    return 1;
}

int invis()
{
    if (is_invis) {
        tell_object(this_object(), "You are already invisible.\n");
        return 1;
    }
    is_invis = 1;
    tell_object(this_object(), "You are now invisible.\n");
    say(cap_name + " " + mmsgout + ".\n", this_object());
    cap_name = "Someone";
    return 1;
}

int vis()
{
    if (!is_invis) {
        tell_object(this_object(), "You are not invisible.\n");
        return 1;
    }
    is_invis = 0;
    tell_object(this_object(), "You are now visible.\n");
    cap_name = capitalize(name);
    say(cap_name + " " + mmsgin + ".\n", this_object());
    return 1;
}

static int home() {
    move_player("home#players/" + name + "/workroom");
    return 1;
}

mixed valid_write(string str) {
    string who, file, owner;

    owner = name;
    if (previous_object() && previous_object() != this_object()) {
        if (sscanf(object_name(previous_object()), "players/%s/", who) == 1)
            owner = who;
    }
    if (str[0] != '/') {
        /* Prepend the name of the wizard that created the object (if any) */
        if (previous_object() && previous_object() != this_object()) {
            str = "players/" + owner + "/" + str;
            return str;
        }
        if (current_path != "")
            str = "/" + current_path + "/" + str;
        else
            str = "/" + str;
    }
    if (sscanf(str, "/players/%s/%s", who, file) == 2) {
        if (who == owner || level > 23)
            return "players/" + who + "/" + file;
        return check_access_list("players/", who, file);
    }
    if (sscanf(str, "/log/%s", who) == 1) {
        if (level < 24 && who[0] >= 'A' && who[0] <= 'Z')
            return 0;
        return "log/" + who;
    }
    if (sscanf(str, "/open/%s", file) == 1)
        return "open/" + file;
    if (sscanf(str, "/ftp/%s", file) == 1)
        return "ftp/" + file;
    if (sscanf(str, "/room/%s/%s", who, file) == 2)
        return check_access_list("room/", who, file);
    return 0;
}

mixed valid_read(string str, int lvl) {
    string who, file;
    int i;

    i = strlen(str) - 1;
    while(i>0) {
        if (str[i] == '.' && str[i-1] == '.') {
            write("Illegal to have '..' in path.\n");
            return 0;
        }
        i -= 1;
    }
    file = valid_write(str);
    if (file)
        return file;
    if (str[0] != '/'){
        if (current_path == "")
            str = "/" + str;
        else
            str = "/" + current_path + "/" + str;
    }
    if (lvl == ARCH)
    {
            if (sscanf(str, "/players/%s", file) == 1 && level < 23)
                return 0;
    }
    if (sscanf(str, "/%s", file) == 1)
        return file;
    write("Bad file name: " + str + "\n");
    return 0;                /* Should not happen */
}

static int wiz_score_list(string arg) {
    if (arg)
        wizlist(arg);
    else
        wizlist();
    return 1;
}

static int remove_file(string str) {
    if (!str)
        return 0;
    rm(str);
    return 1;
}

static int local_commands() {
    localcmd();
    return 1;
}

/*
 * Recursively compute the values of the inventory.
 * Beware that object may selfdestruct when asked for query_value().
 */
int compute_values(object ob) {
    int v;
    while(ob) {
        int tmp;
        object next_ob;

        next_ob = next_inventory(ob);
        tmp = ob->query_value();
        if (tmp > 1000)
            tmp = 1000;
        v += tmp;
        if (ob && first_inventory(ob))
            v += compute_values(first_inventory(ob));
        ob = next_ob;
    }
    return v;
}

void save_me(int value_items)
{
    if (value_items)
        tot_value = compute_values(first_inventory(this_object()));
    else
        tot_value = 0;
    compute_auto_str();
    save_object("players/" + name);
}

int illegal_patch(string what) {
    write("You are struck by a mental bolt from the interior of the game.\n");
    log_file("ILLEGAL", ctime(time()) + ":\n");
    log_file("ILLEGAL",
             this_player()->query_real_name() + " " +
             what + "\n");
    return 0;
}

void load_auto_obj(string str) {
    string file, argument, rest;
    object ob;

    while(str && str != "") {
        if (sscanf(str, "%s:%s^!%s", file, argument, rest) != 3) {
            write("Auto load string corrupt.\n");
            return;
        }
        str = rest;
        ob = find_object(file);
        if (!ob)
    {
        write("Can't autoload '" + file + "': not in game.\n");
            continue;
    }
        ob = clone_object(file);
        if (argument)
            ob->init_arg(argument);
        move_object(ob, this_object());
    }
}

void compute_auto_str() {
    object ob;
    string str;

    auto_load = "";
    ob = first_inventory(this_object());
    while(ob) {
        str = ob->query_auto_load();
        ob = next_inventory(ob);
        if (!str)
            continue;
        auto_load = auto_load + str + "^!";
    }
}

mixed query_quests(string str) {
    string tmp, rest, rest_tmp;
    int i;

    if (str == 0)
        return quests;
    rest = quests;
    while(rest) {
        if (str == rest)
            return 1;
        i = sscanf(rest, "%s#%s", tmp, rest_tmp);
        if (i == 0)
            return 0;
        if (tmp == str)
            return 1;
        if (i == 1)
            return 0;
        rest = rest_tmp;
    }
    return 0;
}

int set_quest(string q) {
    if (!q)
        return 0;
    if (query_quests(q))
        return 0;
#ifdef LOG_SET_QUEST
    if (previous_object()) {
        log_file("QUESTS", name + ": " + q + " from " +
                 object_name(previous_object()) + "\n");
        if (this_player() && this_player() != this_object() &&
          query_ip_number(this_player()))
            log_file("QUESTS", "Done by " +
                     this_player()->query_real_name() + "\n");
    }
#endif /* LOG_SET_QUEST */
    if (quests == 0)
        quests = q;
    else
        quests = quests + "#" + q;
    return 1;
}

string query_real_name() {
    return name;
}

void time_out() {
    write("Time out\n");
    destruct(this_object());
}

int who()
{
    object * list;
    int i;

    list = users();
    while(i<sizeof(list)) {
        string sh;
        sh = list[i]->short();
        if (sh == 0 && level >= 20)
            write("(" + list[i]->query_real_name() + ")\n");
        else if (sh)
            write(sh + "\n");
        i += 1;
    }
    return 1;
}

static int cd(string str) {
    string old_path;

    old_path = current_path;
    if (str == "..") {
        int i;
        if (current_path == "")
            return 0;
        i = strlen(current_path) - 1;
        while(i > 0 && current_path[i] != '/')
            i -= 1;
        if (i == 0)
            current_path = "";
        else
            current_path = current_path[0..i-1];
    } else if (!str)
        current_path = "players/" + name;
    else if (str == "/")
        current_path = "";
    else if (str[0] != '/') {
        if (current_path == "")
            current_path = str;
        else
            current_path += "/" + str;
    } else {
        current_path = str[1..];
    }
    write("/" + current_path + "\n");
    return 1;
}

#define CHUNK 16

static string more_file;        /* Used by the more command */
static int more_line;

int more(string str) {
    if (!str)
        return 0;
    more_file = str;
    more_line = 1;
    if (cat(more_file, more_line, CHUNK) == 0) {
        write("No such file\n");
        return 1;
    }
    input_to("even_more");
    write("More: (line " + (CHUNK + 1) + ") ");
    return 1;
}

static void even_more(string str) {
    if (str == "" || str == "d")
        more_line += CHUNK;
    else if (str == "q") {
        write("Ok.\n");
        return;
    } else if (str == "u") {
        more_line -= CHUNK;
        if (more_line < 1)
            more_line = 1;
    }
    if (cat(more_file, more_line, CHUNK) == 0) {
        more_file = 0;
        write("EOF\n");
        return;
    }
    write("More: (line " + (more_line + CHUNK) + ") ");
    input_to("even_more");
}

int pwd() {
    write("/" + current_path + "\n");
    return 1;
}

int makedir(string str) {
    if (!str)
        return 0;
    if (mkdir(str))
        write("Ok.\n");
    else
        write("Fail.\n");
    return 1;
}

int removedir(string str) {
    if (!str)
        return 0;
    if (rmdir(str))
        write("Ok.\n");
    else
        write("Fail.\n");
    return 1;
}

string query_path() {
    return current_path;
}

string check_access_list(string top, string dir, string file) {
    string tmp1, tmp2;

    if (!access_list)
        return 0;
    if (sscanf(access_list, "%s" + dir + "#%s", tmp1, tmp2) == 2)
        return top + dir + "/" + file;
    return 0;
}

static int set_email(string str) {
    if (!str) {
        write("Your official electric mail address is: " + mailaddr + "\n");
        return 1;
    }
    mailaddr = str;
    write("Changed your email address.\n");
    return 1;
}

void add_standard_commands() {
    add_action("set_email", "email");
    add_action("give_object", "give");
    add_action("score", "score");
    add_action("save_character", "save");
    add_action("quit", "quit");
    add_action("kill", "kill");
    add_action("communicate", "say");
    add_action("communicate", "'", 1);
    add_action("shout_to_all", "shout");
    add_action("put", "put");
    add_action("pick_up", "get");
    add_action("pick_up", "take");
    add_action("drop_thing", "drop");
    add_action("inventory", "i");
    add_action("look", "look");
    add_action("examine", "examine");
    add_action("examine", "exa", 1);
    add_action("help", "help");
    add_action("tell", "tell");
    add_action("whisper", "whisper");
    add_action("change_password", "password");
    add_action("idea", "idea");
    add_action("typo", "typo");
    add_action("bug", "bug");
    add_action("converse", "converse");
    add_action("toggle_brief", "brief");
    add_action("toggle_whimpy", "wimpy");
    add_action("stop_hunting_mode", "stop");
    add_action("spell_missile", "missile");
    add_action("spell_shock", "shock");
    add_action("spell_fire_ball", "fireball");
    add_action("pose", "pose");
    add_action("who", "who");
}

