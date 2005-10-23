/*
 * Team object. It is inside the leader. Every player can make a team.
 */

#define INIT_SIZE 2

object *members, *pot_members, leader_ob;
string leader_name, mout;

/*
 * Add a member to the member array or the potential member array.
 * The arrays is extended if needed. The leader adds to the potential
 * members array.
 */
object * add(object item) {
    int i;
    object * vec2;

    if (members == 0) {
	pot_members = allocate(INIT_SIZE);
	members = allocate(INIT_SIZE);
    }
    i = 0;
    while(i<sizeof(members)) {
	if (members[i] == 0) {
	    members[i] = item;
	    return members;
	}
	i += 1;
    }
    vec2 = allocate(sizeof(members) + INIT_SIZE);
    i = 0;
    while(i<sizeof(members)) {
	vec2[i] = members[i];
	i += 1;
    }
    vec2[i] = item;
    return vec2;
}

void long() {
    int i;
    write("Leader " + leader_name + ": ");
    if (!pointerp(members)) {
	write("No members.\n");
	return;
    }
    while(i<sizeof(members)) {
	if (members[i])
	    write(members[i]->query_name() + " ");
	i += 1;
    }
    write("\n");
}

int id(string str) {
    return str == "team object";
}

void init() {
    if (leader_ob == 0) {
	leader_ob = this_player();
	leader_name = environment()->query_name();
	mout = environment()->query_msgout();
    }
    add_action("join", "join");
    add_action("disband", "disband");
}

int join(string str) {
    object ob;
    if (!str)
	return 0;
    ob = present(str, environment(environment()));
    if (!ob) {
	write("No such player here.\n");
	return 1;
    }
    say(leader_name + " joined " +
	ob->query_name() + " to his team.\n", ob);
    tell_object(ob, "You are joined to the team of " +
		leader_name + ".\n");
    members = add(ob); /* takes members implicitely */
    write("Ok.\n");
    enable_commands();
    return 1;
}

int drop() { return 1; }

int get() { return 1; }

string direction;
object last_room;

void catch_tell(string str) {
    string dir;

    if (sscanf(str, leader_name + " " + mout + " %s.", dir) != 1)
	return;
    if (direction)
	return;
    direction = dir;
    last_room = environment(this_player());
    call_out("move", 0);
}

void msg(string str) {
    tell_object(environment(), str);
}

void move() {
    int i;

    if (!pointerp(members)){
	msg("Bad members list.\n");
	return;
    }
    if (last_room == environment(environment())) {
	direction = 0;
	return;
    }
    i = 0;
    while(i < sizeof(members)) {
	if (members[i] && environment(members[i]) == last_room) {
	    tell_object(members[i], "You follow " + leader_name + " '" +
			direction + "'.\n");
	    members[i]->force_us(direction);
	} else if (members[i]) {
	    tell_object(members[i], "Your leader moves without you!\n");
	}
	i += 1;
    }
    direction = 0;
    last_room = 0;
}

int disband(string str) {
    object ob;
    int i;
    if (!str)
	return 0;
    ob = present(str, environment(this_player()));
    if (!ob) {
	write("No such player.\n");
	return 0;
    }
    i = 0;
    while(i<sizeof(members)) {
	if (members[i] == ob) {
	    members[i] = 0;
	    write("Ok.\n");
	    say(ob->query_name() +
		" is removed from the team of " + leader_name + ".\n", ob);
	    tell_object(ob, "You are removed from the team of " +
			leader_name + ".\n");
	    return 1;
	}
	i += 1;
    }
    write("That person is not in your team.\n");
    return 1;
}
