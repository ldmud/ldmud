object next;
object monster;
string chat_str;

/*
short() { return chat_str; }
*/

void link(object ob) {
    next = ob;
}

void load_chat(string str) {
    chat_str = str;
}

void set_monster(object m) {
    monster = m;
}

int chat(int nr) {
    object room;

    if (nr == 0){
	room = environment(monster);
	if(room)
        {
	    tell_room(room,chat_str);
            return 0;
        }
    }
    nr -= 1;
    if (next)
	return next->chat(nr);
    else
	return 0;
}

object remove_chat(string str) {
    if (str == chat_str) {
	destruct(this_object());
	return next;
    }
    if (next)
	next = next->remove_chat(str);
    return this_object();
}

void collaps()
{
    if(next)
	next->collaps();
    destruct(this_object());
}

int drop() { return 1; }
