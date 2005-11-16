object next;
object monster;
string chat_str;

/*
short() { return chat_str; }
*/

link(ob) {
    next = ob;
}

load_chat(str) {
    chat_str = str;
}

set_monster(m) {
    monster = m;
}

chat(nr) {
    object room;

    if (nr == 0){
	room = environment(monster);
	if(room)
	    return tell_room(room,chat_str);
    }
    nr -= 1;
    if (next)
	return next->chat(nr);
    else
	return 0;
}

remove_chat(str) {
    if (str == chat_str) {
	destruct(this_object());
	return next;
    }
    if (next)
	next = next->remove_chat(str);
    return this_object();
}

collaps()
{
    if(next)
	next->collaps();
    destruct(this_object());
}

drop() { return 1; }
