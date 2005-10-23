inherit "room/room";

string messages;
int new_mail;

void reset(int arg) {
    if (arg)
	return;
    set_light(1);
    dest_dir = ({ "room/narr_alley", "north" });
    short_desc = "The post office";
    long_desc = "You are in the post office. Commands:\n" +
	"read         Read from the mailbox.\n" +
	"mail <name>  Mail to player 'name'.\n" +
	"from         List all headers.\n";
    no_castle_flag = 1;
}

void init() {
    ::init();
    move_object(clone_object("obj/mail_reader"), this_player());
}

void exit() {
    object ob;
    if (ob = present("mailread", this_player()))
	destruct(ob);
}

int query_mail(int silent) {
    string name;
    string new;

    name = lower_case(this_player()->query_name());
    if (!restore_object("room/post_dir/" + name) || messages == "") return 0;
    if (silent) return 1;
    new = "";
    if (new_mail)
	new = " NEW";
    write("\nThere is" + new + " mail for you in the post office\n"+
        "   (south from village road).\n\n");
    return 1;
}
