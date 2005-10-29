#include "std.h"

#undef EXTRA_INIT
#define EXTRA_INIT\
    add_action("mail"); add_verb("mail");\
    add_action("read_mail"); add_verb("read");

string messages;
string more_message, dest;
object now_writing;

#undef EXTRA_LONG
#define EXTRA_LONG\
    if (query_mail(1)) write("You have mail.\n");

ONE_EXIT("room/narr_alley", "north",
	  "The post office",
	  "This is the post office. If you want to send a mail to someone,\n"+
	  "do 'mail name'. If you want to read mail, do 'read'.\n" +
	  "If you want to give a message to the game master, use 'bug'.\n", 1)

mail(str)
{
    if (now_writing && living(now_writing)) {
	write("You have to wait for " + call_other(now_writing, "query_name") +
	    ".\n");
	return 1;
    }
    if (!str) {
	write("Mail who ?\n");
	return 1;
    }
    str = lower_case(str);
    if (!call_other(this_player(), "valid_name", str))
	return 1;
    if (!restore_object("players/" + str)) {
	write("There is no player with that name !\n");
	return 1;
    }
    now_writing = this_player();
    write("Give message. Terminate with '**'\n");
    write("]");
    input_to("more_mail");
    more_message = "";
    dest = str;
    return 1;
}

more_mail(str)
{
    if (str == "**") {
	save_message();
	now_writing = 0;
	return;
    }
    more_message = more_message + str + "\n";
    input_to("more_mail");
    write("]");
}

save_message()
{
    string mess, dest2;
    object ob;

    if (more_message == "") {
	write("Empty message. None sent.\n");
	return;
    }
    mess = more_message;
    dest2 = lower_case(dest);
    messages = "";
    restore_object("room/post_dir/" + dest2);
    if (messages == 0)
	messages = "";
    messages = messages + capitalize(call_other(this_player(), "query_real_name")) + "\n**\n"
		+ mess + "**\n";
    more_message = "";
    save_object("room/post_dir/" + dest2);
    write("Ok.\n");
    ob = find_living(dest2);
    if (ob)
	tell_object(ob, "You have new mail!\n");
}

read_mail() {
    string from, mess, name, rest_of_mess;
    int n;

    if (now_writing && living(now_writing)) {
	write("You have to wait for " + call_other(now_writing, "query_name") +
	    ".\n");
	return 1;
    }
    name = lower_case(call_other(this_player(), "query_name"));
    if (!restore_object("room/post_dir/" + name) || messages == "") {
	write("No mail !\n");
	return 1;
    }
    n = sscanf(messages, "%s\n**\n%s\n**\n%s", from, mess, rest_of_mess);
    if (n == 2)
	messages = "";
    else if (n != 3) {
	write("Mailbox corrupt. (" + n + ").\n");
	    return 1;
    }
    write("Mail from " + from + ":\n\n" + mess + "\n");
    messages = rest_of_mess;
    save_object("room/post_dir/" + name);
    return 1;
}

query_mail(silent) {
    string name;
    if (now_writing && living(now_writing))
	return 0;
    name = lower_case(call_other(this_player(), "query_name"));
    if (!restore_object("room/post_dir/" + name) || messages == "") {
	return 0;
    }
    if (silent)
	return 1;
    write("There is mail to you in the post office (south from village road).\n");
    return 1;
}
