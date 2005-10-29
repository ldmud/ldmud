int current_page;

reset()
{
    current_page = 0;
}

short() { return "a book in a chain"; }

long(str) {
    if (str == "chain") {
	write("The chain is secured to the wall.\n");
	return;
    }
    write("There is a book hanging in a chain from the wall.\n");
    write("The title is: 'ADVENTURING'.\n");
}

init() {
    add_action("open"); add_verb("open");
    add_action("close"); add_verb("close");
    add_action("read_book"); add_verb("read");
}

id(str) { return str == "book" || str == "chain"; }

open(str) {
    if (!id(str))
	return 0;
    if (current_page > 0) {
	write("The book is already open at page " + current_page + ".\n");
	return 1;
    }
    current_page = 1;
    write("Ok.\n");
    say(call_other(this_player(), "query_name", 0) +
	" opens the book.\n");
    return 1;
}

close(str)
{
    if (!id(str))
	return 0;
    if (current_page == 0) {
	write("It is already closed.\n");
	return 1;
    }
    current_page = 0;
    write("Ok.\n");
    say(call_other(this_player(), "query_name", 0) +
	" closed the book.\n");
    return 1;
}

read_book(str)
{
    if (!id(str) && str != "page")
	return 0;
    if (current_page == 0) {
	write("It is closed.\n");
	return 1;
    }
    if (current_page == 1)
	cat("/obj/book_page1");
    return 1;
}

get(str) {
    if (str == "book") {
	write("The book is attached to a chain.\n");
	return 0;
    }
    if (str == "chain") {
	write("The chain is secured to the wall.\n");
	return 0;
    }
    return 0;
}
