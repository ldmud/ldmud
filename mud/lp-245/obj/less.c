#define CHUNK 16

string file;
int line;

int id(string str) {
    return str == "less" || str == "lesser";
}

string short() {
    if (!file)
	return "Lesser object";
    return "Less " + file;
}

void init() {
    add_action("less", "less");
}

void input(string str) {
    if (str == "" || str == "d")
	line += CHUNK;
    else if (str == "q") {
	write("Ok.\n");
	return;
    } else if (str == "u" && line > 0) {
	line -= CHUNK;
	if (line < 1)
	    line = 1;
    }
    if (cat(file, line, CHUNK) == 0) {
	file = 0;
	write("EOF\n");
	return;
    }
    write(line + CHUNK + " More: ");
    input_to("input");
}

int less(string str) {
    file = str;
    line = 1;
    if (cat(file, line, CHUNK) == 0) {
	write("No such file\n");
	return 0;
    }
    input_to("input");
    write(CHUNK + 1 + " more: ");
    return 1;
}

int get() { return 1; }

int query_value() { return 20; }
