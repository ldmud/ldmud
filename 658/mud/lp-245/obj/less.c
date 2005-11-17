#define CHUNK 16

string file;
int line;

id(str) {
    return str == "less" || str == "lesser";
}

short() {
    if (!file)
	return "Lesser object";
    return "Less " + file;
}

init() {
    add_action("less", "less");
}

input(str) {
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

less(str) {
    file = str;
    line = 1;
    if (cat(file, line, CHUNK) == 0) {
	write("No such file\n");
	return;
    }
    input_to("input");
    write(CHUNK + 1 + " more: ");
    return 1;
}

get() { return 1; }

query_value() { return 20; }
