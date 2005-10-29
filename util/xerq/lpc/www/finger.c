string www_main(string *args)
{
    if (sizeof(args) != 1) return "<PLAINTEXT>Need to specify a name.\n";
    return sprintf(
"<TITLE>Finger: %s</TITLE>\n"
"<PLAINTEXT>%s",
	args[0], "/cmds/std/_finger"->get_finger(args[0], 0));
}

status clean_up(status arg) { destruct(this_object()); }
