string www_main(string *args)
{
    string data, *info;

    info="/cmds/std/_who"->who_data()[2..<2];
    data=sprintf(
"<TITLE>Timewarp Who List</TITLE>\n"
"<BODY>\n"
"<H1 align=center>Timewarp Who List</H1>\n"
"\n"
"There are %d people currently logged in.\n"
"<PRE><HR>\n"
"%s\n"
"<HR></PRE>\n"
"<ADDRESS><A HREF=\"http://quark.gmi.edu/people/People.html\">\n"
"<IMG SRC=\"http://quark.gmi.edu/pics/buttons/people_page.gif\">\n"
"Return to Timewarp's People Page</A>.</ADDRESS>\n"
"</BODY>\n",
	sizeof(info), implode(info,""));
    return data;	
}

status clean_up(status arg) { destruct(this_object()); }
