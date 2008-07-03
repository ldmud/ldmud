#include "/inc/base.inc"
#include "/inc/gc.inc"

/* These functions are for the clone (the player object). */
int active;
void start_client()
{
    net_connect("127.0.0.1", query_mud_port());
    active = 1;
}
 
int logon(int flag)
{
    enable_telnet(0);
    set_prompt("");
    
    if(active)
	ed("/dummy","ed_ends");
    return 1;
}

void ed_ends()
{
    __MASTER_OBJECT__->check_test();
}

/* These functions are for the blueprint (the virtual player that
   sends us the commands). */

int fnamecounter;
object connect()
{
    enable_telnet(0);
    set_prompt("");

    fnamecounter = random(1000);
    write_file("/dummy"+fnamecounter, "");
    write("r /dummy"+fnamecounter+"\nQ\n");

    return clone_object(this_object()); // Just a dummy object.
}

void run_test()
{
    msg("\nRunning test for #0000488:\n"
          "--------------------------\n");

    /* Waiting until LDMud is ready for users. */
    call_out("run_test2", 0);
}

void run_test2()
{
    object dummy;
    dummy = clone_object(this_object());
    dummy->start_client();
}

void check_test()
{
    start_gc(
	(:
	    rm("/dummy"+fnamecounter);
	    shutdown($1);
	    return;
	:));
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
