#include "/inc/base.inc"
#include "/inc/gc.inc"
#include "/inc/client.inc"


void run_server()
{
    ed("/dummy","ed_ends");
}

void ed_ends()
{
    __MASTER_OBJECT__->check_test();
}

int fnamecounter;
void run_client()
{
    fnamecounter = random(1000);
    write_file("/dummy"+fnamecounter, "");
    write("r /dummy"+fnamecounter+"\nQ\n");
}

void run_test()
{
    msg("\nRunning test for #0000488:\n"
          "--------------------------\n");

    connect_self("run_server", "run_client");
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
