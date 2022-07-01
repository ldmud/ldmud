#include "/inc/base.inc"
#include "/inc/gc.inc"
#include "/inc/client.inc"

void run_server()
{
    ed("/dummy-0000488","ed_ends");
}

void ed_ends()
{
    __MASTER_OBJECT__->check_test();
}

void run_client()
{
    int fnamecounter = random(1000);
    write_file("/dummy-0000488-"+fnamecounter, "");
    write("r /dummy-0000488-"+fnamecounter+"\nQ\n");
    __MASTER_OBJECT__->set_fnamecounter(fnamecounter);
}

void run_test()
{
    msg("\nRunning test for #0000488:\n"
          "--------------------------\n");

    connect_self("run_server", "run_client");
}

int fnamecounter;
void set_fnamecounter(int counter)
{
    fnamecounter = counter;
}

void check_test()
{
    start_gc(
	(:
	    rm("/dummy-0000488-"+fnamecounter);
	    shutdown($1);
	    return;
	:));
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
