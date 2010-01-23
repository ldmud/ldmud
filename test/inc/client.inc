/* Functions to connect to ourself.
 *
 *    connect_self("server_fun", "client_fun")
 *
 * Calls server_fun() and client_fun() in the
 * corresponding object. (The client initiates the
 * connection.)
 */

/* These functions are called in the master object. */
string connect_server_fun;

void start_connect(string server_fun, string client_fun)
{
    object client_ob;

    client_ob = clone_object(this_object());
    client_ob->start_client(client_fun);
    connect_server_fun = server_fun;
}

void connect_self(string server_fun, string client_fun)
{
    /* Waiting until LDMud is ready for users. */
    call_out(#'start_connect, 0, server_fun, client_fun);
}

object connect()
{
    object server_ob;

    server_ob = clone_object(this_object());
    server_ob->set_server_fun(connect_server_fun);
    return server_ob;
}


/* These functions are for the clones. */
string connect_client_fun = 0; // 0, if this is the server ob.
void start_client(string client_fun)
{
    net_connect("127.0.0.1", query_mud_port());
    connect_client_fun = client_fun;
}

void set_server_fun(string server_fun)
{
    connect_server_fun = server_fun;
}

int logon(int flag)
{
    enable_telnet(0);
    enable_commands();
    set_prompt("");

    if(connect_client_fun)
        call_other(this_object(), connect_client_fun);
    else if(connect_server_fun)
        call_other(this_object(), connect_server_fun);

    return 1;
}
