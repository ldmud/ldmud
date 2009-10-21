#include "/sys/driver_hook.h"
#include "msg.inc"

#ifndef OWN_INAUGURATE_MASTER
void inaugurate_master (int arg)
{
    set_driver_hook(H_LOAD_UIDS, unbound_lambda(({}), "uid"));
    set_driver_hook(H_CLONE_UIDS, unbound_lambda(({}), "uid"));
    set_driver_hook(H_NOTIFY_FAIL, "");
}
#endif

#ifndef OWN_GET_MASTER_UID
string get_master_uid()
{
    return "I am the master of this universe!";
}
#endif

#ifndef OWN_PRIVILEGE_VIOLATION
int privilege_violation(string op, mixed who, mixed arg, mixed arg2)
{
    return 1;
}
#endif

#ifndef OWN_VALID_READ
mixed valid_read(string path, string uid, string func, object ob)
{
    return 1;
}
#endif

#ifndef OWN_VALID_WRITE
mixed valid_write(string path, string uid, string func, object ob)
{
    return 1;
}
#endif

#ifndef OWN_VALID_EXEC
mixed valid_exec(string prog, object ob, object obfrom)
{
    return 1;
}
#endif

#ifndef OWN_PREPARE_DESTRUCT
mixed prepare_destruct (object obj)
{
    return 0;
}
#endif
