#pragma save_types, strong_types, rtt_checks, lightweight

// This LWO will produce errors for different creation stages.

// For the blueprint:
int on_init, on_new, on_copy, on_restore;

void activate_error_on_init() { on_init = 1; }
void do_error_on_init()
{
    if (on_init)
    {
        on_init = 0;
        raise_error("Intentional error on __INIT().\n");
    }
}

void activate_error_on_new() { on_new = 1; }
void do_error_on_new()
{
    if (on_new)
    {
        on_new = 0;
        raise_error("Intentional error on new().\n");
    }
}

void activate_error_on_copy() { on_copy = 1; }
void do_error_on_copy()
{
    if (on_copy)
    {
        on_copy = 0;
        raise_error("Intentional error on copied().\n");
    }
}

void activate_error_on_restore() { on_restore = 1; }
void do_error_on_restore()
{
    if (on_restore)
    {
        on_restore = 0;
        raise_error("Intentional error on restored().\n");
    }
}

// For the LWOs:

mapping data = ([ "Some": "Data for GC" ]);

int do_error = blueprint().do_error_on_init();

void new()
{
    blueprint().do_error_on_new();
}

void copied()
{
    blueprint().do_error_on_copy();
}

void restored()
{
    blueprint().do_error_on_restore();
}
