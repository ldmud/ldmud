struct Door
{
    string flags;
};

struct Exit
{
    string flags;
};

struct Exits
{
    struct Exit up;
    struct Exit down;
};

struct Room
{
    struct Exits exit;
};

void main()
{
    struct Room room;
    string dir;

    // The in the following line the driver should be able to determine
    // that only a struct Exit could be the result of the computed lookup.

    room->exit->(dir)->flags;

    // In the following line, the & should bind less strong than the -> .
    input_to("foo", 0, &room->exit);
}
