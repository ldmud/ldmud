struct Detail {
	string name;
	string description;
};

struct Door {
	string name;
	string description;
	int key;
	string flags;
};

struct Exit {
	int destination;
	string flags;
	struct Door door;
};

struct Exits {
    struct Exit northwest;
    struct Exit north;
    struct Exit northeast;
    struct Exit east;
    struct Exit southeast;
    struct Exit south;
    struct Exit southwest;
    struct Exit west;
    struct Exit up;
    struct Exit down;
};

struct Room {
    int vnum;
    int zone;
    string name;
    string description;
    string flags;
    string sector;
    struct Exits exit;
    struct Detail *detail;
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
