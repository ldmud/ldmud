inherit "room/room";

void reset(int  arg) {
    if (arg) return;

    set_light(1);
    short_desc = "A room";
    no_castle_flag = 0;
    long_desc =
        "Long desc.\n";
    dest_dir =
        ({
        "/players/lars/workroom", "north",
        });
    1/0;
}

int query_light() {
    return 1;
}
int query_room_maker() {
    return 101;
}

/*
    remove the comments around the "room is modified()" code
    below to prevent changes you have done to this room to
    to be lost by using the room maker
*/
/*
room_is_modified() {
    return 1;
}
*/

/*
 make your additions below this comment, do NOT remove this comment
--END-ROOM-MAKER-CODE--
*/

void exit() {
    1/0;
}
