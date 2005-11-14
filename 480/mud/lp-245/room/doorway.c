#define EXTRA_MOVE /* nothing */

string name,short_desc,long_desc,dir1,dir2,dest;

id(s) {
 if(!s) return;
 return(s==name);
}

short() { return short_desc; }

long() { write(long_desc); }

init() {
 if(name) add_action("enter","enter");
 if(dir1) add_action("go_room",dir1);
 if(dir2) add_action("go_room",dir2);
}

set_name(s) { name=s; }

set_short(s) { short_desc=s; }

set_long(s) { if(s) long_desc=s; }

set_dirs(d1,d2) {
 if(d1) {
  dir1=d1;
  dir2=d2;
 }
}

set_dest(s) { if(s) dest=s; }

enter(s) {
 if(!id(s)) return;
 EXTRA_MOVE
 this_player()->move_player("into the "+name+"#"+dest);
 return 1;
}

go_room() {
 EXTRA_MOVE
 this_player()->move_player(dir1+"#"+dest);
 return 1;
}

reset(arg) {
 if(arg) return;
 dest="room/church";
 long_desc="You see nothing special.\n";
}
