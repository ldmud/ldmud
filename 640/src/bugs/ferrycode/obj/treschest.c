#include <mudlib.h>
#include <ships.h>
inherit CONTAINER;

int unlocked, opened;
object ship_ob;

void reset(int arg) {
  ::reset(arg);
  if(arg) return;
  set_name("chest");
  set_short("A treasure chest");
  set_long("\
A huge wooden treasure chest that contain's the captain's prized possessions\n\
during a voyage. Only those the captains allowes can use it.\n");
  set_weight(10000);
  set_value(0);
  set_max_weight(20);
  unlocked = 0;
  opened = 0;
}

void init() {
  ::init();
  add_action("lock","lock");
  add_action("unlock","unlock");
  add_action("open","open");
  add_action("close","close");
}

string long() {
  write("\
A huge ironbound treasure chest that the captain of this ship uses to store\n\
all of their most prized possessions.\n");
  if(opened) {
    write("It is open and ");
  } else {
    write("It is closed and ");
  }
  if(unlocked) {
    write("unlocked.\n");
  } else {
    write("locked.\n");
  }
}

int can_put_and_get() {
  return(opened);
}

int open(string str) {
  if(str == "chest") {
    if(unlocked) {
      if(opened) {
        write("It is already opened.\n");
        return 1;
      }
      else {
        write("You open the chest.\n");
        opened = 1;
        return 1;
      }
    }
    else {
      write("The chest is locked, it would be to hard to open.\n");
      return 1;
    }
  }
  return 0;
}

int close(string str) {
  if(str == "chest") {
    if(opened) {
      write("You close the chest.\n");
      opened = 0;
      return 1;
    }
    else {
      write("It is already closed.\n");
      return 1;
    }
  }
}

int lock(string str) {
  ship_ob = environment(this_object())->query_ship();
  if(!ship_ob->query_captains(this_player()->query_real_name())) {
    write("You don't have permissions to lock this chest.\n");
    return 1;
  }
  if(!str) {
    write("Lock what?\n");
    return 1;
  }
  if(str == "chest") {
    if(opened) {
      write("It is open, you might want to close it first.\n");
      return 1;
    }
    if(unlocked) {
      write("You lock the chest.\n");
      unlocked = 0;
      return 1;
    }
    else {
      write("It is already locked.\n");
      return 1;
    }
  }
  return 0;
}
int unlock(string str) {
  ship_ob = environment(this_object())->query_ship();
  if(!ship_ob->query_captains(this_player()->query_real_name())) {
    write("You do not have permission to unlock this chest.\n");
    return 1;
  }
  if(!str) {
    write("Unlock what?\n");
    return 1;
  }
  if(str == "chest") {
    if(!unlocked) {
      write("You unlock the chest.\n");
      unlocked = 1;
      return 1;
    }
    else {
      write("It is already unlocked.\n");
      return 1;
    }
  }
  return 0;
}
