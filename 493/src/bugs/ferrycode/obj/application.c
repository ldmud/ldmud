#include <mudlib.h>
#include <material.h>
#include <daemons.h>

#define CAP(xxx) capitalize(xxx)

inherit OBJECT;

object moPlayer;
string msPlayer;
string msShipName;
string msShipClass;

void reset(int iArg) {
  if(!is_clone(this_object())) return;
  ::reset();
  if(iArg) return;
  set_name("ship application");
  set_alias("app");
  add_alias("application");
  set_short("application for ship ownership");
  set_long("\
This is the application you need to fill out before you\n\
can purchase a ship. Please be careful when you do.\n");
  set_value(0);
}

void init() {
  ::init();
  add_action("read_func","read");
  add_action("fillin_func","fillin");
  add_action("junk_it","junk");
}

void setup(object oArg) {
  moPlayer = oArg;
  msPlayer = moPlayer->query_real_name();
}

status is_valid_name(string sText) {
  string junk1;
  string junk2;

  if(sscanf(sText, "%s %s", junk1, junk2) != 2) {
    if(file_size("/banish/" + sText + ".o") >= 0){
      write("You may not use the name of a former player.\n");
      return 0;
    }
    if(find_object(PLAYER_INFO_D))
      destruct(find_object(PLAYER_INFO_D));
    if (PLAYER_INFO_D->restore_player(lower_case(sText)) > 0) {
      write("You may not use the name of a current player.\n");
      return 0;
    }
  }
  junk1 = PROFANITY_D->ProfanityScan(sText,this_player());
  if(junk1 != sText) {
    write("Ship and fleet names may not contain profanity.\n");
    return 0;
  }
  return 1;
}

status read_func(string sArg) {
  if(!id(sArg)) {
    notify_fail("Read what?\n");
    return 0;
  }
  write("\
----------------------------------------------------------------------\n");
  write("\
          Ship Application for " + CAP(msPlayer) + "\n\n");
  write("1) Ship Name: " +
    (msShipName ? CAP(msShipName) : "____________") + "\n");
  write("2) Ship Class: " +
    (msShipClass ? CAP(msShipClass) : "____________") + "\n");
  write("Ship costs per class are as follows:\n");
  write("    Cog -------------- 120000 coins\n");
  write("    Lateen Galley ---- 180000 coins\n");
  write("    Carrack ---------- 400000 coins\n\n");
  write("Please \"fillin [number] [text]\" to complete this application.\n");
  write("When you are finished, \"turnin\" the application at a ship shop.\n\n");
  write("\
----------------------------------------------------------------------\n\n");
  return 1;
}

status fillin_func(string sArg) {
  int iItem;
  string sText;
  string junk1;
  string junk2;

  if(!sArg) {
    notify_fail("Fillin what?\n");
    return 0;
  }
  if(sscanf(sArg,"%d %s",iItem,sText) != 2) {
    notify_fail("\
Please use the proper format. Read your application for details.\n");
    return 0;
  }
  sText = lower_case(sText);
  if(iItem > 2 || iItem < 1) {
    write("There are only 2 items on the application!\n");
    return 1;
  }
  if(iItem == 1) {
    if(SHIP_D->is_ship_name(sText)) {
      write(CAP(sText) + " is already the name of a ship. Choose another name.\n");
      return 1;
    }
    if(sscanf(sText,"%s %s",junk1, junk2) == 2) {
      write("Ship names may not contain spaces.\n");
      return 1;
    }
    if(!is_valid_name(sText)) return 1;
    write("You fill in the name of the ship: \"" + CAP(sText) + "\"\n");
    write("You can always fillin again if you made an error.\n");
    msShipName = sText;
    return 1;
  }
  if(iItem == 2) {
    if(sText != "carrack" && sText != "galley" && sText != "cog") {
      write("\
That is not a valid class of ship! Read your application for valid\n\
choices and their prices.\n");
      return 1;
    }
    write("You fill in the class of the ship: \"" + CAP(sText) + "\"\n");
    write("You can always fillin again if you made an error.\n");
    msShipClass = sText;
    return 1;
  }
}

status junk_it(string sArg) {
  if(!id(sArg))  return 0;
  write("You change your mind and junk the application.\n");
  destruct(this_object());
  return 1;
}

string query_player() {return msPlayer; }
string query_ship_name() {return msShipName; }
string query_ship_class() { return msShipClass; }
status query_finished() {
  if(msShipName &&  msShipClass)
    return 1;
  return 0;
}

int drop(int iSilently) { return 1; }