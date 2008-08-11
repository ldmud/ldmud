
#include "room.h"
/* this constant should be 1 for virgin Mud's */
#define FOODS 2
#define Level this_player()->query_level()
#define Name this_player()->query_name()
#define Speak(s)\
 write("Innkeeper says: "+s+"\n")

int cm,mm,rmm;
string last_eater;

#undef EXTRA_RESET
#define EXTRA_RESET\
  cm=FOODS*4;\
  mm=FOODS*2;\
  rmm=FOODS;\
  if(arg) return;\
  items=({"menu","The menu looks like this"});

ONE_EXIT("room/eastroad5","east",
"Eastroad Inn",
"You are in the Eastroad Inn. Here you can buy food to still your\n"+
"hunger, but only a limited selection is available.\n",
1)

void init() {
 add_action("buy","buy");
 add_action("buy","order");
 ::init();
}

void show_menu() {
 write("\n");
 if(!(cm||mm||rmm))
  Speak("We have completely sold out...come back later.");
 else {
  write("1: Commonor's Meal     ");write(cm);write(" at 20 gp\n");
  write("2: Merchant's Meal     ");write(mm);write(" at 50 gp\n");
  write("3: Rich Man's Meal     ");write(rmm);write(" at 90 gp\n");
  write("4: A Mug of Beer       ");write(" ");write("     2 gp\n");
  write("\n");
  write("Use 'buy <number>' to buy the desired Food. The food will\n"+
        "be consumed at once. Good appetite.\n\n");
 }
 return;
}

void long(string s) {
 ::long(s);
 show_menu();
}

int no_food() {
 Speak("Sorry - we have sold out of that.");
 if(cm||mm||rmm)
  Speak("Why don't you try something else ?");
 else
  Speak("Why don't you come back later ?");
 return 1;
}

int pays(int n) {
 if(this_player()->query_money()<n) {
  Speak("You cannot afford that.");
  return 0;
 }
 this_player()->add_money(-n);
 return 1;
}

int tease(int n) {
 if(Name==last_eater)
  Speak("My - Are we hungry today.");
 last_eater=Name;
 this_player()->heal_self(n);
 return 1;
}

int buy(string s) {
 if(!s) {
  Speak("What do you want to buy ?");
  return 1;
 }
/* commonor's meal, price 20, heals 4, preferably for levels 1-6 */
if(s=="1"||s=="<1>") {
  if(!cm) return no_food();
  if(!pays(20)) return 1;
  if(Level>6) {
   Speak("You don't look like a commonor to me.");
   if(mm||rmm) {
    Speak("You should eat food more suited for you.");
    return 1;
   }
   Speak("But as we have no better food - here you are.");
  }
  write("You are served a commonor's meal - very nourishing\n");
  say(Name+" orders a commonor's meal\n");
  cm=cm-1;
  return tease(4);
 }
/* merchant's meal, price 50, heals 8, preferably for levels 7-12 */
 if(s=="2"||s=="<2>") {
  if(!mm) return no_food();
  if(!pays(50)) return 1;
  if(Level>12) {
   Speak("You look more like a richman to me.");
   if(rmm) {
    Speak("You should eat food more suited for you.");
    return 1;
   }
   Speak("But as we have no better food - here you are.");
  }
  write("You are served a merchant's meal - very good\n");
  say(Name+" orders a merchant's meal\n");
  mm=mm-1;
  return tease(8);
 }
/* rich man's meal, price 90, heals 12, preferably for levels 13+ */
 if(s=="3"||s=="<3>") {
  if(!rmm) return no_food();
  if(!pays(90)) return 1;
  write("You are served a rich man's meal - very delicious\n");
  say(Name+" orders a rich man's meal\n");
  rmm=rmm-1;
  return tease(12);
 }
 if(s=="4"||s=="<4>"||s=="mug"||s=="beer") {
  if(!pays(2)) return 1;
  if(!this_player()->drink_alcohol(2)) {
   Speak("You look a little too drunk for that..let me take it back.");
   this_player()->add_money(2);
   return 1;
  }
  write("You drink a mug of first class beer - That feels good.\n");
  say(Name+" drinks a beer.\n");
  return 1;
 }
 Speak("We have no such number on the menu, try 1, 2 or 3.");
 return 1;
}

