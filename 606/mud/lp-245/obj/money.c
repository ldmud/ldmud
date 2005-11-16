int money;

reset(arg) {
    if (arg)
	return;
    money = 1;
}

query_weight() { return 0; }

short() {
    if (money == 0)
	return 0;
    return money + " gold coins";
}

/*
 * If we are picked up by a player, then move the money to his "purse",
 * and destruct this object.

 901128: Changed by JnA to not destruct object until surely picked by the
 player, i.e. object moved to the players inventory with move_object()
*/
init()
{
  if (environment(this_object())==this_player()) {
    this_player()->add_money(money);
    money = 0;
    set_heart_beat(1);
  }
}

get()
{
  return money>0;
}

set_money(m) {
    money = m;
}

id(str) {
    if (str == "coins")
	return 1;
    if (str == "money")
	return 1;
}

heart_beat() {
    if (money == 0)
	destruct(this_object());
}
