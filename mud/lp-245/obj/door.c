string direction;
object partner_door;
string key_code;
string key_type;
string door_long;
int can_lock, is_locked, is_closed;

string query_dir() { return direction;}
void set_dir( string str) { direction = str;}
void set_code( string str) { key_code = str;}
void set_type( string str) { key_type = str;}
void set_door( object obj) { partner_door = obj;}
void set_closed( int val)
{
  if ( is_closed != val ) {
    tell_room( environment( this_object()), "The " + direction + " door ");
    if ( val)
      tell_room( environment( this_object()),"closes.\n");
    else
      tell_room( environment( this_object()),"opens.\n");
  }
is_closed = val;
}

void door_sound( string str)
{
  tell_room( environment( this_object()),
	    str + " is heard from the " + direction + " door.\n");
}

void set_locked( int val) {
  if ( is_locked != val )
    door_sound("Klick!");
  is_locked = val;
}
void set_can_lock( int val) { can_lock = val; }
void set_both_status()
{
  partner_door->set_closed(is_closed);
  partner_door->set_locked(is_locked);
  partner_door->set_can_lock(can_lock);
  partner_door->set_type(key_type);
  partner_door->set_code(key_code);
  partner_door->set_door_long(door_long);
}
void set_door_long( string str)
{
  door_long = str;
}

string door_room;

int set_all( string str)
{
 if (!str)
   return 0;
 if ( sscanf( str, "%s %s %s %s %d %d %d", door_room, direction, key_type,
	     key_code, is_closed, is_locked, can_lock) == 7 ) {
   if( key_type == "0")
     key_type = 0;
   move_object(this_object(), door_room);
   return 1;
 }
 return 0;
}

string query_room()
{
    return door_room;
}

void player_enters( string str)
{
  tell_room( environment( this_object()), str + " enters through the " +
	    direction + " door.\n");
}

void both_door_sound( string str)
{
  door_sound( str);
  partner_door->door_sound(str);
}

string short()
{
  string str;
  if ( is_closed ) str = " ( closed )";
  else
    str = " ( open )";

  return "A door to the " + direction + str;
}

void long()
{
  string str;
  int rnd;
  write( door_long);
  if ( key_type)
    write( "On the door there is a " + key_type + " lock.\n");

  if ( is_closed ) str = "closed.\n";
  else
    str = "open.\n";
  write( "The door is " + str);

  rnd = random( 20);
  if ( rnd == 7 ) {
    write("You notice a small sign stating:\n");
    write("A product from Hebol Inc.\n");
    if ( this_player())
      this_player()->add_exp(10);
  }
}

int id( string strang)
{
  if ( ( strang == "door" ) ||
      ( strang == direction + " door" ) ||
      ( strang == "H_door" ) )
    return 1;
  return 0;
}

void init()
{
  if ( direction ) {
    add_action( "go_door", direction);
  }

  add_action( "go", "go");

  add_action( "close", "close");
  add_action( "open", "open");

  if ( can_lock ) {
    add_action( "unlock", "unlock");

    add_action( "lock", "lock");
  }
}

int number_of_doors()
{
  object ob;
  int num_door;

  num_door = 0;

  ob = first_inventory(environment(this_object()));
  while(ob) {
    if (ob->id("H_door"))
      num_door += 1;

    ob = next_inventory(ob);
  }
  return num_door;
}

void which_door()
{
  object ob;
  int num_door;
  int tmp_num;
  string str;

  tmp_num = 0;
  num_door = number_of_doors();

  write("Which door do You mean");

  ob = first_inventory(environment(this_object()));
  while(ob) {
    if (ob->id("H_door")) {
      tmp_num += 1;
      str = ob->query_dir();

      if ( tmp_num == num_door )
	write(" or the " + str + " door.\n");
      else
	write( ", the " + str + " door");
    }
    if ( tmp_num == num_door ) return;

    ob = next_inventory(ob);
  }
}

/* this_door( str) tests if the argument arg refers to this door.
   Return values: 0 => str not refering to a door,
   1 => str refering to a door but there are several alternatives tells the
   player that this has occurred.
   2 => str refers to this door.
   */

int this_door(string  str)
{
  string type;


  if ( !str)
    return 0;

  if ( (sscanf(str, "%s", type) == 1) && ( type == "door" ) ) {
    if ( number_of_doors() == 1)
      return 2;
    else
      which_door();

    return 1;
  }

  if ( (sscanf(str, "%s door", type) == 1) && ( type == direction ) ) {
    return 2;
  }

  return 0;
}

int go_door()
{
  string str;

  if ( is_closed ) {
    write("You can't do that, the door is closed.\n");
    return 1;
  }

  if ( partner_door) {
    str = this_player()->query_name();
    partner_door->player_enters(str);
    write( "You go through the " + direction + " door.\n");
    /*
    move_object( this_player(), environment(partner_door));
    */
    this_player()->move_player(
	direction + "#" + partner_door->query_room());
  }
  return 1;
	
}

int go( string str)
{
  int tmp;

  if ( !str)
    return 0;

  tmp = this_door( str);
  if ( tmp == 2 ) {
    go_door();
    return 1;
  }
  else
    if ( tmp == 1 )
      return 1;
    else
      return 0;
}

void open_door()
{
  string str;
  int tmp;

  if ( ! is_closed ) {
    write("But why? It's already open!\n");
    return;
  }

  if ( is_locked )
    write("You can't open the " + direction + " door, it's locked!\n");
  else  {
    write("You open the " + direction + " door.\n");
    set_closed( 0);
    partner_door->set_closed(is_closed);
  }

  return;
}

void close_door()
{
  string str;
  int tmp;

  if ( is_closed ) {
    write("But why? It's already closed!\n");
    return;
  }

  if ( is_locked )
    write("You can't close the " + direction + " door, it's locked!\n");
  else {
    write("You close the " + direction + " door.\n");
    set_closed(1);
    partner_door->set_closed(is_closed);
  }

  return;
}

int open( string str)
{
  int tmp;

  if ( !str)
    return 0;

  tmp = this_door( str);
  if ( tmp == 2 ) {
    open_door();
    return 1;
  }
  else
    if ( tmp == 1 )
      return 1;
    else
      return 0;
}

int close( string str)
{
  int tmp;

  if ( !str)
    return 0;

  tmp = this_door( str);
  if ( tmp == 2 ) {
    close_door();
    return 1;
  }
  else
    if ( tmp == 1 )
      return 1;
    else
      return 0;
}

int number_of_keys()
{
  object ob;
  int num_key;

  num_key = 0;

  ob = first_inventory(this_player());
  while(ob) {
    if (ob->id("H_key"))
      num_key += 1;

    ob = next_inventory(ob);
  }
  return num_key;
}

void which_key()
{
  object ob;
  int num_key;
  int tmp_num;
  string str;

  tmp_num = 0;
  num_key = number_of_keys();

  write("Which key do You mean");

  ob = first_inventory(this_player());
  while(ob) {
    if (ob->id("H_key")) {
      tmp_num += 1;
      str = ob->query_type();

      if ( tmp_num == num_key )
	write(" or the " + str + " key.\n");
      else
	write( ", the " + str + " key");
    }
    if ( tmp_num == num_key ) return;

    ob = next_inventory(ob);
  }
}


object get_key(string type)
{
  object ob;
  int num_key;
  int tmp_num;
  string str;
  string k_type;

  if ( ! (sscanf( type, "%s key", k_type) == 1) )
    k_type = 0;

  tmp_num = 0;
  num_key = number_of_keys();

  ob = first_inventory(this_player());
  while(ob) {
    if (ob->id("key")) {
      tmp_num += 1;
      str = ob->query_type();

      if  ( ( str == k_type ) || ( ! k_type ) )
	return ob;
    }
    if ( tmp_num == num_key ) return 0;

    ob = next_inventory(ob);
  }
  return ob;
}

int this_key(string  str)
{
  string type;

  if ( !str)
    return 0;

  if ( (sscanf(str, "%s", type) == 1) && ( type == "key" ) ) {
    if ( number_of_keys() == 1)
      return 2;
    else
      if ( number_of_keys() == 0) {
	write("You haven't got a key!\n");
	return 1;
      }
      else
	which_key();

    return 1;
  }

  if (sscanf(str, "%s key", type) == 1) {
    if ( present( type + " key", this_player()))
      return 2;
    write("You haven't got such a key!\n");
  }

  return 0;
}

void unlock_door(object  key)
{
  string str;

  if ( ! is_locked ){
    write("But why? It's already unlocked!\n");
    return;
  }

  if ( key)
    str = key->query_code();

  if ( ( str == key_code ) || ( str == "zap" ) ) {
    write("You unlock the " + direction + " door.\n");
    set_locked( 0);
    partner_door->set_locked(is_locked);
  }
  else
    write("The key doesn't fit!\n");

  return;
}

int unlock( string str)
{
  object ob;
  int tmp;
  string type, door;

  if ( !str)
    return 0;

  if ( str == "door" ) {
    write( "Unlock the door with what?\n");
    return 1;
  }
  type = 0;

  if (sscanf(str, "%s with %s", door, type) == 2) {
    tmp = this_door( door);
    if ( tmp != 2 )
      return tmp;

    tmp = this_key( type);
    if ( tmp != 2 )
      return 1;
    ob = get_key( type);

    unlock_door( ob);
    return 1;
  }
}

void lock_door(object  key)
{
  string str;
  int tmp;

  if ( is_locked ) {
    write("But why? It's already locked!\n");
    return;
  }
  if ( key)
    str = key->query_code();

  if ( ( str == key_code ) || ( str == "zap" ) ) {
    write("\nYou lock the " + direction + " door.\n");
    set_locked( 1);
    partner_door->set_locked(is_locked);
  }
  else
    write("The key doesn't fit!\n");

  return;
}


int lock( string str)
{
  object ob;
  int tmp;
  string type, door;

  if ( !str)
    return 0;

  if ( str == "door" ) {
    write( "Lock the door with what?\n");
    return 1;
  }
  type = 0;

  if (sscanf(str, "%s with %s", door, type) == 2) {
    tmp = this_door( door);
    if ( tmp != 2 )
      return tmp;

    tmp = this_key( type);
    if ( tmp != 2 )
      return 1;
    ob = get_key( type);

    lock_door( ob);
    return 1;
  }
}




