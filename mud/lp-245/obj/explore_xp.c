/* An object which will give a small amount of XP to a player the first time
 * that player visits that location.
 * A level 1 player will get 30 experience points.
 * A level 19 player will get 3374 experience points.
 * Defaults are given, so if you only need one of such object, no
 * configuration is needed.
 */
#define XP_FOR_LEVEL_ONE      30
#define DELIMITER             "^!"
#define DEFAULT_NAME          "playerlogger"
#define DEFAULT_FILE_NAME     DEFAULT_NAME+"file"

string SaveString;
string SaveName;
string FailMessage;
string OKMessage;

void set_fail_message( string str)
{
    FailMessage = str;
}

void set_ok_message( string str)
{
    OKMessage = str;
}

void set_name(string str)
{
    string TmpString;
    if (object_name(previous_object())[0..4] != "/obj" &&
	!creator(this_object())&&!creator(previous_object())) {
	write("Illegal usage, Savefile-path is illegal.\n");
	destruct(this_object());
        return;
    }
    if ( TmpString = creator( this_object()) || ( TmpString = creator( previous_object())))
      SaveName = "players/"+TmpString+"/";
    else
      SaveName = "obj/";
    if ( str)
	SaveName=SaveName+str;
    else
	SaveName=SaveName+DEFAULT_FILE_NAME;
    if( !restore_object( SaveName)) {
	SaveString = DELIMITER;
	save_object( SaveName);
	log_file( "PlayerLogger",TmpString + " <" + SaveName +"> "+ctime(time())+"\n");
    }
}

string query_name()
{
    return SaveName;
}

int id(string str)
{
    return (str==DEFAULT_NAME)||(SaveName&&(str==SaveName));
}

string short()
{
    if ( this_player() && this_player()->query_level() >= 20)
	return "A player logger, name: <"+SaveName+">";
    else
	return 0;
}

int PlayerHasVisited( string str)
{
    if ( str)
	return sscanf( SaveString, "%s"+DELIMITER+str+DELIMITER, str);
    else
	return 0;
}

void AddPlayerToList( string str)
{
    if ( str) {
	SaveString = SaveString+str+DELIMITER;
	save_object( SaveName);
    }
}

int AmountOfPlayerXP()
{
    int Amount, Level;
    if ( this_player()) {
	Amount = XP_FOR_LEVEL_ONE;
	Level = this_player()->query_level();
	while( Level > 0) {
	    Amount = ( Amount * 13) / 10;
	    Level -= 1;
	}
	return Amount;
    }
    return 0;
}

void init()
{
    if ( this_player() && query_ip_number( this_player())) {
	if (! PlayerHasVisited( this_player()->query_name())) {
	    if ( OKMessage)
		tell_object( this_player(), OKMessage);
	    else
		tell_object( this_player(), "You feel more experienced.\n");
	    this_player()->add_exp( AmountOfPlayerXP());
	    AddPlayerToList( this_player()->query_name());
	}
	else
	    if ( FailMessage)
		tell_object( this_player(), FailMessage);
    }
}

void reset(int arg)
{
    if(arg)
	return;
    if(!SaveName)
	SaveName = DEFAULT_FILE_NAME;
    set_name( SaveName);
}
